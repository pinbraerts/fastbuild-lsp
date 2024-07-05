pub mod parser_pool;
pub mod queries;
pub mod error;
pub mod helpers;

use dashmap::DashMap;
use helpers::W;
use queries::Queries;
use tree_sitter::{Node, Parser, QueryCursor, Tree};

use std::collections::HashMap;
use std::fs::File;

use std::ops::DerefMut;
use std::path::Path;

use parser_pool::ParserPool;
use tower_lsp::{jsonrpc, lsp_types::*};
use tower_lsp::{Client, LanguageServer, Server, LspService};
use error::{Result, Error};

use tracing::{info, Level, warn};

use crate::parser_pool::new_pool;

#[derive(Debug, PartialEq, Eq)]
enum Reference {
    Define,
    Undef,
}

#[derive(Debug, Default)]
struct Symbol {
    value: bool,
    references: Vec<(Location, Reference)>,
}

impl Symbol {

    fn define(&mut self, url: Url, node: Node) -> Option<Location> {
        let location = match self.references.last() {
            Some((location, Reference::Define)) => Some(location.clone()),
            _ => None,
        };
        self.value = true;
        self.references.push((Location::new(url, W(&node).into()), Reference::Define));
        location
    }

    fn undefine(&mut self, url: Url, node: Node) -> Option<Option<Location>> {
        let location = match self.references.last() {
            Some((location, Reference::Undef)) => Some(Some(location.clone())),
            Some((_, Reference::Define)) => None,
            _ => Some(None),
        };
        self.value = false;
        self.references.push((Location::new(url, W(&node).into()), Reference::Undef));
        location
    }

}

#[derive(Debug)]
struct FileInfo {
    version: i32,
    content: String,
    tree: Tree,
    diagnostics: Vec<Diagnostic>,
    definitions: HashMap<String, Symbol>,
    semantic_tokens: Vec<SemanticToken>,
    once: bool,
}

impl FileInfo {
    fn new(version: i32, parser: &mut Parser, content: String) -> Result<Self> {
        let tree = parser.parse(content.as_bytes(), None).ok_or(Error::Parse)?;
        Ok(FileInfo {
            version,
            content,
            tree,
            diagnostics: Vec::default(),
            definitions: HashMap::default(),
            semantic_tokens: Vec::default(),
            once: bool::default(),
        })
    }
}

struct Backend {
    client: Client,
    parsers: ParserPool,
    files: DashMap<Url, FileInfo>,
    queries: Queries,
}

impl Backend {

    async fn process_file(&self, url: Url, content: String, version: i32) -> Result<()> {
        let entry = self.files.entry(url.clone());
        let mut parser = self.parsers.get().await?;
        let mut scope = entry.insert(FileInfo::new(version, parser.deref_mut(), content)?);
        self.syntax_pass(&mut scope).await;
        self.preprocessor_pass(url.clone(), &mut scope).await?;
        self.client.publish_diagnostics(url.clone(), scope.diagnostics.clone(), Some(scope.version)).await;
        Ok(())
    }

    async fn syntax_pass(&self, scope: &mut FileInfo) {
        let mut cursor = QueryCursor::new();
        let matches = cursor.matches(&self.queries.error, scope.tree.root_node(), scope.content.as_bytes());
        scope.diagnostics.extend(
            matches
            .filter_map(|m| m.captures.first())
            .map(|c| Diagnostic::new(
                W(&c.node).into(),
                Some(DiagnosticSeverity::ERROR),
                None,
                None,
                "Syntax Error".into(),
                None,
                None,
            ))
        );
    }

    async fn preprocessor_pass(&self, url: Url, scope: &mut FileInfo) -> Result<()> {
        let mut cursor = scope.tree.walk();
        let mut if_stack: Vec<(bool, Node, Option<Node>)> = vec![];
        let mut run = true;
        while run {
            let skip = if_stack.last().map(|(if_condition, _, n_else)| *if_condition == n_else.is_none()).unwrap_or_default();
            let node = cursor.node();
            match node.kind() {
                "preprocessor_define" => if !skip {
                    let variable = node.child_by_field_name("variable").ok_or(Error::Parse)?;
                    let text = variable.utf8_text(scope.content.as_bytes())?.to_owned();
                    let entry = scope.definitions.entry(text).or_default();
                    if let Some(location) = entry.define(url.clone(), variable) {
                        scope.diagnostics.push(Diagnostic::new(
                            W(&variable).into(),
                            Some(DiagnosticSeverity::ERROR),
                            None,
                            None,
                            "error: macro redefinition".into(),
                            Some(vec![DiagnosticRelatedInformation {
                                location,
                                message: "note: defined here".into(),
                            }]),
                            None,
                        ));
                    }
                },
                "preprocessor_import" => if !skip {
                    let variable = node.child_by_field_name("variable").ok_or(Error::Parse)?;
                    let text = variable.utf8_text(scope.content.as_bytes())?.to_owned();
                    let entry = scope.definitions.entry(text).or_default();
                    if let Some(location) = entry.define(url.clone(), variable) {
                        scope.diagnostics.push(Diagnostic::new(
                            W(&variable).into(),
                            Some(DiagnosticSeverity::ERROR),
                            None,
                            None,
                            "error: macro redefinition".into(),
                            Some(vec![DiagnosticRelatedInformation {
                                location,
                                message: "note: defined here".into(),
                            }]),
                            None,
                        ));
                    }
                },
                "preprocessor_undef" => if !skip {
                    let variable = node.child_by_field_name("variable").ok_or(Error::Parse)?;
                    let text = variable.utf8_text(scope.content.as_bytes())?.to_owned();
                    let entry = scope.definitions.entry(text).or_default();
                    if let Some(location) = entry.undefine(url.clone(), variable) {
                        scope.diagnostics.push(Diagnostic::new(
                            W(&variable).into(),
                            Some(DiagnosticSeverity::ERROR),
                            None,
                            None,
                            "error: trying to undefine undefined macro".into(),
                            location.map(|location| vec![DiagnosticRelatedInformation {
                                location,
                                message: "note: last undefined".into(),
                            }]),
                            None,
                        ));
                    }
                },
                "preprocessor_once" => if !skip { scope.once = true; },
                "preprocessor_if" => {
                    let condition = node.child_by_field_name("condition").ok_or(Error::Parse)?;
                    let result = self.preprocess_expression(&url, scope, condition)?;
                    if_stack.push((result, node, None));
                },
                "preprocessor_else" => {
                    match if_stack.last() {
                        Some((_, _, None)) => {
                            if let Some(last) = if_stack.last_mut() {
                                last.2 = Some(node);
                            }
                        },
                        Some((_, _, Some(n_else))) => {
                            scope.diagnostics.push(Diagnostic::new(
                                W(&node).into(),
                                Some(DiagnosticSeverity::ERROR),
                                None,
                                None,
                                "error: duplicate #else directive".into(),
                                Some(vec![DiagnosticRelatedInformation {
                                    location: Location::new(url.clone(), W(n_else).into()),
                                    message: "note: previous #else here".into(),
                                }]),
                                None,
                            ));
                        },
                        None => {
                            scope.diagnostics.push(Diagnostic::new(
                                W(&node).into(),
                                Some(DiagnosticSeverity::ERROR),
                                None,
                                None,
                                "error: missing #if directive".into(),
                                None,
                                None,
                            ));
                        },
                    };
                },
                "preprocessor_endif" => {
                    let last = if_stack.last().ok_or(Error::Parse)?;
                    if let Some((start, end)) = match last {
                        (true, _, None) => None,
                        (true, _, Some(n_else)) => Some((n_else, node)),
                        (false, n_if, n_else) => Some((n_if, n_else.unwrap_or(node))),
                    } {
                        for line in start.end_position().row + 1..end.start_position().row {
                            scope.semantic_tokens.push(SemanticToken {
                                delta_line: line as u32,
                                delta_start: 0,
                                length: u32::max_value(),
                                token_type: 0,
                                token_modifiers_bitset: 0,
                            });
                        }
                    }
                    if_stack.pop();
                },
                _ => {
                    if cursor.goto_first_child() {
                        continue;
                    }
                },
            }
            while !cursor.goto_next_sibling() {
                if !cursor.goto_parent() {
                    run = false;
                    break;
                }
            }
        }
        for (_, n_if, n_else) in if_stack {
            scope.diagnostics.push(Diagnostic::new(
                W(&n_if).into(),
                Some(DiagnosticSeverity::ERROR),
                None,
                None,
                "error: missing #endif directive".into(),
                n_else.map(|e| vec![DiagnosticRelatedInformation {
                    message: "note: #else here".into(),
                    location: Location::new(url.clone(), W(&e).into()),
                }]),
                None,
            ));
        }
        Ok(())
    }

    fn preprocess_expression(&self, url: &Url, scope: &FileInfo, node: Node) -> Result<bool> {
        Ok(match node.kind() {
            "string" => W(node).is_empty(),
            "decimal" => node
                .utf8_text(scope.content.as_bytes())?
                .parse::<i32>()
                .unwrap_or_default() != 0,
            "identifier" => {
                let text = node.utf8_text(scope.content.as_bytes())?;
                let symbol = scope.definitions.get(text).ok_or(Error::SymbolNotFound)?;
                symbol.value
            },
            "not" => {
                !self.preprocess_expression(url, scope, node.named_child(1).ok_or(Error::Parse)?)?
            },
            "and" => {
                self.preprocess_expression(url, scope, node.child_by_field_name("left").ok_or(Error::Parse)?)?
                &&
                self.preprocess_expression(url, scope, node.child_by_field_name("right").ok_or(Error::Parse)?)?
            },
            "or" => {
                self.preprocess_expression(url, scope, node.child_by_field_name("left").ok_or(Error::Parse)?)?
                ||
                self.preprocess_expression(url, scope, node.child_by_field_name("right").ok_or(Error::Parse)?)?
            },
            "call" => {
                let argument = node.child_by_field_name("arguments").ok_or(Error::Parse)?;
                match node.child_by_field_name("name").ok_or(Error::Parse)?.utf8_text(scope.content.as_bytes())? {
                    "exists" => self.preprocess_expression(url, scope, argument)?,
                    "file_exists" => {
                        let string = node.utf8_text(scope.content.as_bytes())?;
                        self.find_file(url, string.get(1..string.len() - 1).ok_or(Error::Parse)?)?
                    },
                    _ => {
                        return Err(Box::new(Error::Parse));
                    }
                }
            },
            _ => {
                return Err(Box::new(Error::Parse));
            }
        })
    }

    fn find_file(&self, url: &Url, filename: &str) -> Result<bool> {
        let path = Path::new(filename);
        if path.exists() {
            return Ok(true);
        }
        let parent = url.to_file_path().map_err(|_| Error::FileNotFound)?;
        Ok(parent.parent().ok_or(Error::FileNotFound)?.join(path).exists())
    }

}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {

    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                // declaration_provider: Some(DeclarationCapability::Simple(true)),
                // definition_provider: Some(OneOf::Left(true)),
                // hover_provider: Some(HoverProviderCapability::Simple(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![
                                        DocumentFilter {
                                            language: Some("fastbuild".into()),
                                            scheme: Some("file".into()),
                                            pattern: Some("*.bff".into())
                                        }
                                    ])
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                legend: SemanticTokensLegend {
                                    token_types: vec!(SemanticTokenType::COMMENT),
                                    ..Default::default()
                                },
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                                ..Default::default()
                            },
                            static_registration_options: Default::default(),
                        },
                    )
                ),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "server initialized!")
        .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        info!("text document open {:?}", params.text_document.uri);
        let document = params.text_document;
        if document.language_id != "fastbuild" {
            return;
        }
        self.process_file(
            document.uri,
            document.text,
            document.version
        ).await.unwrap();
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        info!("text document change {:?}", params.text_document.uri);
        let document = params.text_document;
        if params.content_changes.len() != 1 {
            warn!("file changes: {:?}", params.content_changes);
            return;
        }
        let _ = self.process_file(
            document.uri,
            std::mem::take(&mut params.content_changes[0].text),
            document.version
        ).await;
    }

    async fn semantic_tokens_full(&self, params: SemanticTokensParams) -> jsonrpc::Result<Option<SemanticTokensResult>> {
        info!("textDocument/semanticTokens/full {:?}", params);
        let mut prev = SemanticToken::default();
        Ok(self.files
            .get(&params.text_document.uri)
            .map(|f| f.semantic_tokens.iter().map(|t| W(t).delta(&mut prev)).collect())
            .map(|t| SemanticTokensResult::Tokens(SemanticTokens {
                data: t,
                ..Default::default()
            }))
        )
    }

    // async fn semantic_tokens_range(&self, params: SemanticTokensRangeParams) -> jsonrpc::Result<Option<SemanticTokensRangeResult>> {
    //     info!("textDocument/semanticTokens/full {:?}", params);
    //     Ok(self.files
    //         .get(&params.text_document.uri)
    //         .map(|f| f.semantic_tokens.clone())
    //         .map(|t| SemanticTokensRangeResult::Tokens(SemanticTokens {
    //             data: t,
    //             ..Default::default()
    //         }))
    //     )
    // }

}

#[tokio::main]
async fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let args : Vec<String> = std::env::args().collect();

    let filename = args
        .get(1)
        .cloned()
        .unwrap_or_else(|| "/home/pinbraerts/src/fastbuild-lsp/log.log".to_string());
    if let Ok(file) = File::create(filename) {
        tracing_subscriber::fmt()
            .with_writer(file)
            .with_target(false)
            .with_max_level(Level::DEBUG)
            .init();
    }

    let language = tree_sitter_fastbuild::language();
    let queries = Queries::new(&language)?;
    let parsers = new_pool(language)?;
    let files = DashMap::new();

    info!("starting LSP server");
    let (service, socket) = LspService::new(|client| Backend {
        client,
        parsers,
        files,
        queries,
    });

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    Server::new(stdin, stdout, socket).serve(service).await;
    Ok(())
}

#[cfg(test)]
mod tests {

    use super::*;

    async fn make() -> LspService<Backend> {
        let language = tree_sitter_fastbuild::language();
        let queries = Queries::new(&language).expect("failed to load queries");
        let parsers = new_pool(language).expect("failed to create parser pool");
        let files = DashMap::new();
        let (service, _) = LspService::new(|client| Backend {
            client,
            files,
            parsers,
            queries,
        });
        let backend = service.inner();
        backend.initialize(InitializeParams::default()).await.expect("failed to initialize server");
        backend.initialized(InitializedParams{}).await;
        service
    }

    async fn make_with_file(uri: &str, content: &str) -> (Url, LspService<Backend>) {
        let uri = Url::parse(uri).expect("failed to parse url");
        let service = make().await;
        let backend = service.inner();
        backend.did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "fastbuild".into(),
                version: 0,
                text: content.into(),
            },
        }).await;
        (uri, service)
    }

    #[tokio::test]
    async fn syntax_error() {
        let (uri, service) = make_with_file("memory://syntax_error.bff", ".A =").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "Syntax Error");
    }

    #[tokio::test]
    async fn redefinition() {
        let (uri, service) = make_with_file("memory://redefinition.bff", "#define A\n#define A").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "error: macro redefinition");
        let binding = diagnostic.related_information.clone().expect("no related information");
        let related = binding.first().expect("no related entrys");
        assert_eq!(related.message, "note: defined here");
        assert_eq!(related.location.uri, uri);
        assert_eq!(related.location.range.start, Position::new(0, 8));
        assert_eq!(related.location.range.end, Position::new(0, 9));
    }

    #[tokio::test]
    async fn reimport() {
        let (uri, service) = make_with_file("memory://reimport.bff", "#import A\n#define A").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "error: macro redefinition");
        let binding = diagnostic.related_information.clone().expect("no related information");
        let related = binding.first().expect("no related entrys");
        assert_eq!(related.message, "note: defined here");
        assert_eq!(related.location.uri, uri);
        assert_eq!(related.location.range.start, Position::new(0, 8));
        assert_eq!(related.location.range.end, Position::new(0, 9));
    }

    #[tokio::test]
    async fn undef() {
        let (uri, service) = make_with_file("memory://undefine.bff", "#define A\n#undef A").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.diagnostics, vec!());
    }

    #[tokio::test]
    async fn undef_undefined() {
        let (uri, service) = make_with_file("memory://undefined.bff", "#undef A").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "error: trying to undefine undefined macro");
    }

    #[tokio::test]
    async fn undef_import() {
        let (uri, service) = make_with_file("memory://undef_import.bff", "#import A\n#undef A").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.diagnostics, vec!());
    }

    #[tokio::test]
    async fn once() {
        let (uri, service) = make_with_file("memory://once.bff", "#once").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert!(scope.once);
    }

    #[tokio::test]
    async fn preprocessor_if() {
        let (uri, service) = make_with_file("memory://preprocessor_if.bff", "#if 1\n.A = 3\n#else\n.B = 4\n#endif").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        let token = scope.semantic_tokens.first().expect("no semantic tokens");
        assert_eq!(token.delta_line, 3);
        assert_eq!(token.delta_start, 0);
        assert_eq!(token.length, u32::max_value());
        assert_eq!(token.token_modifiers_bitset, 0);
        assert_eq!(token.token_type, 0);
    }

    #[tokio::test]
    async fn preprocessor_else() {
        let (uri, service) = make_with_file("memory://preprocessor_else.bff", "#if 0\n.A = 3\n#else\n.B = 4\n#endif").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        let token = scope.semantic_tokens.first().expect("no semantic tokens");
        assert_eq!(token.delta_line, 1, "mismatching lines");
        assert_eq!(token.delta_start, 0, "mismatching columns");
        assert_eq!(token.length, u32::max_value(), "mismatching length");
        assert_eq!(token.token_modifiers_bitset, 0, "mismatching modifiers");
        assert_eq!(token.token_type, 0, "mismatching types");
    }

}
