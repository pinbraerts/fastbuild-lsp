pub mod parser_pool;
pub mod queries;
pub mod error;
pub mod helpers;

use dashmap::DashMap;
use helpers::W;
use queries::Queries;
use tokio::sync::OnceCell;
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

    fn define(&mut self, url: &Url, node: W<Node>) -> std::result::Result<(), W<Diagnostic>> {
        match self.references.last() {
            Some((location, Reference::Define)) => Err(node.error(
                "macro redefinition"
            ).with(Some(vec![DiagnosticRelatedInformation {
                location: location.clone(),
                message: "defined here".into(),
            }]))),
            _ => {
                self.value = true;
                self.references.push((Location::new(url.clone(), node.into()), Reference::Define));
                Ok(())
            },
        }
    }

    fn undefine(&mut self, url: &Url, node: W<Node>) -> std::result::Result<(), W<Diagnostic>> {
        match self.references.last() {
            Some((location, Reference::Undef)) => Err(Some(location.clone())),
            Some((_, Reference::Define)) => {
                self.value = false;
                self.references.push((Location::new(url.clone(), node.into()), Reference::Undef));
                Ok(())
            },
            _ => Err(None),
        }.map_err(|location| node.error("trying to undefine undefined macro")
            .with(location.map(|location| vec![DiagnosticRelatedInformation {
                location,
                message: "note: last undefined".into(),
            }]))
        )
    }

}

static NULL: OnceCell<Tree> = OnceCell::const_new();

#[derive(Debug)]
struct FileInfo {
    url: Url,
    version: i32,
    content: String,
    tree: Tree,
    diagnostics: Vec<Diagnostic>,
    definitions: HashMap<String, Symbol>,
    semantic_tokens: Vec<SemanticToken>,
    once: bool,
}

type IfStack<'tree> = Vec<(bool, W<Node<'tree>>, Option<W<Node<'tree>>>)>;

impl FileInfo {

    fn new(url: Url, version: i32, content: String, parser: &mut Parser) -> Result<Self> {
        let tree = parser.parse(content.as_bytes(), None).ok_or(Error::Parse)?;
        Ok(FileInfo {
            url,
            version,
            content,
            tree,
            diagnostics: Default::default(),
            definitions: Default::default(),
            semantic_tokens: Default::default(),
            once: Default::default(),
        })
    }

    fn preprocessor_pass(&mut self) {
        let tree = std::mem::replace(&mut self.tree, NULL.get().expect("null tree is not initialized").clone());
        let mut cursor = tree.walk();
        let mut if_stack = vec![];
        let mut run = true;
        while run {
            let traverse = self.preprocessor_directive(cursor.node().into(), &mut if_stack);
            match traverse {
                Err(diagnostic) => self.diagnostics.push(diagnostic.0),
                Ok(true) => {
                    if cursor.goto_first_child() {
                        continue;
                    }
                },
                _ => {},
            }
            while !cursor.goto_next_sibling() {
                if !cursor.goto_parent() {
                    run = false;
                    break;
                }
            }
        }
        for (_, n_if, n_else) in if_stack {
            self.diagnostics.push(n_if.error("missing #endif directive").with(
                n_else.map(|e| e.related(&self.url, "#else here"))
            ).0);
        }
        drop(cursor);
        self.tree = tree;
    }

    fn preprocess_expression(&self, node: W<Node>) -> std::result::Result<bool, W<Diagnostic>> {
        Ok(match node.kind() {
            "string" => node.is_empty(),
            "decimal" => node
                .text(self.content.as_bytes())?
                .parse::<i32>()
                .unwrap_or_default() != 0,
            "identifier" => {
                let text = node.text(self.content.as_bytes())?;
                self.definitions.get(text).ok_or_else(|| node.error("undefined"))?.value
            },
            "not" => {
                !self.preprocess_expression(node.expect("right")?)?
            },
            "and" => {
                self.preprocess_expression(node.expect("left")?)?
                &&
                self.preprocess_expression(node.expect("right")?)?
            },
            "or" => {
                self.preprocess_expression(node.expect("left")?)?
                ||
                self.preprocess_expression(node.expect("right")?)?
            },
            "call" => {
                let argument = node.expect("arguments")?;
                match node.expect("name")?.text(self.content.as_bytes())? {
                    "exists" => self.preprocess_expression(argument)?,
                    "file_exists" => self.find_file(node)?,
                    _ => {
                        return Err(node.error("unknown macro function"));
                    }
                }
            },
            _ => {
                return Err(node.error("unknown macro expression"));
            }
        })
    }

    fn find_file(&self, node: W<Node>) -> std::result::Result<bool, W<Diagnostic>> {
        let string = node.text(self.content.as_bytes())?;
        let filename = string.get(1..string.len() - 1).ok_or_else(|| node.error("not a string literal"))?;
        let path = Path::new(filename);
        if path.exists() {
            return Ok(true);
        }
        self.url.to_file_path().ok()
            .and_then(|x| x.parent().map(|x| x.to_owned()))
            .map(|x| x.join(path).exists())
            .ok_or_else(|| node.error("could not fild file"))
    }

    fn preprocessor_directive<'tree>(&mut self, node: W<Node<'tree>>, if_stack: &mut IfStack<'tree>) -> std::result::Result<bool, W<Diagnostic>> {
        let skip = if_stack.last().map(|(if_condition, _, n_else)| *if_condition == n_else.is_none()).unwrap_or_default();
        match node.kind() {
            "preprocessor_define" => if !skip {
                let variable = node.expect("variable")?;
                let text = variable.text(self.content.as_bytes())?.to_owned();
                let entry = self.definitions.entry(text).or_default();
                entry.define(&self.url, variable)?;
            },
            "preprocessor_import" => if !skip {
                let variable = node.expect("variable")?;
                let text = variable.text(self.content.as_bytes())?.to_owned();
                let entry = self.definitions.entry(text).or_default();
                entry.define(&self.url, variable)?;
            },
            "preprocessor_undef" => if !skip {
                let variable = node.expect("variable")?;
                let text = variable.text(self.content.as_bytes())?.to_owned();
                let entry = self.definitions.entry(text).or_default();
                entry.undefine(&self.url, variable)?;
            },
            "preprocessor_once" => if !skip { self.once = true; },
            "preprocessor_if" => {
                let condition = node.expect("condition")?;
                let result = self.preprocess_expression(condition);
                if_stack.push((result.unwrap_or_default(), node, None));
            },
            "preprocessor_else" => {
                match if_stack.last() {
                    Some((_, _, None)) => {
                        if let Some(last) = if_stack.last_mut() {
                            last.2 = Some(node);
                        }
                    },
                    Some((_, _, Some(n_else))) => {
                        return Err(node.error("duplicate #else directive").with(
                            Some(n_else.related(&self.url, "previous #else here"))
                        ));
                    },
                    None => {
                        return Err(node.error("missing #if directive"));
                    },
                };
            },
            "preprocessor_endif" => {
                let last = if_stack.last().ok_or_else(|| node.error("expected #if"))?;
                if let Some((start, end)) = match last {
                    (true, _, None) => None,
                    (true, _, Some(n_else)) => Some((n_else, node)),
                    (false, n_if, n_else) => Some((n_if, n_else.unwrap_or(node))),
                } {
                    for line in start.end_position().row + 1..end.start_position().row {
                        self.semantic_tokens.push(SemanticToken {
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
                return Ok(true);
            },
        }
        Ok(false)
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
        let mut scope = entry.insert(FileInfo::new(url.clone(), version, content, parser.deref_mut())?);
        self.syntax_pass(&mut scope);
        scope.preprocessor_pass();
        self.client.publish_diagnostics(
            url,
            scope.diagnostics.clone(),
            Some(scope.version)
        ).await;
        Ok(())
    }

    fn syntax_pass(&self, scope: &mut FileInfo) {
        let mut cursor = QueryCursor::new();
        let matches = cursor.matches(&self.queries.error, scope.tree.root_node(), scope.content.as_bytes());
        scope.diagnostics.extend(
            matches
            .filter_map(|m| m.captures.first())
            .map(|c| W(c.node).error("Syntax Error").0)
        );
    }

    fn new(client: Client) -> Result<Self> {
        let language = tree_sitter_fastbuild::language();
        let queries = Queries::new(&language)?;
        let mut parser = Parser::new();
        parser.set_language(&language)?;
        let tree = parser.parse("", None).ok_or(Error::Parse)?;
        let _ = NULL.set(tree);
        let parsers = new_pool(language)?;
        let files = DashMap::new();
        Ok(Backend { client, parsers, files, queries })
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
            .map(|f| f.semantic_tokens.iter().map(|t| W(*t).delta(&mut prev)).collect())
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

    info!("starting LSP server");
    let (service, socket) = LspService::new(|c| Backend::new(c).expect("failed to initialize backend"));

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    Server::new(stdin, stdout, socket).serve(service).await;
    Ok(())
}

#[cfg(test)]
mod tests {

    use super::*;

    async fn make() -> LspService<Backend> {
        let (service, _) = LspService::new(|client| Backend::new(client).expect("failed to initialize backend"));
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
        assert_eq!(diagnostic.message, "macro redefinition");
        let binding = diagnostic.related_information.clone().expect("no related information");
        let related = binding.first().expect("no related entrys");
        assert_eq!(related.message, "defined here");
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
        assert_eq!(diagnostic.message, "macro redefinition");
        let binding = diagnostic.related_information.clone().expect("no related information");
        let related = binding.first().expect("no related entrys");
        assert_eq!(related.message, "defined here");
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
        assert_eq!(diagnostic.message, "trying to undefine undefined macro");
        assert_eq!(diagnostic.range.start.line, 0);
        assert_eq!(diagnostic.range.start.character, 7);
        assert_eq!(diagnostic.range.end.line, 0);
        assert_eq!(diagnostic.range.end.character, 8);
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

    #[tokio::test]
    #[tokio::test]
    async fn preprocessor_if_missing() {
        let (uri, service) = make_with_file("memory://preprocessor_if_missing.bff", "#else").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "expected #if");
        assert_eq!(diagnostic.range.start.line, 0);
        assert_eq!(diagnostic.range.start.character, 1);
        assert_eq!(diagnostic.range.end.line, 0);
        assert_eq!(diagnostic.range.end.character, 5);
    }

    #[tokio::test]
    async fn preprocessor_endif_missing() {
        let (uri, service) = make_with_file("memory://preprocessor_if_missing.bff", "#if 1").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "expected #endif");
        assert_eq!(diagnostic.range.start.line, 0);
        assert_eq!(diagnostic.range.start.character, 1);
        assert_eq!(diagnostic.range.end.line, 0);
        assert_eq!(diagnostic.range.end.character, 5);
    }

    #[tokio::test]
    async fn preprocessor_unknown() {
        let (uri, service) = make_with_file("memory://preprocessor_unknown.bff", "#unknown").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "unknown directive");
        assert_eq!(diagnostic.range.start.line, 0);
        assert_eq!(diagnostic.range.start.character, 1);
        assert_eq!(diagnostic.range.end.line, 0);
        assert_eq!(diagnostic.range.end.character, 8);
    }

}
