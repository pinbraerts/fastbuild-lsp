pub mod parser_pool;
pub mod error;
pub mod helpers;

use dashmap::mapref::entry::Entry;
use dashmap::DashMap;
use futures::future::{join_all, BoxFuture};
use futures::FutureExt;
use helpers::W;
use tokio::io::AsyncReadExt;
use tokio::sync::OnceCell;
use tree_sitter::{Node, Parser, Tree};

use tokio::fs::File;

use std::collections::{HashMap, HashSet};
use std::env::VarError::NotPresent;
use parser_pool::ParserPool;
use tower_lsp::{jsonrpc, lsp_types::*};
use tower_lsp::{Client, LanguageServer, Server, LspService};
use error::{Result, Error};

use tracing::{info, warn};

use crate::parser_pool::new_pool;

#[derive(Debug, PartialEq, Eq, Clone)]
enum Reference {
    Define,
    Undef,
    Ref,
}

#[derive(Debug, Default, Clone)]
struct Symbol {
    value: bool,
    references: Vec<(Location, Reference)>,
    documentation: String,
}

impl Symbol {

    fn define(&mut self, url: &Url, node: W<Node>, documentation: String) -> std::result::Result<(), W<Diagnostic>> {
        match self.references.last() {
            Some((location, Reference::Define)) => Err(node.error(
                "macro redefinition"
            ).with(Some(vec![DiagnosticRelatedInformation {
                location: location.clone(),
                message: "defined here".into(),
            }]))),
            _ => {
                self.value = true;
                self.documentation = documentation;
                self.references.push((node.url(url), Reference::Define));
                Ok(())
            },
        }
    }

    fn undefine(&mut self, url: &Url, node: W<Node>) -> std::result::Result<(), W<Diagnostic>> {
        match self.references.last() {
            Some((location, Reference::Undef)) => Err(Some(location.clone())),
            Some(_) => {
                self.value = false;
                self.references.push((node.url(url), Reference::Undef));
                Ok(())
            },
            _ => Err(None),
        }.map_err(|location| node.error("trying to undefine undefined macro")
            .with(location.map(|location| vec![DiagnosticRelatedInformation {
                location,
                message: "undefined here".into(),
            }]))
        )
    }

    fn reference(&mut self, url: &Url, node: W<Node>) -> std::result::Result<bool, W<Diagnostic>> {
        match self.references.last() {
            //Some((location, Reference::Undef)) => Err(Some(location.clone())),
            Some(_) => {
                self.references.push((node.url(url), Reference::Ref));
                Ok(self.value)
            },
            _ => Err(None),
        }.map_err(|location| node.error("accessing undefined variable")
            .with(location.map(|location| vec![DiagnosticRelatedInformation {
                location,
                message: "undefined here".into(),
            }]))
        )
    }

}

static NULL: OnceCell<Tree> = OnceCell::const_new();
type Definitions = HashMap<String, Symbol>;

#[derive(Debug, Default)]
struct Scope {
    version: i32,
    diagnostics: Vec<Diagnostic>,
    definitions: Definitions,
    semantic_tokens: Vec<SemanticToken>,
    once: bool,
    content: String,
    references: HashSet<Url>,
}

type IfStack<'tree> = Vec<(bool, W<Node<'tree>>, Option<W<Node<'tree>>>)>;

impl Scope {

    fn new(version: i32, content: String) -> Self {
        Scope { version, content, ..Default::default() }
    }

    fn define(&mut self, url: &Url, node: W<Node>, documentation: String) -> std::result::Result<(), W<Diagnostic>> {
        let name = node.text(self.content.as_bytes())?;
        self.definitions.entry(name.into()).or_default().define(url, node, documentation)
    }

    fn undefine(&mut self, url: &Url, node: W<Node>) -> std::result::Result<(), W<Diagnostic>> {
        let name = node.text(self.content.as_bytes())?;
        self.definitions.entry(name.into()).or_default().undefine(url, node)
    }

    fn reference(&mut self, url: &Url, node: W<Node>) -> std::result::Result<bool, W<Diagnostic>> {
        let name = node.text(self.content.as_bytes())?;
        self.definitions.entry(name.into()).or_default().reference(url, node)
    }

    fn find_env_variable(&self, node: W<Node>) -> std::result::Result<(), W<Diagnostic>> {
        let name = node.text(self.content.as_bytes())?;
        match std::env::var(name) {
            Err(NotPresent) => Err(node.error("environment variable not found")),
            _ => Ok(()),
        }
    }

}

struct Backend {
    client: Client,
    parsers: ParserPool,
    files: DashMap<Url, Scope>,
    builtins: HashMap<Url, String>,
}

impl Backend {

    async fn parse(&self, content: impl Into<&[u8]>) -> Result<Tree> {
        Ok(self.parsers.get().await?.parse(content.into(), None).ok_or(Error::Parse)?)
    }

    async fn on_file_open(&self, url: Url) -> Result<()> {
        let entry = self.files.entry(url.clone());
        if let Entry::Occupied(_) = entry {
            return Ok(());
        }
        drop(entry);
        let content = self.get_content(url.clone()).await.ok_or(Error::Parse)?;
        self.on_file_update(url, 0, content).await
    }

    async fn on_file_update(&self, url: Url, version: i32, content: String) -> Result<()> {
        if let Entry::Occupied(entry) = self.files.entry(url.clone()) {
            if entry.get().version > version {
                return Ok(());
            }
        }
        self.on_text_change(url, version, content).await
    }

    fn on_text_change(&self, url: Url, version: i32, content: String) -> BoxFuture<Result<()>> {
        async move {
            self.client.log_message(MessageType::LOG, format!("parsing {}", url)).await;
            let tree = self.parse(content.as_bytes()).await?;
            self.on_tree_change(url, version, content, tree).await
        }.boxed()
    }

    async fn on_tree_change(&self, url: Url, version: i32, content: String, tree: Tree) -> Result<()> {
        self.client.log_message(MessageType::LOG, format!("analysing {}", url)).await;
        let mut cursor = tree.walk();
        let mut if_stack = vec![];
        let mut run = true;
        let mut scope = Scope::new(version, content);
        if !self.builtins.contains_key(&url) {
            for (builtin_url, _) in self.builtins.iter() {
                if let Some(file_scope) = self.files.get(builtin_url) {
                    scope.definitions.extend(file_scope.definitions.clone());
                    scope.references.insert(builtin_url.clone());
                }
            }
        }
        let mut documentation = String::new();
        while run {
            let traverse = self.enter_node(&url, &mut scope, cursor.node().into(), &mut if_stack, &mut documentation).await;
            match traverse {
                Err(diagnostic) => scope.diagnostics.push(diagnostic.0),
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
            scope.diagnostics.push(n_if.error("expected #endif").with(
                n_else.map(|e| e.related(&url, "#else here"))
            ).0);
        }
        drop(cursor);
        self.on_semantics_change(url, scope).await
    }

    async fn on_semantics_change(&self, url: Url, scope: Scope) -> Result<()> {
        self.client.log_message(MessageType::LOG, format!("updating diagnostics {}", url)).await;
        self.client.publish_diagnostics(
            url.clone(),
            scope.diagnostics.clone(),
            Some(scope.version)
        ).await;
        let _ = self.files.insert(url.clone(), scope);
        let references: Vec<Url> = self.files.iter().filter_map(|v| if v.value().references.contains(&url) { Some(v.key().clone()) } else { None }).collect();
        let should_refresh = !references.is_empty();
        let items: Vec<(Url, Scope)> = references.into_iter().filter_map(|v| self.files.remove(&v)).collect();
        self.client.log_message(MessageType::LOG, format!("updating dependants {}", url)).await;
        let _ = join_all(
            items
                .into_iter()
                .map(|(reference, scope)| self.on_text_change(reference, scope.version, scope.content))
        ).await;
        if should_refresh {
            self.client.log_message(MessageType::LOG, format!("refreshing semantic tokens {}", url)).await;
            self.client.semantic_tokens_refresh().await?;
        }
        Ok(())
    }

    fn preprocess_expression(&self, url: &Url, scope: &mut Scope, node: W<Node>) -> std::result::Result<bool, W<Diagnostic>> {
        Ok(match node.kind() {
            "string" => node.is_empty(),
            "decimal" => node
                .text(scope.content.as_bytes())?
                .parse::<i32>()
                .unwrap_or_default() != 0,
            "identifier" => {
                scope.reference(url, node)?
            },
            "not" => {
                !self.preprocess_expression(url, scope, node.expect("right")?)?
            },
            "and" => {
                self.preprocess_expression(url, scope, node.expect("left")?)?
                &&
                self.preprocess_expression(url, scope, node.expect("right")?)?
            },
            "or" => {
                self.preprocess_expression(url, scope, node.expect("left")?)?
                ||
                self.preprocess_expression(url, scope, node.expect("right")?)?
            },
            "function_call" => {
                let argument = node.expect("arguments")?;
                match node.expect("name")?.text(scope.content.as_bytes())? {
                    "exists" => scope.find_env_variable(argument),
                    "file_exists" => self.find_file(url, scope, argument.expect("double_quoted")?).and(Ok(())),
                    _ => Err(node.error("unknown macro function")),
                }.and(Ok(true))?
            },
            _ => {
                return Err(node.error("unknown macro expression"));
            }
        })
    }

    fn search_file(&self, url: &Url, path: &str) -> Option<Url> {
        if url.scheme() == "file" {
            if let Ok(current) = std::env::current_dir() {
                if let Ok(path) = Url::from_file_path(current.join(path)) {
                    return Some(path);
                }
            }
        }

        //let mut url = url.clone();
        //let dir = url.to_file_path().ok()?;
        //url.set_path(dir.parent()?.to_str()?);
        url.join(path).ok()
    }

    fn find_file(&self, url: &Url, scope: &mut Scope, node: W<Node>) -> std::result::Result<Url, W<Diagnostic>> {
        let path = node.text(scope.content.as_bytes())?;
        self.search_file(url, path).ok_or_else(|| node.error("could not find file"))
    }

    async fn get_content(&self, path: Url) -> Option<String> {
        self.client.log_message(MessageType::LOG, format!("reading {}", path)).await;
        let mut file = File::open(path.to_file_path().ok()?).await.ok()?;
        let mut result = String::new();
        file.read_to_string(&mut result).await.ok()?;
        Some(result)
    }

    async fn enter_node<'tree>(&self, url: &Url, scope: &mut Scope, node: W<Node<'tree>>, if_stack: &mut IfStack<'tree>, documentation: &mut String) -> std::result::Result<bool, W<Diagnostic>> {
        let skip = if_stack.last().map(|(if_condition, _, n_else)| *if_condition != n_else.is_none()).unwrap_or_default();
        if !node.is_named() {
            return Ok(false);
        }
        let mut traverse = false;
        match node.kind() {
            "preprocessor_define" => if !skip {
                scope.define(url, node.expect("variable")?, documentation.clone())?;
            },
            "preprocessor_import" => if !skip {
                scope.find_env_variable(node.expect("variable")?)?;
            },
            "preprocessor_undef" => if !skip {
                scope.undefine(url, node.expect("variable")?)?;
            },
            "preprocessor_include" => if !skip {
                let filename = node.expect("filename")?.expect("double_quoted")?;
                let file_url = self.find_file(url, scope, filename)?;
                self.on_file_open(file_url.clone()).await.map_err(|_| filename.error("could not read file"))?;
                if let Some(file_scope) = self.files.get(&file_url) {
                    scope.definitions.extend(file_scope.definitions.clone());
                }
                else {
                    Err(filename.error("error while processing file"))?;
                }
                scope.references.insert(file_url.clone());
            },
            "preprocessor_once" => if !skip { scope.once = true; },
            "preprocessor_unknown" => if !skip { Err(node.error("unknown directive"))? },
            "ERROR" => if !skip { Err(node.error("syntax"))? },
            "preprocessor_if" => {
                let condition = node.expect("condition")?;
                let result = match self.preprocess_expression(url, scope, condition) {
                    Ok(result) => result,
                    Err(diagnostic) => {
                        scope.diagnostics.push(diagnostic.0);
                        false
                    },
                };
                if_stack.push((result, node, None));
            },
            "preprocessor_else" => {
                match if_stack.last() {
                    Some((_, _, None)) => {
                        if let Some(last) = if_stack.last_mut() {
                            last.2 = Some(node);
                        }
                    },
                    Some((_, _, Some(n_else))) => Err(
                        node.error("duplicate #else directive").with(
                            Some(n_else.related(url, "previous #else here"))
                        )
                    )?,
                    None => Err(node.error("expected #if"))?,
                }
            },
            "preprocessor_endif" => {
                let last = if_stack.last().ok_or_else(|| node.error("expected #if"))?;
                if let Some((start, end)) = match last {
                    (true, _, None) => None,
                    (true, _, Some(n_else)) => Some((n_else, node)),
                    (false, n_if, n_else) => Some((n_if, n_else.unwrap_or(node))),
                } {
                    scope.semantic_tokens.extend(
                        (start.end_position().row + 1..end.start_position().row)
                            .map(|line| SemanticToken {
                                delta_line: line as u32,
                                delta_start: 0,
                                length: u32::max_value(),
                                token_type: 0,
                                token_modifiers_bitset: 0,
                            })
                    );
                }
                if_stack.pop();
            },
            "comment" => {
                let text = node.text(scope.content.as_bytes())?;
                *documentation += text.get(2..).unwrap_or_default();
                documentation.push('\n');
                return Ok(false);
            },
            _ => {
                traverse = true;
            },
        }
        documentation.clear();
        Ok(traverse)
    }

    fn new(client: Client) -> Result<Self> {
        let mut builtins = HashMap::new();
        builtins.insert(Url::parse("memory:///builtins/alias.bff")?, include_str!("../builtins/alias.bff").into());
        match std::env::consts::OS {
            "linux" => builtins.insert(Url::parse("memory:///builtins/linux.bff")?, include_str!("../builtins/linux.bff").into()),
            "windows" => builtins.insert(Url::parse("memory:///builtins/windows.bff")?, include_str!("../builtins/windows.bff").into()),
            "macos" => builtins.insert(Url::parse("memory:///builtins/macos.bff")?, include_str!("../builtins/macos.bff").into()),
            _ => None,
        };
        let language = tree_sitter_fastbuild::language();
        let mut parser = Parser::new();
        parser.set_language(&language)?;
        let tree = parser.parse("", None).ok_or(Error::Parse)?;
        let _ = NULL.set(tree);
        let parsers = new_pool(language)?;
        let files = DashMap::new();
        Ok(Backend { client, parsers, files, builtins })
    }

}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {

    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        for (url, content) in self.builtins.iter() {
            let _ = self.on_file_update(url.clone(), 0, content.clone()).await;
        }
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
        self.client.log_message(MessageType::INFO, "server initialized!").await;
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
        self.on_file_update(
            document.uri,
            document.version,
            document.text,
        ).await.unwrap();
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        info!("text document change {:?}", params.text_document.uri);
        let document = params.text_document;
        if params.content_changes.len() != 1 {
            warn!("file changes: {:?}", params.content_changes);
            return;
        }
        let _ = self.on_file_update(
            document.uri,
            document.version,
            std::mem::take(&mut params.content_changes[0].text),
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

    async fn make_with_files(files: Vec<(&str, &str)>) -> (Vec<Url>, LspService<Backend>) {
        let service = make().await;
        let backend = service.inner();
        let mut urls = Vec::new();
        for (uri, content) in files {
            let uri = Url::parse(uri).expect("failed to parse url");
            backend.did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "fastbuild".into(),
                    version: 0,
                    text: content.into(),
                },
            }).await;
            urls.push(uri);
        }
        (urls, service)
    }

    #[tokio::test]
    async fn syntax_error() {
        let (uri, service) = make_with_file("memory:///syntax_error.bff", ".A =").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "syntax");
        assert_eq!(scope.semantic_tokens, Vec::new());
    }

    #[tokio::test]
    async fn redefinition() {
        let (uri, service) = make_with_file("memory:///redefinition.bff", "#define A\n#define A").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(scope.semantic_tokens, Vec::new());
        assert_eq!(diagnostic.message, "macro redefinition");
        let binding = diagnostic.related_information.clone().expect("no related information");
        let related = binding.first().expect("no related entrys");
        assert_eq!(related.message, "defined here");
        assert_eq!(related.location.uri, uri);
        assert_eq!(related.location.range.start, Position::new(0, 8));
        assert_eq!(related.location.range.end, Position::new(0, 9));
    }

    #[tokio::test]
    async fn import_not_found() {
        let (uri, service) = make_with_file("memory:///import_not_found.bff", "#import __SURELYNOSUCHENVVAR").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.semantic_tokens, Vec::new());
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "environment variable not found");
        assert_eq!(diagnostic.range.start.line, 0);
        assert_eq!(diagnostic.range.start.character, 8);
        assert_eq!(diagnostic.range.end.line, 0);
        assert_eq!(diagnostic.range.end.character, 28);
    }

    #[tokio::test]
    async fn undef() {
        let (uri, service) = make_with_file("memory:///undefine.bff", "#define A\n#undef A").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
    }

    #[tokio::test]
    async fn undef_undefined() {
        let (uri, service) = make_with_file("memory:///undefined.bff", "#undef A").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.semantic_tokens, Vec::new());
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "trying to undefine undefined macro");
        assert_eq!(diagnostic.range.start.line, 0);
        assert_eq!(diagnostic.range.start.character, 7);
        assert_eq!(diagnostic.range.end.line, 0);
        assert_eq!(diagnostic.range.end.character, 8);
    }

    #[tokio::test]
    async fn import() {
        let (uri, service) = make_with_file("memory:///import.bff", "#import PATH").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.diagnostics, vec!());
        assert_eq!(scope.semantic_tokens, Vec::new());
    }

    #[tokio::test]
    async fn once() {
        let (uri, service) = make_with_file("memory:///once.bff", "#once").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert!(scope.once);
        assert_eq!(scope.semantic_tokens, Vec::new());
        assert_eq!(scope.diagnostics, Vec::new());
    }

    #[tokio::test]
    async fn preprocessor_if() {
        let (uri, service) = make_with_file("memory:///preprocessor_if.bff", "#if 1\n.A = 3\n#else\n.B = 4\n#endif").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.diagnostics, Vec::new());
        let token = scope.semantic_tokens.first().expect("no semantic tokens");
        assert_eq!(token.delta_line, 3);
        assert_eq!(token.delta_start, 0);
        assert_eq!(token.length, u32::max_value());
        assert_eq!(token.token_modifiers_bitset, 0);
        assert_eq!(token.token_type, 0);
    }

    #[tokio::test]
    async fn preprocessor_else() {
        let (uri, service) = make_with_file("memory:///preprocessor_else.bff", "#if 0\n.A = 3\n#else\n.B = 4\n#endif").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.diagnostics, Vec::new());
        let token = scope.semantic_tokens.first().expect("no semantic tokens");
        assert_eq!(token.delta_line, 1, "mismatching lines");
        assert_eq!(token.delta_start, 0, "mismatching columns");
        assert_eq!(token.length, u32::max_value(), "mismatching length");
        assert_eq!(token.token_modifiers_bitset, 0, "mismatching modifiers");
        assert_eq!(token.token_type, 0, "mismatching types");
    }

    #[tokio::test]
    async fn preprocessor_if_evaluation() {
        let (uri, service) = make_with_file("memory:///preprocessor_if_missing.bff", "#define A\n#if 1\n#undef A\n\n#endif\n#if A\n.A = 3\n#endif").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.diagnostics, Vec::new());
        let token = scope.semantic_tokens.first().expect("no semantic tokens");
        assert_eq!(scope.diagnostics, vec![]);
        assert_eq!(token.delta_line, 6, "mismatching lines");
        assert_eq!(token.delta_start, 0, "mismatching columns");
        assert_eq!(token.length, u32::max_value(), "mismatching length");
        assert_eq!(token.token_modifiers_bitset, 0, "mismatching modifiers");
        assert_eq!(token.token_type, 0, "mismatching types");
    }

    #[tokio::test]
    async fn preprocessor_if_missing() {
        let (uri, service) = make_with_file("memory:///preprocessor_if_missing.bff", "#else").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.semantic_tokens, Vec::new());
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "expected #if");
        assert_eq!(diagnostic.range.start.line, 0);
        assert_eq!(diagnostic.range.start.character, 1);
        assert_eq!(diagnostic.range.end.line, 0);
        assert_eq!(diagnostic.range.end.character, 5);
    }

    #[tokio::test]
    async fn preprocessor_endif_missing() {
        let (uri, service) = make_with_file("memory:///preprocessor_if_missing.bff", "#if 1").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.semantic_tokens, Vec::new());
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "expected #endif");
        assert_eq!(diagnostic.range.start.line, 0);
        assert_eq!(diagnostic.range.start.character, 1);
        assert_eq!(diagnostic.range.end.line, 0);
        assert_eq!(diagnostic.range.end.character, 5);
    }

    #[tokio::test]
    async fn preprocessor_unknown() {
        let (uri, service) = make_with_file("memory:///preprocessor_unknown.bff", "#unknown").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.semantic_tokens, Vec::new());
        let diagnostic = scope.diagnostics.first().expect("no diagnostic");
        assert_eq!(diagnostic.message, "unknown directive");
        assert_eq!(diagnostic.range.start.line, 0);
        assert_eq!(diagnostic.range.start.character, 1);
        assert_eq!(diagnostic.range.end.line, 0);
        assert_eq!(diagnostic.range.end.character, 8);
    }

    #[tokio::test]
    async fn preprocessor_include() {
        let (uri, service) = make_with_file("memory:///builtins/preprocessor_include.bff", "#include \"alias.bff\"").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
    }

    #[tokio::test]
    async fn preprocessor_if_file_exists() {
        let (uri, service) = make_with_file("memory:///builtins/preprocessor_if_file_exists.bff", "#if file_exists(\"alias.bff\")\n#else\n.A = 3\n#endif").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.diagnostics, vec![]);
        assert_eq!(scope.semantic_tokens.first(), Some(&SemanticToken {
            delta_line: 2,
            delta_start: 0,
            length: u32::max_value(),
            token_type: 0,
            token_modifiers_bitset: 0,
        }));
    }

    #[tokio::test]
    async fn os() {
        let (uri, service) = make_with_file("memory:///os.bff", "").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.semantic_tokens, Vec::new());
        assert_eq!(scope.diagnostics, Vec::new());
        match std::env::consts::OS {
            "linux" => {
                assert!(matches!(scope.definitions.get("__LINUX__"), Some(Symbol { value: true, .. })));
                assert!(scope.definitions.get("__WINDOWS__").is_none());
                assert!(scope.definitions.get("__OSX__").is_none());
            },
            "macos" => {
                assert!(scope.definitions.get("__LINUX__").is_none());
                assert!(scope.definitions.get("__WINDOWS__").is_none());
                assert!(matches!(scope.definitions.get("__OSX__"), Some(Symbol { value: true, .. })));
            },
            "windows" => {
                assert!(scope.definitions.get("__LINUX__").is_none());
                assert!(matches!(scope.definitions.get("__WINDOWS__"), Some(Symbol { value: true, .. })));
                assert!(scope.definitions.get("__OSX__").is_none());
            },
            _ => {},
        }
    }

    #[tokio::test]
    async fn multi_include() {
        let (urls, service) = make_with_files(vec![
            ("memory:///multi_include1.bff", "\n#define A\n#if 0\n.A = 3\n#else\n#undef A\n#endif"),
            ("memory:///multi_include2.bff", "#include \"multi_include1.bff\"\n#define B\n#if A\n#undef B\n#endif"),
            ("memory:///multi_include3.bff", "#include \"multi_include2.bff\"\n#define C\n#if !B\n#undef C\n#endif"),
        ]).await;
        let backend = service.inner();
        for x in backend.files.iter() {
            let scope = x.value();
            let url = x.key();
            assert_eq!(scope.diagnostics, Vec::new());
            if urls.contains(url) {
                assert_eq!(scope.semantic_tokens, vec![SemanticToken {
                    delta_line: 3,
                    delta_start: 0,
                    length: u32::max_value(),
                    token_type: 0,
                    token_modifiers_bitset: 0,
                }]);
            }
            else {
                assert_eq!(scope.semantic_tokens, Vec::new());
            }
        }
    }

    #[tokio::test]
    async fn define_with_documentation() {
        let (uri, service) = make_with_file("memory:///define_with_documentation.bff", "; documentation\n; for A\n#define A").await;
        let backend = service.inner();
        let scope = backend.files.get(&uri).expect("no file");
        assert_eq!(scope.diagnostics, Vec::new());
        assert_eq!(scope.semantic_tokens, Vec::new());
        let definition = scope.definitions.get("A").expect("undefined");
        assert_eq!(definition.documentation, "documentation\nfor A\n");
    }

}
