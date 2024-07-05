pub mod parser_pool;
pub mod queries;
pub mod error;
pub mod helpers;

use dashmap::DashMap;
use helpers::W;
use queries::{PreprocessorCapture, Queries};
use tree_sitter::{Parser, QueryCursor, Tree};
use std::collections::HashMap;
use std::fs::File;
use std::ops::DerefMut;

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
    references: Vec<(Location, Reference)>,
}

#[derive(Debug)]
struct FileInfo {
    version: i32,
    content: String,
    tree: Tree,
    diagnostics: Vec<Diagnostic>,
    definitions: HashMap<String, Symbol>,
    once: bool,
}

impl FileInfo {
    fn new(version: i32, parser: &mut Parser, content: String) -> Result<Self> {
        let tree = parser.parse(content.as_bytes(), None).ok_or(Error::Parse)?;
        Ok(FileInfo { version, content, tree, diagnostics: Vec::default(), definitions: HashMap::default(), once: bool::default(), })
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
        self.preprocessor_pass(url.clone(), &mut scope).await;
        self.client.publish_diagnostics(url.clone(), scope.diagnostics.clone(), Some(version)).await;
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

    async fn preprocessor_pass(&self, url: Url, scope: &mut FileInfo) {
        let mut cursor = QueryCursor::new();
        let matches = cursor.matches(&self.queries.preprocessor, scope.tree.root_node(), scope.content.as_bytes());
        matches
            .filter_map(PreprocessorCapture::new)
            .for_each(|c| match c {
                PreprocessorCapture::Define(node) => {
                    let text = match node.utf8_text(scope.content.as_bytes()) {
                        Ok(text) => text.to_owned(),
                        Err(_) => {return;},
                    };
                    let entry = scope.definitions.entry(text).or_default();
                    if let Some(related_information) = entry
                        .references.iter().rev()
                        .find(|(_, t)| *t == Reference::Define)
                        .map(|(location, _)| DiagnosticRelatedInformation {
                            location: location.clone(),
                            message: "note: defined here".into(),
                        })
                        .map(|d| vec!(d)) {
                        scope.diagnostics.push(Diagnostic::new(
                            W(&node).into(),
                            Some(DiagnosticSeverity::ERROR),
                            None,
                            None,
                            "error: macro redefinition".into(),
                            Some(related_information),
                            None,
                        ));
                    }
                    entry.references.push((Location::new(url.clone(), W(&node).into()), Reference::Define));
                },
                PreprocessorCapture::Import(node) => {
                    let text = match node.utf8_text(scope.content.as_bytes()) {
                        Ok(text) => text.to_owned(),
                        Err(_) => {return;},
                    };
                    let entry = scope.definitions.entry(text).or_default();
                    if let Some(related_information) = entry
                        .references.iter().rev()
                        .find(|(_, t)| *t == Reference::Define)
                        .map(|(location, _)| DiagnosticRelatedInformation {
                            location: location.clone(),
                            message: "note: defined here".into(),
                        })
                        .map(|d| vec!(d)) {
                        scope.diagnostics.push(Diagnostic::new(
                            W(&node).into(),
                            Some(DiagnosticSeverity::ERROR),
                            None,
                            None,
                            "error: macro redefinition".into(),
                            Some(related_information),
                            None,
                        ));
                    }
                    entry.references.push((Location::new(url.clone(), W(&node).into()), Reference::Define));
                },
                PreprocessorCapture::Undef(node) => {
                    let text = match node.utf8_text(scope.content.as_bytes()) {
                        Ok(text) => text.to_owned(),
                        Err(_) => {return;},
                    };
                    let entry = scope.definitions.entry(text).or_default();
                    let (error, related_information) = match entry.references.last() {
                        Some((_, Reference::Define)) => (false, None),
                        Some((location, Reference::Undef)) => (true, Some(vec!(DiagnosticRelatedInformation {
                            location: location.clone(),
                            message: "note: last undefined here".into(),
                        }))),
                        None => (true, None),
                    };
                    if error {
                        scope.diagnostics.push(Diagnostic::new(
                            W(&node).into(),
                            Some(DiagnosticSeverity::ERROR),
                            None,
                            None,
                            "error: trying to undefine not defined macro".into(),
                            related_information,
                            None,
                        ));
                    }
                    entry.references.push((Location::new(url.clone(), W(&node).into()), Reference::Undef));
                },
                PreprocessorCapture::Include(node) => {},
                PreprocessorCapture::Once => {
                    scope.once = true;
                },
                PreprocessorCapture::If { condition, body, alternative } => {},
            });
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
        info!("text document open {:?}", params);
        let document = params.text_document;
        if document.language_id != "fastbuild" {
            return;
        }
        let _ = self.process_file(
            document.uri,
            document.text,
            document.version
        ).await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        info!("text document change {:?}", params);
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
        assert_eq!(diagnostic.message, "error: trying to undefine not defined macro");
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

}
