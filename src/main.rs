pub mod parser_pool;
pub mod queries;
pub mod error;
pub mod helpers;

use dashmap::DashMap;
use helpers::W;
use queries::Queries;
use tree_sitter::{Node, QueryCursor, Tree};
use std::fs::File;

use parser_pool::ParserPool;
use tower_lsp::{jsonrpc, lsp_types::*};
use tower_lsp::{Client, LanguageServer, Server, LspService};
use error::{Result, Error};

use tracing::{info, Level, warn};

use crate::parser_pool::new_pool;

#[derive(Debug)]
struct FileInfo {
    content: String,
    tree: Tree,
    version: i32,
}

struct Backend {
    client: Client,
    parsers: ParserPool,
    files: DashMap<Url, FileInfo>,
    queries: Queries,
}

impl Backend {

    async fn process_file(&self, url: Url, content: String, version: i32) -> Result<()> {
        let mut parser = self.parsers.get().await?;
        let tree = parser.parse(content.as_bytes(), None).ok_or(Error::Parse)?;

        self.process_syntax_errors(url.clone(), &content, &tree, version).await;

        let entry = self.files.entry(url);
        entry.insert(FileInfo { content, tree, version });
        Ok(())
    }

    async fn process_syntax_errors(&self, url: Url, content: &String, tree: &Tree, version: i32) {
        let mut cursor = QueryCursor::new();
        let matches = cursor.matches(&self.queries.error, tree.root_node(), content.as_bytes());
        let diagnostics = matches
            .filter_map(|m| m.captures.first())
            .map(|c| Diagnostic::new(
                W(&c.node).into(),
                Some(DiagnosticSeverity::ERROR),
                None,
                None,
                "Syntax Error".into(),
                None,
                None,
            )).collect()
        ;
        self.client.publish_diagnostics(url.clone(), diagnostics, Some(version)).await;
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
        info!("text document open {:?}", params);
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
        service
    }

}
