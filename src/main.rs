pub mod cache;
pub mod parser_pool;
pub mod queries;
pub mod helpers;

use std::ops::{Deref, DerefMut};
use std::{fs::File, path::Path, collections::HashMap, io};

use cache::Cache;
use parser_pool::{ParserPool, ParserManager};
use tower_lsp::lsp_types::{InitializeResult, InitializedParams, MessageType};
use tower_lsp::{Client, LanguageServer, Server, LspService};
use tower_lsp::lsp_types::*;
use tower_lsp::jsonrpc::{Error, Result};

use tracing::{info, debug, Level, error, trace, warn};

use crate::cache::FileCache;
use crate::parser_pool::new_pool;

struct Backend {
    client: Client,
    cache: Cache,
    parsers: ParserPool,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {

    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                declaration_provider: Some(DeclarationCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
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

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn goto_definition(&self, parameters: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        info!("go to definition request {:?}", parameters);
        let document = parameters.text_document_position_params;
        Ok(self.cache.find_definition(document.text_document.uri, document.position)
            .map(GotoDefinitionResponse::Scalar))
    }

    async fn goto_declaration(&self, parameters: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        info!("go to definition request {:?}", parameters);
        let document = parameters.text_document_position_params;
        Ok(self.cache.find_definition(document.text_document.uri, document.position)
            .map(GotoDefinitionResponse::Scalar))
    }

    async fn hover(&self, parameters: HoverParams) -> Result<Option<Hover>> {
        info!("hover request {:?}", parameters);
        let document = parameters.text_document_position_params;
        Ok(self.cache.find_hover(document.text_document.uri, document.position)
            .map(HoverContents::Markup)
            .map(|contents| Hover { contents, range: None }))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        info!("text document open {:?}", params);
        let document = params.text_document;
        if document.language_id != "fastbuild" {
            return;
        }
        let mut parser = match self.parsers.get().await {
            Ok(x) => x,
            Err(_) => { return; },
        };
        let _ = self.cache.add_file(parser.deref_mut(), document.uri, document.text, document.version);
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        info!("text document open {:?}", params);
        let document = params.text_document;
        if params.content_changes.len() != 1 {
            warn!("file changes: {:?}", params.content_changes);
            return;
        }
        let mut parser = match self.parsers.get().await {
            Ok(x) => x,
            Err(_) => { return; },
        };
        let _ = self.cache.add_file(parser.deref_mut(), document.uri, std::mem::take(&mut params.content_changes[0].text), document.version);
    }

}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

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

    info!("loading builtin declarations");
    let path = Path::new("/home/pinbraerts/src/fastbuild-lsp/builtins/alias.bff");
    let (url, content) = Cache::load_file(path).unwrap();

    let language = tree_sitter_fastbuild::language();
    let parsers = new_pool(language.clone()).expect("failed to create language");
    let mut parser = parsers.get().await.expect("parser is unavailable");

    info!("creating cache");
    let cache = Cache::new(&language).unwrap();
    cache.add_file(parser.as_mut(), url, content, 0).unwrap();

    info!("starting LSP server");
    let (service, socket) = LspService::new(|client| Backend {
        client,
        cache,
        parsers,
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
