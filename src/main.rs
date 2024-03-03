pub mod cache;
pub mod parser_pool;
pub mod queries;

use std::{fs::File, path::Path, collections::HashMap, io};

use cache::Cache;
use tower_lsp::lsp_types::{InitializeResult, InitializedParams, MessageType};
use tower_lsp::{Client, LanguageServer, Server, LspService};
use tower_lsp::lsp_types::*;
use tower_lsp::jsonrpc::{Error, Result};

use tracing::{info, debug, Level, error, trace, warn};

use crate::cache::FileCache;

struct Backend {
    client: Client,
    cache: Cache,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {

    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                declaration_provider: Some(DeclarationCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
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
        Ok(self.cache.find_definition()
            .map(|declaration| GotoDefinitionResponse::Scalar(declaration)))
    }

    async fn goto_declaration(&self, parameters: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        info!("go to definition request {:?}", parameters);
        Ok(self.cache.find_definition()
            .map(|declaration| GotoDefinitionResponse::Scalar(declaration)))
    }

    async fn hover(&self, parameters: HoverParams) -> Result<Option<Hover>> {
        info!("hover request {:?}", parameters);
        Ok(self.cache.find_definition()
            .map(|declaration| Hover { 
                contents: HoverContents::Scalar(MarkedString::String("default hover".to_owned())),
                range: None
            }))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        info!("text document open {:?}", params);
        let document = params.text_document;
        if document.language_id != "fastbuild" {
            return;
        }
        let _ = self.cache.add_file(document.uri, document.text, document.version).await;
    }

}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let file = File::create("/home/pinbraerts/src/fastbuild-lsp/log.log").unwrap();
    tracing_subscriber::fmt()
        .with_writer(file)
        .with_target(false)
        .with_max_level(Level::DEBUG)
        .init();

    info!("loading builtin declarations");
    let path = Path::new("/home/pinbraerts/src/fastbuild-lsp/builtins/alias.bff");
    let (url, content) = Cache::load_file(path).unwrap();
    let cache = Cache::new();
    cache.add_file(url, content, 0).await.unwrap();

    info!("starting LSP server");
    let (service, socket) = LspService::new(|client| Backend {
        client,
        cache,
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
