#[allow(unused_imports)]

pub mod cache;
pub mod parser_pool;
pub mod queries;
pub mod helpers;

use std::error::Error;
use futures::future;
use tokio::fs;
use std::ops::DerefMut;
use std::path::PathBuf;
use std::fs::File;

use cache::Cache;
use parser_pool::ParserPool;
use tokio::io::AsyncReadExt;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, Server, LspService};
use tower_lsp::jsonrpc::Result;

use tracing::{info, Level, warn};

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

async fn process_file(cache: &Cache, parsers: &ParserPool, mut path: PathBuf, filename: &str) -> std::result::Result<(), Box<dyn Error>> {
    path.push(filename);
    let uri = Url::from_file_path(&path).map_err(|_| cache::Error::FileNotFound)?;
    let mut file = tokio::fs::File::open(path).await?;
    let mut content = String::new();
    file.read_to_string(&mut content).await?;
    let mut parser = parsers.get().await?;
    cache.add_file(parser.as_mut(), uri, content, 0)?;
    Ok(())
}

async fn load_builtins(cache: &Cache, parsers: &ParserPool) -> std::result::Result<(), Box<dyn Error>> {
    info!("loading builtin declarations");
    let mut path = fs::canonicalize(file!()).await?;
    path.pop();
    path.pop();
    path.push("builtins");
    let files = [
        "alias.bff",
    ];
    let future = future::try_join_all(files.iter().map(|filename| {
        process_file(cache, parsers, path.clone(), filename)
    }));
    future.await?;
    Ok(())
}

#[tokio::main]
async fn main() -> std::result::Result<(), Box<dyn Error>> {
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

    let language = tree_sitter_fastbuild::language();
    let parsers = new_pool(language.clone())?;
    let cache = Cache::new(&language)?;

    load_builtins(&cache, &parsers).await?;

    info!("starting LSP server");
    let (service, socket) = LspService::new(|client| Backend {
        client,
        cache,
        parsers,
    });
    Server::new(stdin, stdout, socket).serve(service).await;
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::error::Error;
    
    use url::Url;

    use super::{new_pool, ParserPool, Cache, load_builtins, cache, cache::Value};

    async fn make() -> Result<(ParserPool, Cache), Box<dyn Error>> {
        let language = tree_sitter_fastbuild::language();
        let parsers = new_pool(language.clone())?;
        let cache = Cache::new(&language)?;
        load_builtins(&cache, &parsers).await?;
        Ok((parsers, cache))
    }

    #[tokio::test]
    async fn builtins() -> Result<(), Box<dyn Error>> {
        let (_, cache) = make().await?;
        let mut path = tokio::fs::canonicalize(file!()).await?;
        assert!(path.pop());
        assert!(path.pop());
        path.push("builtins");
        path.push("alias.bff");
        let uri = Url::from_file_path(path).map_err(|_| cache::Error::FileNotFound)?;
        let file_info = cache.files.get(&uri).ok_or(cache::Error::FileNotFound)?;
        assert!(file_info.once);
        let alias = file_info.symbols.get("Alias").ok_or(cache::Error::SymbolNotFound)?;
        assert!(alias.documentation.is_some());
        assert_eq!(alias.value, Ok(Value::Function));
        Ok(())
    }
}
