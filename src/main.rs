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

use crate::cache::Symbol;
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
        Ok(self.cache.find_symbol(document.text_document.uri, document.position)
            .map(|(u, _, s)| Location::new(u, s.range))
            .map(GotoDefinitionResponse::Scalar))
    }

    async fn goto_declaration(&self, parameters: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        info!("go to definition request {:?}", parameters);
        let document = parameters.text_document_position_params;
        Ok(self.cache.find_symbol(document.text_document.uri, document.position)
            .map(|(u, _, s)| Location::new(u, s.range))
            .map(GotoDefinitionResponse::Scalar))
    }

    async fn hover(&self, parameters: HoverParams) -> Result<Option<Hover>> {
        info!("hover request {:?}", parameters);
        let document = parameters.text_document_position_params;
        let result = self.cache.find_symbol(document.text_document.uri, document.position);
        if let Some((_, range, Symbol { documentation: Some(docs), .. })) = result {
            Ok(Some(Hover {
                contents: HoverContents::Markup(docs),
                range: Some(range),
            }))
        }
        else {
            Ok(None)
        }
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
    let parsers = new_pool(language)?;
    let cache = Cache::new();

    load_builtins(&cache, &parsers).await?;

    info!("starting LSP server");
    let (service, socket) = LspService::new(|client| Backend {
        client,
        cache,
        parsers,
    });

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    Server::new(stdin, stdout, socket).serve(service).await;
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::error::Error;
    
    use tower_lsp::{LspService, LanguageServer};
    use tower_lsp::lsp_types::*;
    use url::Url;

    use crate::Backend;
    use crate::cache::markdown;

    use super::{new_pool, Cache, load_builtins, cache, cache::Value};

    async fn make(include_builtins: bool) -> Result<LspService<Backend>, Box<dyn Error>> {
        let language = tree_sitter_fastbuild::language();
        let cache = Cache::new();
        let parsers = new_pool(language)?;
        if include_builtins {
            load_builtins(&cache, &parsers).await?;
        }
        let (service, _) = LspService::new(|client| Backend {
            client,
            cache,
            parsers,
        });
        Ok(service)
    }

    #[tokio::test]
    async fn builtins() -> Result<(), Box<dyn Error>> {
        let language = tree_sitter_fastbuild::language();
        let cache = Cache::new();
        let parsers = new_pool(language)?;
        load_builtins(&cache, &parsers).await?;
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

    #[tokio::test]
    async fn goto_declaration() -> Result<(), Box<dyn Error>> {
        let service = make(false).await?;
        let service = service.inner();
        assert!(service.initialize(InitializeParams::default()).await.is_ok());
        service.initialized(InitializedParams { }).await;
        let uri = Url::parse("memory://goto_definition.bff")?;
        let text = "function Alias() {}\nAlias() {}";
        let language_id = "fastbuild".to_string();
        let document = TextDocumentIdentifier { uri: uri.clone() };
        let text_document = TextDocumentItem {
            uri: uri.clone(),
            language_id,
            text: text.to_string(),
            version: 0,
        };
        service.did_open(DidOpenTextDocumentParams { text_document }).await;
        let response = service.goto_declaration(GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: document,
                position: Position { line: 1, character: 0 }
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        }).await?;
        service.shutdown().await?;
        let range = Range::new(
            Position::new(0, 0),
            Position::new(0, text.lines().next().ok_or(cache::Error::SymbolNotFound)?.len() as u32)
        );
        assert_eq!(response, Some(GotoDefinitionResponse::Scalar(
            Location { uri, range }
        )));
        Ok(())
    }

    #[tokio::test]
    async fn hover() -> Result<(), Box<dyn Error>> {
        let service = make(false).await?;
        let service = service.inner();
        assert!(service.initialize(InitializeParams::default()).await.is_ok());
        service.initialized(InitializedParams { }).await;
        let uri = Url::parse("memory://hover.bff")?;
        let text = "// docs\nfunction Alias() {}\nAlias() {}";
        let language_id = "fastbuild".to_string();
        let document = TextDocumentIdentifier { uri: uri.clone() };
        let text_document = TextDocumentItem {
            uri: uri.clone(),
            language_id,
            text: text.to_string(),
            version: 0,
        };
        service.did_open(DidOpenTextDocumentParams { text_document }).await;
        let response = service.hover(HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: document,
                position: Position { line: 2, character: 0 }
            },
            work_done_progress_params: Default::default(),
        }).await?.ok_or(cache::Error::SymbolNotFound)?;
        service.shutdown().await?;
        assert_eq!(response.contents, HoverContents::Markup(markdown("docs\n")));
        let range = response.range.expect("should have range");
        assert_eq!(range.start, Position::new(2, 0));
        assert_eq!(range.end, Position::new(2, 5));
        Ok(())
    }

}
