use std::{fs::File, path::Path, collections::HashMap, io};

use tower_lsp::lsp_types::{InitializeResult, InitializedParams, MessageType};
use tower_lsp::{Client, LanguageServer, Server, LspService};
use tower_lsp::lsp_types::*;
use tower_lsp::jsonrpc::{Error, Result};

use tracing::{info, debug, Level, error, trace, warn};

#[derive(thiserror::Error, Debug)]
enum ParseError {
    #[error("serde_json error")]
    Json(#[from] serde_json::Error),
    #[error("IO error")]
    IO(#[from] std::io::Error),
    #[error("Uri parse")]
    Uri { uri: String },
}

impl From<ParseError> for Error {
    fn from(val: ParseError) -> Self
    {
        match val {
            ParseError::Json(error) => Error::invalid_params(error.to_string()),
            ParseError::IO(_)       => Error::internal_error(),
            ParseError::Uri { uri } => Error::parse_error(),
        }
    }
}

#[derive(Debug, Clone)]
struct Declaration {
    pub location: Location,
    pub documentation: MarkupContent,
}
type Builtins = HashMap::<& 'static str, Declaration>;

fn load_builtins(file: &Path) -> std::result::Result<Builtins, ParseError> {
    let uri = Url::from_file_path(file).map_err(|_| ParseError::Uri { uri: file.to_string_lossy().to_string() })?;
    let file = File::open(file)?;
    let content = io::read_to_string(file)?;
    let lines = content.lines();
    Ok(HashMap::from([
        ("Alias", Declaration {
            location: Location { uri, range: Range {
                start: Position { line: 42, character: 9 },
                end:   Position { line: 42, character: 14 },
            }},
            documentation: MarkupContent {
                kind: MarkupKind::Markdown,
                value: lines.take(35)
                    .map(|line| line.get(3..line.len()).unwrap_or("") )
                    .fold(String::new(), |a, b| a + b + "\n")
                ,
            },
        }),
    ]))
}

#[derive(Debug, Clone)]
struct Backend {
    client: Client,
    builtins: Builtins,
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
        Ok(Some(GotoDefinitionResponse::Scalar(self.builtins["Alias"].location.clone())))
    }

    async fn goto_declaration(&self, parameters: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        info!("go to definition request {:?}", parameters);
        Ok(Some(GotoDefinitionResponse::Scalar(self.builtins["Alias"].location.clone())))
    }

    async fn hover(&self, parameters: HoverParams) -> Result<Option<Hover>> {
        info!("hover request {:?}", parameters);
        Ok(Some(Hover {
            contents: HoverContents::Markup(self.builtins["Alias"].documentation.clone()),
            range: None,
        }))
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
        .with_max_level(Level::TRACE)
        .init();

    info!("loading builtin declarations");
    let path = Path::new("/home/pinbraerts/src/fastbuild-lsp/builtins/alias.bff");
    let builtins = load_builtins(path).unwrap();

    info!("starting LSP server");
    let (service, socket) = LspService::new(|client| Backend {
        client,
        builtins,
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
