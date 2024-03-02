use std::fs::File;

use tracing::{info, debug, Level, error, trace, warn};
use crossbeam_channel::SendError;
use thiserror::Error;

use lsp_types::{OneOf, ServerCapabilities, request::{GotoDefinition, GotoDeclarationParams, GotoDeclaration, Request}, GotoDefinitionResponse, InitializeParams, GotoDefinitionParams, ClientCapabilities, DeclarationCapability};
use lsp_server::{Connection, ExtractError, Message, RequestId, Response, ProtocolError, ResponseError};

#[derive(Error, Debug)]
enum Error {
    #[error("serde_json error")]
    Json(#[from] serde_json::Error),
    #[error("Extract error")]
    Extract(#[from] ExtractError<lsp_server::Request>),
    #[error("Protocol error")]
    Protocol(#[from] ProtocolError),
    #[error("IO error")]
    IO(#[from] std::io::Error),
    #[error("Send")]
    Send(#[from] crossbeam_channel::SendError<Message>),
    #[error("Unimplemented")]
    Unimplemented { method: String },
}

struct Server {
    connection: Connection,
    server_capabilities: ServerCapabilities,
    initialize_parameters: InitializeParams,
}

fn main() -> Result<(), Error> {
    let file = File::create("/home/pinbraerts/src/fastbuild-lsp/log.log")?;
    tracing_subscriber::fmt()
        .with_writer(file)
        .with_target(false)
        .with_max_level(Level::TRACE)
        .init();
    info!("starting LSP server");

    let (connection, io) = Connection::stdio();

    let server_capabilities = ServerCapabilities {
        definition_provider: Some(OneOf::Left(true)),
        declaration_provider: Some(DeclarationCapability::Simple(true)),
        ..Default::default()
    };
    let capabilities = serde_json::to_value(&server_capabilities)?;
    let capabilities = connection.initialize(capabilities)?;
    let initialize_parameters = serde_json::from_value::<InitializeParams>(capabilities)?;
    let server = Server { connection, server_capabilities, initialize_parameters };
    server.main()?;
    io.join()?;

    info!("shutting down LSP server");
    Ok(())
}

fn pack<T>(id: RequestId, response: Result<T, Error>) -> Response
where T: serde::Serialize,
{
    let response = match response {
        Ok(response) => response,
        Err(error) => {
            return Response {
                id, result: None, error: Some(ResponseError {
                    code: -2,
                    message: format!("{:?}", error),
                    data: None,
                })
            };
        }
    };
    match serde_json::to_value(Some(response)) {
        Ok(result) => Response { id, result: Some(result), error: None },
        Err(error) => Response { id, result: None, error: Some(ResponseError {
            code: -1,
            message: format!("serde json error: {:?}", error),
            data: None,
        }) },
    }
}

impl Server {

    fn process_request(&self, request: lsp_server::Request) -> Result<Response, Error> {
        Ok(match request.method.as_str() {
            GotoDefinition::METHOD => {
                let (id, param) = cast::<GotoDefinition>(request)?;
                pack(id, self.goto_definition(param))
            },
            GotoDeclaration::METHOD => {
                let (id, param) = cast::<GotoDefinition>(request)?;
                pack(id, self.goto_definition(param))
            },
            _ => { return Err(Error::Unimplemented{ method: request.method }) },
        })
    }

    fn main(self) -> Result<(), Error> {
        info!("starting main loop");
        for message in &self.connection.receiver {
            info!("message received: {:?}", message);
            match message {
                Message::Request(request) => {
                    if self.connection.handle_shutdown(&request)? {
                        return Ok(());
                    }
                    info!("request received: {:?}", request);
                    let responce = match self.process_request(request) {
                        Ok(response) => response,
                        Err(Error::Unimplemented { method }) => {
                            warn!("Unimplemented: {}", method);
                            continue;
                        },
                        Err(error) => { return Err(error); },
                    };
                    self.connection.sender.send(lsp_server::Message::Response(responce))?;
                }
                Message::Response(response) => {
                    info!("response received: {:?}", response);
                }
                Message::Notification(notification) => {
                    info!("notification received: {:?}", notification);
                }
            }
        }
        Ok(())
    }

    fn goto_definition(&self, parameters: GotoDefinitionParams) -> Result<GotoDefinitionResponse, Error> {
        info!("go to definition request {:?}", parameters);
        Ok(GotoDefinitionResponse::Array(Vec::new()))
    }

}

fn cast<R>(request: lsp_server::Request) -> Result<(RequestId, R::Params), ExtractError<lsp_server::Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    request.extract(R::METHOD)
}
