use std::{collections::HashMap, io, fs::File, path::Path};

use tower_lsp::lsp_types::{Location, MarkupContent, Url, Position, Range, MarkupKind, GotoDefinitionResponse};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("serde_json error")]
    Json(#[from] serde_json::Error),
    #[error("IO error")]
    IO(#[from] std::io::Error),
    #[error("Uri parse")]
    Uri { uri: String },
}

impl From<Error> for tower_lsp::jsonrpc::Error {
    fn from(val: Error) -> Self
    {
        match val {
            Error::Json(error) => tower_lsp::jsonrpc::Error::invalid_params(error.to_string()),
            Error::IO(_)       => tower_lsp::jsonrpc::Error::internal_error(),
            Error::Uri { uri } => tower_lsp::jsonrpc::Error::parse_error(),
        }
    }
}
type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Declaration {
    pub location: Location,
    pub documentation: MarkupContent,
}
pub type Declarations = HashMap::<& 'static str, Declaration>;

#[derive(Debug, Default, Clone)]
pub struct FileInformation {
    pub declarations: Declarations,
}

pub type FileCache = HashMap<Url, FileInformation>;

#[derive(Debug, Default, Clone)]
pub struct Cache {
    pub files: FileCache,
}

impl Cache {
    pub async fn load_file(filename: &Path) -> Result<(Url, FileInformation)> {
        let uri = Url::from_file_path(filename)
            .map_err(|_| Error::Uri { uri: filename.to_string_lossy().to_string() })?;
        let file = File::open(filename)?;
        let content = io::read_to_string(file)?;
        let lines = content.lines();
        Ok((uri.clone(), FileInformation {
            declarations: HashMap::from([
                ("Alias", Declaration {
                    location: Location { uri, range: Range {
                        start: Position { line: 42, character: 9 },
                        end:   Position { line: 42, character: 14 },
                    }},
                    documentation: MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: lines.take(35)
                            .map(|line| line.get(4..line.len()).unwrap_or("") )
                            .fold(String::new(), |a, b| a + b + "\n")
                        ,
                    },
                }),
            ])
        }))
    }

    pub fn find_definition(&self) -> Option<&Declaration> {
        for info in self.files.values() {
            if let Some(declaration) = info.declarations.get("Alias") {
                return Some(declaration);
            }
        }
        None
    }
}
