use std::{io, fs::File, path::Path};
use dashmap::{DashMap, mapref::entry::Entry};
use tower_lsp::lsp_types::{Location, MarkupContent, Url, Position, MarkupKind, Range};
use tree_sitter::{Point, QueryCursor, QueryCapture};


use crate::{parser_pool::{ParserPool, new_pool, ParserManager}, queries::Queries};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("serde_json error")]
    Json(#[from] serde_json::Error),
    #[error("IO error")]
    IO(#[from] std::io::Error),
    #[error("Uri parse")]
    Uri { uri: String },
    #[error("Treesitter")]
    TreeSitter,
    #[error("Parser unavailable")]
    Unavailable,
}

impl From<Error> for tower_lsp::jsonrpc::Error {
    fn from(val: Error) -> Self
    {
        match val {
            Error::Json(error) => tower_lsp::jsonrpc::Error::invalid_params(error.to_string()),
            Error::IO(_)       => tower_lsp::jsonrpc::Error::internal_error(),
            Error::Uri { uri } => tower_lsp::jsonrpc::Error::parse_error(),
            Error::TreeSitter  => tower_lsp::jsonrpc::Error::parse_error(),
            Error::Unavailable => tower_lsp::jsonrpc::Error::internal_error(),
        }
    }
}
type Result<T> = std::result::Result<T, Error>;

pub fn markdown(value: String) -> MarkupContent {
    MarkupContent { kind: MarkupKind::Markdown, value }
}

struct Declaration {
    location: Location,
    documentation: Option<MarkupContent>,
}

impl Declaration {
    fn new(uri: &Url, capture: &QueryCapture, documentation: Option<&str>) -> Self {
        Self {
            location: Location {
                uri: uri.clone(),
                range: range(capture),
            },
            documentation: documentation.map(|text| {
                text.lines()
                    .map(|line| line.get(3..line.len()).unwrap_or_default())
                    .fold(String::new(), |a, b| a + b + "\n")
            }).map(markdown),
        }
    }
}

pub type Declarations = DashMap::<String, Declaration>;

#[derive(Debug, Clone)]
pub struct FileInformation {
    pub content: String,
    pub version: i32,
    pub tree: tree_sitter::Tree,
}

pub type FileCache = DashMap<Url, FileInformation>;

pub struct Cache {
    declarations: Declarations,
    files: FileCache,
    parsers: ParserPool,
    queries: Queries,
}

fn position(point: Point) -> Position {
    Position {
        line: point.row as u32,
        character: point.column as u32,
    }
}

fn range(capture: &QueryCapture) -> Range {
    Range {
        start: position(capture.node.start_position()),
        end:   position(capture.node.end_position()),
    }
}

fn content(capture: &QueryCapture, source: &[u8]) -> (Range, String) {
    (
        range(capture),
        capture.node.utf8_text(source).unwrap_or_default().to_owned(),
    )
}

pub fn point(position: Position) -> Point {
    Point {
       row: position.line as usize,
        column: position.character as usize,
    }
}

impl Cache {
    pub fn new() -> Self {
        let language = tree_sitter_fastbuild::language();
        let queries = Queries::new(&language).unwrap();
        let parsers = new_pool(language).unwrap();
        Cache {
            declarations: Declarations::new(),
            files: FileCache::default(),
            parsers,
            queries,
        }
    }

    pub fn load_file(filename: &Path) -> Result<(Url, String)> {
        let uri = Url::from_file_path(filename)
            .map_err(|_| Error::Uri { uri: filename.to_string_lossy().to_string() })?;
        let file = File::open(filename)?;
        Ok((uri, io::read_to_string(file)?))
    }

    pub async fn add_file(&self, uri: Url, content: String, version: i32) -> Result<()> {
        let entry = self.files.entry(uri.clone());
        let tree = match &entry {
            Entry::Occupied(occupied) => {
                let old_version = occupied.get().version;
                if old_version > version {
                    return Ok(());
                }
                if old_version < version {
                    None
                }
                else {
                    Some(&occupied.get().tree)
                }
            },
            Entry::Vacant(_) => None,
        };

        let tree = self.get_parser()
            .await?
            .parse(&content, tree)
            .ok_or(Error::TreeSitter)?;

        let source = content.as_bytes();

        QueryCursor::new()
            .matches(&self.queries.function_definition, tree.root_node(), source)
            .flat_map(|m| m.captures)
            .for_each(|capture| {
                let text = match capture.node.utf8_text(source) {
                    Ok(text) => text,
                    Err(_) => { return; },
                }.to_owned();
                let documentation = capture.node.parent()
                    .map(|parent| {
                        match parent.kind() {
                            "function_definition" => parent.prev_sibling(),
                            _ => None,
                        }
                    }).unwrap_or(None)
                    .map(|sibling| {
                        if sibling.kind() == "comment" {
                            sibling.utf8_text(source).ok()
                        }
                        else {
                            None
                        }
                    }).unwrap_or(None);
                self.declarations.insert(text, Declaration::new(&uri, capture, documentation));
            });

        entry.insert(FileInformation { content, tree, version });
        Ok(())
    }

    pub fn find_definition(&self, uri: Url, position: Position) -> Option<Location> {
        self.declarations
            .get(&self.get_word(uri, position)?)
            .map(|reference| reference.location.clone())
    }

    pub fn find_hover(&self, uri: Url, position: Position) -> Option<MarkupContent> {
        self.declarations
            .get(&self.get_word(uri, position)?)
            .map(|reference| reference.documentation.clone())
            .unwrap_or(None)
    }

    fn get_word(&self, uri: Url, position: Position) -> Option<String> {
        let file = self.files.get(&uri)?;
        let closest = file.tree.root_node().descendant_for_point_range(point(position), point(position))?;
        match closest.kind() {
            "identifier" => {
                Some(closest)
            },
            "usage" => {
                closest.child(0)
            },
            _ => None,
        }?.utf8_text(file.content.as_bytes()).ok().map(|s| s.to_owned())
    }

    async fn get_parser(&self) -> Result<deadpool::managed::Object<ParserManager>> {
        self.parsers
            .get()
            .await
            .map_err(|_| Error::Unavailable)
    }

}

impl Default for Cache {
    fn default() -> Self {
        Self::new()
    }
}
