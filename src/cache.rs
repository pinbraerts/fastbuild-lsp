use std::{collections::HashMap, io, fs::File, path::Path, cell::{Cell, RefCell}};

use dashmap::DashMap;
use tower_lsp::lsp_types::{Location, MarkupContent, Url, Position, Range, MarkupKind, GotoDefinitionResponse};
use tracing::info;
use tree_sitter::{Parser, Query, QueryError, Language, QueryCursor, Tree, Node, Point, QueryCapture};

use crate::{parser_pool::{ParserPool, new_pool}, queries::{Queries, query}};

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

pub type Declarations = DashMap::<String, Location>;

#[derive(Debug, Default, Clone)]
pub struct FileInformation {
    pub content: String,
    pub version: i32,
    pub tree: Option<tree_sitter::Tree>,
}

pub type FileCache = DashMap<Url, FileInformation>;

pub struct Cache {
    declarations: Declarations,
    files: FileCache,
    parsers: ParserPool,
    queries: Queries,
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
        // let lines = content.lines();
        // Ok((uri.clone(), FileInformation {
        //     content,
        //     declarations: HashMap::from([
        //         ("Alias", Declaration {
        //             location: Location { uri, range: Range {
        //                 start: Position { line: 42, character: 9 },
        //                 end:   Position { line: 42, character: 14 },
        //             }},
        //             documentation: MarkupContent {
        //                 kind: MarkupKind::Markdown,
        //                 value: lines.take(35)
        //                     .map(|line| line.get(4..line.len()).unwrap_or("") )
        //                     .fold(String::new(), |a, b| a + b + "\n")
        //                 ,
        //             },
        //         }),
        //     ]),
        //     tree: None,
        // }))
    }

    pub async fn add_file(&self, uri: Url, content: String, version: i32) -> Result<()> {
        let mut entry = self.files.entry(uri.clone()).or_default();
        let info = entry.value_mut();
        if info.version > version {
            return Ok(());
        }
        if info.version > version {
            info.tree = None;
        }
        let new_tree = self.parsers
            .get()
            .await
            .map_err(|_| Error::Unavailable)?
            .parse(&content, info.tree.as_ref())
            .ok_or(Error::TreeSitter)?;

        query(&self.queries.function_definition, &new_tree, content.as_bytes())
            .into_iter()
            .for_each(|(range, content)| {
                self.declarations.insert(content, Location { uri: uri.clone(), range });
            })
        ;

        info.content = content;
        info.tree = Some(new_tree);
        Ok(())
    }

    pub fn find_definition(&self) -> Option<Location> {
        self.declarations.get("Alias").map(|reference| reference.clone())
    }
}

impl Default for Cache {
    fn default() -> Self {
        Self::new()
    }
}
