use std::{io, fs::File, path::Path, collections::BTreeMap};
use dashmap::{DashMap, mapref::entry::Entry};
use deadpool::managed::BuildError;
use tower_lsp::lsp_types::{Location, MarkupContent, Url, Position, MarkupKind, Range};
use tracing::trace;
use tree_sitter::{QueryCursor, QueryCapture, QueryError, Tree, QueryMatch};


use crate::{parser_pool::{ParserPool, new_pool, ParserManager}, queries::Queries, helpers::W};

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

pub struct Declaration {
    location: Location,
    documentation: Option<MarkupContent>,
}

impl Declaration {
    fn new(uri: &Url, capture: &QueryCapture, documentation: Option<&str>) -> Self {
        Self {
            location: Location {
                uri: uri.clone(),
                range: W(capture).into(),
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Include(String),
    Import {
        name: String,
        value: String,
    },
}

pub type Definitions = BTreeMap<W<Range>, Value>;

#[derive(Debug, Clone)]
pub struct FileInformation {
    pub content: String,
    pub version: i32,
    pub tree: tree_sitter::Tree,
    pub definitions: Definitions,
}

pub type FileCache = DashMap<Url, FileInformation>;

pub struct Cache {
    declarations: Declarations,
    files: FileCache,
    parsers: ParserPool,
    queries: Queries,
}

#[derive(thiserror::Error, Debug)]
pub enum CacheCreateError {
    #[error("query")]
    Query(#[from] QueryError),
    #[error("pool build")]
    Pool(#[from] BuildError),
}


impl Cache {

    pub fn new() -> std::result::Result<Self, CacheCreateError> {
        let language = tree_sitter_fastbuild::language();
        let queries = Queries::new(&language)?;
        let parsers = new_pool(language)?;
        Ok(Cache {
            declarations: Declarations::new(),
            files: FileCache::default(),
            parsers,
            queries,
        })
    }

    pub fn load_file(filename: &Path) -> Result<(Url, String)> {
        let uri = Url::from_file_path(filename)
            .map_err(|_| Error::Uri { uri: filename.to_string_lossy().to_string() })?;
        let file = File::open(filename)?;
        Ok((uri, io::read_to_string(file)?))
    }

    pub fn process_match(m: QueryMatch, names: &[&str], source: &[u8], tree: &Tree) -> Option<(Range, Value)> {
        let capture = W(m.captures.first()?);
        let name = names[capture.index as usize];
        match name {
            "include" => {
                let variable = m.captures.get(1)?;
                if names[variable.index as usize] != "filename" {
                    return None
                }
                let name = variable.node.utf8_text(source).ok()?;
                Some((capture.into(), Value::Include(name.into())))
            },
            "import"  => {
                let variable = m.captures.get(1)?;
                if names[variable.index as usize] != "variable" {
                    return None
                }
                let name = variable.node.utf8_text(source).ok()?;
                let value = std::env::var(name).ok()?;
                Some((capture.into(), Value::Import { name: name.into(), value }))
            },
            _ => None,
        }
    }

    pub fn preprocess(&self, source: &[u8], tree: &Tree, definitions: &mut Definitions) -> Result<()> {
        let names = self.queries.preprocessor.capture_names();
        QueryCursor::new()
            .matches(&self.queries.preprocessor, tree.root_node(), source)
            .for_each(|m| {
                if let Some((range, value)) = Self::process_match(m, names, source, tree) {
                    definitions.insert(W(range), value);
                }
            });
        Ok(())
    }

    pub async fn add_file(&self, uri: Url, content: String, version: i32) -> Result<()> {
        trace!("processing file {:?}", uri);
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
        let names = &self.queries.function_definition.capture_names();

        let mut definitions = Definitions::new();
        let _ = self.preprocess(source, &tree, &mut definitions);

        QueryCursor::new()
            .matches(&self.queries.function_definition, tree.root_node(), source)
            .for_each(|m| {
                let documentation = m.captures
                    .iter()
                    .find(|c| names[c.index as usize] == "documentation")
                    .map(|c| c.node.utf8_text(source).unwrap_or_default())
                ;
                let definition = m.captures
                    .iter()
                    .find(|c| names[c.index as usize] == "name");
                let definition = match definition {
                    Some(d) => d,
                    None => { return; },
                };
                let text = match definition.node.utf8_text(source) {
                    Ok(text) => text.to_owned(),
                    Err(_) => { return; },
                };
                self.declarations.insert(
                    text,
                    Declaration::new(&uri, definition, documentation)
                );
            });
        entry.insert(FileInformation { content, tree, version, definitions });
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
        let point = W(&position).into();
        let closest = file.tree.root_node().descendant_for_point_range(point, point)?;
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

#[cfg(test)]
mod tests {
    use tower_lsp::lsp_types::Url;

    use super::{Cache, Value, FileInformation};

    async fn make_file(uri: impl Into<&str>, content: impl Into<String>) -> Option<FileInformation> {
        let cache = Cache::new().unwrap();
        let content: String = content.into();
        let uri = Url::parse(uri.into()).ok()?;
        // let uri = Url::parse("memory://inlcude.bff").unwrap();
        let version = 0;
        cache.add_file(uri.clone(), content, version).await.ok()?;
        let file = cache.files.get(&uri)?;
        Some(file.clone())
    }

    #[tokio::test]
    async fn include() {
        let file = make_file("memory://include.bff", r#"#include "builtins/alias.bff""#).await.expect("should have include");
        let definition = file.definitions.first_key_value().expect("should have definition");
        assert_eq!(
            Value::Include("builtins/alias.bff".to_string()),
            definition.1.clone()
        )
    }

    #[tokio::test]
    async fn import() {
        let file = make_file("memory://import.bff", r#"#import HOME"#).await.expect("should have import");
        let definition = file.definitions.first_key_value().expect("should have definition");
        assert_eq!(
            Value::Import { name: "HOME".into(), value: std::env::var("HOME").expect("HOME SHOULD EXIST") },
            definition.1.clone()
        )
    }

}
