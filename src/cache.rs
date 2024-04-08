use std::{io, fs::File, path::Path, collections::BTreeMap};
use dashmap::{DashMap, mapref::entry::Entry};
use deadpool::managed::BuildError;
use tower_lsp::lsp_types::{Location, MarkupContent, Position, MarkupKind, Range};
use url::Url;
use tracing::trace;
use tree_sitter::{QueryCursor, QueryCapture, QueryError, Tree, QueryMatch, Node, Language, Parser};

use crate::{parser_pool::{ParserPool, new_pool, ParserManager}, queries::{Queries, Preprocessor}, helpers::W};

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
            Error::Uri { .. }  => tower_lsp::jsonrpc::Error::parse_error(),
            Error::TreeSitter  => tower_lsp::jsonrpc::Error::parse_error(),
            Error::Unavailable => tower_lsp::jsonrpc::Error::internal_error(),
        }
    }
}
type Result<T> = std::result::Result<T, Error>;

pub fn markdown(value: String) -> MarkupContent {
    MarkupContent { kind: MarkupKind::Markdown, value }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Declaration {
    location: Location,
    documentation: MarkupContent,
}

impl Declaration {
    fn new(uri: &Url, node: &Node, documentation: Vec<&str>) -> Self {
        Self {
            location: Location {
                uri: uri.clone(),
                range: W(node).into(),
            },
            documentation: markdown(documentation.iter()
                .map(|line| line.get(3..line.len()).unwrap_or_default())
                .fold(String::new(), |a, b| a + b + "\n")),
        }
    }
}

pub type Declarations = DashMap::<String, Declaration>;

#[derive(Debug, Clone, Eq, PartialEq, thiserror::Error)]
pub enum ExecutionError {
    #[error("EnvironmentVariable")]
    EnvironmentVariable(#[from] std::env::VarError),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    String(String),
    Boolean(bool),
    Integer(i32),
    Array(Vec<Value>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Import {
    name: String,
    variable: std::result::Result<Value, std::env::VarError>,
}

impl Import {
    fn new(name: impl Into<String>, variable: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            variable: Ok(Value::String(variable.into())),
        }
    }

    fn error(name: impl Into<String>, error: impl Into<std::env::VarError>) -> Self {
        Self {
            name: name.into(),
            variable: Err(error.into()),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, thiserror::Error)]
pub enum UrlError {
    #[error("file does not exist")]
    NotExist,
    #[error("parse")]
    Parse,
    #[error("io")]
    IO,
    #[error("non-unicode")]
    NotUnicode,
}

pub type Include = std::result::Result<Url, UrlError>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Decl {
    Import(Import),
    Include(Include),
}

pub type Definitions = BTreeMap<W<Range>, Decl>;

#[derive(Debug, Clone)]
pub struct FileInformation {
    pub content: String,
    pub version: i32,
    pub tree: tree_sitter::Tree,
    pub definitions: Definitions,
    pub once: bool,
}

pub type FileCache = DashMap<Url, FileInformation>;

pub struct Cache {
    declarations: Declarations,
    files: FileCache,
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

    pub fn new(language: &Language) -> std::result::Result<Self, CacheCreateError> {
        let queries = Queries::new(language)?;
        Ok(Cache {
            declarations: Declarations::new(),
            files: FileCache::default(),
            queries,
        })
    }

    pub fn load_file(filename: &Path) -> Result<(Url, String)> {
        let uri = Url::from_file_path(filename)
            .map_err(|_| Error::Uri { uri: filename.to_string_lossy().to_string() })?;
        let file = File::open(filename)?;
        Ok((uri, io::read_to_string(file)?))
    }

    fn find_file(filename: &str) -> Include {
        let mut current = std::env::current_dir().map_err(|_| UrlError::Parse)?;
        current.push(filename);
        if !current.exists() {
            return Err(UrlError::NotExist)
        }
        Url::from_file_path(current).map_err(|_| UrlError::Parse)
    }

    fn find_environment_variable(name: &str) -> Import {
        let variable = std::env::var(name).map(Value::String);
        Import { name: name.to_string(), variable }
    }

    pub fn preprocess(&self, file: &mut FileInformation) {
        
    }

    pub fn add_file(&self, parser: &mut Parser, uri: Url, content: String, version: i32) -> Result<()> {
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

        let tree = parser
            .parse(&content, tree)
            .ok_or(Error::TreeSitter)?;

        let mut definitions = Definitions::new();
        let source = content.as_bytes();
        let mut once = false;

        QueryCursor::new()
            .matches(&self.queries.preprocessor.0, tree.root_node(), source)
            .map(|m| self.queries.preprocessor.parse(source, m))
            .for_each(|(n, p)| {
                match p {
                    Preprocessor::Include(filename) => {
                        definitions.insert(W(W(&n).into()), Decl::Include(Self::find_file(filename)));
                    },
                    Preprocessor::Import(variable) => {
                        definitions.insert(W(W(&n).into()), Decl::Import(Self::find_environment_variable(variable)));
                    }
                    Preprocessor::Once => {
                        once = true;
                    }
                    _ => { },
                }
            });

        QueryCursor::new()
            .matches(&self.queries.function_definition.0, tree.root_node(), source)
            .map(|m| self.queries.function_definition.parse(source, m))
            .for_each(|(n, f)| {
                self.declarations.insert(f.name.to_string(), Declaration::new(&uri, &n, f.documentation));
            })
        ;
        
        entry.insert(FileInformation { content, tree, version, definitions, once });

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

}

#[cfg(test)]
mod tests {
    use std::{ffi::OsString, os::unix::ffi::OsStringExt, ops::Deref};

    use tower_lsp::lsp_types::{Url, lsif::LocationOrRangeId, MarkupContent, DeclarationCapability, Location, Range, Position};
    use tree_sitter::Parser;

    use crate::cache::{Decl, Import, UrlError, Declaration};

    use super::{Cache, FileInformation};

    fn make_file<'a>(uri: impl Into<&'a str>, content: impl Into<String>) -> Option<(Cache, FileInformation)> {
        let language = tree_sitter_fastbuild::language();
        let mut parser = Parser::new();
        parser.set_language(&language).ok()?;
        let cache = Cache::new(&language).unwrap();
        let content: String = content.into();
        let uri = Url::parse(uri.into()).ok()?;
        // let uri = Url::parse("memory://inlcude.bff").unwrap();
        let version = 0;
        cache.add_file(&mut parser, uri.clone(), content, version).ok()?;
        let file = cache.files.get(&uri)?.clone();
        Some((cache, file))
    }

    #[test]
    fn include() {
        let (cache, file) = make_file("memory://include.bff", r#"#include "builtins/alias.bff""#).expect("should have include");
        let definition = file.definitions.first_key_value().expect("should have definition");
        let mut path = std::env::current_dir().expect("should have current dir");
        path.push("builtins/alias.bff");
        assert!(!file.once);
        assert_eq!(
            Decl::Include(Url::from_file_path(path).map_err(|_| UrlError::Parse)),
            definition.1.clone()
        )
    }

    #[test]
    fn include_braces() {
        let (cache, file) = make_file("memory://include.bff", r#"#include <builtins/alias.bff>"#).expect("should have include");
        let definition = file.definitions.first_key_value().expect("should have definition");
        let mut path = std::env::current_dir().expect("should have current dir");
        path.push("builtins/alias.bff");
        assert!(!file.once);
        assert_eq!(
            Decl::Include(Url::from_file_path(path).map_err(|_| UrlError::Parse)),
            definition.1.clone()
        )
    }

    #[test]
    fn include_does_not_exist() {
        let (cache, file) = make_file("memory://include.bff", r#"#include <not_exist>"#).expect("should have include");
        let definition = file.definitions.first_key_value().expect("should have definition");
        assert!(!file.once);
        assert_eq!(
            Decl::Include(Err(UrlError::NotExist)),
            definition.1.clone()
        )
    }

    #[test]
    fn import() {
        let (cache, file) = make_file("memory://import.bff", r#"#import HOME"#).expect("should have import");
        let definition = file.definitions.first_key_value().expect("should have definition");
        assert!(!file.once);
        assert_eq!(
            Decl::Import(Import::new("HOME", std::env::var("HOME").expect("HOME should be defined"))),
            definition.1.clone()
        )
    }

    #[test]
    fn once() {
        let (cache, file) = make_file("memory://once.bff", "#once").expect("should have import");
        assert!(file.once);
    }

    #[test]
    fn import_not_present() {
        let (cache, file) = make_file("memory://import.bff", r#"#import not_present"#).expect("should have import");
        let definition = file.definitions.first_key_value().expect("should have definition");
        assert_eq!(
            Decl::Import(Import::error("not_present", std::env::VarError::NotPresent)),
            definition.1.clone()
        )
    }

    #[test]
    fn import_not_unicode() {
        let non_unicode = OsString::from_vec(vec![255]);
        std::env::set_var("not_unicode", non_unicode.clone());
        let (cache, file) = make_file("memory://import.bff", r#"#import not_unicode"#).expect("should have import");
        let definition = file.definitions.first_key_value().expect("should have definition");
        assert_eq!(
            Decl::Import(Import::error("not_unicode", std::env::VarError::NotUnicode(non_unicode))),
            definition.1.clone()
        )
    }

    #[test]
    fn function_definition() {
        let (cache, file) = make_file("memory://function.bff", "function f() {}").expect("should be correct");
        let definition = cache.declarations.get("f").unwrap();
        assert_eq!(
            *definition.deref(),
            Declaration {
                location: Location {
                    uri: Url::parse("memory://function.bff").unwrap(),
                    range: Range {
                        start: Position::new(0, 0),
                        end: Position::new(0, 15),
                    },
                },
                documentation: MarkupContent {
                    kind: tower_lsp::lsp_types::MarkupKind::Markdown,
                    value: "".to_string(),
                },
            }
        )
    }

    #[test]
    fn function_definition_with_documentation() {
        let (cache, file) = make_file("memory://function.bff", "// docs\nfunction f() {}").expect("should be correct");
        let definition = cache.declarations.get("f").unwrap();
        assert_eq!(
            *definition.deref(),
            Declaration {
                location: Location {
                    uri: Url::parse("memory://function.bff").unwrap(),
                    range: Range {
                        start: Position::new(1, 0),
                        end: Position::new(1, 15),
                    },
                },
                documentation: MarkupContent {
                    kind: tower_lsp::lsp_types::MarkupKind::Markdown,
                    value: "docs\n".to_string(),
                },
            }
        )
    }

}
