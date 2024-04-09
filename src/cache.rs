use std::{collections::HashMap, env::VarError};
use dashmap::{DashMap, mapref::entry::Entry};
use tower_lsp::lsp_types::{MarkupContent, Position, MarkupKind, Range};
use url::Url;
use tracing::trace;
use tree_sitter::{QueryCursor, QueryError, Node, Language, Parser};

use crate::{queries::{Queries, Preprocessor}, helpers::W};

#[derive(thiserror::Error, Debug, Eq, PartialEq, Clone)]
pub enum Error {
    #[error("symbol not found")]
    SymbolNotFound,
    #[error("parse error")]
    Parse,
    #[error("file not found")]
    FileNotFound,
    #[error("no current dir")]
    CurrentDirNotFound,
    #[error("environment variable not found")]
    EnvironmentVariableNotFound,
    #[error("not unicode")]
    NotUnicode,
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn markdown(value: impl Into<String>) -> MarkupContent {
    MarkupContent { kind: MarkupKind::Markdown, value: value.into() }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    String(String),
    Boolean(bool),
    Integer(i32),
    Array(Vec<Value>),
    Filename(Url),
    Function,
    Macro,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Symbol {
    pub range: Range,
    pub documentation: Option<MarkupContent>,
    pub value: Result<Value>,
}

impl Symbol {
    fn new(node: &Node, value: Result<Value>) -> Self {
        Self {
            range: W(node).into(),
            value,
            documentation: None,
        }
    }
}

pub type Symbols = HashMap::<Box<str>, Symbol>;

#[derive(Debug)]
pub struct FileInformation {
    pub content: String,
    pub version: i32,
    pub tree: tree_sitter::Tree,
    pub symbols: Symbols,
    pub once: bool,
}

pub type FileCache = DashMap<Url, FileInformation>;

pub struct Cache {
    pub(crate) files: FileCache,
    queries: Queries,
}

impl Cache {

    pub fn new(language: &Language) -> std::result::Result<Self, QueryError> {
        let queries = Queries::new(language)?;
        Ok(Cache {
            files: FileCache::default(),
            queries,
        })
    }

    fn find_file(filename: &str) -> Result<Value> {
        let mut current = std::env::current_dir().map_err(|_| Error::CurrentDirNotFound)?;
        current.push(filename);
        if !current.exists() {
            return Err(Error::FileNotFound)
        }
        match Url::from_file_path(current) {
            Ok(url) => Ok(Value::Filename(url)),
            Err(_) => Err(Error::FileNotFound),
        }
    }

    fn find_environment_variable(name: &str) -> Result<Value> {
        match std::env::var(name) {
            Ok(variable) => Ok(Value::String(variable)),
            Err(VarError::NotPresent) => Err(Error::EnvironmentVariableNotFound),
            Err(VarError::NotUnicode(_)) => Err(Error::NotUnicode),
        }
    }

    fn make_documentation(lines: &[&str]) -> Option<MarkupContent> {
        if lines.is_empty() {
            return None
        }
        Some(MarkupContent {
            kind: MarkupKind::Markdown,
            value: lines.iter()
                .map(|line| line.get(3..).unwrap_or_default())
                .fold(String::new(), |s, l| s + l + "\n")
        })
    }

    pub fn add_file(&self, parser: &mut Parser, uri: Url, content: String, version: i32) -> Result<()> {
        trace!("processing file {:?}", uri);
        let entry = self.files.entry(uri);
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
            .ok_or(Error::Parse)?;

        let mut symbols = Symbols::new();
        let source = content.as_bytes();
        let mut once = false;

        QueryCursor::new()
            .matches(&self.queries.preprocessor.0, tree.root_node(), source)
            .map(|m| self.queries.preprocessor.parse(source, m))
            .for_each(|(n, p)| {
                match p {
                    Preprocessor::Include(filename) => {
                        symbols.insert(filename.into(), Symbol::new(&n, Self::find_file(filename)));
                    }
                    Preprocessor::Import(variable) => {
                        symbols.insert(variable.into(), Symbol::new(&n, Self::find_environment_variable(variable)));
                    }
                    Preprocessor::Once => {
                        once = true;
                    }
                    Preprocessor::Define(variable) => {
                        symbols.insert(variable.into(), Symbol::new(&n, Ok(Value::Macro)));
                    }
                    Preprocessor::Undef(variable) => {
                        symbols.remove(variable);
                    }
                    _ => { }
                }
            });

        QueryCursor::new()
            .matches(&self.queries.function_definition.0, tree.root_node(), source)
            .map(|m| self.queries.function_definition.parse(source, m))
            .for_each(|(n, f)| {
                symbols.insert(f.name.into(), Symbol {
                    range: W(&n).into(),
                    value: Ok(Value::Function),
                    documentation: Self::make_documentation(f.documentation.as_slice()),
                });
            })
        ;
        
        entry.insert(FileInformation { content, tree, version, symbols, once });

        Ok(())
    }

    pub fn find_symbol(&self, uri: Url, position: Position) -> Option<(Url, Range, Symbol)> {
        let (word, range) = self.get_word(uri, position)?;
        self.files.iter()
            .find_map(|f| f.symbols
                .get(word.as_str())
                .map(|s| (f.key().clone(), range, (*s).clone()))
            )
    }

    fn get_word(&self, uri: Url, position: Position) -> Option<(String, Range)> {
        let file = self.files.get(&uri)?;
        let point = W(&position).into();
        let closest = file.tree.root_node().descendant_for_point_range(point, point)?;
        let closest = match closest.kind() {
            "identifier" => {
                Some(closest)
            },
            "usage" => {
                closest.child(0)
            },
            _ => None,
        }?;
        let word = closest.utf8_text(file.content.as_bytes()).ok()?;
        Some((word.to_string(), W(&closest).into()))
    }

}

#[cfg(test)]
mod tests {
    use std::{ffi::OsString, os::unix::ffi::OsStringExt};

    use tower_lsp::lsp_types::{Url, Position};
    use tree_sitter::Parser;

    use super::{Cache, Value, Error, markdown};

    fn make_file(uri: Url, content: impl Into<String>) -> Option<Cache> {
        let language = tree_sitter_fastbuild::language();
        let mut parser = Parser::new();
        parser.set_language(&language).ok()?;
        let cache = Cache::new(&language).ok()?;
        cache.add_file(&mut parser, uri, content.into(), 0).ok()?;
        Some(cache)
    }

    #[test]
    fn include() {
        let uri = Url::parse("memory://include.bff").expect("surely this is valid");
        let reference = "builtins/alias.bff";
        let content = format!("#include \"{}\"", reference);
        let cache = make_file(uri.clone(), content.clone()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        let mut path = std::env::current_dir().expect("should have current dir");
        path.push(reference);
        let include_uri = Url::from_file_path(path).expect("should parse URL");
        assert_eq!(definition.range.start, Position::new(0, 1));
        assert_eq!(definition.range.end, Position::new(0, content.len() as u32));
        assert!(definition.documentation.is_none());
        assert_eq!(definition.value, Ok(Value::Filename(include_uri)));
    }

    #[test]
    fn include_braces() {
        let uri = Url::parse("memory://include.bff").expect("surely this is valid");
        let reference = "builtins/alias.bff";
        let content = format!("#include <{}>", reference);
        let cache = make_file(uri.clone(), content.clone()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        let mut path = std::env::current_dir().expect("should have current dir");
        path.push(reference);
        let include_uri = Url::from_file_path(path).expect("should parse URL");
        assert_eq!(definition.range.start, Position::new(0, 1));
        assert_eq!(definition.range.end, Position::new(0, content.len() as u32));
        assert!(definition.documentation.is_none());
        assert_eq!(definition.value, Ok(Value::Filename(include_uri)));
    }

    #[test]
    fn include_does_not_exist() {
        let uri = Url::parse("memory://include.bff").expect("surely this is valid");
        let reference = "not-existent";
        let content = format!("#include \"{}\"", reference);
        let cache = make_file(uri.clone(), content.clone()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        assert_eq!(definition.range.start, Position::new(0, 1));
        assert_eq!(definition.range.end, Position::new(0, content.len() as u32));
        assert!(definition.documentation.is_none());
        assert_eq!(definition.value, Err(Error::FileNotFound));
    }

    #[test]
    fn import() {
        let uri = Url::parse("memory://import.bff").expect("surely this is valid");
        let reference = "HOME";
        let content = format!("#import {}", reference);
        let cache = make_file(uri.clone(), content.to_string()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        let home = std::env::var(reference).expect("HOME should exist");
        assert_eq!(definition.range.start, Position::new(0, 1));
        assert_eq!(definition.range.end, Position::new(0, content.len() as u32));
        assert!(definition.documentation.is_none());
        assert_eq!(definition.value, Ok(Value::String(home)));        
    }

    #[test]
    fn once() {
        let uri = Url::parse("memory://once.bff").expect("surely this is valid");
        let content = "#once";
        let cache = make_file(uri.clone(), content).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(file_info.once);
    }

    #[test]
    fn import_not_present() {
        let uri = Url::parse("memory://import.bff").expect("surely this is valid");
        let reference = "not_present";
        let content = format!("#import {}", reference);
        let cache = make_file(uri.clone(), content.clone()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        assert_eq!(definition.range.start, Position::new(0, 1));
        assert_eq!(definition.range.end, Position::new(0, content.len() as u32));
        assert!(definition.documentation.is_none());
        assert_eq!(definition.value, Err(Error::EnvironmentVariableNotFound));        
    }

    #[test]
    fn import_not_unicode() {
        let reference = "not_unicode";
        let non_unicode = OsString::from_vec(vec![255]);
        std::env::set_var(reference, non_unicode);
        let uri = Url::parse("memory://import.bff").expect("surely this is valid");
        let content = format!("#import {}", reference);
        let cache = make_file(uri.clone(), content.clone()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        assert_eq!(definition.range.start, Position::new(0, 1));
        assert_eq!(definition.range.end, Position::new(0, content.len() as u32));
        assert!(definition.documentation.is_none());
        assert_eq!(definition.value, Err(Error::NotUnicode));        
    }

    #[test]
    fn function_definition() {
        let uri = Url::parse("memory://function.bff").expect("surely this is valid");
        let reference = "f";
        let content = format!("function {}() {{}}", reference);
        let cache = make_file(uri.clone(), content.clone()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        assert_eq!(definition.range.start, Position::new(0, 0));
        assert_eq!(definition.range.end, Position::new(0, content.len() as u32));
        assert!(definition.documentation.is_none());
        assert_eq!(definition.value, Ok(Value::Function));        
    }

    #[test]
    fn function_definition_with_documentation() {
        let uri = Url::parse("memory://function.bff").expect("surely this is valid");
        let reference = "f";
        let content = format!("// docs\nfunction {}() {{}}", reference);
        let cache = make_file(uri.clone(), content.clone()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        assert_eq!(definition.range.start, Position::new(1, 0));
        assert_eq!(definition.range.end, Position::new(1, content.lines().last().expect("content is empty").len() as u32));
        assert_eq!(definition.documentation, Some(markdown("docs\n")));
        assert_eq!(definition.value, Ok(Value::Function));        
    }

    #[test]
    fn function_definition_with_multiline_documentation() {
        let uri = Url::parse("memory://function.bff").expect("surely this is valid");
        let reference = "f";
        let content = format!("// docs\n// docs\nfunction {}() {{}}", reference);
        let cache = make_file(uri.clone(), content.clone()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        assert_eq!(definition.range.start, Position::new(2, 0));
        assert_eq!(definition.range.end, Position::new(2, content.lines().last().expect("content is empty").len() as u32));
        assert_eq!(definition.documentation, Some(markdown("docs\ndocs\n")));
        assert_eq!(definition.value, Ok(Value::Function));
    }

}
