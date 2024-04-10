use std::{collections::HashMap, env::VarError, str::Utf8Error, os::unix::ffi::OsStrExt};
use dashmap::DashMap;
use tower_lsp::lsp_types::{MarkupContent, Position, MarkupKind, Range};
use url::Url;
use tracing::trace;
use tree_sitter::{Node, Parser, Tree};

use crate::helpers::W;

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
    NotUnicode(#[from] Utf8Error),
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
    fn new(node: &Node, value: Result<Value>, documentation: &mut String) -> Self {
        Self {
            range: W(node).into(),
            value,
            documentation: if documentation.is_empty() {
                None
            }
            else {
                Some(markdown(std::mem::take(documentation)))
            },
        }
    }
}

pub type Symbols = HashMap::<Box<str>, Symbol>;

#[derive(Debug)]
pub struct FileInformation {
    pub content: String,
    pub tree: tree_sitter::Tree,
    pub version: i32,
    pub symbols: Symbols,
    pub once: bool,
}

pub type FileCache = DashMap<Url, FileInformation>;

#[derive(Default)]
pub struct Cache {
    pub(crate) files: FileCache,
}

impl Cache {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn map_node<'tree>(&self, documentation: &mut String, node: &'tree Node, source: &'tree [u8], once: &mut bool) -> Result<Option<(&'tree str, Symbol)>> {
        Ok(match node.kind() {
            "comment" => {
                if let Some(trimmed) = node.utf8_text(source)?.get(3..) {
                    *documentation += trimmed;
                    *documentation += "\n";
                }
                None
            }
            "preprocessor_define" => {
                let name = node.child_by_field_name("variable").ok_or(Error::Parse)?.utf8_text(source)?;
                Some((
                    name,
                    Symbol::new(node, Ok(Value::Macro), documentation)
                ))
            }
            "preprocessor_undef" => {
                let name = node.child_by_field_name("variable").ok_or(Error::Parse)?.utf8_text(source)?;
                Some((
                    name,
                    Symbol::new(node, Ok(Value::Macro), documentation)
                ))
            }
            "preprocessor_import" => {
                let name = node.child_by_field_name("variable").ok_or(Error::Parse)?.utf8_text(source)?;
                Some((
                    name,
                    Symbol::new(
                        node,
                        self.find_environment_variable(name),
                        documentation
                    )
                ))
            }
            "preprocessor_include" => {
                let name = node.child_by_field_name("filename").ok_or(Error::Parse)?.utf8_text(source)?;
                Some((
                    name,
                    Symbol::new(
                        node,
                        self.find_file(name),
                        documentation
                    )
                ))
            }
            "preprocessor_once" => {
                *once = true;
                if !documentation.is_empty() {
                    *documentation = String::new();
                }
                None
            },
            "function_definition" => {
                let name = node.child_by_field_name("name").ok_or(Error::Parse)?.utf8_text(source)?;
                Some((
                    name,
                    Symbol::new(node, Ok(Value::Function), documentation)
                ))
            }
            "#" => None,
            &_ => {
                if !documentation.is_empty() {
                    *documentation = String::new();
                }
                None
            }
        })
    }

    pub fn convert_tree(&self, tree: &Tree, source: &[u8]) -> Result<(Symbols, bool)> {
        let mut cursor = tree.walk();
        if !cursor.goto_first_child() {
            return Err(Error::Parse);
        }
        let mut documentation = String::new();
        let mut symbols = Symbols::new();
        let mut once = false;
        // Symbol {
        //     range: W(node).into(),
        //     documentation: None,
        //     value
        // };
        loop {
            if let Some((name, symbol)) = self.map_node(&mut documentation, &cursor.node(), source, &mut once)? {
                symbols.insert(name.into(), symbol);
            }
            if !cursor.goto_next_sibling() {
                break;
            }
        }
        Ok((symbols, once))
    }


    fn find_file(&self, filename: &str) -> Result<Value> {
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

    fn find_environment_variable(&self, name: &str) -> Result<Value> {
        match std::env::var(name) {
            Ok(variable) => Ok(Value::String(variable)),
            Err(VarError::NotPresent) => Err(Error::EnvironmentVariableNotFound),
            Err(VarError::NotUnicode(s)) => match std::str::from_utf8(s.as_bytes()) {
                Ok(s) => Ok(Value::String(s.to_string())),
                Err(e) => Err(Error::NotUnicode(e)),
            },
        }
    }

    pub fn add_file(&self, parser: &mut Parser, uri: Url, content: String, version: i32) -> Result<()> {
        trace!("processing file {:?}", uri);
        let entry = self.files.entry(uri);

        let tree = parser.parse(&content, None).ok_or(Error::Parse)?;

        let (symbols, once) = self.convert_tree(&tree, content.as_bytes())?;
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
        let cache = Cache::new();
        cache.add_file(&mut parser, uri, content.into(), 0).ok()?;
        Some(cache)
    }

    #[test]
    fn include() {
        let uri = Url::parse("memory://include.bff").expect("surely this is valid");
        let reference = "builtins/alias.bff";
        let content = format!("// docs\n#include \"{}\"", reference);
        let cache = make_file(uri.clone(), content.clone()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        let mut path = std::env::current_dir().expect("should have current dir");
        path.push(reference);
        let include_uri = Url::from_file_path(path).expect("should parse URL");
        assert_eq!(definition.range.start, Position::new(1, 1));
        assert_eq!(definition.range.end, Position::new(1, content.lines().last().unwrap().len() as u32));
        assert_eq!(definition.documentation, Some(markdown("docs\n")));
        assert_eq!(definition.value, Ok(Value::Filename(include_uri)));
    }

    #[test]
    fn include_braces() {
        let uri = Url::parse("memory://include.bff").expect("surely this is valid");
        let reference = "builtins/alias.bff";
        let content = format!("// docs\n#include <{}>", reference);
        let cache = make_file(uri.clone(), content.clone()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        let mut path = std::env::current_dir().expect("should have current dir");
        path.push(reference);
        let include_uri = Url::from_file_path(path).expect("should parse URL");
        assert_eq!(definition.range.start, Position::new(1, 1));
        assert_eq!(definition.range.end, Position::new(1, content.lines().last().unwrap().len() as u32));
        assert_eq!(definition.documentation, Some(markdown("docs\n")));
        assert_eq!(definition.value, Ok(Value::Filename(include_uri)));
    }

    #[test]
    fn include_does_not_exist() {
        let uri = Url::parse("memory://include.bff").expect("surely this is valid");
        let reference = "not-existent";
        let content = format!("// docs\n#include \"{}\"", reference);
        let cache = make_file(uri.clone(), content.clone()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        assert_eq!(definition.range.start, Position::new(1, 1));
        assert_eq!(definition.range.end, Position::new(1, content.lines().last().unwrap().len() as u32));
        assert_eq!(definition.documentation, Some(markdown("docs\n")));
        assert_eq!(definition.value, Err(Error::FileNotFound));
    }

    #[test]
    fn import() {
        let uri = Url::parse("memory://import.bff").expect("surely this is valid");
        let reference = "HOME";
        let content = format!("// docs\n#import {}", reference);
        let cache = make_file(uri.clone(), content.to_string()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        let home = std::env::var(reference).expect("HOME should exist");
        assert_eq!(definition.range.start, Position::new(1, 1));
        assert_eq!(definition.range.end, Position::new(1, content.lines().last().unwrap().len() as u32));
        assert_eq!(definition.documentation, Some(markdown("docs\n")));
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
        let content = format!("// import\n#import {}", reference);
        let cache = make_file(uri.clone(), content.clone()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        assert_eq!(definition.range.start, Position::new(1, 1));
        assert_eq!(definition.range.end, Position::new(1, content.lines().last().unwrap().len() as u32));
        assert_eq!(definition.documentation, Some(markdown("import\n")));
        assert_eq!(definition.value, Err(Error::EnvironmentVariableNotFound));        
    }

    #[test]
    fn import_not_unicode() {
        let reference = "not_unicode";
        let non_unicode = OsString::from_vec(vec![255]);
        std::env::set_var(reference, non_unicode);
        let uri = Url::parse("memory://import.bff").expect("surely this is valid");
        let content = format!("// import\n#import {}", reference);
        let cache = make_file(uri.clone(), content.clone()).expect("should have include");
        let file_info = cache.files.get(&uri).expect("file not found");
        assert!(!file_info.once);
        let definition = file_info.symbols.get(reference).expect("should have definition");
        assert_eq!(definition.range.start, Position::new(1, 1));
        assert_eq!(definition.range.end, Position::new(1, content.lines().last().unwrap().len() as u32));
        assert_eq!(definition.documentation, Some(markdown("import\n")));
        assert!(definition.value.is_err());        
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
