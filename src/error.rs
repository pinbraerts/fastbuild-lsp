use std::str::Utf8Error;

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

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
