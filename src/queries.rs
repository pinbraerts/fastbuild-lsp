use std::ops::Deref;

use tree_sitter::{Language, Node, Query, QueryCapture, QueryError, QueryMatch};

pub enum PreprocessorCapture<'tree> {
    Define(Node<'tree>),
    Undef(Node<'tree>),
    Import(Node<'tree>),
    Include(Node<'tree>),
    Once,
    If {
        condition: Node<'tree>,
        body: Vec<Node<'tree>>,
        alternative: Vec<Node<'tree>>,
    },
}

impl<'tree> PreprocessorCapture<'tree> {
    pub fn new<'cursor>(value: QueryMatch<'cursor, 'tree>) -> Option<Self> {
        match value.captures.first() {
            Some(capture) => match capture.index {
                0 => Some(PreprocessorCapture::Define(capture.node)),
                1 => Some(PreprocessorCapture::Undef(capture.node)),
                2 => Some(PreprocessorCapture::Import(capture.node)),
                3 => Some(PreprocessorCapture::Include(capture.node)),
                4 => Some(PreprocessorCapture::Once),
                5 => Some(PreprocessorCapture::If {
                    condition: capture.node,
                    body: value.captures.iter().filter(|c| c.index == 6).map(|c| c.node).collect(),
                    alternative: value.captures.iter().filter(|c| c.index == 7).map(|c| c.node).collect(),
                }),
                _ => None,
            }
            None => None,
        }
    }
}

pub struct Queries {
    pub preprocessor: Query,
    pub error: Query,
}

impl Queries {
    pub fn new(language: &Language) -> Result<Self, QueryError> {
        Ok(Queries {
            preprocessor: Query::new(language, include_str!("../queries/preprocessor.scm"))?,
            error: Query::new(language, "(ERROR) @error")?,
        })
    }
}

#[cfg(test)]
mod test {
    use super::Queries;

    #[test]
    fn create() {
        let language = tree_sitter_fastbuild::language();
        assert!(Queries::new(&language).is_ok());
    }
}
