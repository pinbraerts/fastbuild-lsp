use std::ops::Deref;

use tree_sitter::{Query, Language, QueryError, QueryCapture, QueryMatch, QueryCursor, Node};

pub enum Preprocessor<'tree> {
    Define (&'tree str),
    Undef  (&'tree str),
    Import (&'tree str),
    Include(&'tree str),
    Once,
    If,
    Error,
}

pub struct PreprocessorQuery(pub Query);

impl PreprocessorQuery {
    pub fn new(language: &Language) -> std::result::Result<Self, QueryError> {
        Query::new(language, include_str!("../queries/preprocessor.scm")).map(PreprocessorQuery)
    }

    pub fn parse<'tree>(&self, source: &'tree [u8], _match: QueryMatch<'_, 'tree>) -> (Node<'tree>, Preprocessor<'tree>) {
        let names = self.0.capture_names();
        let capture = _match.captures.first().expect("queries should have captures");
        (capture.node, match names[capture.index as usize] {
            "define" => {
                _match.captures.get(1)
                    .map(|v| {
                        v.node.utf8_text(source)
                            .map(Preprocessor::Define)
                            .unwrap_or_else(|_| Preprocessor::Error)
                    })
                    .unwrap_or_else(|| Preprocessor::Error)
            },
            "undef" => {
                _match.captures.get(1)
                    .map(|v| {
                        v.node.utf8_text(source)
                            .map(Preprocessor::Undef)
                            .unwrap_or_else(|_| Preprocessor::Error)
                    })
                    .unwrap_or_else(|| Preprocessor::Error)
            },
            "import" => {
                _match.captures.get(1)
                    .map(|v| {
                        v.node.utf8_text(source)
                            .map(Preprocessor::Import)
                            .unwrap_or_else(|_| Preprocessor::Error)
                    })
                    .unwrap_or_else(|| Preprocessor::Error)
            },
            "include" => {
                _match.captures.get(1)
                    .map(|v| {
                        v.node.utf8_text(source)
                            .map(Preprocessor::Include)
                            .unwrap_or_else(|_| Preprocessor::Error)
                    })
                    .unwrap_or_else(|| Preprocessor::Error)
            },
            "once" => Preprocessor::Once,
            "if" => Preprocessor::If,
            &_ => Preprocessor::Error,
        })
    }
}

pub struct Queries {
    pub preprocessor: PreprocessorQuery,
    pub function_definition: Query,
}

impl Queries {
    pub fn new(language: &Language) -> Result<Self, QueryError> {
        Ok(Queries {
            preprocessor: PreprocessorQuery::new(language)?,
            function_definition: Query::new(language, include_str!("../queries/function.scm"))?,
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
