use tree_sitter::{Query, Language, QueryError, QueryMatch, Node};

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
        Query::new(language, include_str!("../queries/preprocessor.scm")).map(Self)
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

pub struct Function<'tree> {
    pub documentation: Vec<&'tree str>,
    pub name: &'tree str,
}

pub struct FunctionQuery(pub Query);

impl FunctionQuery {
    fn new(language: &Language) -> std::result::Result<Self, QueryError> {
        Query::new(language, include_str!("../queries/function.scm")).map(Self)
    }

    pub fn parse<'tree>(&self, source: &'tree [u8], _match: QueryMatch<'_, 'tree>) -> (Node<'tree>, Function<'tree>) {
        let names = self.0.capture_names();
        let mut documentation: Vec<&'tree str> = Vec::new();
        let mut function: Option<Node<'tree>> = None;
        let mut name: Option<&'tree str> = None;
        _match.captures.iter()
            .map(|c| (names[c.index as usize], c.node))
            .for_each(|(kind, node)| {
                match kind {
                    "documentation" => {
                        documentation.push(node.utf8_text(source).expect("should be unicode"));
                    },
                    "function" => {
                        function = Some(node);
                    },
                    "name" => {
                        name = Some(node.utf8_text(source).expect("should be unicode"));
                    },
                    &_ => { },
                };
            });
        (function.unwrap(), Function {
            name: name.unwrap(),
            documentation,
        })
    }
}

pub struct Queries {
    pub preprocessor: PreprocessorQuery,
    pub function_definition: FunctionQuery,
}

impl Queries {
    pub fn new(language: &Language) -> Result<Self, QueryError> {
        Ok(Queries {
            preprocessor: PreprocessorQuery::new(language)?,
            function_definition: FunctionQuery::new(language)?,
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
