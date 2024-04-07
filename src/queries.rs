use tree_sitter::{Query, Language, QueryError};

pub struct Queries {
    pub preprocessor: Query,
    pub function_definition: Query,
}

impl Queries {
    pub fn new(language: &Language) -> Result<Self, QueryError> {
        Ok(Queries {
            preprocessor: Query::new(language, include_str!("../queries/preprocessor.scm"))?,
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
