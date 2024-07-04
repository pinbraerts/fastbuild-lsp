use tree_sitter::{Query, Language, QueryError};

pub struct Queries {
    pub error: Query,
}

impl Queries {
    pub fn new(language: &Language) -> Result<Self, QueryError> {
        Ok(Queries {
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
