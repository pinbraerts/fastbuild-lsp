use tree_sitter::{Query, Language, QueryError};

pub struct Queries {
    pub function_definition: Query,
}

// (
//  (comment)* @documentation
//  .
//  (function_definition (identifier) @definition)
// )
impl Queries {
    pub fn new(language: &Language) -> Result<Self, QueryError> {
        Query::new(language, "
(
  (comment)* @documentation
  .
  (function_definition name: (identifier) @name)
)
")
            .map(|query| Queries { function_definition: query })
    }
}
