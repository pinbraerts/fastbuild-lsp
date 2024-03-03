use tower_lsp::lsp_types::{Range, Position};
use tree_sitter::{Query, Language, QueryError, Point, Node, Tree, QueryCursor, QueryCapture};

fn position(point: Point) -> Position {
    Position {
        line: point.row as u32,
        character: point.column as u32,
    }
}

fn range(capture: &QueryCapture) -> Range {
    Range {
        start: position(capture.node.start_position()),
        end:   position(capture.node.end_position()),
    }
}

fn content(capture: &QueryCapture, source: &[u8]) -> (Range, String) {
    (
        range(capture),
        capture.node.utf8_text(source).unwrap_or_default().to_owned(),
    )
}

pub fn query(query: &Query, tree: &Tree, source: &[u8]) -> Vec<(Range, String)> {
    // let names = query.capture_names();
    QueryCursor::new()
        .matches(query, tree.root_node(), source)
        .flat_map(|m| m.captures)
        .map(|capture| content(capture, source))
        .collect()
}

pub struct Queries {
    pub function_definition: Query,
}

// (
//  (comment) @documentation
//  .
//  (function_definition)
// )
impl Queries {
    pub fn new(language: &Language) -> Result<Self, QueryError> {
        Query::new(language, "(function_definition name: (identifier) @name)")
            .map(|query| Queries { function_definition: query })
    }
}
