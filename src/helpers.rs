use tower_lsp::lsp_types::{Position, Range, SemanticToken};
use tree_sitter::{Point, Node};
use std::ops::Deref;

#[derive(Debug, Eq, PartialEq, Copy, Clone, Default)]
pub struct W<T>(pub T);

impl<T> Deref for W<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<W<&Point>> for Position {
    fn from(value: W<&Point>) -> Self {
        Self {
            line: value.row as u32,
            character: value.column as u32,
        }
    }
}

impl From<W<&Position>> for Point {
    fn from(value: W<&Position>) -> Self {
        Self {
            row: value.line as usize,
            column: value.character as usize,
        }
    }
}

impl<'tree> From<W<&Node<'tree>>> for Range {
    fn from(value: W<&Node>) -> Self {
        Self {
            start: W(&value.start_position()).into(),
            end:   W(&value.end_position()).into(),
        }
    }
}

impl PartialOrd for W<Range> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.start.cmp(&other.start))
    }
}

impl Ord for W<Range> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.start.cmp(&other.start)
    }
}

impl W<&SemanticToken> {

    pub fn delta(&self, prev: &mut SemanticToken) -> SemanticToken {
        let result = SemanticToken {
            delta_line: self.delta_line - prev.delta_line,
            delta_start: if self.delta_line == prev.delta_line {
                self.delta_start - prev.delta_start
            }
            else {
                self.delta_start
            },
            length: self.length,
            token_type: self.token_type,
            token_modifiers_bitset: self.token_modifiers_bitset,
        };
        prev.delta_start = self.delta_start;
        prev.delta_line = self.delta_line;
        result
    }

}

impl W<tree_sitter::Range> {

    pub fn is_empty(&self) -> bool {
        self.0.end_byte <= self.0.start_byte
    }

    pub fn size(&self) -> usize {
        if self.is_empty() {
            0
        }
        else {
            self.0.end_byte - self.0.start_byte
        }
    }

}

impl<'tree> W<Node<'tree>> {

    pub fn is_empty(&self) -> bool {
        W(self.0.range()).is_empty()
    }

    pub fn size(&self) -> usize {
        W(self.0.range()).size()
    }

}
