use tower_lsp::lsp_types::{Range, Position};
use tree_sitter::{QueryCapture, Point};
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

impl<'tree> From<W<&QueryCapture<'tree>>> for Range {
    fn from(value: W<&QueryCapture>) -> Self {
        Self {
            start: W(&value.node.start_position()).into(),
            end:   W(&value.node.end_position()).into(),
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
