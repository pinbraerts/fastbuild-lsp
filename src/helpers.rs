use tower_lsp::lsp_types::{Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, Position, Range, SemanticToken};
use tree_sitter::{Node, Point};
use url::Url;
use std::{cmp::Ordering, ops::{Deref, DerefMut}};

#[derive(Debug, Eq, PartialEq, Copy, Clone, Default)]
pub struct W<T>(pub T);

impl<T> From<T> for W<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<T> Deref for W<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for W<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<W<Point>> for Position {
    fn from(value: W<Point>) -> Self {
        Self {
            line: value.row as u32,
            character: value.column as u32,
        }
    }
}

impl From<W<Position>> for Point {
    fn from(value: W<Position>) -> Self {
        Self {
            row: value.line as usize,
            column: value.character as usize,
        }
    }
}

impl<'tree> From<W<Node<'tree>>> for Range {
    fn from(value: W<Node>) -> Self {
        Self {
            start: W(value.start_position()).into(),
            end:   W(value.end_position()).into(),
        }
    }
}

impl PartialOrd for W<Range> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.start.cmp(&other.start))
    }
}

impl Ord for W<Range> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.start.cmp(&other.start)
    }
}

impl PartialOrd for W<Position> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for W<Position> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.line.cmp(&other.line) {
            Ordering::Equal => self.character.cmp(&other.character),
            ordering => ordering,
        }
    }
}

impl W<SemanticToken> {

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

    pub fn complement(self, ranges: Vec<tree_sitter::Range>) -> Vec<tree_sitter::Range> {
        let mut result = Vec::new();
        let mut start_point = self.start_point;
        let mut start_byte = self.start_byte;
        for range in ranges {
            result.push(tree_sitter::Range {
                start_point,
                start_byte,
                end_point: range.start_point,
                end_byte: range.start_byte,
            });
            start_point = range.end_point;
            start_byte = range.end_byte;
        }
        if start_byte < self.end_byte {
            result.push(tree_sitter::Range {
                start_point,
                start_byte,
                end_point: self.end_point,
                end_byte: self.end_byte,
            });
        }
        result
    }

}

impl<'tree> W<Node<'tree>> {

    pub fn is_empty(&self) -> bool {
        W(self.0.range()).is_empty()
    }

    pub fn size(&self) -> usize {
        W(self.0.range()).size()
    }

    pub fn expect(self, name: &str) -> std::result::Result<Self, Diagnostic> {
        self.child_by_field_name(name)
            .map(Self)
            .ok_or_else(|| self.error(format!("expected {}", name)).0)
    }

    pub fn text(self, content: &[u8]) -> std::result::Result<&str, Diagnostic> {
        self.utf8_text(content).map_err(|_| self.error("non-unicode text").0)
    }

    pub fn error(self, message: impl Into<String>) -> W<Diagnostic> {
        Diagnostic::new(
            self.into(),
            Some(DiagnosticSeverity::ERROR),
            None,
            None,
            message.into(),
            None,
            None,
        ).into()
    }

    pub fn warning(self, message: impl Into<String>) -> W<Diagnostic> {
        Diagnostic::new(
            self.into(),
            Some(DiagnosticSeverity::WARNING),
            None,
            None,
            message.into(),
            None,
            None,
        ).into()
    }

    pub fn related(self, url: &Url, message: impl Into<String>) -> Vec<DiagnosticRelatedInformation> {
        vec![DiagnosticRelatedInformation {
            message: message.into(),
            location: self.url(url),
        }]
    }

    pub fn url(self, url: &Url) -> Location {
        Location::new(url.clone(), self.into())
    }

    pub fn get(self, index: usize) -> std::result::Result<Self, W<Diagnostic>> {
        self.named_child(index).map(W).ok_or_else(|| self.error("expected named node"))
    }

}

impl W<Diagnostic> {

    pub fn with(mut self, related: Option<Vec<DiagnosticRelatedInformation>>) -> Self {
        self.related_information = related;
        self
    }

}
