use tower_lsp::lsp_types::{Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString, Position, Range, SemanticToken};
use tree_sitter::{Node, Point};
use url::Url;

pub trait ToPosition {
    fn to_position(&self) -> Position;
}

pub trait ToPoint {
    fn to_point(&self) -> Point;
}

pub trait ToLspRange {
    fn to_range(&self) -> Range;
}

impl ToPoint for Position {
    fn to_point(&self) -> Point {
        Point::new(self.line as usize, self.character as usize)
    }
}

impl ToPosition for Point {
    fn to_position(&self) -> Position {
        Position::new(self.row as u32, self.column as u32)
    }
}

impl ToLspRange for tree_sitter::Range {
    fn to_range(&self) -> Range {
        Range::new(self.start_point.to_position(), self.end_point.to_position())
    }
}

impl ToLspRange for Node<'_> {
    fn to_range(&self) -> Range {
        self.range().to_range()
    }
}

pub fn delta(left: &SemanticToken, right: &SemanticToken) -> SemanticToken {
    SemanticToken {
        delta_line: left.delta_line - right.delta_line,
        delta_start: if left.delta_line == right.delta_line {
            left.delta_start - right.delta_start
        }
        else {
            left.delta_start
        },
        length: left.length,
        token_type: left.token_type,
        token_modifiers_bitset: left.token_modifiers_bitset,
    }
}

pub trait Sizable {
    fn is_empty(&self) -> bool;
    fn size(&self) -> usize;
}

pub trait Completable: Sized {
    fn complement(&self, ranges: Vec<Self>) -> Vec<Self>;
}

impl Sizable for tree_sitter::Range {
    fn is_empty(&self) -> bool {
        self.end_byte <= self.start_byte
    }

    fn size(&self) -> usize {
        if self.is_empty() {
            0
        }
        else {
            self.end_byte - self.start_byte
        }
    }
}

impl Completable for tree_sitter::Range {
    fn complement(&self, ranges: Vec<Self>) -> Vec<Self> {
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

impl Sizable for Node<'_> {
    fn is_empty(&self) -> bool {
        self.range().is_empty()
    }

    fn size(&self) -> usize {
        self.range().size()
    }
}

pub trait ExtendedLocation {
    fn message(&self, message: impl Into<String>) -> DiagnosticRelatedInformation;
}

impl ExtendedLocation for Location {
    fn message(&self, message: impl Into<String>) -> DiagnosticRelatedInformation {
        DiagnosticRelatedInformation {
            message: message.into(),
            location: self.clone(),
        }
    }
}

pub trait ExtendedNode: Sized {
    fn error(&self, message: impl Into<String>) -> Diagnostic;
    fn warning(&self, message: impl Into<String>) -> Diagnostic;
    fn expect(&self, name: &str) -> std::result::Result<Self, Diagnostic>;
    fn get(&self, index: usize) -> std::result::Result<Self, Diagnostic>;
    fn text<'content>(&self, content: &'content [u8]) -> std::result::Result<&'content str, Diagnostic>;
    fn url(&self, url: &Url) -> Location;
    fn related(&self, url: &Url, message: impl Into<String>) -> DiagnosticRelatedInformation;
}

impl ExtendedNode for Node<'_> {

    fn error(&self, message: impl Into<String>) -> Diagnostic {
        Diagnostic::default()
            .severity(DiagnosticSeverity::ERROR)
            .message(message)
            .range(self.range())
    }

    fn warning(&self, message: impl Into<String>) -> Diagnostic {
        Diagnostic::default()
            .severity(DiagnosticSeverity::WARNING)
            .message(message)
            .range(self.range())
    }

    fn related(&self, url: &Url, message: impl Into<String>) -> DiagnosticRelatedInformation {
        self.url(url).message(message)
    }

    fn url(&self, url: &Url) -> Location {
        Location::new(url.clone(), self.to_range())
    }

    fn get(&self, index: usize) -> std::result::Result<Self, Diagnostic> {
        self.named_child(index).ok_or_else(|| self.error("expected named node"))
    }

    fn expect(&self, name: &str) -> std::result::Result<Self, Diagnostic> {
        self.child_by_field_name(name)
            .ok_or_else(|| self.error(format!("expected {}", name)))
    }

    fn text<'content>(&self, content: &'content [u8]) -> std::result::Result<&'content str, Diagnostic> {
        self.utf8_text(content).map_err(|_| self.error("non-unicode text"))
    }
}

pub trait DiagnosticBuilder {
    fn range(self, range: impl ToLspRange) -> Self;
    fn related(self, related: impl IntoIterator<Item = DiagnosticRelatedInformation>) -> Self;
    fn message(self, message: impl Into<String>) -> Self;
    fn code(self, code: impl Into<i32>) -> Self;
    fn source(self, source: impl Into<String>) -> Self;
    fn severity(self, severity: DiagnosticSeverity) -> Self;
}

impl DiagnosticBuilder for Diagnostic {
    fn range(mut self, range: impl ToLspRange) -> Self {
        self.range = range.to_range();
        self
    }

    fn related(mut self, related: impl IntoIterator<Item = DiagnosticRelatedInformation>) -> Self {
        let mut vec = self.related_information.take().unwrap_or_default();
        vec.extend(related);
        self.related_information = Some(vec);
        self
    }

    fn message(mut self, message: impl Into<String>) -> Self {
        self.message = message.into();
        self
    }

    fn code(mut self, code: impl Into<i32>) -> Self {
        self.code = Some(NumberOrString::Number(code.into()));
        self
    }

    fn source(mut self, source: impl Into<String>) -> Self {
        self.source = Some(source.into());
        self
    }

    fn severity(mut self, severity: DiagnosticSeverity) -> Self {
        self.severity = Some(severity);
        self
    }
}
