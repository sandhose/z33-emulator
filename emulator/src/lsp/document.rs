use std::collections::BTreeMap;

use crate::compiler::layout::{self, Layout, MemoryLayoutError};
use crate::compiler::memory::{self, MemoryFillError};
use crate::constants::Address;
use crate::parser::assembly::ParseResult;
use crate::parser::line::Program;
use crate::parser::shared::ParseDiagnostic;
use crate::parser::{self};

/// Cached analysis state for a single document.
///
/// Runs the parse and layout pipeline synchronously on construction. This is
/// fast enough for typical Z33 programs (under 1ms for <1000 lines).
pub struct DocumentState {
    source: String,
    parse_result: ParseResult,
    layout: Layout,
    layout_errors: Vec<MemoryLayoutError>,
    fill_errors: Vec<MemoryFillError>,
}

impl DocumentState {
    /// Analyze a document: parse, layout, and fill.
    ///
    /// All three stages run even if earlier stages produce errors, so that we
    /// get as many diagnostics and labels as possible.
    #[must_use]
    pub fn new(source: String) -> Self {
        let parse_result = parser::parse(&source);
        let (layout, layout_errors) = layout::layout_memory(&parse_result.program.inner.lines);
        let (_, fill_errors) = memory::fill_memory(&layout);

        Self {
            source,
            parse_result,
            layout,
            layout_errors,
            fill_errors,
        }
    }

    #[must_use]
    pub fn source(&self) -> &str {
        &self.source
    }

    #[must_use]
    pub fn program(&self) -> &Program {
        &self.parse_result.program.inner
    }

    #[must_use]
    pub fn labels(&self) -> &BTreeMap<String, Address> {
        &self.layout.labels
    }

    #[must_use]
    pub fn parse_diagnostics(&self) -> &[ParseDiagnostic] {
        &self.parse_result.diagnostics
    }

    #[must_use]
    pub fn layout_errors(&self) -> &[MemoryLayoutError] {
        &self.layout_errors
    }

    #[must_use]
    pub fn fill_errors(&self) -> &[MemoryFillError] {
        &self.fill_errors
    }
}
