//! Bidirectional source ↔ address index exposed to JS.
//!
//! Wraps the emulator's [`LineIndex`] (built from the compiler debug info and
//! the preprocessor source map) so the web IDE can place gutter breakpoints and
//! resolve the program counter back to a source location.

use std::rc::Rc;

use serde::Serialize;
use tsify::{Ts, Tsify};
use wasm_bindgen::prelude::*;
use z33_emulator::dap::LineIndex;

/// A resolved source position (all fields 1-based).
#[derive(Serialize, Tsify, Clone)]
pub struct SourcePosition {
    pub file: String,
    pub line: u32,
    pub column: u32,
}

/// The result of snapping a requested breakpoint line to the next line that
/// actually carries code.
#[derive(Serialize, Tsify, Clone)]
pub struct ResolvedBreakpoint {
    /// The adjusted 1-based line (>= requested line) that has code.
    pub line: u32,
    /// The address the breakpoint maps to.
    pub address: u32,
}

/// A bidirectional line ↔ address index.
#[wasm_bindgen]
pub struct SourceIndex {
    inner: Rc<LineIndex>,
}

impl SourceIndex {
    pub(crate) fn new(inner: Rc<LineIndex>) -> Self {
        Self { inner }
    }
}

#[wasm_bindgen]
impl SourceIndex {
    /// Resolve an address back to its original source position, if it maps to
    /// code.
    pub fn location_for(&self, address: u32) -> Result<Option<Ts<SourcePosition>>, JsError> {
        self.inner
            .location_owned(address)
            .map(|(file, line, column)| SourcePosition { file, line, column }.into_ts())
            .transpose()
            .map_err(Into::into)
    }

    /// Resolve a source breakpoint (`file`, 1-based `line`) to the next line
    /// with code and its address, if any.
    pub fn address_for(
        &self,
        file: &str,
        line: u32,
    ) -> Result<Option<Ts<ResolvedBreakpoint>>, JsError> {
        self.inner
            .resolve_breakpoint(file, line)
            .map(|(line, address)| ResolvedBreakpoint { line, address }.into_ts())
            .transpose()
            .map_err(Into::into)
    }
}
