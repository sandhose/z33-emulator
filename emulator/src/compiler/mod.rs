use std::collections::BTreeMap;
use std::ops::Range;

use codespan_reporting::diagnostic::Diagnostic;
use thiserror::Error;
use tracing::debug;

use self::layout::MemoryLayoutError;
use self::memory::MemoryFillError;
use crate::constants as C;
use crate::diagnostic::{compilation_error_to_diagnostic, parse_diagnostic_to_codespan, FileId};
use crate::parser::line::Program;
use crate::parser::shared::ParseDiagnostic;
use crate::runtime::{Computer, Registers};

pub mod layout;
pub mod memory;

type Labels = BTreeMap<String, C::Address>;

/// Holds information about the compilation — always available even with errors.
pub struct DebugInfo {
    /// Map of labels to addresses
    pub labels: Labels,

    /// Map of addresses to byte ranges in the preprocessor output
    pub source_map: BTreeMap<C::Address, Range<usize>>,
}

#[derive(Debug, Error)]
pub enum CompilationError {
    #[error("could not layout memory")]
    MemoryLayout(#[from] MemoryLayoutError),

    #[error("could not fill memory")]
    MemoryFill(#[from] MemoryFillError),

    #[error("unknown entrypoint: {0}")]
    UnknownEntrypoint(String),
}

/// Result of compilation. Always produces `debug_info` (labels + source map)
/// and `diagnostics`. Only produces a runnable `Computer` if there are zero
/// errors.
pub struct CompileResult {
    /// A runnable computer, only if compilation had no errors.
    pub computer: Option<Computer>,

    /// Debug info (labels, source map) — always available.
    pub debug_info: DebugInfo,

    /// All diagnostics (parse errors + layout errors + fill errors), already
    /// in codespan format ready for rendering.
    pub diagnostics: Vec<Diagnostic<FileId>>,
}

/// Compile a program, accumulating all diagnostics from parsing, layout,
/// and memory fill.
///
/// If `entrypoint` is `Some`, the resulting computer's PC is set to that
/// label. If `None`, no entrypoint is selected (check-only mode).
///
/// Parse diagnostics are included in the output alongside compilation errors.
#[tracing::instrument(skip(program, parse_diagnostics))]
pub fn compile(
    program: &Program,
    parse_diagnostics: &[ParseDiagnostic],
    entrypoint: Option<&str>,
    preprocessed_file_id: FileId,
) -> CompileResult {
    // Convert parse diagnostics to codespan format
    let mut diagnostics: Vec<Diagnostic<FileId>> = parse_diagnostics
        .iter()
        .map(|d| parse_diagnostic_to_codespan(d, preprocessed_file_id))
        .collect();

    // Layout: assign addresses to labels and place instructions/directives
    let (layout, layout_errors) = self::layout::layout_memory(&program.lines);
    let has_layout_errors = !layout_errors.is_empty();
    for e in layout_errors {
        let ce = CompilationError::MemoryLayout(e);
        diagnostics.push(compilation_error_to_diagnostic(&ce, preprocessed_file_id));
    }

    // Fill: evaluate expressions and compile instructions into memory cells
    let (memory, fill_errors) = self::memory::fill_memory(&layout);
    let has_fill_errors = !fill_errors.is_empty();
    for e in fill_errors {
        let ce = CompilationError::MemoryFill(e);
        diagnostics.push(compilation_error_to_diagnostic(&ce, preprocessed_file_id));
    }

    // Build computer only if there are no errors at all
    let has_errors = has_layout_errors || has_fill_errors || !parse_diagnostics.is_empty();

    let computer = if has_errors {
        None
    } else if let Some(ep) = entrypoint {
        if let Some(&pc) = layout.labels.get(ep) {
            debug!(pc, entrypoint = ep, "Found entrypoint");
            Some(Computer {
                memory,
                registers: Registers {
                    pc,
                    sp: C::STACK_START,
                    ..Default::default()
                },
                ..Default::default()
            })
        } else {
            diagnostics.push(compilation_error_to_diagnostic(
                &CompilationError::UnknownEntrypoint(ep.to_string()),
                preprocessed_file_id,
            ));
            None
        }
    } else {
        None
    };

    let source_map = layout.source_map();
    let debug_info = DebugInfo {
        labels: layout.labels,
        source_map,
    };

    CompileResult {
        computer,
        debug_info,
        diagnostics,
    }
}
