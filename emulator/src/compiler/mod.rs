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
use crate::runtime::{Computer, Memory, Registers};

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

/// The result of assembling a program: layout + fill are done once and do not
/// depend on any entrypoint. Turn it into a runnable [`Computer`] with
/// [`into_computer`](Self::into_computer).
pub struct Assembled {
    /// Debug info (labels, source map) — always available.
    pub debug_info: DebugInfo,

    /// All diagnostics (parse errors + layout errors + fill errors), already
    /// in codespan format ready for rendering.
    pub diagnostics: Vec<Diagnostic<FileId>>,

    /// The filled memory image.
    memory: Memory,

    /// Whether any parse/layout/fill errors were collected.
    has_errors: bool,
}

impl Assembled {
    /// The labels discovered during layout.
    #[must_use]
    pub fn labels(&self) -> &Labels {
        &self.debug_info.labels
    }

    /// Build a runnable [`Computer`] with its program counter set to
    /// `entrypoint`.
    ///
    /// # Errors
    ///
    /// Returns [`CompilationError::UnknownEntrypoint`] if `entrypoint` is not a
    /// known label.
    pub fn into_computer(self, entrypoint: &str) -> Result<Computer, CompilationError> {
        let Some(&pc) = self.debug_info.labels.get(entrypoint) else {
            return Err(CompilationError::UnknownEntrypoint(entrypoint.to_string()));
        };
        debug!(pc, entrypoint, "Found entrypoint");
        Ok(build_computer(self.memory, pc))
    }
}

// r[impl arch.initial-state]
/// Wrap a filled memory image into a [`Computer`] with its PC and SP set up.
fn build_computer(memory: Memory, pc: C::Address) -> Computer {
    Computer {
        memory,
        registers: Registers {
            pc,
            sp: C::STACK_START,
            ..Default::default()
        },
        ..Default::default()
    }
}

/// Assemble a program: run parse-diagnostic collection, layout and fill once,
/// producing the debug info and the filled memory. This does not depend on an
/// entrypoint; use [`Assembled::into_computer`] to obtain a runnable machine.
#[tracing::instrument(skip(program, parse_diagnostics))]
pub fn assemble(
    program: &Program,
    parse_diagnostics: &[ParseDiagnostic],
    preprocessed_file_id: FileId,
) -> Assembled {
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

    let has_errors = has_layout_errors || has_fill_errors || !parse_diagnostics.is_empty();

    let source_map = layout.source_map();
    let debug_info = DebugInfo {
        labels: layout.labels,
        source_map,
    };

    Assembled {
        debug_info,
        diagnostics,
        memory,
        has_errors,
    }
}

/// Compile a program, accumulating all diagnostics from parsing, layout,
/// and memory fill.
///
/// If `entrypoint` is `Some`, the resulting computer's PC is set to that
/// label. If `None`, no entrypoint is selected (check-only mode).
///
/// Parse diagnostics are included in the output alongside compilation errors.
///
/// This is a thin wrapper over [`assemble`]: it assembles once, then
/// conditionally builds the [`Computer`] for the requested entrypoint.
pub fn compile(
    program: &Program,
    parse_diagnostics: &[ParseDiagnostic],
    entrypoint: Option<&str>,
    preprocessed_file_id: FileId,
) -> CompileResult {
    let Assembled {
        debug_info,
        mut diagnostics,
        memory,
        has_errors,
    } = assemble(program, parse_diagnostics, preprocessed_file_id);

    // Build computer only if there are no errors and an entrypoint was given.
    let computer = if has_errors {
        None
    } else if let Some(ep) = entrypoint {
        if let Some(&pc) = debug_info.labels.get(ep) {
            debug!(pc, entrypoint = ep, "Found entrypoint");
            Some(build_computer(memory, pc))
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

    CompileResult {
        computer,
        debug_info,
        diagnostics,
    }
}
