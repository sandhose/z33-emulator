use std::collections::BTreeMap;
use std::ops::Range;

use thiserror::Error;
use tracing::debug;

use self::layout::MemoryLayoutError;
use self::memory::MemoryFillError;
use crate::constants as C;
use crate::parser::line::Program;
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

/// Result of compilation. Always produces `debug_info` (labels + source map).
/// Only produces a runnable `Computer` if there are zero errors.
pub struct CompileResult {
    /// A runnable computer, only if compilation had no errors.
    pub computer: Option<Computer>,

    /// Debug info (labels, source map) — always available.
    pub debug_info: DebugInfo,

    /// All accumulated errors from layout and fill stages.
    pub errors: Vec<CompilationError>,
}

/// Run layout and fill without selecting an entrypoint.
///
/// Returns all errors found plus the debug info (labels, source map).
pub fn check(program: &Program) -> CompileResult {
    let (layout, layout_errors) = self::layout::layout_memory(&program.lines);
    let (memory, fill_errors) = self::memory::fill_memory(&layout);

    let errors: Vec<CompilationError> = layout_errors
        .into_iter()
        .map(CompilationError::from)
        .chain(fill_errors.into_iter().map(CompilationError::from))
        .collect();

    let computer = if errors.is_empty() {
        Some(Computer {
            memory,
            registers: Registers::default(),
            ..Default::default()
        })
    } else {
        None
    };

    // Compute source_map before moving labels
    let source_map = layout.source_map();
    let debug_info = DebugInfo {
        labels: layout.labels,
        source_map,
    };

    CompileResult {
        computer,
        debug_info,
        errors,
    }
}

#[tracing::instrument(skip(program))]
pub fn compile(program: &Program, entrypoint: &str) -> CompileResult {
    let (layout, layout_errors) = self::layout::layout_memory(&program.lines);
    let (memory, fill_errors) = self::memory::fill_memory(&layout);

    let mut errors: Vec<CompilationError> = layout_errors
        .into_iter()
        .map(CompilationError::from)
        .chain(fill_errors.into_iter().map(CompilationError::from))
        .collect();

    let computer = if errors.is_empty() {
        if let Some(&pc) = layout.labels.get(entrypoint) {
            debug!(pc, entrypoint, "Found entrypoint");
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
            errors.push(CompilationError::UnknownEntrypoint(entrypoint.to_string()));
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
        errors,
    }
}
