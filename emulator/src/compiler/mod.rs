use std::collections::BTreeMap;
use std::ops::Range;

use thiserror::Error;
use tracing::debug;

use self::layout::MemoryLayoutError;
use self::memory::MemoryFillError;
use crate::constants as C;
use crate::parser::line::{LineContent, Program};
use crate::runtime::{Computer, Registers};

pub mod layout;
pub mod memory;

type Labels = BTreeMap<String, C::Address>;

/// Holds informations about the compilation
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

    #[error("program contains syntax errors")]
    HasParseErrors,
}

/// Check whether the program contains any error-recovery placeholders.
fn has_parse_errors(program: &Program) -> bool {
    program.lines.iter().any(|line| {
        line.inner
            .content
            .as_ref()
            .is_some_and(|c| matches!(c.inner, LineContent::Error))
    })
}

/// Run layout and fill without selecting an entrypoint.
/// Returns `Ok(())` if the program can be assembled, or a `CompilationError`.
pub fn check(program: &Program) -> Result<(), CompilationError> {
    if has_parse_errors(program) {
        return Err(CompilationError::HasParseErrors);
    }
    let layout = self::layout::layout_memory(&program.lines)?;
    self::memory::fill_memory(&layout)?;
    Ok(())
}

#[tracing::instrument(skip(program))]
pub fn compile(
    program: &Program,
    entrypoint: &str,
) -> Result<(Computer, DebugInfo), CompilationError> {
    if has_parse_errors(program) {
        return Err(CompilationError::HasParseErrors);
    }
    let layout = self::layout::layout_memory(&program.lines)?;
    let memory = self::memory::fill_memory(&layout)?;

    // Lookup the entrypoint
    let pc = *layout
        .labels
        .get(entrypoint)
        .ok_or_else(|| CompilationError::UnknownEntrypoint(entrypoint.to_string()))?;
    debug!(pc, entrypoint, "Found entrypoint");

    // r[impl arch.initial-state]
    let computer = Computer {
        memory,
        registers: Registers {
            pc,
            sp: C::STACK_START,
            ..Default::default()
        },
        ..Default::default()
    };

    let debug_info = DebugInfo {
        labels: layout
            .labels
            .iter()
            .map(|(key, value)| (key.clone(), *value))
            .collect(),
        source_map: layout.source_map(),
    };

    Ok((computer, debug_info))
}
