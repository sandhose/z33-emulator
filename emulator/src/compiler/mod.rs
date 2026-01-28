use std::collections::BTreeMap;

use thiserror::Error;
use tracing::debug;

use self::layout::MemoryLayoutError;
use self::memory::MemoryFillError;
use crate::constants as C;
use crate::parser::line::Program;
use crate::runtime::{Computer, Registers};

pub mod layout;
pub(crate) mod memory;

type Labels = BTreeMap<String, C::Address>;

/// Holds informations about the compilation
pub struct DebugInfo {
    /// Map of labels to addresses
    pub labels: Labels,
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

#[tracing::instrument(skip(program))]
pub fn compile(
    program: &Program,
    entrypoint: &str,
) -> Result<(Computer, DebugInfo), CompilationError> {
    let layout = self::layout::layout_memory(&program.lines)?;
    let memory = self::memory::fill_memory(&layout)?;

    // Lookup the entrypoint
    let pc = *layout
        .labels
        .get(entrypoint)
        .ok_or_else(|| CompilationError::UnknownEntrypoint(entrypoint.to_string()))?;
    debug!(pc, entrypoint, "Found entrypoint");

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
    };

    Ok((computer, debug_info))
}
