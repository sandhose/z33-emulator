use std::collections::HashMap;

use thiserror::Error;
use tracing::debug;

use crate::{constants as C, parser::line::Program, runtime::Computer, runtime::Registers};

use self::{layout::MemoryLayoutError, memory::MemoryFillError};

pub(crate) mod layout;
pub(crate) mod memory;

type Labels = HashMap<String, C::Address>;

/// Holds informations about the compilation
pub struct DebugInfo {
    /// Map of labels to addresses
    pub labels: Labels,
}

#[derive(Debug, Error)]
pub enum CompilationError<L> {
    #[error("could not layout memory")]
    MemoryLayout(#[from] MemoryLayoutError<L>),

    #[error("could not fill memory")]
    MemoryFill(#[from] MemoryFillError<L>),

    #[error("unknown entrypoint: {0}")]
    UnknownEntrypoint(String),
}

pub fn layout<L: Clone + Default>(
    program: Program<L>,
) -> Result<layout::Layout<L>, MemoryLayoutError<L>> {
    let lines: Vec<_> = program.lines.into_iter().map(|l| l.inner).collect();
    self::layout::layout_memory(&lines)
}

#[tracing::instrument(skip(program))]
pub fn compile<L: Clone + Default + std::fmt::Debug>(
    program: Program<L>,
    entrypoint: &str,
) -> Result<(Computer, DebugInfo), CompilationError<L>> {
    let lines: Vec<_> = program.lines.into_iter().map(|l| l.inner).collect();
    let layout = self::layout::layout_memory(&lines)?;
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
            .map(|(key, value)| (key.to_string(), *value))
            .collect(),
    };

    Ok((computer, debug_info))
}
