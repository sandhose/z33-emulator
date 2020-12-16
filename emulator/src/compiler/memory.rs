use std::collections::HashMap;

use thiserror::Error;

use crate::memory::{Cell, Memory};
use crate::parser::LineContent;

use super::layout::{Labels, Layout, Placement};

#[derive(Debug, Error)]
pub enum MemoryFillError<'a> {
    #[error("could not compile: {0}")]
    CompilationError(CompilationError<'a>),
}

impl<'a> From<CompilationError<'a>> for MemoryFillError<'a> {
    fn from(inner: CompilationError<'a>) -> Self {
        Self::CompilationError(inner)
    }
}

#[derive(Debug, Error)]
pub enum CompilationError<'a> {
    #[error("unsupported directive {directive}")]
    UnsupportedDirective { directive: &'a str },
}

fn compile_placement<'a>(
    _labels: &Labels<'a>,
    placement: &Placement<'a>,
) -> Result<Cell, CompilationError<'a>> {
    match placement {
        Placement::Reserved => Ok(Cell::Empty),
        Placement::Char(c) => Ok(Cell::Char(*c)),
        Placement::Line(LineContent::Directive {
            directive: "word", ..
        }) => {
            todo!()
        }
        Placement::Line(LineContent::Instruction { .. }) => {
            todo!()
        }
        Placement::Line(LineContent::Directive { directive, .. }) => {
            Err(CompilationError::UnsupportedDirective { directive })
        }
    }
}

#[allow(dead_code)]
pub fn fill_memory<'a>(layout: &Layout<'a>) -> Result<Memory, CompilationError<'a>> {
    let mut memory = Memory::default();

    let cells: Result<HashMap<u64, Cell>, CompilationError> = layout
        .memory
        .iter()
        .map(|(index, placement)| Ok((*index, compile_placement(&layout.labels, placement)?)))
        .collect();

    for (address, content) in cells? {
        let cell = memory.get_mut(address).unwrap();
        *cell = content;
    }

    Ok(memory)
}
