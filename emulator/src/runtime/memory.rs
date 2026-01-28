use std::convert::TryInto;

use thiserror::Error;

use super::instructions::Instruction;
use crate::constants::{Address, Word, MEMORY_SIZE};

/// Type of cells
///
/// There is a 1-1 mapping with the [`Cell`] type in this module.
#[derive(Debug)]
pub enum CellKind {
    Instruction,
    Word,
    Empty,
}

#[derive(Debug, Error)]
pub enum CellError {
    #[error("invalid cell type {was:?} expected {expected:?}")]
    InvalidType { expected: CellKind, was: CellKind },

    #[error("could not downcast word to address: {word}")]
    InvalidAddress { word: Word },
}

/// Represents a cell in memory and in general purpose registers
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Cell {
    /// An empty cell, no value was ever set here,
    #[default]
    Empty,

    /// An instruction
    ///
    /// The instruction can be a big type, so only a reference is saved here.
    Instruction(Box<Instruction>),

    /// An unsigned word
    ///
    /// In contrast, a word is small enough to be copied.
    Word(Word),
}

impl std::fmt::Display for Cell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Instruction(i) => write!(f, "{i}"),
            Self::Word(w) => write!(f, "{w}"),
            Self::Empty => write!(f, "0"),
        }
    }
}

impl Cell {
    #[inline]
    fn cell_kind(&self) -> CellKind {
        match self {
            Self::Instruction(_) => CellKind::Instruction,
            Self::Word(_) => CellKind::Word,
            Self::Empty => CellKind::Empty,
        }
    }

    /// Extract a word from the cell.
    ///
    /// If the cell is empty, it extracts "0"
    pub(crate) fn extract_word(&self) -> Result<Word, CellError> {
        match self {
            Self::Word(w) => Ok(*w),
            Self::Empty => Ok(0),
            Self::Instruction(_) => Err(CellError::InvalidType {
                expected: CellKind::Word,
                was: CellKind::Instruction,
            }),
        }
    }

    /// Extract an address from the cell.
    ///
    /// If the cell is empty, it extracts "0"
    pub(crate) fn extract_address(&self) -> Result<Address, CellError> {
        let word = self.extract_word()?;
        word.try_into()
            .map_err(|_| CellError::InvalidAddress { word })
    }

    /// Extract an [`Instruction`] from the cell.
    ///
    /// # Errors
    ///
    /// Raises an error if it is any other type
    pub fn extract_instruction(&self) -> Result<&Instruction, CellError> {
        match self {
            Self::Instruction(i) => Ok(i),
            t => Err(CellError::InvalidType {
                expected: CellKind::Instruction,
                was: t.cell_kind(),
            }),
        }
    }
}

impl From<Instruction> for Cell {
    fn from(instruction: Instruction) -> Self {
        Self::Instruction(Box::new(instruction))
    }
}

impl TryFrom<&Cell> for Instruction {
    type Error = CellError;

    fn try_from(value: &Cell) -> Result<Self, Self::Error> {
        value.extract_instruction().cloned()
    }
}

impl From<Word> for Cell {
    fn from(word: Word) -> Self {
        Self::Word(word)
    }
}

impl TryFrom<&Cell> for Word {
    type Error = CellError;

    fn try_from(value: &Cell) -> Result<Self, Self::Error> {
        value.extract_word()
    }
}

impl From<Address> for Cell {
    fn from(addr: Address) -> Self {
        Self::Word(addr.into())
    }
}

impl TryFrom<&Cell> for Address {
    type Error = CellError;

    fn try_from(value: &Cell) -> Result<Self, Self::Error> {
        value.extract_address()
    }
}

/// Represents errors related to memory manipulations
#[derive(Debug, Error, Clone, Copy)]
pub enum MemoryError {
    /// The given address was invalid
    #[error("invalid address {0}")]
    InvalidAddress(Address),
}

/// Holds the memory cells of the computer.
///
/// It has 10000 cells
pub struct Memory {
    inner: Box<[Cell; MEMORY_SIZE as _]>,
}

// Implement clone without destroying the stack
impl Clone for Memory {
    fn clone(&self) -> Self {
        let mut new = Self::default();
        for (i, cell) in self.inner.iter().enumerate() {
            new.inner[i] = cell.clone();
        }
        new
    }
}

const DEFAULT_CELL_VALUE: Cell = Cell::Empty;
impl Default for Memory {
    fn default() -> Self {
        Self {
            inner: vec![DEFAULT_CELL_VALUE; MEMORY_SIZE as _]
                .into_boxed_slice()
                .try_into()
                .unwrap(),
        }
    }
}

impl Memory {
    /// Get a cell at an address
    ///
    /// # Errors
    ///
    /// It fails if the address is invalid or out of bounds.
    pub fn get(&self, address: Address) -> Result<&Cell, MemoryError> {
        let addr: usize = address
            .try_into()
            .map_err(|_e| MemoryError::InvalidAddress(address))?;

        self.inner
            .get(addr)
            .ok_or(MemoryError::InvalidAddress(address))
    }

    /// Get a mutable reference to a cell at an address
    ///
    /// # Errors
    ///
    /// It fails if the address is invalid or out of bounds.
    pub fn get_mut(&mut self, address: Address) -> Result<&mut Cell, MemoryError> {
        let addr: usize = address
            .try_into()
            .map_err(|_e| MemoryError::InvalidAddress(address))?;

        self.inner
            .get_mut(addr)
            .ok_or(MemoryError::InvalidAddress(address))
    }
}
