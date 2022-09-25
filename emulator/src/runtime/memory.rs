use std::convert::TryInto;

use parse_display::Display;
use thiserror::Error;

use crate::constants::*;

use super::instructions::Instruction;

/// Type of cells
///
/// There is a 1-1 mapping with the `Cell` type in this module.
#[derive(Debug)]
pub enum CellKind {
    Instruction,
    Word,
    Char,
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
#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum Cell {
    /// An instruction
    ///
    /// The instruction can be a big type, so only a reference is saved here.
    #[display("{0}")]
    Instruction(Box<Instruction>),

    /// An unsigned word
    ///
    /// In contrast, a word is small enough to be copied.
    #[display("{0}")]
    Word(Word),

    /// A signle char
    #[display("{:?}")]
    Char(Char),

    /// An empty cell, no value was ever set here,
    #[display("0")]
    Empty,
}

impl Default for Cell {
    fn default() -> Self {
        Cell::Empty
    }
}

impl Cell {
    #[inline]
    fn cell_kind(&self) -> CellKind {
        match self {
            Self::Instruction(_) => CellKind::Instruction,
            Self::Word(_) => CellKind::Word,
            Self::Char(_) => CellKind::Char,
            Self::Empty => CellKind::Empty,
        }
    }

    /// Extract a word from the cell.
    ///
    /// If the cell is empty, it extracts "0"
    /// If it is a char, it tries to convert it to its ASCII code
    pub(crate) fn extract_word(&self) -> Result<Word, CellError> {
        match self {
            Self::Word(w) => Ok(*w),
            Self::Empty => Ok(0),
            Self::Char(c) if c.is_ascii() => {
                let mut buf = [0; 1];
                // TODO: check that this does not panic
                c.encode_utf8(&mut buf);
                Ok(buf[0] as _)
            }
            t => Err(CellError::InvalidType {
                expected: CellKind::Word,
                was: t.cell_kind(),
            }),
        }
    }

    /// Extract an address from the cell.
    ///
    /// If the cell is empty, it extracts "0"
    /// If it is a char, it tries to convert it to its ASCII code
    pub(crate) fn extract_address(&self) -> Result<Address, CellError> {
        let word = self.extract_word()?;
        word.try_into()
            .map_err(|_| CellError::InvalidAddress { word })
    }

    /// Extract an instruction from the cell.
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

    /// Extracts a char from the cell.
    ///
    /// If the cell is empty, it extracts '\0'
    /// If the cell is a word, it tries converting it to a char if it is in the ASCII range
    fn extract_char(&self) -> Result<Char, CellError> {
        match self {
            Self::Char(c) => Ok(*c),
            Self::Empty => Ok('\0'),
            Self::Word(w) if (0x00..=0xFF).contains(w) => Ok(char::from(*w as u8)),
            t => Err(CellError::InvalidType {
                expected: CellKind::Char,
                was: t.cell_kind(),
            }),
        }
    }
}

/// Trait to help converting from cells
pub(crate) trait TryFromCell: Sized {
    /// Convert the cell to a value
    ///
    /// The inner value of the cell is copied/cloned.
    fn try_from_cell(value: &Cell) -> Result<Self, CellError>;
}

impl From<Instruction> for Cell {
    fn from(instruction: Instruction) -> Self {
        Self::Instruction(Box::new(instruction))
    }
}

impl TryFromCell for Instruction {
    fn try_from_cell(value: &Cell) -> Result<Self, CellError> {
        value.extract_instruction().map(|i| i.clone())
    }
}

impl From<Word> for Cell {
    fn from(word: Word) -> Self {
        Self::Word(word)
    }
}

impl TryFromCell for Word {
    fn try_from_cell(value: &Cell) -> Result<Self, CellError> {
        value.extract_word()
    }
}

impl From<Address> for Cell {
    fn from(addr: Address) -> Self {
        Self::Word(addr.into())
    }
}

impl TryFromCell for Address {
    fn try_from_cell(value: &Cell) -> Result<Self, CellError> {
        let word = value.extract_word()?;

        word.try_into()
            .map_err(|_| CellError::InvalidAddress { word })
    }
}

impl From<Char> for Cell {
    fn from(c: Char) -> Self {
        Self::Char(c)
    }
}

impl TryFromCell for Char {
    fn try_from_cell(value: &Cell) -> Result<Self, CellError> {
        value.extract_char()
    }
}

/// Represents errors related to memory manipulations
#[derive(Debug, Error)]
pub enum MemoryError {
    /// The given address was invalid
    #[error("invalid address {0}")]
    InvalidAddress(Address),
}

/// Holds the memory cells of the computer.
///
/// It has 65536 cells by default.
pub struct Memory {
    inner: Vec<Cell>,
}

impl Default for Memory {
    fn default() -> Self {
        Self::new(MEMORY_SIZE as _)
    }
}

impl Memory {
    /// Create a new memory component with a given size
    pub(crate) fn new(size: usize) -> Self {
        let inner = std::iter::repeat(Cell::Empty) // Fill the memory with empty cells
            .take(size)
            .collect();
        Self { inner }
    }

    /// Get a cell at an address
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
    /// It fails if the address is invalid or out of bounds.
    pub(crate) fn get_mut(&mut self, address: Address) -> Result<&mut Cell, MemoryError> {
        let addr: usize = address
            .try_into()
            .map_err(|_e| MemoryError::InvalidAddress(address))?;

        self.inner
            .get_mut(addr)
            .ok_or(MemoryError::InvalidAddress(address))
    }
}
