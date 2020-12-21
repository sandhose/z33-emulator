use std::convert::TryInto;

use thiserror::Error;

use crate::constants::*;

use super::instructions::Instruction;

pub type Address = u64;
pub type Word = u64;
pub type Char = char;

/// Type of cells
///
/// There is a 1-1 mapping with the `Cell` type in this module.
#[derive(Debug)]
pub enum CellType {
    Instruction,
    Word,
    Char,
    Empty,
}

#[derive(Debug, Error)]
pub enum CellError {
    #[error("invalid cell type {was:?} expected {expected:?}")]
    InvalidType { expected: CellType, was: CellType },
}

/// Represents a cell in memory and in general purpose registers
#[derive(Debug, Clone, PartialEq)]
pub enum Cell {
    /// An instruction
    ///
    /// The instruction can be a big type, so only a reference is saved here.
    Instruction(Box<Instruction>),

    /// An unsigned word
    ///
    /// In contrast, a word is small enough to be copied.
    Word(Word),

    /// A signle char
    Char(Char),

    /// An empty cell, no value was ever set here,
    Empty,
}

impl Default for Cell {
    fn default() -> Self {
        Cell::Empty
    }
}

impl Cell {
    #[inline]
    fn cell_type(&self) -> CellType {
        match self {
            Self::Instruction(_) => CellType::Instruction,
            Self::Word(_) => CellType::Word,
            Self::Char(_) => CellType::Char,
            Self::Empty => CellType::Empty,
        }
    }

    /// Extract a word from the cell.
    ///
    /// If the cell is empty, it extracts "0"
    /// If it is a char, it tries to convert it to its ASCII code
    pub fn extract_word(&self) -> Result<Word, CellError> {
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
                expected: CellType::Word,
                was: t.cell_type(),
            }),
        }
    }

    /// Extract an instruction from the cell.
    ///
    /// Raises an error if it is any other type
    pub fn extract_instruction(&self) -> Result<&Instruction, CellError> {
        match self {
            Self::Instruction(i) => Ok(i),
            t => Err(CellError::InvalidType {
                expected: CellType::Instruction,
                was: t.cell_type(),
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
                expected: CellType::Char,
                was: t.cell_type(),
            }),
        }
    }
}

impl std::fmt::Display for Cell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Cell::Instruction(i) => write!(f, "{}", i),
            Cell::Word(w) => write!(f, "{}", w),
            Cell::Char(c) => write!(f, "{:?}", c),
            Cell::Empty => write!(f, "0"),
        }
    }
}

/// Trait to help converting from cells
pub trait TryFromCell: Sized {
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
    pub fn new(size: usize) -> Self {
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
    pub fn get_mut(&mut self, address: Address) -> Result<&mut Cell, MemoryError> {
        let addr: usize = address
            .try_into()
            .map_err(|_e| MemoryError::InvalidAddress(address))?;

        self.inner
            .get_mut(addr)
            .ok_or(MemoryError::InvalidAddress(address))
    }
}
