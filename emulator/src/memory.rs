use std::convert::TryInto;

use thiserror::Error;

use super::processor::Instruction;

pub type Address = u64;
pub type Word = u64;
pub type Char = char;

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

    pub fn extract_word(&self) -> Result<Word, CellError> {
        // TODO: convert from char
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

    pub fn extract_instruction(&self) -> Result<&Instruction, CellError> {
        match self {
            Self::Instruction(i) => Ok(i),
            t => Err(CellError::InvalidType {
                expected: CellType::Instruction,
                was: t.cell_type(),
            }),
        }
    }

    fn extract_char(&self) -> Result<Char, CellError> {
        // TODO: convert from words
        match self {
            Self::Char(c) => Ok(*c),
            Self::Empty => Ok('\0'),
            t => Err(CellError::InvalidType {
                expected: CellType::Char,
                was: t.cell_type(),
            }),
        }
    }
}

pub trait TryFromCell: Sized {
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

#[derive(Debug, Error)]
pub enum MemoryError {
    #[error("invalid address {0:#x}")]
    InvalidAddress(Address),
}

pub struct Memory {
    inner: Vec<Cell>,
}

impl Default for Memory {
    fn default() -> Self {
        let inner: Vec<_> = std::iter::repeat(Cell::default())
            .take(u16::MAX as usize)
            .collect();
        Self { inner }
    }
}

impl Memory {
    pub fn get(&self, address: Address) -> Result<&Cell, MemoryError> {
        let addr: usize = address
            .try_into()
            .map_err(|_e| MemoryError::InvalidAddress(address))?;

        self.inner
            .get(addr)
            .ok_or(MemoryError::InvalidAddress(address))
    }

    pub fn get_mut(&mut self, address: Address) -> Result<&mut Cell, MemoryError> {
        let addr: usize = address
            .try_into()
            .map_err(|_e| MemoryError::InvalidAddress(address))?;

        self.inner
            .get_mut(addr)
            .ok_or(MemoryError::InvalidAddress(address))
    }
}
