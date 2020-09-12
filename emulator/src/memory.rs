use std::convert::TryInto;

use thiserror::Error;

use super::processor::Instruction;

pub type Address = u64;
pub type UnsignedWord = u64;
pub type SignedWord = i64;
pub type Char = char;

#[derive(Debug)]
pub enum CellType {
    Instruction,
    UnsignedWord,
    SignedWord,
    Char,
    Empty,
}

#[derive(Debug, Error)]
pub enum CellError {
    #[error("invalid cell type {was:?} expected {expected:?}")]
    InvalidType { expected: CellType, was: CellType },

    #[error("failed conversion from {from:?} to {to:?}")]
    InvalidConversion { from: CellType, to: CellType },
}

impl CellError {
    pub fn to_invalid_cell(self, address: Address) -> MemoryError {
        MemoryError::InvalidCell {
            inner: self,
            address,
        }
    }
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
    UnsignedWord(UnsignedWord),

    /// A signed word
    SignedWord(SignedWord),

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
            Self::UnsignedWord(_) => CellType::UnsignedWord,
            Self::SignedWord(_) => CellType::SignedWord,
            Self::Char(_) => CellType::Char,
            Self::Empty => CellType::Empty,
        }
    }

    fn extract_unsigned_word(&self) -> Result<UnsignedWord, CellError> {
        // TODO: convert from char
        match self {
            Self::UnsignedWord(w) => Ok(*w),
            Self::SignedWord(w) => (*w).try_into().map_err(|_| CellError::InvalidConversion {
                from: CellType::SignedWord,
                to: CellType::UnsignedWord,
            }),
            t => Err(CellError::InvalidType {
                expected: CellType::UnsignedWord,
                was: t.cell_type(),
            }),
        }
    }

    fn extract_signed_word(&self) -> Result<SignedWord, CellError> {
        // TODO: convert from char
        match self {
            Self::SignedWord(w) => Ok(*w),
            Self::UnsignedWord(w) => (*w).try_into().map_err(|_| CellError::InvalidConversion {
                from: CellType::UnsignedWord,
                to: CellType::SignedWord,
            }),
            t => Err(CellError::InvalidType {
                expected: CellType::SignedWord,
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

impl From<UnsignedWord> for Cell {
    fn from(word: UnsignedWord) -> Self {
        Self::UnsignedWord(word)
    }
}

impl TryFromCell for UnsignedWord {
    fn try_from_cell(value: &Cell) -> Result<Self, CellError> {
        value.extract_unsigned_word()
    }
}

impl From<SignedWord> for Cell {
    fn from(word: SignedWord) -> Self {
        Self::SignedWord(word)
    }
}

impl TryFromCell for SignedWord {
    fn try_from_cell(value: &Cell) -> Result<Self, CellError> {
        value.extract_signed_word()
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
    #[error("error at address {address:#x}: {inner}")]
    InvalidCell { inner: CellError, address: Address },

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

    fn extract_unsigned_word(&self, address: Address) -> Result<UnsignedWord, MemoryError> {
        let cell = self.get(address)?;
        cell.extract_unsigned_word()
            .map_err(|err| err.to_invalid_cell(address))
    }

    fn extract_instruction(&self, address: Address) -> Result<&Instruction, MemoryError> {
        let cell = self.get(address)?;
        cell.extract_instruction()
            .map_err(|err| err.to_invalid_cell(address))
    }

    fn insert<T: Into<Cell>>(&mut self, address: Address, value: T) -> Result<(), MemoryError> {
        let cell = self.get_mut(address)?;
        *cell = value.into();
        Ok(())
    }
}
