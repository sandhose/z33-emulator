//! Structures to represent most of argument combination

use std::convert::TryFrom;

use parse_display::Display;
use thiserror::Error;

use super::{
    memory::{CellError, MemoryError, TryFromCell},
    registers::Reg,
    Cell, Computer, Registers,
};

use crate::constants as C;

#[derive(Debug, Error)]
pub enum ExtractError {
    #[error("cell error: {0}")]
    CellError(#[from] CellError),

    #[error("memory error: {0}")]
    MemoryError(#[from] MemoryError),

    #[error("invalid address: {0}")]
    InvalidAddress(#[from] std::num::TryFromIntError),
}

pub trait ResolveAddress {
    fn resolve_address(&self, c: &Registers) -> Result<C::Address, CellError>;
}

pub trait ExtractValue {
    fn extract_cell(&self, c: &Computer) -> Result<Cell, ExtractError>;
    fn extract_word(&self, c: &Computer) -> Result<C::Word, ExtractError> {
        let cell = self.extract_cell(c)?;
        let word = cell.extract_word()?;
        Ok(word)
    }
    fn extract_address(&self, c: &Computer) -> Result<C::Address, ExtractError> {
        let word = self.extract_word(c)?;
        let addr = C::Address::try_from(word)?;
        Ok(addr)
    }
}

impl<T: ResolveAddress> ExtractValue for T {
    fn extract_cell(&self, c: &Computer) -> Result<Cell, ExtractError> {
        let addr = self.resolve_address(&c.registers)?;
        let cell = c.memory.get(addr)?;
        Ok(cell.clone())
    }

    fn extract_word(&self, c: &Computer) -> Result<C::Word, ExtractError> {
        let addr = self.resolve_address(&c.registers)?;
        let cell = c.memory.get(addr)?;
        Ok(cell.extract_word()?)
    }
}

#[derive(PartialEq, Clone, Debug, Display)]
#[display(style = "lowercase")]
enum ArgKind {
    Imm,
    Reg,
    Ind,
    Dir,
    Idx,
}

#[derive(Error, Debug)]
#[error("invalid argument type {got:?}, expected one of {expected:?}")]
pub struct ArgConversionError {
    expected: Vec<ArgKind>,
    got: ArgKind,
}

#[derive(PartialEq, Clone, Debug, Display)]
#[display("{0}")]
pub struct Dir(pub C::Address);

impl Dir {
    pub const fn cost(&self) -> usize {
        1
    }
}

impl ResolveAddress for Dir {
    fn resolve_address(&self, _c: &Registers) -> Result<C::Address, CellError> {
        Ok(self.0)
    }
}

#[derive(PartialEq, Clone, Debug, Display)]
#[display("[{0}]")]
pub struct Ind(pub Reg);

impl Ind {
    pub const fn cost(&self) -> usize {
        1
    }
}

impl ResolveAddress for Ind {
    fn resolve_address(&self, c: &Registers) -> Result<C::Address, CellError> {
        // Get the register value
        let cell = c.get(&self.0);
        // and try converting it to an address
        let addr = C::Address::try_from_cell(&cell)?;
        Ok(addr)
    }
}

#[derive(PartialEq, Clone, Debug, Display)]
#[display("[{0}{1:+}]")]
pub struct Idx(pub Reg, pub C::Word);

impl Idx {
    pub const fn cost(&self) -> usize {
        1
    }
}

impl ResolveAddress for Idx {
    fn resolve_address(&self, c: &Registers) -> Result<C::Address, CellError> {
        // Get the register value
        let cell = c.get(&self.0);
        // and try converting it to a word
        let addr = C::Word::try_from_cell(&cell)?;
        // add the offset
        let addr = addr + self.1;
        // and convert it to an address
        let addr = C::Address::try_from_cell(&addr.into())?;
        Ok(addr)
    }
}

#[derive(PartialEq, Clone, Debug, Display)]
#[display("[{0}]")]
pub struct Imm(pub C::Word);

impl Imm {
    pub const fn cost(&self) -> usize {
        0
    }
}

impl ExtractValue for Imm {
    fn extract_cell(&self, _c: &Computer) -> Result<Cell, ExtractError> {
        Ok(Cell::Word(self.0))
    }

    fn extract_word(&self, _c: &Computer) -> Result<C::Word, ExtractError> {
        Ok(self.0)
    }
}

#[derive(PartialEq, Clone, Debug, Display)]
#[display("{0}")]
pub enum ImmRegDirIndIdx {
    Imm(Imm),
    Reg(Reg),
    Dir(Dir),
    Ind(Ind),
    Idx(Idx),
}

impl ImmRegDirIndIdx {
    pub const fn cost(&self) -> usize {
        match self {
            ImmRegDirIndIdx::Imm(a) => a.cost(),
            ImmRegDirIndIdx::Reg(a) => a.cost(),
            ImmRegDirIndIdx::Dir(a) => a.cost(),
            ImmRegDirIndIdx::Ind(a) => a.cost(),
            ImmRegDirIndIdx::Idx(a) => a.cost(),
        }
    }

    const fn kind(&self) -> ArgKind {
        match self {
            Self::Imm(_) => ArgKind::Imm,
            Self::Reg(_) => ArgKind::Reg,
            Self::Dir(_) => ArgKind::Dir,
            Self::Ind(_) => ArgKind::Ind,
            Self::Idx(_) => ArgKind::Idx,
        }
    }
}

impl ExtractValue for Reg {
    fn extract_cell(&self, c: &Computer) -> Result<Cell, ExtractError> {
        Ok(c.registers.get(self))
    }

    fn extract_word(&self, c: &Computer) -> Result<C::Word, ExtractError> {
        Ok(c.registers.get_word(self)?)
    }
}

impl ExtractValue for ImmRegDirIndIdx {
    fn extract_cell(&self, c: &Computer) -> Result<Cell, ExtractError> {
        match self {
            ImmRegDirIndIdx::Imm(a) => a.extract_cell(c),
            ImmRegDirIndIdx::Reg(a) => a.extract_cell(c),
            ImmRegDirIndIdx::Dir(a) => a.extract_cell(c),
            ImmRegDirIndIdx::Ind(a) => a.extract_cell(c),
            ImmRegDirIndIdx::Idx(a) => a.extract_cell(c),
        }
    }

    fn extract_word(&self, c: &Computer) -> Result<C::Word, ExtractError> {
        match self {
            ImmRegDirIndIdx::Imm(a) => a.extract_word(c),
            ImmRegDirIndIdx::Reg(a) => a.extract_word(c),
            ImmRegDirIndIdx::Dir(a) => a.extract_word(c),
            ImmRegDirIndIdx::Ind(a) => a.extract_word(c),
            ImmRegDirIndIdx::Idx(a) => a.extract_word(c),
        }
    }
}

impl TryFrom<ImmRegDirIndIdx> for Reg {
    type Error = ArgConversionError;

    fn try_from(value: ImmRegDirIndIdx) -> Result<Self, Self::Error> {
        match value {
            ImmRegDirIndIdx::Reg(a) => Ok(a),
            other => Err(ArgConversionError {
                expected: vec![ArgKind::Reg],
                got: other.kind(),
            }),
        }
    }
}

#[derive(PartialEq, Clone, Debug, Display)]
#[display("{0}")]
pub enum ImmReg {
    Imm(Imm),
    Reg(Reg),
}

impl ImmReg {
    pub const fn cost(&self) -> usize {
        match self {
            ImmReg::Imm(a) => a.cost(),
            ImmReg::Reg(a) => a.cost(),
        }
    }

    fn kinds() -> Vec<ArgKind> {
        use ArgKind::*;
        vec![Imm, Reg]
    }
}

impl ExtractValue for ImmReg {
    fn extract_cell(&self, c: &Computer) -> Result<Cell, ExtractError> {
        match self {
            ImmReg::Imm(a) => a.extract_cell(c),
            ImmReg::Reg(a) => a.extract_cell(c),
        }
    }

    fn extract_word(&self, c: &Computer) -> Result<C::Word, ExtractError> {
        match self {
            ImmReg::Imm(a) => a.extract_word(c),
            ImmReg::Reg(a) => a.extract_word(c),
        }
    }
}

impl TryFrom<ImmRegDirIndIdx> for ImmReg {
    type Error = ArgConversionError;

    fn try_from(value: ImmRegDirIndIdx) -> Result<Self, Self::Error> {
        match value {
            ImmRegDirIndIdx::Imm(a) => Ok(Self::Imm(a)),
            ImmRegDirIndIdx::Reg(a) => Ok(Self::Reg(a)),
            other => Err(ArgConversionError {
                expected: Self::kinds(),
                got: other.kind(),
            }),
        }
    }
}

#[derive(PartialEq, Clone, Debug, Display)]
#[display("{0}")]
pub enum DirIndIdx {
    Dir(Dir),
    Ind(Ind),
    Idx(Idx),
}

impl DirIndIdx {
    pub const fn cost(&self) -> usize {
        match self {
            DirIndIdx::Dir(a) => a.cost(),
            DirIndIdx::Ind(a) => a.cost(),
            DirIndIdx::Idx(a) => a.cost(),
        }
    }

    fn kinds() -> Vec<ArgKind> {
        use ArgKind::*;
        vec![Dir, Ind, Idx]
    }
}

impl ResolveAddress for DirIndIdx {
    fn resolve_address(&self, c: &Registers) -> Result<C::Address, CellError> {
        match self {
            DirIndIdx::Dir(a) => a.resolve_address(c),
            DirIndIdx::Ind(a) => a.resolve_address(c),
            DirIndIdx::Idx(a) => a.resolve_address(c),
        }
    }
}

impl TryFrom<ImmRegDirIndIdx> for DirIndIdx {
    type Error = ArgConversionError;

    fn try_from(value: ImmRegDirIndIdx) -> Result<Self, Self::Error> {
        match value {
            ImmRegDirIndIdx::Dir(a) => Ok(Self::Dir(a)),
            ImmRegDirIndIdx::Ind(a) => Ok(Self::Ind(a)),
            ImmRegDirIndIdx::Idx(a) => Ok(Self::Idx(a)),
            other => Err(ArgConversionError {
                expected: Self::kinds(),
                got: other.kind(),
            }),
        }
    }
}

#[derive(PartialEq, Clone, Debug, Display)]
#[display("{0}")]
pub enum RegDirIndIdx {
    Reg(Reg),
    Dir(Dir),
    Ind(Ind),
    Idx(Idx),
}

impl RegDirIndIdx {
    pub const fn cost(&self) -> usize {
        match self {
            RegDirIndIdx::Reg(a) => a.cost(),
            RegDirIndIdx::Dir(a) => a.cost(),
            RegDirIndIdx::Ind(a) => a.cost(),
            RegDirIndIdx::Idx(a) => a.cost(),
        }
    }

    fn kinds() -> Vec<ArgKind> {
        use ArgKind::*;
        vec![Reg, Dir, Ind, Idx]
    }
}

impl TryFrom<ImmRegDirIndIdx> for RegDirIndIdx {
    type Error = ArgConversionError;

    fn try_from(value: ImmRegDirIndIdx) -> Result<Self, Self::Error> {
        match value {
            ImmRegDirIndIdx::Reg(a) => Ok(Self::Reg(a)),
            ImmRegDirIndIdx::Dir(a) => Ok(Self::Dir(a)),
            ImmRegDirIndIdx::Ind(a) => Ok(Self::Ind(a)),
            ImmRegDirIndIdx::Idx(a) => Ok(Self::Idx(a)),
            other => Err(ArgConversionError {
                expected: Self::kinds(),
                got: other.kind(),
            }),
        }
    }
}
