use bitflags::bitflags;
use thiserror::Error;

use super::memory::{Cell, CellError, TryFromCell, Word};

bitflags! {
    #[derive(Default)]
    pub struct StatusRegister: Word {
        const CARRY            = 0b000_0000_0001;
        const ZERO             = 0b000_0000_0010;
        const NEGATIVE         = 0b000_0000_0100;
        const OVERFLOW         = 0b000_0000_1000;
        const INTERRUPT_ENABLE = 0b001_0000_0000;
        const SUPERVISOR       = 0b010_0000_0000;
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Registers {
    pub a: Cell,
    pub b: Cell,
    pub pc: Word,
    pub sp: Word,
    pub sr: StatusRegister,
}

impl Registers {
    pub fn get(&self, reg: Reg) -> Cell {
        match reg {
            Reg::A => self.a.clone(),
            Reg::B => self.b.clone(),
            Reg::PC => self.pc.into(),
            Reg::SP => self.sp.into(),
            Reg::SR => self.sr.bits().into(),
        }
    }

    pub fn get_word(&self, reg: Reg) -> Result<Word, CellError> {
        match reg {
            Reg::A => self.a.extract_word(),
            Reg::B => self.b.extract_word(),
            Reg::PC => Ok(self.pc),
            Reg::SP => Ok(self.sp),
            Reg::SR => Ok(self.sr.bits),
        }
    }

    pub fn set(&mut self, reg: Reg, value: Cell) -> Result<(), CellError> {
        match reg {
            Reg::A => self.a = value,
            Reg::B => self.b = value,
            Reg::PC => self.pc = Word::try_from_cell(&value)?,
            Reg::SP => self.sp = Word::try_from_cell(&value)?,
            Reg::SR => self.sr.bits = Word::try_from_cell(&value)?,
        };
        Ok(())
    }
}

impl std::fmt::Display for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%a = {} | %b = {} | %pc = {} | %sp = {} | %sr = {:?}",
            self.a, self.b, self.pc, self.sp, self.sr
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Reg {
    A,
    B,
    PC,
    SP,
    SR,
}

impl Reg {
    /// CPU cycles count to use this value
    pub const fn cost(&self) -> usize {
        // Accessing a register does not take any CPU cycle
        0
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::A => write!(f, "%a"),
            Self::B => write!(f, "%b"),
            Self::PC => write!(f, "%pc"),
            Self::SP => write!(f, "%sp"),
            Self::SR => write!(f, "%sr"),
        }
    }
}

#[derive(Error, Debug)]
#[error("could not parse register")]
pub struct RegisterParseError;

impl std::str::FromStr for Reg {
    type Err = RegisterParseError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "%a" | "a" => Ok(Reg::A),
            "%b" | "b" => Ok(Reg::B),
            "%pc" | "pc" => Ok(Reg::PC),
            "%sp" | "sp" => Ok(Reg::SP),
            "%sr" | "sr" => Ok(Reg::SR),
            _ => Err(RegisterParseError),
        }
    }
}
