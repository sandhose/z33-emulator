use bitflags::bitflags;
use parse_display::Display;
use thiserror::Error;

use super::memory::{Cell, CellError};
use crate::ast::{AstNode, NodeKind};
use crate::constants as C;

bitflags! {
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct StatusRegister: C::Word {
        const CARRY            = 0b000_0000_0001;
        const ZERO             = 0b000_0000_0010;
        const NEGATIVE         = 0b000_0000_0100;
        const OVERFLOW         = 0b000_0000_1000;
        const INTERRUPT_ENABLE = 0b001_0000_0000;
        const SUPERVISOR       = 0b010_0000_0000;
    }
}

impl Default for StatusRegister {
    fn default() -> Self {
        // On startup, only the supervisor bit is set
        StatusRegister::SUPERVISOR
    }
}

impl std::fmt::Debug for StatusRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#012b}", self.bits())
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Registers {
    /// General purpose
    pub a: Cell,

    /// General purpose
    pub b: Cell,

    /// Program counter
    pub pc: C::Address,

    /// Stack pointer
    pub sp: C::Address,

    /// Status register
    pub sr: StatusRegister,
}

impl Registers {
    #[must_use]
    pub fn get(&self, reg: &Reg) -> Cell {
        match reg {
            Reg::A => self.a.clone(),
            Reg::B => self.b.clone(),
            Reg::PC => self.pc.into(),
            Reg::SP => self.sp.into(),
            Reg::SR => self.sr.bits().into(),
        }
    }

    pub(crate) fn get_word(&self, reg: Reg) -> Result<C::Word, CellError> {
        match reg {
            Reg::A => self.a.extract_word(),
            Reg::B => self.b.extract_word(),
            Reg::PC => Ok(self.pc.into()),
            Reg::SP => Ok(self.sp.into()),
            Reg::SR => Ok(self.sr.bits()),
        }
    }

    /// Set a register value
    ///
    /// # Errors
    ///
    /// This function will return an error if an invalid address is set to the
    /// %pc or %sp register, or an invalid value is given to the %sr register.
    pub fn set(&mut self, reg: Reg, value: Cell) -> Result<(), CellError> {
        match reg {
            Reg::A => self.a = value,
            Reg::B => self.b = value,
            Reg::PC => {
                self.pc = C::Address::try_from(&value)?;
            }
            Reg::SP => {
                self.sp = C::Address::try_from(&value)?;
            }
            Reg::SR => self.sr = StatusRegister::from_bits_retain(C::Word::try_from(&value)?),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
#[display("%{}", style = "lowercase")]
pub enum Reg {
    /// General purpose
    A,

    /// General purpose
    B,

    /// Program counter
    PC,

    /// Stack pointer
    SP,

    /// Status register
    SR,
}

impl Reg {
    /// CPU cycles count to use this value
    pub(crate) const fn cost() -> usize {
        // Accessing a register does not take any CPU cycle
        0
    }
}

impl AstNode for Reg {
    fn kind(&self) -> NodeKind {
        NodeKind::Register
    }

    fn content(&self) -> Option<String> {
        Some(format!("{self}"))
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
