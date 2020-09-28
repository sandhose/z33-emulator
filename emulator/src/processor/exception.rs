use thiserror::Error;

use crate::memory::{MemoryError, Word};

#[derive(Error, Debug)]
pub enum Exception {
    #[error("hardware interrupt")]
    HardwareInterrupt,

    #[error("division by zero")]
    DivByZero,

    #[error("invalid instruction")]
    InvalidInstruction,

    #[error("privileged instruction")]
    PrivilegedInstruction,

    #[error("trap")]
    Trap,

    #[error("invalid memory access ({0})")]
    InvalidMemoryAccess(#[from] MemoryError),
}

impl Exception {
    pub fn code(&self) -> Word {
        match self {
            Exception::HardwareInterrupt => 0,
            Exception::DivByZero => 1,
            Exception::InvalidInstruction => 2,
            Exception::PrivilegedInstruction => 3,
            Exception::Trap => 4,
            Exception::InvalidMemoryAccess(_) => 5,
        }
    }

    pub fn is_hardware_interrupt(&self) -> bool {
        match self {
            Exception::HardwareInterrupt => true,
            _ => false,
        }
    }
}