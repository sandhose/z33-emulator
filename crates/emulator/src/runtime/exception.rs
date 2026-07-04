use thiserror::Error;

use super::memory::MemoryError;
use crate::constants::Word;

#[derive(Error, Debug, Clone)]
pub enum Exception {
    // r[impl exc.code.hardware-interrupt]
    #[error("hardware interrupt")]
    HardwareInterrupt,

    // r[impl exc.code.division-by-zero]
    #[error("division by zero")]
    DivByZero,

    // r[impl exc.code.invalid-instruction]
    #[error("invalid instruction")]
    InvalidInstruction,

    // r[impl exc.code.privileged-instruction]
    #[error("privileged instruction")]
    PrivilegedInstruction,

    // r[impl exc.code.trap]
    #[error("trap")]
    Trap,

    // r[impl exc.code.invalid-memory-access]
    #[error("invalid memory access ({0})")]
    InvalidMemoryAccess(#[from] MemoryError),
}

impl Exception {
    pub(crate) fn code(&self) -> Word {
        match self {
            // r[related exc.code.hardware-interrupt]
            Exception::HardwareInterrupt => 0,
            // r[related exc.code.division-by-zero]
            Exception::DivByZero => 1,
            // r[related exc.code.invalid-instruction]
            Exception::InvalidInstruction => 2,
            // r[related exc.code.privileged-instruction]
            Exception::PrivilegedInstruction => 3,
            // r[related exc.code.trap]
            Exception::Trap => 4,
            // r[related exc.code.invalid-memory-access]
            Exception::InvalidMemoryAccess(_) => 5,
        }
    }

    pub(crate) fn is_hardware_interrupt(&self) -> bool {
        matches!(self, Exception::HardwareInterrupt)
    }
}
