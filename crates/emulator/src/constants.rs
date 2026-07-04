//! Various constants and type definitions used throughout the emulator

// r[impl arch.address]
pub type Address = u32;
// r[impl arch.word]
pub type Word = i64;

/// Total size of the computer memory
// r[impl arch.memory.size]
pub const MEMORY_SIZE: Address = 10_000;

/// Start of the stack pointer
// r[impl arch.stack]
pub const STACK_START: Address = MEMORY_SIZE;

/// Default place to store the beginning of the program
// r[impl arch.special-addr.program-start]
pub const PROGRAM_START: Address = 1000;

/// Address of the interrupt handler
// r[impl arch.special-addr.interrupt-handler]
pub const INTERRUPT_HANDLER: Address = 200;

/// Address where %pc is saved when an interruption occurs
// r[impl arch.special-addr.interrupt-pc-save]
pub const INTERRUPT_PC_SAVE: Address = 100;

/// Address where %sr is saved when an interruption occurs
// r[impl arch.special-addr.interrupt-sr-save]
pub const INTERRUPT_SR_SAVE: Address = 101;

/// Address the exception code is saved when an interruption occurs
// r[impl arch.special-addr.interrupt-exception]
pub const INTERRUPT_EXCEPTION: Address = 102;
