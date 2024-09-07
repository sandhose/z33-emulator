//! The actual emulator runtime

use std::fmt::Debug;

use thiserror::Error;
use tracing::{debug, info, trace};

use crate::constants as C;

pub(crate) mod arguments;
mod exception;
mod instructions;
mod memory;
mod registers;

pub use self::arguments::ExtractValue;
use self::arguments::{ExtractError, Ind, ResolveAddress};
pub use self::exception::Exception;
pub(crate) use self::instructions::Instruction;
pub use self::memory::{Cell, Memory};
use self::memory::{CellError, MemoryError};
use self::registers::StatusRegister;
pub use self::registers::{Reg, Registers};

#[derive(Error, Debug)]
pub enum ProcessorError {
    #[error("CPU exception: {0}")]
    Exception(#[from] Exception),

    #[error("cell error: {0}")]
    CellError(#[from] CellError),

    #[error("extract word: {0}")]
    Extract(#[from] ExtractError),

    #[error("invalid value for register {reg}: {inner}")]
    InvalidRegister { reg: Reg, inner: CellError },

    #[error("invalid address {address}")]
    InvalidAddress { address: C::Word },

    #[error("computer reset")]
    Reset,
}

// Implement a MemoryError -> ProcessorError conversion to simplify code
impl From<MemoryError> for ProcessorError {
    fn from(e: MemoryError) -> Self {
        Self::Exception(Exception::InvalidMemoryAccess(e))
    }
}

type Result<T> = std::result::Result<T, ProcessorError>;

#[derive(Default)]
pub struct Computer {
    pub registers: Registers,
    pub memory: Memory,
    pub cycles: usize,
}

impl std::fmt::Debug for Computer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Computer {{ registers: {:?}, memory: [...] }}",
            self.registers
        )
    }
}

impl Computer {
    pub(crate) fn write<T: Into<Cell> + Debug>(
        &mut self,
        address: C::Address,
        value: T,
    ) -> Result<()> {
        let cell = self.memory.get_mut(address)?;
        *cell = value.into();
        Ok(())
    }

    /// Set the value of a register
    ///
    /// If the instruction tries to set the %sr register, it checks if the
    /// processor is running in supervisor mode first.
    #[tracing::instrument(skip(self))]
    pub(crate) fn set_register(&mut self, reg: &Reg, val: Cell) -> Result<()> {
        let reg = *reg;
        if reg == Reg::SR {
            self.check_privileged()?;
        }

        self.registers
            .set(reg, val)
            .map_err(|inner| ProcessorError::InvalidRegister { reg, inner })
    }

    fn jump(&mut self, address: C::Address) {
        debug!("Jumping to address {}", address);
        self.registers.pc = address;
    }

    fn decode_instruction(&mut self) -> Result<&Instruction> {
        let address = Ind(Reg::PC).resolve_address(&self.registers)?;
        let cell = self.memory.get(address)?;
        self.registers.pc += 1;
        cell.extract_instruction()
            .map_err(|_| Exception::InvalidInstruction.into())
    }

    #[tracing::instrument(skip(self), level = "debug", fields(cost))]
    pub fn step(&mut self) -> Result<()> {
        // Wrapping the part that can be recovered from in another function
        fn inner(c: &mut Computer) -> Result<usize> {
            let inst = c.decode_instruction()?;
            let cost = inst.cost();
            tracing::Span::current().record("cost", cost);
            info!("Executing instruction \"{}\"", inst);
            // This clone is necessary as `inst` is borrowed from `self`.
            // The computer might modify the cell where the instruction is stored when
            // executing it.
            inst.clone().execute(c)?;
            Ok(cost)
        }

        let cost = inner(self).or_else(|e| {
            if let ProcessorError::Exception(e) = e {
                self.recover_from_exception(&e)
                    .map_err(ProcessorError::Exception)
                    .map(|()| 1) // TODO: fixed cost for exceptions?
            } else {
                Err(e)
            }
        })?;
        self.cycles += cost;
        trace!("Register state {:?}", self.registers);
        Ok(())
    }

    /// Recover from an exception
    ///
    /// This will:
    ///   - save the %pc and %sr registers in memory address 100 and 101
    ///     respectively
    ///   - set the %sr.supervisor bit
    ///   - set the %sr "interrupt enable"" bit if it is an hardware interrupt
    ///   - save the exception code in memory address 102
    ///   - set the %pc to the address of the exception handler (200)
    ///
    /// # Errors
    ///
    /// This function will return an error if any of the memory accesses fails.
    pub fn recover_from_exception(
        &mut self,
        exception: &Exception,
    ) -> std::result::Result<(), Exception> {
        // We don't want to recover from the exception if there is no handler setup
        let handler = self.memory.get(C::INTERRUPT_HANDLER)?;
        if handler.extract_instruction().is_err() {
            tracing::warn!(
                "No exception handler at address 200 for exception {:?}",
                exception
            );
            return Err(exception.clone());
        }

        debug!(exception = %exception, "Recovering from exception");
        *(self.memory.get_mut(C::INTERRUPT_PC_SAVE)?) = self.registers.get(&Reg::PC);
        *(self.memory.get_mut(C::INTERRUPT_SR_SAVE)?) = self.registers.get(&Reg::SR);
        *(self.memory.get_mut(C::INTERRUPT_EXCEPTION)?) = exception.code().into();
        self.registers.sr.set(StatusRegister::SUPERVISOR, true);
        self.registers.sr.set(
            StatusRegister::INTERRUPT_ENABLE,
            !exception.is_hardware_interrupt(),
        );
        self.registers.pc = C::INTERRUPT_HANDLER;
        Ok(())
    }

    fn check_privileged(&self) -> Result<()> {
        if self.registers.sr.contains(StatusRegister::SUPERVISOR) {
            Ok(())
        } else {
            Err(Exception::PrivilegedInstruction.into())
        }
    }

    #[tracing::instrument(skip(self))]
    pub fn run(&mut self) -> Result<()> {
        loop {
            match self.step() {
                Ok(()) => {}
                Err(ProcessorError::Reset) => return Ok(()),
                Err(v) => return Err(v),
            }
        }
    }

    #[tracing::instrument(skip(self))]
    fn push<T: Into<Cell> + Debug>(&mut self, value: T) -> std::result::Result<(), Exception> {
        self.registers.sp -= 1;

        // And write it on memeory
        let address = self.registers.sp;
        let cell = self.memory.get_mut(address)?;
        *cell = value.into();
        Ok(())
    }

    #[tracing::instrument(skip(self))]
    fn pop(&mut self) -> std::result::Result<&Cell, Exception> {
        // First read the value
        let val = self.memory.get(self.registers.sp)?;
        // Then move the SP
        self.registers.sp += 1;
        debug!("Poping value: {:?}", val);
        Ok(val)
    }
}

#[derive(Error, Debug)]
#[error("could not parse address")]
pub struct AddressParseError;

#[cfg(test)]
mod tests {
    // This is fine in tests
    #![allow(clippy::cast_possible_truncation)]

    use super::arguments::{Idx, Imm, ImmRegDirIndIdx};
    use super::*;
    use crate::constants::Word;

    #[test]
    fn inst_execute_test() {
        let mut computer = Computer::default();

        let instruction = Instruction::Add(ImmRegDirIndIdx::Imm(Imm(5)), Reg::A);
        instruction.execute(&mut computer).unwrap();
        assert_eq!(computer.registers.get(&Reg::A), Cell::Word(5));

        // Write some memory (with indirect access)
        computer.write(0x42, 100_i64).unwrap();
        computer.registers.set(Reg::B, Cell::Word(0x32)).unwrap();
        let instruction = Instruction::Add(ImmRegDirIndIdx::Idx(Idx(Reg::B, 0x10)), Reg::A);
        instruction.execute(&mut computer).unwrap();
        assert_eq!(computer.registers.get(&Reg::A), Cell::Word(105));
    }

    #[test]
    fn step_test() {
        let mut computer = Computer::default();
        let start: C::Address = 0x100;
        let program = vec![
            Instruction::Ld(ImmRegDirIndIdx::Imm(Imm(0x42)), Reg::A),
            Instruction::Ld(ImmRegDirIndIdx::Imm(Imm(0x24)), Reg::B),
            Instruction::Add(ImmRegDirIndIdx::Reg(Reg::A), Reg::B),
        ];

        for (offset, instruction) in program.into_iter().enumerate() {
            computer
                .write(start + offset as C::Address, instruction)
                .unwrap();
        }

        computer.jump(start);

        assert_eq!(computer.registers.a, Cell::Empty);
        assert_eq!(computer.registers.b, Cell::Empty);
        assert_eq!(computer.registers.pc, start);
        computer.step().unwrap();

        assert_eq!(computer.registers.a, Cell::Word(0x42));
        assert_eq!(computer.registers.b, Cell::Empty);
        assert_eq!(computer.registers.pc, start + 1);
        computer.step().unwrap();

        assert_eq!(computer.registers.a, Cell::Word(0x42));
        assert_eq!(computer.registers.b, Cell::Word(0x24));
        assert_eq!(computer.registers.pc, start + 2);
        computer.step().unwrap();

        assert_eq!(computer.registers.a, Cell::Word(0x42));
        assert_eq!(computer.registers.b, Cell::Word(0x66));
        assert_eq!(computer.registers.pc, start + 3);
    }

    #[test]
    fn call_test() {
        let mut computer = Computer::default();
        let start: C::Address = 100;
        let subroutine: C::Address = 200;
        let stack = C::STACK_START;
        computer.registers.sp = stack; // Set the stack pointer somewhere

        // program:
        //  start:
        //      call subroutine
        //      add %a, %b
        //      (halt)
        //
        //  subroutine:
        //      ld 0x42, %a
        //      ld 0x24, %b
        //      rtn

        let start_inst = vec![
            Instruction::Call(ImmRegDirIndIdx::Imm(Imm(Word::from(subroutine)))),
            Instruction::Add(ImmRegDirIndIdx::Reg(Reg::A), Reg::B),
        ]
        .into_iter()
        .enumerate()
        .map(|(offset, instruction)| (start + offset as C::Address, instruction));

        let subroutine_inst = vec![
            Instruction::Ld(ImmRegDirIndIdx::Imm(Imm(42)), Reg::A),
            Instruction::Ld(ImmRegDirIndIdx::Imm(Imm(24)), Reg::B),
            Instruction::Rtn,
        ]
        .into_iter()
        .enumerate()
        .map(|(offset, instruction)| (subroutine + offset as C::Address, instruction));

        for (addr, inst) in start_inst.chain(subroutine_inst) {
            computer.write(addr, inst).unwrap();
        }

        computer.jump(start);

        assert_eq!(computer.registers.a, Cell::Empty);
        assert_eq!(computer.registers.b, Cell::Empty);
        assert_eq!(computer.registers.pc, start);
        assert_eq!(computer.registers.sp, stack);
        // call subroutine
        computer.step().unwrap();

        assert_eq!(computer.registers.a, Cell::Empty);
        assert_eq!(computer.registers.b, Cell::Empty);
        assert_eq!(computer.registers.pc, subroutine);
        assert_eq!(computer.registers.sp, stack - 1);
        // ld 0x42, %a
        computer.step().unwrap();

        assert_eq!(computer.registers.a, Cell::Word(42));
        assert_eq!(computer.registers.b, Cell::Empty);
        assert_eq!(computer.registers.pc, subroutine + 1);
        assert_eq!(computer.registers.sp, stack - 1);
        // ld 0x24, %b
        computer.step().unwrap();

        assert_eq!(computer.registers.a, Cell::Word(42));
        assert_eq!(computer.registers.b, Cell::Word(24));
        assert_eq!(computer.registers.pc, subroutine + 2);
        assert_eq!(computer.registers.sp, stack - 1);
        // rtn
        computer.step().unwrap();

        assert_eq!(computer.registers.a, Cell::Word(42));
        assert_eq!(computer.registers.b, Cell::Word(24));
        assert_eq!(computer.registers.pc, start + 1);
        assert_eq!(computer.registers.sp, stack);
        // add %a, %b
        computer.step().unwrap();

        assert_eq!(computer.registers.a, Cell::Word(42));
        assert_eq!(computer.registers.b, Cell::Word(66));
        assert_eq!(computer.registers.pc, start + 2);
        assert_eq!(computer.registers.sp, stack);
    }

    /*
    #[test]
    fn overflow_test() {
        let mut computer = Computer::default();
        computer.registers.a = 0xFFFF;

        let instruction = Instruction::Add(Arg::Value(Value::Imm(1)), Reg::A);
        instruction.execute(&mut computer).unwrap();
        assert_eq!(computer.registers.a, 0);
        assert!(computer.registers.sr.contains(StatusRegister::OVERFLOW));

        let instruction = Instruction::Add(Arg::Value(Value::Imm(1)), Reg::A);
        instruction.execute(&mut computer).unwrap();
        assert_eq!(computer.registers.a, 1);
        assert!(!computer.registers.sr.contains(StatusRegister::OVERFLOW));
    }
    */
}
