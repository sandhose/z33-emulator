use std::convert::TryFrom;
use std::fmt::Debug;
use thiserror::Error;
use tracing::{debug, info};

use crate::constants::*;
use crate::memory::{Cell, CellError, Memory, MemoryError, TryFromCell, Word};
use crate::parser::Parsable;

mod exception;
mod instructions;
mod registers;

pub use self::exception::Exception;
pub use self::instructions::Instruction;
pub use self::registers::{Reg, Registers, StatusRegister};

#[derive(Error, Debug)]
pub enum ProcessorError {
    #[error("CPU exception: {0}")]
    Exception(#[from] Exception),

    #[error("cell error: {0}")]
    CellError(#[from] CellError),

    #[error("invalid value for register {reg}: {inner}")]
    InvalidRegister { reg: Reg, inner: CellError },

    #[error("invalid address {address}")]
    InvalidAddress { address: i64 },

    #[error("computer reset")]
    Reset,

    #[error("TODO error handling")]
    Todo,
}

// Implement a MemoryError -> ProcessorError conversion to simplify code
impl From<MemoryError> for ProcessorError {
    fn from(e: MemoryError) -> Self {
        Self::Exception(Exception::InvalidMemoryAccess(e))
    }
}

type Result<T> = std::result::Result<T, ProcessorError>;

pub trait Labelable: Sized {
    fn resolve_label(self, address: u64) -> Option<Self>;
    fn label() -> Self;
}

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
    #[tracing::instrument(skip(self))]
    pub fn resolve_address(&self, address: Address) -> Result<u64> {
        match address {
            Address::Dir(addr) => Ok(addr),
            Address::Ind(reg) => u64::try_from_cell(&self.registers.get(reg))
                .map_err(|e| ProcessorError::InvalidRegister { reg, inner: e }),

            Address::Idx(reg, off) => u64::try_from_cell(&self.registers.get(reg))
                .map_err(|e| ProcessorError::InvalidRegister { reg, inner: e })
                .and_then(|address| {
                    let address = address as i64 + off;
                    u64::try_from(address).map_err(|_| ProcessorError::InvalidAddress { address })
                }),
        }
    }

    pub fn write<T: Into<Cell> + Debug>(&mut self, address: Address, value: T) -> Result<()> {
        let address = self.resolve_address(address)?;
        let cell = self.memory.get_mut(address)?;
        *cell = value.into();
        Ok(())
    }

    /// Set the value of a register
    ///
    /// If the instruction tries to set the %sr register, it checks if the processor is running in
    /// supervisor mode first.
    #[tracing::instrument(skip(self))]
    pub fn set_register(&mut self, reg: Reg, val: Cell) -> Result<()> {
        if reg == Reg::SR {
            self.check_privileged()?;
        }

        self.registers
            .set(reg, val)
            .map_err(|inner| ProcessorError::InvalidRegister { reg, inner })
    }

    #[tracing::instrument(skip(self))]
    fn word_from_reg(&self, reg: Reg) -> Result<Word> {
        self.registers
            .get_word(reg)
            .map_err(|inner| ProcessorError::InvalidRegister { reg, inner })
    }

    #[tracing::instrument(skip(self))]
    fn word_from_arg(&self, arg: Arg) -> Result<Word> {
        match arg {
            Arg::Address(addr) => {
                let addr = self.resolve_address(addr)?;
                self.memory
                    .get(addr)
                    .map_err(|err| err.into())
                    .and_then(|cell| cell.extract_word().map_err(|_err| ProcessorError::Todo))
            }
            Arg::Value(val) => self.word_from_value(val),
        }
    }

    #[tracing::instrument(skip(self))]
    fn arg(&self, arg: Arg) -> Result<Cell> {
        match arg {
            Arg::Address(addr) => {
                let addr = self.resolve_address(addr)?;
                // TODO: better conversion
                self.memory
                    .get(addr)
                    .map(|c| c.clone())
                    .map_err(|e| e.into())
            }
            Arg::Value(val) => Ok(self.value(val)),
        }
    }

    #[tracing::instrument(skip(self))]
    fn value(&self, value: Value) -> Cell {
        match value {
            Value::Imm(imm) => imm.into(),
            Value::Reg(reg) => self.registers.get(reg),
        }
    }

    #[tracing::instrument(skip(self))]
    fn word_from_value(&self, value: Value) -> Result<Word> {
        match value {
            Value::Imm(word) => Ok(word),
            Value::Reg(reg) => self
                .registers
                .get_word(reg)
                .map_err(|inner| ProcessorError::InvalidRegister { reg, inner }),
        }
    }

    fn jump(&mut self, address: Address) -> Result<()> {
        let address = self.resolve_address(address)?;
        debug!("Jumping to address {}", address);
        self.registers.pc = address;
        Ok(())
    }

    #[tracing::instrument(skip(self), err)]
    fn decode_instruction(&mut self) -> Result<&Instruction> {
        let address = self.resolve_address(Address::Ind(Reg::PC))?;
        let cell = self.memory.get(address)?;
        self.registers.pc += 1;
        cell.extract_instruction()
            .map_err(|_| Exception::InvalidInstruction.into())
    }

    #[tracing::instrument(skip(self), level = "debug")]
    pub fn step(&mut self) -> Result<()> {
        let inst = self.decode_instruction()?;
        let cost = inst.cost();
        info!(cost, "Executing instruction \"{}\"", inst);
        // TODO: this could be an unnecessary clone
        inst.clone().execute(self).or_else(|e| {
            if let ProcessorError::Exception(e) = e {
                self.recover_from_exception(e)
                    .map_err(ProcessorError::Exception)
            } else {
                Err(e)
            }
        })?;
        self.cycles += cost;
        debug!("Register state {:?}", self.registers);
        Ok(())
    }

    pub fn recover_from_exception(
        &mut self,
        exception: Exception,
    ) -> std::result::Result<(), Exception> {
        debug!(exception = %exception, "Recovering from exception");
        *(self.memory.get_mut(INTERRUPT_PC_SAVE)?) = self.registers.get(Reg::PC);
        *(self.memory.get_mut(INTERRUPT_SR_SAVE)?) = self.registers.get(Reg::SR);
        *(self.memory.get_mut(INTERRUPT_EXCEPTION)?) = exception.code().into();
        self.registers.sr.set(StatusRegister::SUPERVISOR, true);
        self.registers.sr.set(
            StatusRegister::INTERRUPT_ENABLE,
            !exception.is_hardware_interrupt(),
        );
        self.registers.pc = INTERRUPT_HANDLER;
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
                Ok(_) => {}
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

#[derive(Debug, Clone, PartialEq)]
pub enum Address {
    Dir(u64),
    Ind(Reg),
    Idx(Reg, i64),
}

impl Address {
    /// CPU cycles count to use this value
    pub const fn cost(&self) -> usize {
        // Accessing a memory cell, from a direct, indirect or indexed address costs 1 CPU cycle
        1
    }
}

#[derive(Error, Debug)]
#[error("could not parse address")]
pub struct AddressParseError;

impl std::str::FromStr for Address {
    type Err = AddressParseError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        // TODO: better error handling
        crate::parser::parse_inner_address(s)
            .map_err(|_| AddressParseError)
            .map(|(_, a)| a)
    }
}

impl std::fmt::Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dir(x) => write!(f, "[{}]", x),
            Self::Ind(reg) => write!(f, "[{}]", reg),
            Self::Idx(reg, off) => write!(f, "[{}{:+}]", reg, off),
        }
    }
}

impl From<u64> for Address {
    fn from(val: u64) -> Self {
        Self::Dir(val)
    }
}

impl Labelable for Address {
    fn resolve_label(self, address: u64) -> Option<Self> {
        match self {
            Self::Dir(_) => Some(Self::Dir(address)),
            Self::Ind(_) => None,
            Self::Idx(_, _) => None,
        }
    }

    fn label() -> Self {
        Self::Dir(0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Imm(u64),
    Reg(Reg),
}

impl Value {
    /// CPU cycles count to use this value
    pub const fn cost(&self) -> usize {
        // Using a direct value, immediate or from a register costs no CPU cycles
        0
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Imm(x) => write!(f, "{}", x),
            Self::Reg(reg) => write!(f, "{}", reg),
        }
    }
}

impl Labelable for Value {
    fn resolve_label(self, address: u64) -> Option<Self> {
        match self {
            Self::Imm(_) => Some(Self::Imm(address)),
            Self::Reg(_) => None,
        }
    }

    fn label() -> Self {
        Self::Imm(0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Arg {
    Address(Address),
    Value(Value),
}

impl Arg {
    /// CPU cycles count to use this value
    pub const fn cost(&self) -> usize {
        match self {
            Self::Address(a) => a.cost(),
            Self::Value(v) => v.cost(),
        }
    }
}

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Address(a) => write!(f, "{}", a),
            Self::Value(v) => write!(f, "{}", v),
        }
    }
}

impl Labelable for Arg {
    fn resolve_label(self, address: u64) -> Option<Self> {
        match self {
            Self::Address(a) => a.resolve_label(address).map(Self::Address),
            Self::Value(v) => v.resolve_label(address).map(Self::Value),
        }
    }

    fn label() -> Self {
        Self::Value(Value::label())
    }
}

/*
#[derive(Debug, Clone, PartialEq)]
enum ArgSwap {
    Reg(Reg),
    Address(Address),
}
*/

#[cfg(test)]
mod tests {
    use super::*;

    /*
    TODO: rewrite this test
    #[test]
    fn inst_encode_test() {
        Instruction::Xor(Arg::Address(Address::Dir(42)), Reg::B).assert_stable();
        Instruction::Shr(Arg::Address(Address::Idx(Reg::SR, 0x7FFF)), Reg::PC).assert_stable();
    }
    */

    #[test]
    fn inst_execute_test() {
        let mut computer = Computer::default();

        let instruction = Instruction::Add(Arg::Value(Value::Imm(5)), Reg::A);
        instruction.execute(&mut computer).unwrap();
        assert_eq!(computer.registers.get(Reg::A), Cell::Word(5));

        // Write some memory (with indirect access)
        computer.write(Address::Dir(0x42), 100 as u64).unwrap();
        computer.registers.set(Reg::B, Cell::Word(0x32)).unwrap();
        let instruction = Instruction::Add(Arg::Address(Address::Idx(Reg::B, 0x10)), Reg::A);
        instruction.execute(&mut computer).unwrap();
        assert_eq!(computer.registers.get(Reg::A), Cell::Word(105));
    }

    #[test]
    fn step_test() {
        let mut computer = Computer::default();
        let start = 0x100;
        let program = vec![
            Instruction::Ld(Arg::Value(Value::Imm(0x42)), Reg::A),
            Instruction::Ld(Arg::Value(Value::Imm(0x24)), Reg::B),
            Instruction::Add(Arg::Value(Value::Reg(Reg::A)), Reg::B),
        ];

        for (offset, instruction) in program.into_iter().enumerate() {
            computer
                .write((start + offset as u64).into(), instruction)
                .unwrap();
        }

        computer.jump(start.into()).unwrap();

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
        let start = 100;
        let subroutine = 200;
        let stack = STACK_START;
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
            Instruction::Call(Arg::Value(Value::Imm(subroutine))),
            Instruction::Add(Arg::Value(Value::Reg(Reg::A)), Reg::B),
        ]
        .into_iter()
        .enumerate()
        .map(|(offset, instruction)| (start + offset as u64, instruction));

        let subroutine_inst = vec![
            Instruction::Ld(Arg::Value(Value::Imm(42)), Reg::A),
            Instruction::Ld(Arg::Value(Value::Imm(24)), Reg::B),
            Instruction::Rtn,
        ]
        .into_iter()
        .enumerate()
        .map(|(offset, instruction)| (subroutine + offset as u64, instruction));

        for (addr, inst) in start_inst.chain(subroutine_inst) {
            computer.write(addr.into(), inst).unwrap();
        }

        computer.jump(start.into()).unwrap();

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
