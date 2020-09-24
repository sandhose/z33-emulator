use bitflags::bitflags;
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use thiserror::Error;
use tracing::{debug, info};
use z33_instruction_derive::Instruction;

use super::memory::{Cell, CellError, Memory, MemoryError, TryFromCell, Word};
use crate::parser::Parsable;

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

    #[error("invalid memory access")]
    InvalidMemoryAccess,
}

impl Exception {
    fn code(&self) -> Word {
        match self {
            Exception::HardwareInterrupt => 0,
            Exception::DivByZero => 1,
            Exception::InvalidInstruction => 2,
            Exception::PrivilegedInstruction => 3,
            Exception::Trap => 4,
            Exception::InvalidMemoryAccess => 5,
        }
    }

    fn is_hardware_interrupt(&self) -> bool {
        match self {
            Exception::HardwareInterrupt => true,
            _ => false,
        }
    }
}

#[derive(Error, Debug)]
pub enum ProcessorError {
    #[error("CPU exception: {0}")]
    Exception(#[from] Exception),

    #[error("memory error: {0}")]
    MemoryError(#[from] MemoryError),

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

type Result<T> = std::result::Result<T, ProcessorError>;

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

pub trait Labelable: Sized {
    fn resolve_label(self, address: u64) -> Option<Self>;
    fn label() -> Self;
}

trait Mmapped: TryFromCell + Into<Cell> + Sized + PartialEq + Clone + std::fmt::Debug {
    #[cfg(test)]
    fn assert_stable(self) {
        let cell: Cell = self.clone().into();
        let decoded = Self::try_from_cell(&cell).map_err(|_| ()).unwrap();
        assert_eq!(self, decoded);
    }
}

impl<T> Mmapped for T where T: TryFromCell + Into<Cell> + Sized + PartialEq + Clone + std::fmt::Debug
{}

#[derive(Default)]
pub struct Computer {
    pub registers: Registers,
    pub memory: Memory,
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

        self.registers.set(reg, val)
    }

    #[tracing::instrument(skip(self))]
    fn word_from_reg(&self, reg: Reg) -> Result<Word> {
        self.registers.get_word(reg)
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
            Value::Reg(reg) => self.registers.get_word(reg),
        }
    }

    fn jump(&mut self, address: Address) -> Result<()> {
        let address = self.resolve_address(address)?;
        debug!("Jumping to address {:#x}", address);
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

    #[tracing::instrument(skip(self))]
    pub fn step(&mut self) -> Result<()> {
        let inst = self.decode_instruction()?;
        info!("Executing instruction \"{}\"", inst);
        // TODO: this could be an unnecessary clone
        inst.clone().execute(self).or_else(|e| {
            if let ProcessorError::Exception(e) = e {
                self.recover_from_exception(e)
            } else {
                Err(e)
            }
        })?;
        debug!("Register state {:?}", self.registers);
        Ok(())
    }

    pub fn recover_from_exception(&mut self, exception: Exception) -> Result<()> {
        debug!(exception = %exception, "Recovering from exception");
        *(self.memory.get_mut(100)?) = self.registers.get(Reg::PC);
        *(self.memory.get_mut(101)?) = self.registers.get(Reg::SR);
        *(self.memory.get_mut(102)?) = exception.code().into();
        self.registers.sr.set(StatusRegister::SUPERVISOR, true);
        self.registers.sr.set(
            StatusRegister::INTERRUPT_ENABLE,
            !exception.is_hardware_interrupt(),
        );
        self.registers.pc = 200;
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
    fn push<T: Into<Cell> + Debug>(&mut self, value: T) -> Result<()> {
        self.registers.sp -= 1;

        // And write it on memeory
        let address = self.registers.sp;
        let cell = self.memory.get_mut(address)?;
        *cell = value.into();
        Ok(())
    }

    #[tracing::instrument(skip(self))]
    fn pop(&mut self) -> Result<&Cell> {
        // First read the value
        let val = self.memory.get(self.registers.sp)?;
        // Then move the SP
        self.registers.sp += 1;
        debug!("Poping value: {:?}", val);
        Ok(val)
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

    pub fn get_word(&self, reg: Reg) -> Result<Word> {
        match reg {
            Reg::A => self
                .a
                .extract_word()
                .map_err(|inner| ProcessorError::InvalidRegister { reg, inner }),
            Reg::B => self
                .b
                .extract_word()
                .map_err(|inner| ProcessorError::InvalidRegister { reg, inner }),
            Reg::PC => Ok(self.pc),
            Reg::SP => Ok(self.sp),
            Reg::SR => Ok(self.sr.bits),
        }
    }

    pub fn set(&mut self, reg: Reg, value: Cell) -> Result<()> {
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

#[derive(Debug, Clone, PartialEq)]
pub enum Address {
    Dir(u64),
    Ind(Reg),
    Idx(Reg, i64),
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
            Self::Dir(x) => write!(f, "[{:#04x}]", x),
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

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Imm(x) => write!(f, "{:#04x}", x),
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
    pub fn label() -> Self {
        Arg::Value(Value::Imm(0))
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

#[derive(Debug, Clone, PartialEq, Instruction)]
pub enum Instruction {
    /// Add a value to a register
    #[instruction(0x01)]
    Add(#[labelable] Arg, Reg),

    /// Bitwise `and` with a given value
    #[instruction(0x02)]
    And(#[labelable] Arg, Reg),

    /// Push `%pc` and go to the given address
    #[instruction(0x03)]
    Call(#[labelable] Arg),

    /// Compare a value with a register
    #[instruction(0x04)]
    Cmp(#[labelable] Arg, Reg),

    /// Divide a register by a value
    #[instruction(0x05)]
    Div(#[labelable] Arg, Reg),

    /// Load a memory cell to a register and set this cell to 1
    #[instruction(0x06)]
    Fas(Address, Reg),

    /// Read a value from an I/O controller
    #[instruction(0x07)]
    In(Address, Reg),

    /// Unconditional jump
    #[instruction(0x08)]
    Jmp(#[labelable] Arg),

    /// Jump if equal
    #[instruction(0x09)]
    Jeq(#[labelable] Arg),

    /// Jump if not equal
    #[instruction(0x0A)]
    Jne(#[labelable] Arg),

    /// Jump if less or equal
    #[instruction(0x0B)]
    Jle(#[labelable] Arg),

    /// Jump if strictly less
    #[instruction(0x0C)]
    Jlt(#[labelable] Arg),

    /// Jump if greater of equal
    #[instruction(0x0D)]
    Jge(#[labelable] Arg),

    /// Jump if strictly greater
    #[instruction(0x0E)]
    Jgt(#[labelable] Arg),

    /// Load a register with a value
    #[instruction(0x0F)]
    Ld(#[labelable] Arg, Reg),

    /// Multiply a value to a register
    #[instruction(0x10)]
    Mul(#[labelable] Arg, Reg),

    #[instruction(0x11)]
    Neg(Reg),

    /// No-op
    #[instruction(0x12)]
    Nop,

    /// Bitwise negation of a register
    #[instruction(0x13)]
    Not(Reg),

    /// Bitwise `or` with a given value
    #[instruction(0x14)]
    Or(#[labelable] Arg, Reg),

    /// Write a value to an I/O controller
    #[instruction(0x15)]
    Out(Value, Address),

    /// Pop a value from the stack
    #[instruction(0x16)]
    Pop(Reg),

    /// Push a value into the stack
    #[instruction(0x17)]
    Push(Value),

    /// Reset the computer
    #[instruction(0x18)]
    Reset,

    /// Return from an interrupt or an exception
    #[instruction(0x19)]
    Rti,

    /// Return from a `call`
    #[instruction(0x1A)]
    Rtn,

    /// Bitshift to the left
    #[instruction(0x1B)]
    Shl(#[labelable] Arg, Reg),

    /// Bitshift to the right
    #[instruction(0x1C)]
    Shr(#[labelable] Arg, Reg),

    /// Store a register value in memory
    #[instruction(0x1D)]
    St(Reg, Address),

    /// Substract a value from a register
    #[instruction(0x1E)]
    Sub(#[labelable] Arg, Reg),

    // /// Swap a value and a register
    // #[instruction(0x1F)]
    // Swap(ArgSwap, Reg),
    /// Start a `trap` exception
    #[instruction(0x20)]
    Trap,

    /// Bitwise `xor` with a given value
    #[instruction(0x21)]
    Xor(#[labelable] Arg, Reg),
}

impl Instruction {
    #[tracing::instrument]
    fn execute(self, computer: &mut Computer) -> Result<()> {
        match self {
            Instruction::Add(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;
                let (res, overflow) = a.overflowing_add(b);
                debug!("{} + {} = {}", a, b, res);
                computer.set_register(reg, res.into())?;

                computer
                    .registers
                    .sr
                    .set(StatusRegister::OVERFLOW, overflow);
            }
            Instruction::And(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;
                let res = a & b;
                debug!("{} & {} = {}", a, b, res);
                computer.set_register(reg, res.into())?;
            }
            Instruction::Call(arg) => {
                // Push PC
                let pc = computer.registers.pc;
                computer.push(pc)?;

                // Jump
                let addr = computer.word_from_arg(arg)?;
                computer.jump(Address::Dir(addr))?;
            }
            Instruction::Cmp(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;

                computer.registers.sr.set(StatusRegister::ZERO, a == b);
                computer.registers.sr.set(StatusRegister::NEGATIVE, a < b);

                debug!(
                    "cmp({}, {}) => {:?}",
                    a,
                    b,
                    computer.registers.sr & (StatusRegister::ZERO | StatusRegister::NEGATIVE)
                );
            }
            Instruction::Div(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;
                let res = b.checked_div(a).ok_or(Exception::DivByZero)?;
                debug!("{} / {} = {}", b, a, res);
                computer.set_register(reg, res.into())?;
            }
            Instruction::Fas(addr, reg) => {
                let addr = computer.resolve_address(addr)?;
                let cell = computer.memory.get_mut(addr)?;
                let val = cell.clone();
                *cell = Cell::Word(1);
                computer.set_register(reg, val)?;
            }
            Instruction::In(_, _) => {
                computer.check_privileged()?;
                todo!();
            }
            Instruction::Jmp(arg) => {
                let val = computer.word_from_arg(arg)?;
                debug!("Jumping to address {:#x}", val);
                computer.registers.pc = val;
            }
            Instruction::Jeq(arg) => {
                if computer.registers.sr.contains(StatusRegister::ZERO) {
                    let val = computer.word_from_arg(arg)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }
            Instruction::Jne(arg) => {
                if !computer.registers.sr.contains(StatusRegister::ZERO) {
                    let val = computer.word_from_arg(arg)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }
            Instruction::Jle(arg) => {
                if computer.registers.sr.contains(StatusRegister::ZERO)
                    || computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = computer.word_from_arg(arg)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }
            Instruction::Jlt(arg) => {
                if !computer.registers.sr.contains(StatusRegister::ZERO)
                    && computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = computer.word_from_arg(arg)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }
            Instruction::Jge(arg) => {
                if computer.registers.sr.contains(StatusRegister::ZERO)
                    || !computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = computer.word_from_arg(arg)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }
            Instruction::Jgt(arg) => {
                if !computer.registers.sr.contains(StatusRegister::ZERO)
                    && !computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = computer.word_from_arg(arg)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }
            Instruction::Ld(arg, reg) => {
                let val = computer.arg(arg)?;
                computer.set_register(reg, val)?;
            }
            Instruction::Mul(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;
                let (res, overflow) = a.overflowing_mul(b);
                debug!("{} * {} = {}", a, b, res);
                computer.set_register(reg, res.into())?;

                computer
                    .registers
                    .sr
                    .set(StatusRegister::OVERFLOW, overflow);
            }
            Instruction::Neg(reg) => {
                let val = computer.word_from_reg(reg)?;
                let res = -(val as i64);
                let res = res as Word;
                debug!("-{} = {}", val, res);
                computer.set_register(reg, res.into())?;
            }
            Instruction::Nop => {}
            Instruction::Not(reg) => {
                let val = computer.word_from_reg(reg)?;
                let res = !val;
                debug!("!{} = {}", val, res);
                computer.set_register(reg, res.into())?;
            }
            Instruction::Or(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;
                let res = a | b;
                debug!("{} | {} = {}", a, b, res);
                computer.set_register(reg, res.into())?;
            }
            Instruction::Out(_, _) => {
                computer.check_privileged()?;
                todo!()
            }
            Instruction::Pop(reg) => {
                let val = computer.pop()?.clone();
                debug!("pop => {:?}", val);
                computer.set_register(reg, val)?;
            }
            Instruction::Push(val) => {
                let val = computer.value(val);
                debug!("push({:?})", val);
                computer.push(val)?;
            }
            Instruction::Reset => return Err(ProcessorError::Reset),
            Instruction::Rti => {
                computer.check_privileged()?;
                computer.registers.pc = computer.memory.get(100)?.extract_word()?;
                computer.registers.sr =
                    StatusRegister::from_bits_truncate(computer.memory.get(101)?.extract_word()?);
            }
            Instruction::Rtn => {
                let ret = computer.pop()?; // Pop the return address
                let ret = ret.extract_word()?; // Convert it to an address
                debug!("Returning to {}", ret);
                computer.registers.pc = ret; // and jump to it
            }
            Instruction::Shl(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;

                let b: u32 = b.try_into().map_err(|_| Exception::InvalidInstruction)?;
                let res = a.checked_shl(b).ok_or(Exception::InvalidInstruction)?;

                debug!("{} << {} = {}", a, b, res);
                computer.set_register(reg, res.into())?;
            }
            Instruction::Shr(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;

                let b: u32 = b.try_into().map_err(|_| Exception::InvalidInstruction)?;
                let res = a.checked_shr(b).ok_or(Exception::InvalidInstruction)?;

                debug!("{} >> {} = {}", a, b, res);
                computer.set_register(reg, res.into())?;
            }
            Instruction::St(reg, address) => {
                let val = computer.registers.get(reg);
                computer.write(address, val)?;
            }
            Instruction::Sub(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;
                let (res, overflow) = b.overflowing_sub(a);
                computer.set_register(reg, res.into())?;

                debug!("{} - {} = {}", b, a, res);

                computer
                    .registers
                    .sr
                    .set(StatusRegister::OVERFLOW, overflow);
            }
            Instruction::Trap => {
                return Err(Exception::Trap.into());
            }
            Instruction::Xor(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;
                let res = a ^ b;
                debug!("{} ^ {} = {}", a, b, res);
                computer.set_register(reg, res.into())?;
            }
        };

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inst_encode_test() {
        Instruction::Xor(Arg::Address(Address::Dir(42)), Reg::B).assert_stable();
        Instruction::Shr(Arg::Address(Address::Idx(Reg::SR, 0x7FFF)), Reg::PC).assert_stable();
    }

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
        let start = 0x100;
        let subroutine = 0x200;
        let stack = 0xF000;
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
