use bitflags::bitflags;
use std::convert::TryFrom;
use std::fmt::Debug;
use thiserror::Error;
use tracing::{debug, info};
use z33_instruction_derive::Instruction;

use super::memory::{Cell, CellError, Memory, MemoryError, TryFromCell, UnsignedWord};
use crate::parser::Parsable;

#[derive(Error, Debug)]
pub enum Exception {
    #[error("division by zero")]
    DivByZero,

    #[error("invalid instruction")]
    InvalidInstruction,

    #[error("invalid memory access")]
    Segfault,

    #[error("memory error: {0}")]
    MemoryError(#[from] MemoryError),

    #[error("cell error: {0}")]
    CellError(#[from] CellError),

    #[error("invalid value for registry {reg}: {inner}")]
    InvalidRegistry { reg: Reg, inner: CellError },

    #[error("invalid address {address}")]
    InvalidAddress { address: i64 },

    #[error("computer reset")]
    Reset,
}

type Result<T> = std::result::Result<T, Exception>;

bitflags! {
    #[derive(Default)]
    pub struct StatusRegister: UnsignedWord {
        const CARRY            = 0b00000000_00000001;
        const ZERO             = 0b00000000_00000010;
        const NEGATIVE         = 0b00000000_00000100;
        const OVERFLOW         = 0b00000000_00001000;
        const INTERRUPT_ENABLE = 0b00000001_00000000;
        const SUPERVISOR       = 0b00000010_00000000;
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
    fn resolve_address(&self, address: Address) -> Result<u64> {
        match address {
            Address::Dir(addr) => Ok(addr),
            Address::Ind(reg) => u64::try_from_cell(&self.registers.get(reg))
                .map_err(|e| Exception::InvalidRegistry { reg, inner: e }),

            Address::Idx(reg, off) => u64::try_from_cell(&self.registers.get(reg))
                .map_err(|e| Exception::InvalidRegistry { reg, inner: e })
                .and_then(|address| {
                    let address = address as i64 + off;
                    u64::try_from(address).map_err(|_| Exception::InvalidAddress { address })
                }),
        }
    }

    #[tracing::instrument(skip(self), err)]
    fn read<T: TryFromCell>(&self, address: Address) -> Result<T> {
        let address = self.resolve_address(address)?;
        let cell = self.memory.get(address)?;
        T::try_from_cell(cell).map_err(|e| e.to_invalid_cell(address).into())
    }

    #[tracing::instrument(skip(self), err)]
    pub fn write<T: Into<Cell> + Debug>(&mut self, address: Address, value: T) -> Result<()> {
        let address = self.resolve_address(address)?;
        let cell = self.memory.get_mut(address)?;
        *cell = value.into();
        Ok(())
    }

    #[tracing::instrument(skip(self))]
    pub fn set_register(&mut self, reg: Reg, val: u64) -> Result<()> {
        self.registers.set(reg, Cell::UnsignedWord(val))
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

    #[tracing::instrument(skip(self), err)]
    fn jump(&mut self, address: Address) -> Result<()> {
        let address = self.resolve_address(address)?;
        debug!("Jumping to address {:#x}", address);
        self.registers.set(Reg::PC, address.into())?;
        Ok(())
    }

    #[tracing::instrument(skip(self), err)]
    fn decode_instruction(&mut self) -> Result<&Instruction> {
        let address = self.resolve_address(Address::Ind(Reg::PC))?;
        let cell = self.memory.get(address)?;
        self.registers.pc += 1;
        cell.extract_instruction()
            .map_err(|_| Exception::InvalidInstruction)
    }

    #[tracing::instrument(skip(self))]
    fn step(&mut self) -> Result<()> {
        let inst = self.decode_instruction()?;
        info!("Executing instruction \"{}\"", inst);
        // TODO: this could be an unnecessary clone
        inst.clone().execute(self)?;
        debug!("Register state {:?}", self.registers);
        Ok(())
    }

    #[tracing::instrument(skip(self))]
    pub fn run(&mut self) -> Result<()> {
        loop {
            match self.step() {
                Ok(_) => {}
                Err(Exception::Reset) => return Ok(()),
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
    fn pop<T: TryFromCell + Debug>(&mut self) -> Result<T> {
        // First read the value
        let val = self.read(Address::Ind(Reg::SP))?;
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
    pub pc: UnsignedWord,
    pub sp: UnsignedWord,
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

    pub fn set(&mut self, reg: Reg, value: Cell) -> Result<()> {
        match reg {
            Reg::A => self.a = value,
            Reg::B => self.b = value,
            Reg::PC => self.pc = UnsignedWord::try_from_cell(&value)?,
            Reg::SP => self.sp = UnsignedWord::try_from_cell(&value)?,
            Reg::SR => self.sr.bits = UnsignedWord::try_from_cell(&value)?,
        };
        Ok(())
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
            Self::B => write!(f, "%a"),
            Self::PC => write!(f, "%pc"),
            Self::SP => write!(f, "%sp"),
            Self::SR => write!(f, "%sr"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Address {
    Dir(u64),
    Ind(Reg),
    Idx(Reg, i64),
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

#[derive(Debug, Clone, PartialEq)]
enum ArgSwap {
    Reg(Reg),
    Address(Address),
}

#[derive(Debug, Clone, PartialEq, Instruction)]
pub enum Instruction {
    #[instruction(0x01)]
    Add(#[labelable] Arg, Reg),

    #[instruction(0x02)]
    And(#[labelable] Arg, Reg),

    #[instruction(0x03)]
    Call(#[labelable] Arg),

    #[instruction(0x04)]
    Cmp(#[labelable] Arg, Reg),

    #[instruction(0x05)]
    Div(#[labelable] Arg, Reg),

    #[instruction(0x06)]
    Fas(Address, Reg),

    #[instruction(0x07)]
    In(Address, Reg),

    #[instruction(0x08)]
    Jmp(#[labelable] Arg),

    #[instruction(0x09)]
    Jeq(#[labelable] Arg),

    #[instruction(0x0A)]
    Jne(#[labelable] Arg),

    #[instruction(0x0B)]
    Jle(#[labelable] Arg),

    #[instruction(0x0C)]
    Jlt(#[labelable] Arg),

    #[instruction(0x0D)]
    Jge(#[labelable] Arg),

    #[instruction(0x0E)]
    Jgt(#[labelable] Arg),

    #[instruction(0x0F)]
    Ld(#[labelable] Arg, Reg),

    #[instruction(0x10)]
    Mul(#[labelable] Arg, Reg),

    #[instruction(0x11)]
    Neg(Reg),

    #[instruction(0x12)]
    Nop,

    #[instruction(0x13)]
    Not(Reg),

    #[instruction(0x14)]
    Or(#[labelable] Arg, Reg),

    #[instruction(0x15)]
    Out(Value, Address),

    #[instruction(0x16)]
    Pop(Reg),

    #[instruction(0x17)]
    Push(Value),

    #[instruction(0x18)]
    Reset,

    #[instruction(0x19)]
    Rti,

    #[instruction(0x1A)]
    Rtn,

    #[instruction(0x1B)]
    Shl(#[labelable] Arg, Reg),

    #[instruction(0x1C)]
    Shr(#[labelable] Arg, Reg),

    #[instruction(0x1D)]
    St(Reg, Address),

    #[instruction(0x1E)]
    Sub(#[labelable] Arg, Reg),

    // #[instruction(0x1F)]
    // Swap(ArgSwap, Reg),
    #[instruction(0x20)]
    Trap,

    #[instruction(0x21)]
    Xor(#[labelable] Arg, Reg),
}

impl Instruction {
    #[tracing::instrument]
    fn execute(self, computer: &mut Computer) -> Result<()> {
        todo!()
        /*
        match self {
            Instruction::Add(arg, reg) => {
                let val = UnsignedWord::try_from_cell(&computer.arg(arg)?)
                    .map_err(|inner| Exception::InvalidRegistry { reg, inner })?;
                let reg_val = UnsignedWord::try_from_cell(&computer.registers.get(reg))
                    .map_err(|inner| Exception::InvalidRegistry { reg, inner })?;
                let reg_val = computer.registers.get(reg);
                let (res, overflow) = reg.overflowing_add(val);
                *reg = res;

                computer
                    .registers
                    .sr
                    .set(StatusRegister::OVERFLOW, overflow);
            }
            Instruction::And(arg, reg) => {
                let val = computer.arg(arg)?;
                *computer.registers.get_mut(reg) &= val;
            }
            Instruction::Call(arg) => {
                // Push PC
                let pc = computer.registers.get(Reg::PC);
                computer.push(pc)?;

                // Jump
                let addr = computer.arg(arg)?;
                computer.jump(Address::Dir(addr))?;
            }
            Instruction::Cmp(arg, reg) => {
                let val1 = computer.arg(arg)?;
                let val2 = computer.registers.get(reg);

                computer
                    .registers
                    .sr
                    .set(StatusRegister::ZERO, val1 == val2);

                computer
                    .registers
                    .sr
                    .set(StatusRegister::NEGATIVE, val1 < val2);
            }
            Instruction::Div(arg, reg) => {
                let val = computer.arg(arg)?;
                let reg = computer.registers.get_mut(reg);
                *reg = reg.checked_div(val).ok_or(Exception::DivByZero)?;
            }
            Instruction::Fas(addr, reg) => {
                let val = computer.read(addr.clone())?;
                *computer.registers.get_mut(reg) = val;
                computer.write(addr, 1 as Word)?;
            }
            Instruction::In(_, _) => todo!(),
            Instruction::Jmp(arg) => {
                let val = computer.arg(arg)?;
                computer.registers.pc = val;
            }
            Instruction::Jeq(arg) => {
                if computer.registers.sr.contains(StatusRegister::ZERO) {
                    let val = computer.arg(arg)?;
                    computer.registers.pc = val;
                }
            }
            Instruction::Jne(arg) => {
                if !computer.registers.sr.contains(StatusRegister::ZERO) {
                    let val = computer.arg(arg)?;
                    computer.registers.pc = val;
                }
            }
            Instruction::Jle(arg) => {
                if computer.registers.sr.contains(StatusRegister::ZERO)
                    || computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = computer.arg(arg)?;
                    computer.registers.pc = val;
                }
            }
            Instruction::Jlt(arg) => {
                if !computer.registers.sr.contains(StatusRegister::ZERO)
                    && computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = computer.arg(arg)?;
                    computer.registers.pc = val;
                }
            }
            Instruction::Jge(arg) => {
                if computer.registers.sr.contains(StatusRegister::ZERO)
                    || !computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = computer.arg(arg)?;
                    computer.registers.pc = val;
                }
            }
            Instruction::Jgt(arg) => {
                if !computer.registers.sr.contains(StatusRegister::ZERO)
                    && !computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = computer.arg(arg)?;
                    computer.registers.pc = val;
                }
            }
            Instruction::Ld(arg, reg) => {
                let val = computer.arg(arg)?;
                let reg = computer.registers.get_mut(reg);
                *reg = val;
            }
            Instruction::Mul(arg, reg) => {
                let val = computer.arg(arg)?;
                let reg = computer.registers.get_mut(reg);
                let (res, overflow) = reg.overflowing_mul(val);
                *reg = res;

                computer
                    .registers
                    .sr
                    .set(StatusRegister::OVERFLOW, overflow);
            }
            Instruction::Neg(_reg) => todo!(),
            Instruction::Nop => {}
            Instruction::Not(reg) => {
                let reg = computer.registers.get_mut(reg);
                *reg = !*reg;
            }
            Instruction::Or(arg, reg) => {
                let val = computer.arg(arg)?;
                *computer.registers.get_mut(reg) |= val;
            }
            Instruction::Out(_, _) => todo!(),
            Instruction::Pop(reg) => {
                let val = computer.pop()?;
                *computer.registers.get_mut(reg) = val;
            }
            Instruction::Push(val) => {
                let val = computer.value(val);
                computer.push(val)?;
            }
            Instruction::Reset => return Err(Exception::Reset),
            Instruction::Rti => todo!(),
            Instruction::Rtn => {
                let ret = computer.pop()?; // Pop the return address
                computer.jump(Address::Dir(ret))?; // and jump to it
            }
            Instruction::Shl(arg, reg) => {
                let val = computer.arg(arg)?;
                let reg = computer.registers.get_mut(reg);
                *reg = reg
                    .checked_shl(val as u32)
                    .ok_or(Exception::InvalidInstruction)?;
            }
            Instruction::Shr(arg, reg) => {
                let val = computer.arg(arg)?;
                let reg = computer.registers.get_mut(reg);
                *reg = reg
                    .checked_shr(val as u32)
                    .ok_or(Exception::InvalidInstruction)?;
            }
            Instruction::St(reg, address) => {
                let val = computer.registers.get(reg);
                computer.write(address, val)?;
            }
            Instruction::Sub(arg, reg) => {
                let val = computer.arg(arg)?;
                let reg = computer.registers.get_mut(reg);
                let (res, overflow) = reg.overflowing_sub(val);
                *reg = res;

                computer
                    .registers
                    .sr
                    .set(StatusRegister::OVERFLOW, overflow);
            }
            Instruction::Trap => todo!(),
            Instruction::Xor(arg, reg) => {
                let val = computer.arg(arg)?;
                let reg = computer.registers.get_mut(reg);
                *reg ^= val;
            }
        };

        Ok(())
        */
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
        assert_eq!(computer.registers.get(Reg::A), Cell::UnsignedWord(5));

        // Write some memory (with indirect access)
        computer.write(Address::Dir(0x42), 100 as u64).unwrap();
        computer
            .registers
            .set(Reg::B, Cell::UnsignedWord(0x32))
            .unwrap();
        let instruction = Instruction::Add(Arg::Address(Address::Idx(Reg::B, 0x10)), Reg::A);
        instruction.execute(&mut computer).unwrap();
        assert_eq!(computer.registers.get(Reg::A), Cell::UnsignedWord(105));
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

        assert_eq!(computer.registers.a, Cell::UnsignedWord(0x42));
        assert_eq!(computer.registers.b, Cell::Empty);
        assert_eq!(computer.registers.pc, start + 5);
        computer.step().unwrap();

        assert_eq!(computer.registers.a, Cell::UnsignedWord(0x42));
        assert_eq!(computer.registers.b, Cell::UnsignedWord(0x24));
        assert_eq!(computer.registers.pc, start + 10);
        computer.step().unwrap();

        assert_eq!(computer.registers.a, Cell::UnsignedWord(0x42));
        assert_eq!(computer.registers.b, Cell::UnsignedWord(0x66));
        assert_eq!(computer.registers.pc, start + 13);
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
            Instruction::Ld(Arg::Value(Value::Imm(0x42)), Reg::A),
            Instruction::Ld(Arg::Value(Value::Imm(0x24)), Reg::B),
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

        assert_eq!(computer.registers.a, Cell::UnsignedWord(0x42));
        assert_eq!(computer.registers.b, Cell::Empty);
        assert_eq!(computer.registers.pc, subroutine + 1);
        assert_eq!(computer.registers.sp, stack - 1);
        // ld 0x24, %b
        computer.step().unwrap();

        assert_eq!(computer.registers.a, Cell::UnsignedWord(0x42));
        assert_eq!(computer.registers.b, Cell::UnsignedWord(0x24));
        assert_eq!(computer.registers.pc, subroutine + 2);
        assert_eq!(computer.registers.sp, stack - 2);
        // rtn
        computer.step().unwrap();

        assert_eq!(computer.registers.a, Cell::UnsignedWord(0x42));
        assert_eq!(computer.registers.b, Cell::UnsignedWord(0x24));
        assert_eq!(computer.registers.pc, start + 1);
        assert_eq!(computer.registers.sp, stack);
        // add %a, %b
        computer.step().unwrap();

        assert_eq!(computer.registers.a, Cell::UnsignedWord(0x42));
        assert_eq!(computer.registers.b, Cell::UnsignedWord(0x66));
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
