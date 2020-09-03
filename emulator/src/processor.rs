#![allow(dead_code)]

use crate::parser::Parsable;
use bitflags::bitflags;
use std::io::{Cursor, Read, Write};
use thiserror::Error;
use z33_instruction_derive::Instruction;

#[derive(Error, Debug)]
pub enum Exception {
    #[error("hardware interrupt")]
    HardwareInterrupt,

    #[error("division by zero")]
    DivByZero,

    #[error("invalid instruction")]
    InvalidInstruction,

    #[error("instruction denied")]
    Denied,

    #[error("trap")]
    Trap,

    #[error("invalid memory access")]
    Segfault,

    #[error("computer reset")]
    Reset,
}

type Result<T> = std::result::Result<T, Exception>;

type Word = u16;
type Memory = [u8; u16::MAX as usize];

bitflags! {
    #[derive(Default)]
    struct StatusRegister: Word {
        const CARRY            = 0b00000000_00000001;
        const ZERO             = 0b00000000_00000010;
        const NEGATIVE         = 0b00000000_00000100;
        const OVERFLOW         = 0b00000000_00001000;
        const INTERRUPT_ENABLE = 0b00000001_00000000;
        const SUPERVISOR       = 0b00000010_00000000;
    }
}

pub trait Encodable {
    fn encode(self) -> Vec<u8>;
}

pub trait Decodable: Sized {
    fn decode<T: Read>(reader: &mut T) -> Option<Self>;
}

pub trait Labelable: Sized {
    fn resolve_label(self, address: u16) -> Option<Self>;
    fn label() -> Self;
}

trait Mmapped: Encodable + Decodable + Sized + PartialEq + Clone + std::fmt::Debug {
    #[cfg(test)]
    fn assert_stable(self) {
        let binval = self.clone().encode();

        let mut cursor = Cursor::new(binval.clone());
        let decoded = Self::decode(&mut cursor);
        assert_eq!(Some(self), decoded);

        let binval2 = decoded.unwrap().encode();
        assert_eq!(binval, binval2);
    }
}

impl<T> Mmapped for T where T: Encodable + Decodable + Sized + PartialEq + Clone + std::fmt::Debug {}

impl Encodable for u16 {
    fn encode(self) -> Vec<u8> {
        u16::to_le_bytes(self).into()
    }
}

impl Decodable for u16 {
    fn decode<T: Read>(reader: &mut T) -> Option<Self> {
        let mut buf = [0u8; 2];
        reader.read_exact(&mut buf).ok()?;
        Some(u16::from_le_bytes(buf))
    }
}

impl Encodable for i16 {
    fn encode(self) -> Vec<u8> {
        i16::to_le_bytes(self).into()
    }
}

impl Decodable for i16 {
    fn decode<T: Read>(reader: &mut T) -> Option<Self> {
        let mut buf = [0u8; 2];
        reader.read_exact(&mut buf).ok()?;
        Some(i16::from_le_bytes(buf))
    }
}

impl Encodable for u32 {
    fn encode(self) -> Vec<u8> {
        u32::to_le_bytes(self).into()
    }
}

impl Decodable for u32 {
    fn decode<T: Read>(reader: &mut T) -> Option<Self> {
        let mut buf = [0u8; 4];
        reader.read_exact(&mut buf).ok()?;
        Some(u32::from_le_bytes(buf))
    }
}

impl<T> Encodable for Vec<T>
where
    T: Encodable,
{
    fn encode(self) -> Vec<u8> {
        self.into_iter().flat_map(|v| v.encode()).collect()
    }
}

pub struct Computer {
    pub registers: Registers,
    pub memory: Memory,
}

impl Default for Computer {
    fn default() -> Self {
        Self {
            registers: Registers::default(),
            memory: [0; u16::MAX as usize],
        }
    }
}

impl Computer {
    fn resolve_address(&self, address: Address) -> u16 {
        match address {
            Address::Dir(addr) => addr,
            Address::Ind(reg) => self.registers.get(reg),

            // TODO: this seems wrong
            Address::Idx(reg, off) => (self.registers.get(reg) as i32 + off as i32) as u16,
        }
    }

    fn offset_read<T: Decodable>(&self, address: Address) -> Result<(u16, T)> {
        let address = self.resolve_address(address) as u64;
        let mut cursor = Cursor::new(self.memory.as_ref());
        cursor.set_position(address);

        let el = T::decode(&mut cursor).ok_or(Exception::Segfault)?; // TODO: better error
        let offset = (cursor.position() - address) as u16;
        Ok((offset, el))
    }

    fn read<T: Decodable>(&self, address: Address) -> Result<T> {
        let address = self.resolve_address(address) as u64;
        let mut cursor = Cursor::new(self.memory.as_ref());
        cursor.set_position(address);

        T::decode(&mut cursor).ok_or(Exception::Segfault) // TODO: better error
    }

    pub fn write<T: Encodable>(&mut self, address: Address, value: T) -> Result<u16> {
        let address = self.resolve_address(address) as u64;
        let mut cursor = Cursor::new(self.memory.as_mut());
        cursor.set_position(address);

        let bytes = value.encode();
        cursor.write_all(&bytes).map_err(|_| Exception::Segfault)?;
        let offset = (cursor.position() - address) as u16;
        Ok(offset)
    }

    pub fn set_register(&mut self, reg: Reg, val: u16) {
        let reg = self.registers.get_mut(reg);
        *reg = val;
    }

    fn arg(&self, arg: Arg) -> Result<u16> {
        match arg {
            Arg::Address(addr) => self.read(addr),
            Arg::Value(val) => Ok(self.value(val)),
        }
    }

    fn value(&self, value: Value) -> u16 {
        match value {
            Value::Imm(imm) => imm,
            Value::Reg(reg) => self.registers.get(reg),
        }
    }

    fn jump(&mut self, address: Address) -> Result<()> {
        let address = self.resolve_address(address);
        self.registers.pc = address;
        Ok(())
    }

    fn decode_instruction(&mut self) -> Result<Instruction> {
        let (addr, inst) = self.offset_read(Address::Ind(Reg::PC))?;
        self.registers.pc += addr;
        Ok(inst)
    }

    fn step(&mut self) -> Result<()> {
        let inst = self.decode_instruction()?;
        inst.execute(self)?;
        Ok(())
    }

    pub fn run(&mut self) -> Result<()> {
        loop {
            match self.step() {
                Ok(_) => {}
                Err(Exception::Reset) => return Ok(()),
                Err(v) => return Err(v),
            }
        }
    }

    fn push<T: Encodable>(&mut self, value: T) -> Result<()> {
        // First encode the value and move the stack
        let bytes = value.encode();
        self.registers.sp -= bytes.len() as u16;

        // And write it on memeory
        let address = self.registers.sp;
        let mut cursor = Cursor::new(self.memory.as_mut());
        cursor.set_position(address as u64);
        cursor.write_all(&bytes).map_err(|_| Exception::Segfault)
    }

    fn pop<T: Decodable>(&mut self) -> Result<T> {
        // First read the value
        let (offset, val) = self.offset_read(Address::Ind(Reg::SP))?;
        // Then move the SP
        self.registers.sp += offset;
        Ok(val)
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Registers {
    a: Word,
    b: Word,
    pc: Word,
    sp: Word,
    sr: StatusRegister,
}

impl Registers {
    pub fn get(&self, reg: Reg) -> Word {
        match reg {
            Reg::A => self.a,
            Reg::B => self.b,
            Reg::PC => self.pc,
            Reg::SP => self.sp,
            Reg::SR => self.sr.bits(),
        }
    }

    fn get_mut(&mut self, reg: Reg) -> &mut Word {
        match reg {
            Reg::A => &mut self.a,
            Reg::B => &mut self.b,
            Reg::PC => &mut self.pc,
            Reg::SP => &mut self.sp,
            Reg::SR => &mut self.sr.bits,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Reg {
    A,
    B,
    PC,
    SP,
    SR,
}

impl Reg {
    fn from_byte(val: u8) -> Option<Self> {
        match val & 0x7 {
            0x0 => Some(Self::A),
            0x1 => Some(Self::B),
            0x2 => Some(Self::PC),
            0x3 => Some(Self::SP),
            0x4 => Some(Self::SR),
            _ => None,
        }
    }

    fn to_byte(self) -> u8 {
        match self {
            Self::A => 0x0,
            Self::B => 0x1,
            Self::PC => 0x2,
            Self::SP => 0x3,
            Self::SR => 0x4,
        }
    }
}

impl Encodable for Reg {
    fn encode(self) -> Vec<u8> {
        vec![self.to_byte()]
    }
}

impl Decodable for Reg {
    fn decode<T: Read>(reader: &mut T) -> Option<Self> {
        let mut buf = [0];
        reader.read_exact(&mut buf).ok()?;
        Self::from_byte(buf[0])
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Address {
    Dir(u16),
    Ind(Reg),
    Idx(Reg, i16),
}

impl From<u16> for Address {
    fn from(val: u16) -> Self {
        Self::Dir(val)
    }
}

impl Encodable for Address {
    fn encode(self) -> Vec<u8> {
        Arg::Address(self).encode()
    }
}

impl Decodable for Address {
    fn decode<T: Read>(reader: &mut T) -> Option<Self> {
        if let Some(Arg::Address(addr)) = Arg::decode(reader) {
            Some(addr)
        } else {
            None
        }
    }
}

impl Labelable for Address {
    fn resolve_label(self, address: u16) -> Option<Self> {
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
    Imm(u16),
    Reg(Reg),
}

impl Labelable for Value {
    fn resolve_label(self, address: u16) -> Option<Self> {
        match self {
            Self::Imm(_) => Some(Self::Imm(address)),
            Self::Reg(_) => None,
        }
    }

    fn label() -> Self {
        Self::Imm(0)
    }
}

impl Encodable for Value {
    fn encode(self) -> Vec<u8> {
        Arg::Value(self).encode()
    }
}

impl Decodable for Value {
    fn decode<T: Read>(reader: &mut T) -> Option<Self> {
        if let Some(Arg::Value(val)) = Arg::decode(reader) {
            Some(val)
        } else {
            None
        }
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

impl Labelable for Arg {
    fn resolve_label(self, address: u16) -> Option<Self> {
        match self {
            Self::Address(a) => a.resolve_label(address).map(Self::Address),
            Self::Value(v) => v.resolve_label(address).map(Self::Value),
        }
    }

    fn label() -> Self {
        Self::Value(Value::label())
    }
}

impl Encodable for Arg {
    fn encode(self) -> Vec<u8> {
        match self {
            Arg::Address(Address::Dir(dir)) => {
                let v = 0x00 << 4;
                let mut f = vec![v];
                f.extend(dir.encode());
                f
            }
            Arg::Address(Address::Ind(reg)) => {
                let v = (0x01 << 4) | (reg.to_byte() & 0x0F);
                vec![v]
            }
            Arg::Address(Address::Idx(reg, off)) => {
                let v = (0x02 << 4) | (reg.to_byte() & 0x0F);
                let mut f = vec![v];
                f.extend(off.encode());
                f
            }
            Arg::Value(Value::Imm(imm)) => {
                let v = 0x03 << 4;
                let mut f = vec![v];
                f.extend(imm.encode());
                f
            }
            Arg::Value(Value::Reg(reg)) => {
                let v = (0x04 << 4) | (reg.to_byte() & 0x0F);
                vec![v]
            }
        }
    }
}

impl Decodable for Arg {
    fn decode<T: Read>(reader: &mut T) -> Option<Self> {
        let mut buf = [0];
        reader.read_exact(&mut buf).ok()?;
        let inst = buf[0] >> 4 & 0xF;
        let rest = buf[0] & 0xF;
        match inst {
            0x0 => {
                let val = u16::decode(reader)?;
                Some(Arg::Address(Address::Dir(val)))
            }
            0x1 => {
                let reg = Reg::from_byte(rest)?;
                Some(Arg::Address(Address::Ind(reg)))
            }
            0x2 => {
                let reg = Reg::from_byte(rest)?;
                let val = i16::decode(reader)?;
                Some(Arg::Address(Address::Idx(reg, val)))
            }
            0x3 => {
                let val = u16::decode(reader)?;
                Some(Arg::Value(Value::Imm(val)))
            }
            0x4 => {
                let reg = Reg::from_byte(rest)?;
                Some(Arg::Value(Value::Reg(reg)))
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ArgSwap {
    Reg(Reg),
    Address(Address),
}

#[derive(Debug, Clone, PartialEq)]
enum Cond {
    Eq,
    Ne,
    Le,
    Lt,
    Ge,
    Gt,
}

#[derive(Debug, Clone, PartialEq, Instruction)]
pub enum Instruction {
    #[instruction(0x00)]
    Add(#[labelable] Arg, Reg),

    #[instruction(0x01)]
    And(#[labelable] Arg, Reg),

    #[instruction(0x02)]
    Call(#[labelable] Arg),

    #[instruction(0x03)]
    Cmp(#[labelable] Arg, Reg),

    #[instruction(0x04)]
    Div(#[labelable] Arg, Reg),

    #[instruction(0x05)]
    Fas(Address, Reg),

    #[instruction(0x06)]
    In(Address, Reg),

    #[instruction(0x07)]
    Jmp(#[labelable] Arg),

    #[instruction(0x08)]
    Jeq(#[labelable] Arg),

    #[instruction(0x09)]
    Jne(#[labelable] Arg),

    #[instruction(0x0A)]
    Jle(#[labelable] Arg),

    #[instruction(0x0B)]
    Jlt(#[labelable] Arg),

    #[instruction(0x0C)]
    Jge(#[labelable] Arg),

    #[instruction(0x0D)]
    Jgt(#[labelable] Arg),

    #[instruction(0x0E)]
    Ld(#[labelable] Arg, Reg),

    #[instruction(0x0F)]
    Mul(#[labelable] Arg, Reg),

    #[instruction(0x10)]
    Neg(Reg),

    #[instruction(0x11)]
    Nop,

    #[instruction(0x12)]
    Not(Reg),

    #[instruction(0x13)]
    Or(#[labelable] Arg, Reg),

    #[instruction(0x14)]
    Out(Value, Address),

    #[instruction(0x15)]
    Pop(Reg),

    #[instruction(0x16)]
    Push(Value),

    #[instruction(0x17)]
    Reset,

    #[instruction(0x18)]
    Rti,

    #[instruction(0x19)]
    Rtn,

    #[instruction(0x1A)]
    Shl(#[labelable] Arg, Reg),

    #[instruction(0x1B)]
    Shr(#[labelable] Arg, Reg),

    #[instruction(0x1C)]
    St(Reg, Address),

    #[instruction(0x1D)]
    Sub(#[labelable] Arg, Reg),

    // #[instruction(0x1E)]
    // Swap(ArgSwap, Reg),
    #[instruction(0x1F)]
    Trap,

    #[instruction(0x20)]
    Xor(#[labelable] Arg, Reg),
}

impl Instruction {
    fn execute(self, computer: &mut Computer) -> Result<()> {
        match self {
            Instruction::Add(arg, reg) => {
                let val = computer.arg(arg)?;
                let reg = computer.registers.get_mut(reg);
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
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arg_encode_test() {
        Arg::Address(Address::Dir(42)).assert_stable();
        Arg::Address(Address::Dir(0xFFFF)).assert_stable();
        Arg::Address(Address::Ind(Reg::A)).assert_stable();
        Arg::Address(Address::Ind(Reg::B)).assert_stable();
        Arg::Address(Address::Idx(Reg::A, -24)).assert_stable();
        Arg::Address(Address::Idx(Reg::B, 0x7FFF)).assert_stable(); // Max i16
        Arg::Address(Address::Idx(Reg::PC, -0x8000)).assert_stable(); // Min i16
        Arg::Value(Value::Imm(42)).assert_stable();
        Arg::Value(Value::Imm(0xFFFF)).assert_stable();
        Arg::Value(Value::Reg(Reg::SP)).assert_stable();
        Arg::Value(Value::Reg(Reg::SR)).assert_stable();
    }

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
        assert_eq!(computer.registers.get(Reg::A), 5);

        // Write some memory (with indirect access)
        computer.write(Address::Dir(0x42), 100 as Word).unwrap();
        *computer.registers.get_mut(Reg::B) = 0x32;
        let instruction = Instruction::Add(Arg::Address(Address::Idx(Reg::B, 0x10)), Reg::A);
        instruction.execute(&mut computer).unwrap();
        assert_eq!(computer.registers.get(Reg::A), 105);
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

        computer.write(start.into(), program).unwrap();
        computer.jump(start.into()).unwrap();

        assert_eq!(computer.registers.a, 0x00);
        assert_eq!(computer.registers.b, 0x00);
        assert_eq!(computer.registers.pc, start);
        computer.step().unwrap();

        assert_eq!(computer.registers.a, 0x42);
        assert_eq!(computer.registers.b, 0x00);
        assert_eq!(computer.registers.pc, start + 5);
        computer.step().unwrap();

        assert_eq!(computer.registers.a, 0x42);
        assert_eq!(computer.registers.b, 0x24);
        assert_eq!(computer.registers.pc, start + 10);
        computer.step().unwrap();

        assert_eq!(computer.registers.a, 0x42);
        assert_eq!(computer.registers.b, 0x66);
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

        computer
            .write(
                start.into(),
                vec![
                    Instruction::Call(Arg::Value(Value::Imm(subroutine))),
                    Instruction::Add(Arg::Value(Value::Reg(Reg::A)), Reg::B),
                ],
            )
            .unwrap();

        computer
            .write(
                subroutine.into(),
                vec![
                    Instruction::Ld(Arg::Value(Value::Imm(0x42)), Reg::A),
                    Instruction::Ld(Arg::Value(Value::Imm(0x24)), Reg::B),
                    Instruction::Rtn,
                ],
            )
            .unwrap();

        computer.jump(start.into()).unwrap();

        assert_eq!(computer.registers.a, 0x00);
        assert_eq!(computer.registers.b, 0x00);
        assert_eq!(computer.registers.pc, start);
        assert_eq!(computer.registers.sp, stack);
        // call subroutine
        computer.step().unwrap();

        assert_eq!(computer.registers.a, 0x00);
        assert_eq!(computer.registers.b, 0x00);
        assert_eq!(computer.registers.pc, subroutine);
        assert_eq!(computer.registers.sp, stack - 2);
        // ld 0x42, %a
        computer.step().unwrap();

        assert_eq!(computer.registers.a, 0x42);
        assert_eq!(computer.registers.b, 0x00);
        assert_eq!(computer.registers.pc, subroutine + 5);
        assert_eq!(computer.registers.sp, stack - 2);
        // ld 0x24, %b
        computer.step().unwrap();

        assert_eq!(computer.registers.a, 0x42);
        assert_eq!(computer.registers.b, 0x24);
        assert_eq!(computer.registers.pc, subroutine + 10);
        assert_eq!(computer.registers.sp, stack - 2);
        // rtn
        computer.step().unwrap();

        assert_eq!(computer.registers.a, 0x42);
        assert_eq!(computer.registers.b, 0x24);
        assert_eq!(computer.registers.pc, start + 4);
        assert_eq!(computer.registers.sp, stack);
        // add %a, %b
        computer.step().unwrap();

        assert_eq!(computer.registers.a, 0x42);
        assert_eq!(computer.registers.b, 0x66);
        assert_eq!(computer.registers.pc, start + 7);
        assert_eq!(computer.registers.sp, stack);
    }

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
}
