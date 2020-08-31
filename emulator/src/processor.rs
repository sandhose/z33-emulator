#![allow(dead_code)]

use bitflags::bitflags;
use thiserror::Error;
use z33_instruction_derive::Instruction;

#[derive(Error, Debug)]
enum Exception {
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
}

type Result<T> = std::result::Result<T, Exception>;

type Register = u16;

bitflags! {
    #[derive(Default)]
    struct StatusRegister: Register {
        const CARRY            = 0b00000000_00000001;
        const ZERO             = 0b00000000_00000010;
        const NEGATIVE         = 0b00000000_00000100;
        const OVERFLOW         = 0b00000000_00001000;
        const INTERRUPT_ENABLE = 0b00000001_00000000;
        const SUPERVISOR       = 0b00000010_00000000;
    }
}

trait Mmaped: Sized + PartialEq + Clone + std::fmt::Debug {
    fn encode(self) -> u32;
    fn decode(val: u32) -> Option<Self>;
    fn bitsize() -> u8;

    #[cfg(test)]
    fn assert_stable(self) {
        let binval = self.clone().encode();
        if Self::bitsize() < 32 {
            assert_eq!(binval >> Self::bitsize(), 0);
        }

        let decoded = Self::decode(binval);
        assert_eq!(Some(self), decoded);

        let binval2 = decoded.unwrap().encode();
        assert_eq!(binval, binval2);
    }
}

impl Mmaped for u32 {
    fn encode(self) -> u32 {
        self
    }
    fn decode(val: u32) -> Option<Self> {
        Some(val)
    }
    fn bitsize() -> u8 {
        32
    }
}

struct Computer {
    registers: Registers,
    memory: [u8; u16::MAX as usize],
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

    fn read_memory(&self, address: Address) -> Result<u16> {
        let address = self.resolve_address(address) as usize;

        let sl: [u8; 2] = [
            *self.memory.get(address).ok_or(Exception::Segfault)?,
            *self.memory.get(address + 1).ok_or(Exception::Segfault)?,
        ];
        Ok(u16::from_le_bytes(sl))
    }

    fn write_memory(&mut self, address: Address, value: u16) -> Result<()> {
        let [b1, b2] = value.to_le_bytes();
        let address = self.resolve_address(address) as usize;
        let a1 = self
            .memory
            .get_mut(address + 0)
            .ok_or(Exception::Segfault)?;
        *a1 = b1;

        let a2 = self
            .memory
            .get_mut(address + 1)
            .ok_or(Exception::Segfault)?;
        *a2 = b2;
        Ok(())
    }

    fn arg(&self, arg: Arg) -> Result<u16> {
        match arg {
            Arg::Address(addr) => self.read_memory(addr),
            Arg::Value(Value::Imm(val)) => Ok(val),
            Arg::Value(Value::Reg(reg)) => Ok(self.registers.get(reg)),
        }
    }

    fn push(&mut self, _value: u8) -> Result<()> {
        todo!()
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
struct Registers {
    a: Register,
    b: Register,
    pc: Register,
    sp: Register,
    sr: StatusRegister,
}

impl Registers {
    fn get(&self, register: Reg) -> Register {
        match register {
            Reg::A => self.a,
            Reg::B => self.b,
            Reg::PC => self.pc,
            Reg::SP => self.sp,
            Reg::SR => self.sr.bits(),
        }
    }

    fn get_mut(&mut self, register: Reg) -> &mut Register {
        match register {
            Reg::A => &mut self.a,
            Reg::B => &mut self.b,
            Reg::PC => &mut self.pc,
            Reg::SP => &mut self.sp,
            Reg::SR => &mut self.sr.bits,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Reg {
    A,
    B,
    PC,
    SP,
    SR,
}

impl Reg {
    const fn bitsize() -> u8 {
        3
    }
}

impl Mmaped for Reg {
    fn encode(self) -> u32 {
        match self {
            Self::A => 0x0,
            Self::B => 0x1,
            Self::PC => 0x2,
            Self::SP => 0x3,
            Self::SR => 0x4,
        }
    }

    fn decode(val: u32) -> Option<Self> {
        match val & 0x7 {
            0x0 => Some(Self::A),
            0x1 => Some(Self::B),
            0x2 => Some(Self::PC),
            0x3 => Some(Self::SP),
            0x4 => Some(Self::SR),
            _ => None,
        }
    }

    fn bitsize() -> u8 {
        3
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Address {
    Dir(u16),
    Ind(Reg),
    Idx(Reg, i16),
}

impl Mmaped for Address {
    fn decode(val: u32) -> Option<Self> {
        if let Some(Arg::Address(addr)) = Arg::decode(val) {
            Some(addr)
        } else {
            None
        }
    }

    fn encode(self) -> u32 {
        Arg::Address(self).encode()
    }

    fn bitsize() -> u8 {
        Arg::bitsize()
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Imm(u16),
    Reg(Reg),
}

impl Mmaped for Value {
    fn decode(val: u32) -> Option<Self> {
        if let Some(Arg::Value(val)) = Arg::decode(val) {
            Some(val)
        } else {
            None
        }
    }

    fn encode(self) -> u32 {
        Arg::Value(self).encode()
    }

    fn bitsize() -> u8 {
        Arg::bitsize()
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Arg {
    Address(Address),
    Value(Value),
}

impl Arg {
    const fn bitsize() -> u8 {
        3 + Reg::bitsize() + 16
    }
}

impl Mmaped for Arg {
    // Memory layout (3+3+16 = 22 bit wide)
    //  | id| reg | value (u/i16)  |
    // 0b000 00000 0000000000000000

    fn encode(self) -> u32 {
        match self {
            Arg::Address(Address::Dir(dir)) => (0x0 << (3 + 16)) + dir as u32,
            Arg::Address(Address::Ind(reg)) => (0x1 << (3 + 16)) + (reg.encode() << 16),
            Arg::Address(Address::Idx(reg, off)) => {
                (0x2 << 3 + 16) + (reg.encode() << 16) + (off as u16 as u32)
            }
            Arg::Value(Value::Imm(imm)) => (0x3 << 3 + 16) + imm as u32,
            Arg::Value(Value::Reg(reg)) => (0x4 << 3 + 16) + (reg.encode() << 16),
        }
    }

    fn decode(val: u32) -> Option<Self> {
        let inst = (val >> (3 + 16)) & 0x7;
        let reg = (val >> 16) & 0x7;
        let val = (val & 0xFFFF) as u16;
        match inst {
            0x0 => Some(Arg::Address(Address::Dir(val))),
            0x1 => Some(Arg::Address(Address::Ind(Reg::decode(reg)?))),
            0x2 => Some(Arg::Address(Address::Idx(Reg::decode(reg)?, val as i16))),
            0x3 => Some(Arg::Value(Value::Imm(val))),
            0x4 => Some(Arg::Value(Value::Reg(Reg::decode(reg)?))),
            _ => None,
        }
    }

    fn bitsize() -> u8 {
        3 + 3 + 16
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
enum Instruction {
    #[instruction(0x00)]
    Add(Arg, Reg),
    #[instruction(0x01)]
    And(Arg, Reg),
    #[instruction(0x02)]
    Call(Arg),
    #[instruction(0x03)]
    Cmp(Arg, Reg),
    #[instruction(0x04)]
    Div(Arg, Reg),
    #[instruction(0x05)]
    Fas(Address, Reg),
    #[instruction(0x06)]
    In(Address, Reg),
    #[instruction(0x07)]
    Jmp(Arg),
    // #[instruction(0x01)]
    // CondJmp(Arg, Cond),
    #[instruction(0x08)]
    Ld(Arg, Reg),
    #[instruction(0x09)]
    Mul(Arg, Reg),
    #[instruction(0x0A)]
    Neg(Reg),
    #[instruction(0x0B)]
    Nop,
    #[instruction(0x0C)]
    Not(Reg),
    #[instruction(0x0D)]
    Or(Arg, Reg),
    #[instruction(0x0E)]
    Out(Value, Address),
    #[instruction(0x0F)]
    Pop(Reg),
    #[instruction(0x10)]
    Push(Value),
    #[instruction(0x11)]
    Reset,
    #[instruction(0x12)]
    Rti,
    #[instruction(0x13)]
    Rtn,
    #[instruction(0x14)]
    Shl(Arg, Reg),
    #[instruction(0x15)]
    Shr(Arg, Reg),
    #[instruction(0x16)]
    St(Address, Reg), // note: this one is inverted
    #[instruction(0x17)]
    Sub(Arg, Reg),
    // #[instruction(0x18)]
    // Swap(ArgSwap, Reg),
    #[instruction(0x19)]
    Trap,
    #[instruction(0x1A)]
    Xor(Arg, Reg),
}

impl Instruction {
    fn execute(self, computer: &mut Computer) -> Result<()> {
        match self {
            Instruction::Add(arg, reg) => {
                let val = computer.arg(arg)?;
                *computer.registers.get_mut(reg) += val;
            }
            Instruction::And(arg, reg) => {
                let val = computer.arg(arg)?;
                *computer.registers.get_mut(reg) &= val;
            }
            Instruction::Call(_) => todo!(),
            Instruction::Cmp(arg, reg) => {
                let val1 = computer.arg(arg)?;
                let val2 = computer.registers.get(reg);
                if val1 == val2 {
                    computer.registers.sr |= StatusRegister::ZERO;
                } else {
                    computer.registers.sr -= StatusRegister::ZERO;
                }

                if val1 < val2 {
                    computer.registers.sr |= StatusRegister::NEGATIVE;
                } else {
                    computer.registers.sr -= StatusRegister::NEGATIVE;
                }
            }
            Instruction::Div(arg, reg) => {
                let val = computer.arg(arg)?;
                let reg = computer.registers.get_mut(reg);
                *reg = reg.checked_div(val).ok_or(Exception::DivByZero)?;
            }
            Instruction::Fas(addr, reg) => {
                let val = computer.read_memory(addr.clone())?;
                *computer.registers.get_mut(reg) = val;
                computer.write_memory(addr, 1)?;
            }
            Instruction::In(_, _) => todo!(),
            Instruction::Jmp(_) => todo!(),
            Instruction::Ld(_, _) => todo!(),
            Instruction::Mul(_, _) => todo!(),
            Instruction::Neg(_) => todo!(),
            Instruction::Nop => {}
            Instruction::Not(_) => todo!(),
            Instruction::Or(_, _) => todo!(),
            Instruction::Out(_, _) => todo!(),
            Instruction::Pop(_) => todo!(),
            Instruction::Push(_) => todo!(),
            Instruction::Reset => todo!(),
            Instruction::Rti => todo!(),
            Instruction::Rtn => todo!(),
            Instruction::Shl(_, _) => todo!(),
            Instruction::Shr(_, _) => todo!(),
            Instruction::St(_, _) => todo!(),
            Instruction::Sub(_, _) => todo!(),
            Instruction::Trap => todo!(),
            Instruction::Xor(_, _) => todo!(),
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
        computer.write_memory(Address::Dir(0x42), 100).unwrap();
        *computer.registers.get_mut(Reg::B) = 0x32;
        let instruction = Instruction::Add(Arg::Address(Address::Idx(Reg::B, 0x10)), Reg::A);
        instruction.execute(&mut computer).unwrap();
        assert_eq!(computer.registers.get(Reg::A), 105);
    }
}
