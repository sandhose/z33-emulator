use std::convert::TryInto;

use tracing::debug;
use z33_instruction_derive::Instruction;

use crate::constants::*;
use crate::memory::{Cell, Word};

use super::{
    exception::Exception,
    registers::{Reg, StatusRegister},
    Address, Arg, Computer, Labelable, ProcessorError, Value,
};

#[derive(Debug, Clone, PartialEq, Instruction)]
pub enum Instruction {
    /// Add a value to a register
    Add(#[labelable] Arg, Reg),

    /// Bitwise `and` with a given value
    And(#[labelable] Arg, Reg),

    /// Push `%pc` and go to the given address
    Call(#[labelable] Arg),

    /// Compare a value with a register
    Cmp(#[labelable] Arg, Reg),

    /// Divide a register by a value
    Div(#[labelable] Arg, Reg),

    /// Load a memory cell to a register and set this cell to 1
    Fas(Address, Reg),

    /// Read a value from an I/O controller
    In(Address, Reg),

    /// Unconditional jump
    Jmp(#[labelable] Arg),

    /// Jump if equal
    Jeq(#[labelable] Arg),

    /// Jump if not equal
    Jne(#[labelable] Arg),

    /// Jump if less or equal
    Jle(#[labelable] Arg),

    /// Jump if strictly less
    Jlt(#[labelable] Arg),

    /// Jump if greater of equal
    Jge(#[labelable] Arg),

    /// Jump if strictly greater
    Jgt(#[labelable] Arg),

    /// Load a register with a value
    Ld(#[labelable] Arg, Reg),

    /// Multiply a value to a register
    Mul(#[labelable] Arg, Reg),

    Neg(Reg),

    /// No-op
    Nop,

    /// Bitwise negation of a register
    Not(Reg),

    /// Bitwise `or` with a given value
    Or(#[labelable] Arg, Reg),

    /// Write a value to an I/O controller
    Out(Value, Address),

    /// Pop a value from the stack
    Pop(Reg),

    /// Push a value into the stack
    Push(Value),

    /// Reset the computer
    Reset,

    /// Return from an interrupt or an exception
    Rti,

    /// Return from a `call`
    Rtn,

    /// Bitshift to the left
    Shl(#[labelable] Arg, Reg),

    /// Bitshift to the right
    Shr(#[labelable] Arg, Reg),

    /// Store a register value in memory
    St(Reg, Address),

    /// Substract a value from a register
    Sub(#[labelable] Arg, Reg),

    // /// Swap a value and a register
    // Swap(ArgSwap, Reg),
    /// Start a `trap` exception
    Trap,

    /// Bitwise `xor` with a given value
    Xor(#[labelable] Arg, Reg),
}

impl Instruction {
    #[tracing::instrument]
    pub fn execute(self, computer: &mut Computer) -> Result<(), ProcessorError> {
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
                computer.registers.pc = computer.memory.get(INTERRUPT_PC_SAVE)?.extract_word()?;
                computer.registers.sr = StatusRegister::from_bits_truncate(
                    computer.memory.get(INTERRUPT_SR_SAVE)?.extract_word()?,
                );
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

    /// Get the total cost of an instruction in terms of CPU cycles
    pub const fn cost(&self) -> usize {
        match self {
            Instruction::Add(a, b) => 1 + a.cost() + b.cost(),
            Instruction::And(a, b) => 1 + a.cost() + b.cost(),
            Instruction::Call(a) => 1 + a.cost(),
            Instruction::Cmp(a, b) => 1 + a.cost() + b.cost(),
            Instruction::Div(a, b) => 1 + a.cost() + b.cost(),
            Instruction::Fas(a, b) => 1 + a.cost() + b.cost(),
            Instruction::In(a, b) => 1 + a.cost() + b.cost(),
            Instruction::Jmp(a) => 1 + a.cost(),
            Instruction::Jeq(a) => 1 + a.cost(),
            Instruction::Jne(a) => 1 + a.cost(),
            Instruction::Jle(a) => 1 + a.cost(),
            Instruction::Jlt(a) => 1 + a.cost(),
            Instruction::Jge(a) => 1 + a.cost(),
            Instruction::Jgt(a) => 1 + a.cost(),
            Instruction::Ld(a, b) => 1 + a.cost() + b.cost(),
            Instruction::Mul(a, b) => 1 + a.cost() + b.cost(),
            Instruction::Neg(a) => 1 + a.cost(),
            Instruction::Nop => 1,
            Instruction::Not(a) => 1 + a.cost(),
            Instruction::Or(a, b) => 1 + a.cost() + b.cost(),
            Instruction::Out(a, b) => 1 + a.cost() + b.cost(),
            Instruction::Pop(a) => 1 + a.cost(),
            Instruction::Push(a) => 1 + a.cost(),
            Instruction::Reset => 1,
            Instruction::Rti => 1,
            Instruction::Rtn => 1,
            Instruction::Shl(a, b) => 1 + a.cost() + b.cost(),
            Instruction::Shr(a, b) => 1 + a.cost() + b.cost(),
            Instruction::St(a, b) => 1 + a.cost() + b.cost(),
            Instruction::Sub(a, b) => 1 + a.cost() + b.cost(),
            Instruction::Trap => 1,
            Instruction::Xor(a, b) => 1 + a.cost() + b.cost(),
        }
    }
}
