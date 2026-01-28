use std::convert::TryInto;

use parse_display::Display;
use tracing::{debug, info};

use super::arguments::{
    DirIndIdx, ExtractValue, ImmReg, ImmRegDirIndIdx, RegDirIndIdx, ResolveAddress,
};
use super::exception::Exception;
use super::memory::Cell;
use super::registers::{Reg, StatusRegister};
use super::{Computer, ProcessorError};
use crate::constants::{Word, INTERRUPT_PC_SAVE, INTERRUPT_SR_SAVE};

#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum Instruction {
    /// Add a value to a register
    #[display("add  {0}, {1}")]
    Add(ImmRegDirIndIdx, Reg),

    /// Bitwise `and` with a given value
    #[display("and  {0}, {1}")]
    And(ImmRegDirIndIdx, Reg),

    /// Push `%pc` and go to the given address
    #[display("call {0}")]
    Call(ImmRegDirIndIdx),

    /// Compare a value with a register
    #[display("cmp  {0}, {1}")]
    Cmp(ImmRegDirIndIdx, Reg),

    /// Divide a register by a value
    #[display("div  {0}, {1}")]
    Div(ImmRegDirIndIdx, Reg),

    /// Load a memory cell to a register and set this cell to 1
    #[display("fas  {0}, {1}")]
    Fas(DirIndIdx, Reg),

    /// Read a value from an I/O controller
    #[display("in   {0}, {1}")]
    In(DirIndIdx, Reg),

    /// Unconditional jump
    #[display("jmp  {0}")]
    Jmp(ImmRegDirIndIdx),

    /// Jump if equal
    #[display("jeq  {0}")]
    Jeq(ImmRegDirIndIdx),

    /// Jump if not equal
    #[display("jne  {0}")]
    Jne(ImmRegDirIndIdx),

    /// Jump if less or equal
    #[display("jle  {0}")]
    Jle(ImmRegDirIndIdx),

    /// Jump if strictly less
    #[display("jlt  {0}")]
    Jlt(ImmRegDirIndIdx),

    /// Jump if greater of equal
    #[display("jge  {0}")]
    Jge(ImmRegDirIndIdx),

    /// Jump if strictly greater
    #[display("jgt  {0}")]
    Jgt(ImmRegDirIndIdx),

    /// Load a register with a value
    #[display("ld   {0}, {1}")]
    Ld(ImmRegDirIndIdx, Reg),

    /// Multiply a value to a register
    #[display("mul  {0}, {1}")]
    Mul(ImmRegDirIndIdx, Reg),

    #[display("neg  {0}")]
    Neg(Reg),

    /// No-op
    #[display("nop")]
    Nop,

    /// Bitwise negation of a register
    #[display("not  {0}")]
    Not(Reg),

    /// Bitwise `or` with a given value
    #[display("or   {0}, {1}")]
    Or(ImmRegDirIndIdx, Reg),

    /// Write a value to an I/O controller
    #[display("out  {0}, {1}")]
    Out(ImmReg, DirIndIdx),

    /// Pop a value from the stack
    #[display("pop  {0}")]
    Pop(Reg),

    /// Push a value into the stack
    #[display("push {0}")]
    Push(ImmReg),

    /// Reset the computer
    #[display("reset")]
    Reset,

    /// Return from an interrupt or an exception
    #[display("rti")]
    Rti,

    /// Return from a `call`
    #[display("rtn")]
    Rtn,

    /// Bitshift to the left
    #[display("shl  {0}, {1}")]
    Shl(ImmRegDirIndIdx, Reg),

    /// Bitshift to the right
    #[display("shr  {0}, {1}")]
    Shr(ImmRegDirIndIdx, Reg),

    /// Store a register value in memory
    #[display("st   {0}, {1}")]
    St(Reg, DirIndIdx),

    /// Substract a value from a register
    #[display("sub  {0}, {1}")]
    Sub(ImmRegDirIndIdx, Reg),

    /// Swap a value and a register
    #[display("swap {0}, {1}")]
    Swap(RegDirIndIdx, Reg),

    /// Start a `trap` exception
    #[display("trap")]
    Trap,

    /// Bitwise `xor` with a given value
    #[display("xor  {0}, {1}")]
    Xor(ImmRegDirIndIdx, Reg),

    /// Show registers content
    #[display("debugreg")]
    DebugReg,
}

impl Instruction {
    /// Execute the instruction
    #[tracing::instrument(skip(computer))]
    #[allow(clippy::too_many_lines)]
    pub(crate) fn execute(&self, computer: &mut Computer) -> Result<(), ProcessorError> {
        match self {
            Self::Add(arg, reg) => {
                let a = arg.extract_word(computer)?;
                let b = reg.extract_word(computer)?;
                let (res, overflow) = a.overflowing_add(b);
                debug!("{} + {} = {}", a, b, res);
                computer.set_register(*reg, res.into())?;

                computer
                    .registers
                    .sr
                    .set(StatusRegister::OVERFLOW, overflow);
            }

            Self::And(arg, reg) => {
                let a = arg.extract_word(computer)?;
                let b = reg.extract_word(computer)?;
                let res = a & b;
                debug!("{} & {} = {}", a, b, res);
                computer.set_register(*reg, res.into())?;
            }

            Self::Call(arg) => {
                // Push PC
                let pc = computer.registers.pc;
                computer.push(pc)?;

                // Jump
                let addr = arg.extract_address(computer)?;
                computer.jump(addr);
            }

            Self::Cmp(arg, reg) => {
                let a = arg.extract_word(computer)?;
                let b = reg.extract_word(computer)?;

                computer.registers.sr.set(StatusRegister::ZERO, a == b);
                computer.registers.sr.set(StatusRegister::NEGATIVE, a < b);

                debug!(
                    "cmp({}, {}) => {:?}",
                    a,
                    b,
                    computer.registers.sr & (StatusRegister::ZERO | StatusRegister::NEGATIVE)
                );
            }

            Self::Div(arg, reg) => {
                let a = arg.extract_word(computer)?;
                let b = reg.extract_word(computer)?;
                let res = b.checked_div(a).ok_or(Exception::DivByZero)?;
                debug!("{} / {} = {}", b, a, res);
                computer.set_register(*reg, res.into())?;
            }

            Self::Fas(addr, reg) => {
                let addr = addr.resolve_address(&computer.registers)?;
                let cell = computer.memory.get_mut(addr)?;
                let val = cell.clone();
                *cell = Cell::Word(1);
                computer.set_register(*reg, val)?;
            }

            Self::In(_, _) => {
                computer.check_privileged()?;
                todo!();
            }

            Self::Jmp(arg) => {
                let val = arg.extract_address(computer)?;
                debug!("Jumping to address {:#x}", val);
                computer.registers.pc = val;
            }

            Self::Jeq(arg) => {
                if computer.registers.sr.contains(StatusRegister::ZERO) {
                    let val = arg.extract_address(computer)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }

            Self::Jne(arg) => {
                if !computer.registers.sr.contains(StatusRegister::ZERO) {
                    let val = arg.extract_address(computer)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }

            Self::Jle(arg) => {
                if computer.registers.sr.contains(StatusRegister::ZERO)
                    || computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = arg.extract_address(computer)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }

            Self::Jlt(arg) => {
                if !computer.registers.sr.contains(StatusRegister::ZERO)
                    && computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = arg.extract_address(computer)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }

            Self::Jge(arg) => {
                if computer.registers.sr.contains(StatusRegister::ZERO)
                    || !computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = arg.extract_address(computer)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }

            Self::Jgt(arg) => {
                if !computer.registers.sr.contains(StatusRegister::ZERO)
                    && !computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = arg.extract_address(computer)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }

            Self::Ld(arg, reg) => {
                let val = arg.extract_cell(computer)?;
                computer.set_register(*reg, val)?;
            }

            Self::Mul(arg, reg) => {
                let a = arg.extract_word(computer)?;
                let b = reg.extract_word(computer)?;
                let (res, overflow) = a.overflowing_mul(b);
                debug!("{} * {} = {}", a, b, res);
                computer.set_register(*reg, res.into())?;

                computer
                    .registers
                    .sr
                    .set(StatusRegister::OVERFLOW, overflow);
            }

            Self::Neg(reg) => {
                let val = reg.extract_word(computer)?;
                let res = -val;
                let res = res as Word;
                debug!("-{} = {}", val, res);
                computer.set_register(*reg, res.into())?;
            }

            Self::Nop => {}

            Self::Not(reg) => {
                let val = reg.extract_word(computer)?;
                let res = !val;
                debug!("!{} = {}", val, res);
                computer.set_register(*reg, res.into())?;
            }

            Self::Or(arg, reg) => {
                let a = arg.extract_word(computer)?;
                let b = reg.extract_word(computer)?;
                let res = a | b;
                debug!("{} | {} = {}", a, b, res);
                computer.set_register(*reg, res.into())?;
            }

            Self::Out(_, _) => {
                computer.check_privileged()?;
                todo!()
            }

            Self::Pop(reg) => {
                let val = computer.pop()?.clone();
                debug!("pop => {:?}", val);
                computer.set_register(*reg, val)?;
            }

            Self::Push(val) => {
                let val = val.extract_cell(computer)?;
                debug!("push({:?})", val);
                computer.push(val)?;
            }

            Self::Reset => {
                // We go back one instruction on the %pc so that
                // we're pointing to the actual reset instruction when
                // the computer halts
                computer.registers.pc -= 1;
                return Err(ProcessorError::Reset);
            }

            Self::Rti => {
                computer.check_privileged()?;
                computer.registers.pc =
                    computer.memory.get(INTERRUPT_PC_SAVE)?.extract_address()?;
                computer.registers.sr = StatusRegister::from_bits_truncate(
                    computer.memory.get(INTERRUPT_SR_SAVE)?.extract_word()?,
                );
            }

            Self::Rtn => {
                let ret = computer.pop()?; // Pop the return address
                let ret = ret.extract_address()?; // Convert it to an address
                debug!("Returning to {}", ret);
                computer.registers.pc = ret; // and jump to it
            }

            Self::Shl(arg, reg) => {
                let a = arg.extract_word(computer)?;
                let b = reg.extract_word(computer)?;

                let b: u32 = b.try_into().map_err(|_| Exception::InvalidInstruction)?;
                let res = a.checked_shl(b).ok_or(Exception::InvalidInstruction)?;

                debug!("{} << {} = {}", a, b, res);
                computer.set_register(*reg, res.into())?;
            }

            Self::Shr(arg, reg) => {
                let a = arg.extract_word(computer)?;
                let b = reg.extract_word(computer)?;

                let b: u32 = b.try_into().map_err(|_| Exception::InvalidInstruction)?;
                let res = a.checked_shr(b).ok_or(Exception::InvalidInstruction)?;

                debug!("{} >> {} = {}", a, b, res);
                computer.set_register(*reg, res.into())?;
            }

            Self::St(reg, address) => {
                let val = reg.extract_word(computer)?;
                let address = address.resolve_address(&computer.registers)?;
                computer.write(address, val)?;
            }

            Self::Sub(arg, reg) => {
                let a = arg.extract_word(computer)?;
                let b = reg.extract_word(computer)?;
                let (res, overflow) = b.overflowing_sub(a);
                computer.set_register(*reg, res.into())?;

                debug!("{} - {} = {}", b, a, res);

                computer
                    .registers
                    .sr
                    .set(StatusRegister::OVERFLOW, overflow);
            }

            Self::Swap(arg, reg) => {
                // First we extract the value from both arguments
                let value = arg.extract_cell(computer)?;
                let value2 = reg.extract_cell(computer)?;

                // And set the value of the register specified by the second argument
                computer.set_register(*reg, value)?;

                // Then handle the two kind of cases: a register and a memory address
                if let RegDirIndIdx::Reg(arg) = arg {
                    // Set the value of the register by the first argument
                    computer.set_register(*arg, value2)?;
                } else {
                    // Convert the reg/dir/ind/idx arg to only dir/ind/idx, since we already
                    // handled the reg case. Despite the unwrap, this should never fail.
                    let arg: DirIndIdx = arg.clone().try_into().unwrap();

                    // Resolve the address and write the second value
                    let addr = arg.resolve_address(&computer.registers)?;
                    computer.write(addr, value2)?;
                }
            }

            Self::Trap => {
                return Err(Exception::Trap.into());
            }

            Self::Xor(arg, reg) => {
                let a = arg.extract_word(computer)?;
                let b = reg.extract_word(computer)?;
                let res = a ^ b;
                debug!("{} ^ {} = {}", a, b, res);
                computer.set_register(*reg, res.into())?;
            }

            Self::DebugReg => {
                info!("debugreg: {}", computer.registers);
            }
        }

        Ok(())
    }

    /// Get the total cost of an instruction in terms of CPU cycles
    pub(crate) const fn cost(&self) -> usize {
        // All instruction cost one CPU cycle itself, plus the cost of each of its
        // arguments
        match self {
            Self::DebugReg => 0, // The only exception being `debugreg`, which costs nothing

            // imm|reg|dir|ind|idx, reg
            Self::Div(a, _)
            | Self::Or(a, _)
            | Self::Add(a, _)
            | Self::And(a, _)
            | Self::Cmp(a, _)
            | Self::Mul(a, _)
            | Self::Ld(a, _)
            | Self::Sub(a, _)
            | Self::Xor(a, _)
            | Self::Shl(a, _)
            | Self::Shr(a, _) => 1 + a.cost() + Reg::cost(),

            // dir|ind|idx, reg
            Self::Fas(a, _) | Self::In(a, _) => 1 + a.cost() + Reg::cost(),

            // imm|reg|dir|ind|idx
            Self::Call(a)
            | Self::Jmp(a)
            | Self::Jeq(a)
            | Self::Jne(a)
            | Self::Jle(a)
            | Self::Jlt(a)
            | Self::Jge(a)
            | Self::Jgt(a) => 1 + a.cost(),

            // reg
            Self::Pop(_) | Self::Neg(_) | Self::Not(_) => 1 + Reg::cost(),

            // imm|reg, dir|ind|idx
            Self::Out(a, b) => 1 + a.cost() + b.cost(),

            // imm|reg
            Self::Push(a) => 1 + a.cost(),

            // reg, dir|ind|idx
            Self::St(_, b) => 1 + Reg::cost() + b.cost(),

            // reg|dir|ind|idx, reg
            Self::Swap(a, _) => 1 + a.cost() + Reg::cost(),

            Self::Nop | Self::Reset | Self::Rti | Self::Rtn | Self::Trap => 1,
        }
    }
}
