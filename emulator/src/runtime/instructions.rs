use std::convert::TryInto;

use parse_display::Display;
use tracing::{debug, info};

use crate::constants::*;

use super::{
    exception::Exception,
    memory::Cell,
    registers::{Reg, StatusRegister},
    Address, Arg, Computer, ProcessorError, Value,
};

#[derive(Debug, Clone, PartialEq, Display)]
pub enum Instruction {
    /// Add a value to a register
    #[display("add  {0}, {1}")]
    Add(Arg, Reg),

    /// Bitwise `and` with a given value
    #[display("and  {0}, {1}")]
    And(Arg, Reg),

    /// Push `%pc` and go to the given address
    #[display("call {0}")]
    Call(Arg),

    /// Compare a value with a register
    #[display("cmp  {0}, {1}")]
    Cmp(Arg, Reg),

    /// Divide a register by a value
    #[display("div  {0}, {1}")]
    Div(Arg, Reg),

    /// Load a memory cell to a register and set this cell to 1
    #[display("fas  {0}, {1}")]
    Fas(Address, Reg),

    /// Read a value from an I/O controller
    #[display("in   {0}, {1}")]
    In(Address, Reg),

    /// Unconditional jump
    #[display("jmp  {0}")]
    Jmp(Arg),

    /// Jump if equal
    #[display("jeq  {0}")]
    Jeq(Arg),

    /// Jump if not equal
    #[display("jne  {0}")]
    Jne(Arg),

    /// Jump if less or equal
    #[display("jle  {0}")]
    Jle(Arg),

    /// Jump if strictly less
    #[display("jlt  {0}")]
    Jlt(Arg),

    /// Jump if greater of equal
    #[display("jge  {0}")]
    Jge(Arg),

    /// Jump if strictly greater
    #[display("jgt  {0}")]
    Jgt(Arg),

    /// Load a register with a value
    #[display("ld   {0}, {1}")]
    Ld(Arg, Reg),

    /// Multiply a value to a register
    #[display("mul  {0}, {1}")]
    Mul(Arg, Reg),

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
    Or(Arg, Reg),

    /// Write a value to an I/O controller
    #[display("out  {0}, {1}")]
    Out(Value, Address),

    /// Pop a value from the stack
    #[display("pop  {0}")]
    Pop(Reg),

    /// Push a value into the stack
    #[display("push {0}")]
    Push(Value),

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
    Shl(Arg, Reg),

    /// Bitshift to the right
    #[display("shr  {0}, {1}")]
    Shr(Arg, Reg),

    /// Store a register value in memory
    #[display("st   {0}, {1}")]
    St(Reg, Address),

    /// Substract a value from a register
    #[display("sub  {0}, {1}")]
    Sub(Arg, Reg),

    // /// Swap a value and a register
    // Swap(ArgSwap, Reg),
    /// Start a `trap` exception
    #[display("trap")]
    Trap,

    /// Bitwise `xor` with a given value
    #[display("xor  {0}, {1}")]
    Xor(Arg, Reg),

    /// Show registers content
    #[display("debugreg")]
    DebugReg,
}

impl Instruction {
    /// Execute the instruction
    #[tracing::instrument(skip(computer))]
    pub(crate) fn execute(&self, computer: &mut Computer) -> Result<(), ProcessorError> {
        use Instruction::*;

        match self {
            Add(arg, reg) => {
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

            And(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;
                let res = a & b;
                debug!("{} & {} = {}", a, b, res);
                computer.set_register(reg, res.into())?;
            }

            Call(arg) => {
                // Push PC
                let pc = computer.registers.pc;
                computer.push(pc)?;

                // Jump
                let addr = computer.address_from_arg(arg)?;
                computer.jump(&Address::Dir(addr))?;
            }

            Cmp(arg, reg) => {
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

            Div(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;
                let res = b.checked_div(a).ok_or(Exception::DivByZero)?;
                debug!("{} / {} = {}", b, a, res);
                computer.set_register(reg, res.into())?;
            }

            Fas(addr, reg) => {
                let addr = computer.registers.resolve_address(addr)?;
                let cell = computer.memory.get_mut(addr)?;
                let val = cell.clone();
                *cell = Cell::Word(1);
                computer.set_register(reg, val)?;
            }

            In(_, _) => {
                computer.check_privileged()?;
                todo!();
            }

            Jmp(arg) => {
                let val = computer.address_from_arg(arg)?;
                debug!("Jumping to address {:#x}", val);
                computer.registers.pc = val;
            }

            Jeq(arg) => {
                if computer.registers.sr.contains(StatusRegister::ZERO) {
                    let val = computer.address_from_arg(arg)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }

            Jne(arg) => {
                if !computer.registers.sr.contains(StatusRegister::ZERO) {
                    let val = computer.address_from_arg(arg)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }

            Jle(arg) => {
                if computer.registers.sr.contains(StatusRegister::ZERO)
                    || computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = computer.address_from_arg(arg)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }

            Jlt(arg) => {
                if !computer.registers.sr.contains(StatusRegister::ZERO)
                    && computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = computer.address_from_arg(arg)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }

            Jge(arg) => {
                if computer.registers.sr.contains(StatusRegister::ZERO)
                    || !computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = computer.address_from_arg(arg)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }

            Jgt(arg) => {
                if !computer.registers.sr.contains(StatusRegister::ZERO)
                    && !computer.registers.sr.contains(StatusRegister::NEGATIVE)
                {
                    let val = computer.address_from_arg(arg)?;
                    debug!("Jumping to address {:#x}", val);
                    computer.registers.pc = val;
                }
            }

            Ld(arg, reg) => {
                let val = computer.arg(arg)?;
                computer.set_register(reg, val)?;
            }

            Mul(arg, reg) => {
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

            Neg(reg) => {
                let val = computer.word_from_reg(reg)?;
                let res = -(val as i64);
                let res = res as Word;
                debug!("-{} = {}", val, res);
                computer.set_register(reg, res.into())?;
            }

            Nop => {}

            Not(reg) => {
                let val = computer.word_from_reg(reg)?;
                let res = !val;
                debug!("!{} = {}", val, res);
                computer.set_register(reg, res.into())?;
            }

            Or(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;
                let res = a | b;
                debug!("{} | {} = {}", a, b, res);
                computer.set_register(reg, res.into())?;
            }

            Out(_, _) => {
                computer.check_privileged()?;
                todo!()
            }

            Pop(reg) => {
                let val = computer.pop()?.clone();
                debug!("pop => {:?}", val);
                computer.set_register(reg, val)?;
            }

            Push(val) => {
                let val = computer.value(val);
                debug!("push({:?})", val);
                computer.push(val)?;
            }

            Reset => return Err(ProcessorError::Reset),

            Rti => {
                computer.check_privileged()?;
                computer.registers.pc =
                    computer.memory.get(INTERRUPT_PC_SAVE)?.extract_address()?;
                computer.registers.sr = StatusRegister::from_bits_truncate(
                    computer.memory.get(INTERRUPT_SR_SAVE)?.extract_word()?,
                );
            }

            Rtn => {
                let ret = computer.pop()?; // Pop the return address
                let ret = ret.extract_address()?; // Convert it to an address
                debug!("Returning to {}", ret);
                computer.registers.pc = ret; // and jump to it
            }

            Shl(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;

                let b: u32 = b.try_into().map_err(|_| Exception::InvalidInstruction)?;
                let res = a.checked_shl(b).ok_or(Exception::InvalidInstruction)?;

                debug!("{} << {} = {}", a, b, res);
                computer.set_register(reg, res.into())?;
            }

            Shr(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;

                let b: u32 = b.try_into().map_err(|_| Exception::InvalidInstruction)?;
                let res = a.checked_shr(b).ok_or(Exception::InvalidInstruction)?;

                debug!("{} >> {} = {}", a, b, res);
                computer.set_register(reg, res.into())?;
            }

            St(reg, address) => {
                let val = computer.registers.get(reg);
                computer.write(address, val)?;
            }

            Sub(arg, reg) => {
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

            Trap => {
                return Err(Exception::Trap.into());
            }

            Xor(arg, reg) => {
                let a = computer.word_from_arg(arg)?;
                let b = computer.word_from_reg(reg)?;
                let res = a ^ b;
                debug!("{} ^ {} = {}", a, b, res);
                computer.set_register(reg, res.into())?;
            }

            DebugReg => {
                info!("debugreg: {}", computer.registers);
            }
        };

        Ok(())
    }

    /// Get the total cost of an instruction in terms of CPU cycles
    pub(crate) const fn cost(&self) -> usize {
        use Instruction::*;

        match self {
            Add(a, b) => 1 + a.cost() + b.cost(),
            And(a, b) => 1 + a.cost() + b.cost(),
            Call(a) => 1 + a.cost(),
            Cmp(a, b) => 1 + a.cost() + b.cost(),
            Div(a, b) => 1 + a.cost() + b.cost(),
            Fas(a, b) => 1 + a.cost() + b.cost(),
            In(a, b) => 1 + a.cost() + b.cost(),
            Jmp(a) => 1 + a.cost(),
            Jeq(a) => 1 + a.cost(),
            Jne(a) => 1 + a.cost(),
            Jle(a) => 1 + a.cost(),
            Jlt(a) => 1 + a.cost(),
            Jge(a) => 1 + a.cost(),
            Jgt(a) => 1 + a.cost(),
            Ld(a, b) => 1 + a.cost() + b.cost(),
            Mul(a, b) => 1 + a.cost() + b.cost(),
            Neg(a) => 1 + a.cost(),
            Nop => 1,
            Not(a) => 1 + a.cost(),
            Or(a, b) => 1 + a.cost() + b.cost(),
            Out(a, b) => 1 + a.cost() + b.cost(),
            Pop(a) => 1 + a.cost(),
            Push(a) => 1 + a.cost(),
            Reset => 1,
            Rti => 1,
            Rtn => 1,
            Shl(a, b) => 1 + a.cost() + b.cost(),
            Shr(a, b) => 1 + a.cost() + b.cost(),
            St(a, b) => 1 + a.cost() + b.cost(),
            Sub(a, b) => 1 + a.cost() + b.cost(),
            Trap => 1,
            Xor(a, b) => 1 + a.cost() + b.cost(),
            DebugReg => 0,
        }
    }
}
