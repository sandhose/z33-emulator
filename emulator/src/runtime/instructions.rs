use std::convert::TryInto;

use tracing::{debug, info};

use crate::constants::*;

use super::{
    exception::Exception,
    memory::Cell,
    registers::{Reg, StatusRegister},
    Address, Arg, Computer, ProcessorError, Value,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    /// Add a value to a register
    Add(Arg, Reg),

    /// Bitwise `and` with a given value
    And(Arg, Reg),

    /// Push `%pc` and go to the given address
    Call(Arg),

    /// Compare a value with a register
    Cmp(Arg, Reg),

    /// Divide a register by a value
    Div(Arg, Reg),

    /// Load a memory cell to a register and set this cell to 1
    Fas(Address, Reg),

    /// Read a value from an I/O controller
    In(Address, Reg),

    /// Unconditional jump
    Jmp(Arg),

    /// Jump if equal
    Jeq(Arg),

    /// Jump if not equal
    Jne(Arg),

    /// Jump if less or equal
    Jle(Arg),

    /// Jump if strictly less
    Jlt(Arg),

    /// Jump if greater of equal
    Jge(Arg),

    /// Jump if strictly greater
    Jgt(Arg),

    /// Load a register with a value
    Ld(Arg, Reg),

    /// Multiply a value to a register
    Mul(Arg, Reg),

    Neg(Reg),

    /// No-op
    Nop,

    /// Bitwise negation of a register
    Not(Reg),

    /// Bitwise `or` with a given value
    Or(Arg, Reg),

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
    Shl(Arg, Reg),

    /// Bitshift to the right
    Shr(Arg, Reg),

    /// Store a register value in memory
    St(Reg, Address),

    /// Substract a value from a register
    Sub(Arg, Reg),

    // /// Swap a value and a register
    // Swap(ArgSwap, Reg),
    /// Start a `trap` exception
    Trap,

    /// Bitwise `xor` with a given value
    Xor(Arg, Reg),

    /// Show registers content
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

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;

        match self {
            Add(a, b) => write!(f, "add  {}, {}", a, b),
            And(a, b) => write!(f, "and  {}, {}", a, b),
            Call(a) => write!(f, "call {}", a),
            Cmp(a, b) => write!(f, "cmp  {}, {}", a, b),
            Div(a, b) => write!(f, "div  {}, {}", a, b),
            Fas(a, b) => write!(f, "fas  {}, {}", a, b),
            In(a, b) => write!(f, "in   {}, {}", a, b),
            Jmp(a) => write!(f, "jmp  {}", a),
            Jeq(a) => write!(f, "jeq  {}", a),
            Jne(a) => write!(f, "jne  {}", a),
            Jle(a) => write!(f, "jle  {}", a),
            Jlt(a) => write!(f, "jlt  {}", a),
            Jge(a) => write!(f, "jge  {}", a),
            Jgt(a) => write!(f, "jgt  {}", a),
            Ld(a, b) => write!(f, "ld   {}, {}", a, b),
            Mul(a, b) => write!(f, "mul  {}, {}", a, b),
            Neg(a) => write!(f, "neg  {}", a),
            Nop => write!(f, "nop"),
            Not(a) => write!(f, "not  {}", a),
            Or(a, b) => write!(f, "or   {}, {}", a, b),
            Out(a, b) => write!(f, "out  {}, {}", a, b),
            Pop(a) => write!(f, "pop  {}", a),
            Push(a) => write!(f, "push {}", a),
            Reset => write!(f, "reset"),
            Rti => write!(f, "rti"),
            Rtn => write!(f, "rtn"),
            Shl(a, b) => write!(f, "shl  {}, {}", a, b),
            Shr(a, b) => write!(f, "shr  {}, {}", a, b),
            St(a, b) => write!(f, "st   {}, {}", a, b),
            Sub(a, b) => write!(f, "sub  {}, {}", a, b),
            Trap => write!(f, "trap"),
            Xor(a, b) => write!(f, "xor  {}, {}", a, b),
            DebugReg => write!(f, "debugreg"),
        }
    }
}
