use std::collections::HashMap;
use thiserror::Error;
use tracing::{debug, span, Level};

use crate::memory::Cell;
use crate::processor::{Computer, Instruction, Labelable};

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("failed to encode instruction")]
    EncodeError,

    #[error("invalid label {0}")]
    InvalidLabel(String),

    #[error("duplicate label {0}")]
    DuplicateLabel(String),

    #[error("can't resolve label")]
    ResolveError,

    #[error("invalid address")]
    InvalidAddress,
}

type Result<T> = std::result::Result<T, CompilerError>;

type Labels = HashMap<String, u64>;

#[derive(Debug)]
struct PendingLabel {
    label: String,
    instruction: Instruction,
    address: u64,
}

pub trait Compiler {
    type Error;
    fn ingest_label(&mut self, label: String) -> std::result::Result<(), Self::Error>;
    fn ingest_labeled_instruction(
        &mut self,
        instruction: Instruction,
        label: String,
    ) -> std::result::Result<(), Self::Error>;
    fn ingest<T: Into<Cell> + std::fmt::Debug>(
        &mut self,
        val: T,
    ) -> std::result::Result<(), Self::Error>;
    fn change_address(&mut self, addr: u64) -> std::result::Result<(), Self::Error>;
    fn memory_skip(&mut self, offset: u64) -> std::result::Result<(), Self::Error>;
}

#[derive(Default, Debug)]
pub struct CompilerState {
    computer: Computer,
    labels: Labels,
    pending_labels: Vec<PendingLabel>,
    memory_position: u64,
}

impl Compiler for CompilerState {
    type Error = CompilerError;

    #[tracing::instrument]
    fn ingest_label(&mut self, label: String) -> Result<()> {
        if self.labels.get(&label).is_some() {
            return Err(CompilerError::DuplicateLabel(label));
        }

        self.labels.insert(label, self.memory_position);
        Ok(())
    }

    #[tracing::instrument]
    fn ingest_labeled_instruction(
        &mut self,
        instruction: Instruction,
        label: String,
    ) -> Result<()> {
        self.pending_labels.push(PendingLabel {
            label,
            instruction: instruction.clone(),
            address: self.memory_position,
        });
        self.ingest(instruction)?;
        Ok(())
    }

    #[tracing::instrument]
    fn ingest<T: Into<Cell> + std::fmt::Debug>(&mut self, val: T) -> Result<()> {
        self.computer
            .write(self.memory_position.into(), val)
            .map_err(|_| CompilerError::EncodeError)?;
        self.memory_position += 1;
        Ok(())
    }

    #[tracing::instrument]
    fn change_address(&mut self, addr: u64) -> Result<()> {
        self.memory_position = addr;
        Ok(())
    }

    #[tracing::instrument]
    fn memory_skip(&mut self, offset: u64) -> Result<()> {
        self.memory_position = self
            .memory_position
            .checked_add(offset)
            .ok_or(CompilerError::InvalidAddress)?;
        Ok(())
    }
}

impl CompilerState {
    #[tracing::instrument(err)]
    pub fn build(self, start: String) -> Result<Computer> {
        let pending_labels = self.pending_labels;
        let labels = self.labels;
        let mut computer = self.computer;

        debug!("Resolving labels {:?}", labels);

        // Resolve all labels
        for p in pending_labels.into_iter() {
            let span = span!(
                Level::TRACE,
                "resolving_label",
                address = p.address,
                label = p.label.as_str()
            );
            let _enter = span.enter();

            let resolved = labels
                .get(&p.label)
                .ok_or(CompilerError::InvalidLabel(p.label.clone()))?
                .to_owned();

            let target = p.address;

            // Resolve the instruction
            let instruction = p
                .instruction
                .resolve_label(resolved)
                .ok_or(CompilerError::ResolveError)?;

            // Rewrite it in memory
            computer
                .write(target.into(), instruction)
                .map_err(|_| CompilerError::EncodeError)?;
        }

        // Set the PC to the start label
        let start = labels
            .get(&start)
            .ok_or(CompilerError::InvalidLabel(start.clone()))?
            .to_owned();

        computer.registers.pc = start;

        // Initialize the SP
        computer.registers.sp = 0xF000;

        Ok(computer)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::processor::{Arg, Reg, Value};

    #[test]
    fn build_and_run_test() {
        let mut compiler = CompilerState::default();

        // Let's compile and run:
        //  start:
        //      call subroutine
        //      reset
        //  subroutine:
        //      ld 0x42, %a
        //      rtn

        compiler.ingest_label("start".into()).unwrap();
        compiler
            .ingest_labeled_instruction(Instruction::Call(Arg::label()), "subroutine".into())
            .unwrap();
        compiler.ingest(Instruction::Reset).unwrap();

        compiler.ingest_label("subroutine".into()).unwrap();
        compiler
            .ingest(Instruction::Ld(Arg::Value(Value::Imm(0x42)), Reg::A))
            .unwrap();
        compiler.ingest(Instruction::Rtn).unwrap();

        let mut computer = compiler.build("start".into()).unwrap();
        computer.run().unwrap();

        assert_eq!(computer.registers.get(Reg::A), Cell::Word(0x42));
    }
}
