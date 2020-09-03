#![allow(dead_code)]

use std::collections::HashMap;
use thiserror::Error;

use crate::processor::{Computer, Encodable, Instruction, Labelable, Reg};

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("failed to encode instruction")]
    EncodeError,

    #[error("invalid label {0}")]
    InvalidLabel(String),

    #[error("can't resolve label")]
    ResolveError,
}

type Result<T> = std::result::Result<T, CompilerError>;

type Labels = HashMap<String, u16>;

struct PendingLabel {
    label: String,
    instruction: Instruction,
    address: u16,
}

trait Compiler {
    type Error;
    fn ingest_label(&mut self, label: String);
    fn ingest_labeled_instruction(
        &mut self,
        instruction: Instruction,
        label: String,
    ) -> std::result::Result<(), Self::Error>;
    fn ingest<T: Encodable>(&mut self, val: T) -> std::result::Result<(), Self::Error>;
}

#[derive(Default)]
struct CompilerState {
    computer: Computer,
    labels: Labels,
    pending_labels: Vec<PendingLabel>,
    memory_position: u16,
}

impl Compiler for CompilerState {
    type Error = CompilerError;

    fn ingest_label(&mut self, label: String) {
        self.labels.insert(label, self.memory_position);
    }

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

    fn ingest<T: Encodable>(&mut self, val: T) -> Result<()> {
        let offset = self
            .computer
            .write(self.memory_position.into(), val)
            .map_err(|_| CompilerError::EncodeError)?;
        self.memory_position += offset;
        Ok(())
    }
}

impl CompilerState {
    fn build(self, start: String) -> Result<Computer> {
        let pending_labels = self.pending_labels;
        let labels = self.labels;
        let mut computer = self.computer;

        // Resolve all labels
        for p in pending_labels.into_iter() {
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

        computer.set_register(Reg::PC, start);

        // Initialize the SP
        computer.set_register(Reg::SP, 0xF000);

        Ok(computer)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::processor::{Arg, Value};

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

        compiler.ingest_label("start".into());
        compiler
            .ingest_labeled_instruction(Instruction::Call(Arg::label()), "subroutine".into())
            .unwrap();
        compiler.ingest(Instruction::Reset).unwrap();

        compiler.ingest_label("subroutine".into());
        compiler
            .ingest(Instruction::Ld(Arg::Value(Value::Imm(0x42)), Reg::A))
            .unwrap();
        compiler.ingest(Instruction::Rtn).unwrap();

        let mut computer = compiler.build("start".into()).unwrap();
        computer.run().unwrap();

        assert_eq!(computer.registers.get(Reg::A), 0x42);
    }
}
