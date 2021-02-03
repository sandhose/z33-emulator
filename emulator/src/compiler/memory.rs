use std::collections::HashMap;

use thiserror::Error;
use tracing::{debug, event, span, Level};

use crate::{
    parser::expression::EvaluationError as ExpressionEvaluationError,
    parser::line::LineContent,
    parser::{
        location::Located,
        value::{ComputeError, DirectiveArgument, DirectiveKind, InstructionKind},
    },
    runtime::{Arg, ArgConversionError, Cell, Instruction, Memory, TryFromArg},
};

use super::layout::{Labels, Layout, Placement};

#[derive(Debug, Error)]
pub enum MemoryFillError {
    #[error("could not evaluate expression: {0}")]
    Evaluation(ExpressionEvaluationError),

    #[error("could not compute instruction argument: {0}")]
    Compute(ComputeError),

    #[error("could not compile instruction: {0}")]
    InstructionCompilation(InstructionCompilationError),
}

impl From<ExpressionEvaluationError> for MemoryFillError {
    fn from(e: ExpressionEvaluationError) -> Self {
        Self::Evaluation(e)
    }
}

impl From<ComputeError> for MemoryFillError {
    fn from(e: ComputeError) -> Self {
        Self::Compute(e)
    }
}

impl From<InstructionCompilationError> for MemoryFillError {
    fn from(e: InstructionCompilationError) -> Self {
        Self::InstructionCompilation(e)
    }
}

#[derive(Debug, Error)]
pub enum InstructionCompilationError {
    #[error("invalid number of arguments: expected {expected}, got {got}")]
    InvalidArgumentNumber { expected: usize, got: usize },

    #[error("{0}")]
    ArgumentConversion(#[from] ArgConversionError),
}

fn get_tuple<X, Y>(mut args: Vec<Arg>) -> Result<(X, Y), InstructionCompilationError>
where
    X: TryFromArg,
    Y: TryFromArg,
{
    if args.len() != 2 {
        return Err(InstructionCompilationError::InvalidArgumentNumber {
            expected: 2,
            got: args.len(),
        });
    }

    let x = args.remove(0);
    let y = args.remove(0);

    Ok((X::try_from_arg(x)?, Y::try_from_arg(y)?))
}

fn get_singleton<X>(mut args: Vec<Arg>) -> Result<X, InstructionCompilationError>
where
    X: TryFromArg,
{
    if args.len() != 1 {
        return Err(InstructionCompilationError::InvalidArgumentNumber {
            expected: 2,
            got: args.len(),
        });
    }

    let x = args.remove(0);
    Ok(X::try_from_arg(x)?)
}

fn get_none(args: Vec<Arg>) -> Result<(), InstructionCompilationError> {
    if !args.is_empty() {
        return Err(InstructionCompilationError::InvalidArgumentNumber {
            expected: 2,
            got: args.len(),
        });
    }

    Ok(())
}

fn compile_instruction(
    kind: &InstructionKind,
    arguments: Vec<Arg>,
) -> Result<Instruction, InstructionCompilationError> {
    use InstructionKind::*;

    match kind {
        Add => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Add(a, b))
        }

        And => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::And(a, b))
        }

        Call => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Call(a))
        }

        Cmp => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Cmp(a, b))
        }

        Div => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Div(a, b))
        }

        Fas => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Fas(a, b))
        }

        In => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::In(a, b))
        }

        Jmp => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jmp(a))
        }

        Jeq => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jeq(a))
        }

        Jne => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jne(a))
        }

        Jle => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jle(a))
        }

        Jlt => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jlt(a))
        }

        Jge => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jge(a))
        }

        Jgt => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jgt(a))
        }

        Ld => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Ld(a, b))
        }

        Mul => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Mul(a, b))
        }

        Neg => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Neg(a))
        }

        Nop => {
            get_none(arguments)?;
            Ok(Instruction::Nop)
        }

        Not => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Not(a))
        }

        Or => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Or(a, b))
        }

        Out => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Out(a, b))
        }

        Pop => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Pop(a))
        }

        Push => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Push(a))
        }

        Reset => {
            get_none(arguments)?;
            Ok(Instruction::Reset)
        }

        Rti => {
            get_none(arguments)?;
            Ok(Instruction::Rti)
        }

        Rtn => {
            get_none(arguments)?;
            Ok(Instruction::Rtn)
        }

        Shl => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Shl(a, b))
        }

        Shr => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Shr(a, b))
        }

        St => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::St(a, b))
        }

        Sub => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Sub(a, b))
        }

        Swap => todo!(),

        Trap => {
            get_none(arguments)?;
            Ok(Instruction::Trap)
        }

        Xor => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Xor(a, b))
        }
    }
}

#[tracing::instrument(skip(placement, labels), err)]
fn compile_placement<L>(
    labels: &Labels,
    placement: &Placement<L>,
) -> Result<Cell, MemoryFillError> {
    use DirectiveKind::*;

    match placement {
        Placement::Reserved => Ok(Cell::Empty),

        Placement::Char(c) => Ok(Cell::Char(*c)),

        Placement::Line(LineContent::Directive {
            kind: Located { inner: Word, .. },
            argument:
                Located {
                    inner: DirectiveArgument::Expression(expression),
                    ..
                },
        }) => {
            debug!(%expression, "Evaluating directive");
            let value = expression.evaluate(labels)?;
            Ok(Cell::Word(value))
        }

        // We should not have any other directives other than "word" at this point
        Placement::Line(LineContent::Directive { .. }) => {
            unreachable!();
        }

        Placement::Line(LineContent::Instruction { kind, arguments }) => {
            let span = span!(Level::TRACE, "line", %kind);
            let _guard = span.enter();
            let arguments: Result<Vec<_>, _> = arguments
                .iter()
                .map(|argument| {
                    event!(Level::TRACE, "argument evaluation: {}", argument);
                    argument.inner.evaluate(labels)
                })
                .collect();
            let arguments = arguments?;
            let instruction = compile_instruction(&kind.inner, arguments)?;
            Ok(Cell::Instruction(Box::new(instruction)))
        }
    }
}

#[tracing::instrument(skip(layout))]
pub(crate) fn fill_memory<L>(layout: &Layout<L>) -> Result<Memory, MemoryFillError> {
    debug!("Filling memory");
    let mut memory = Memory::default();

    let cells: Result<HashMap<u64, Cell>, MemoryFillError> = layout
        .memory
        .iter()
        .map(|(index, placement)| Ok((*index, compile_placement(&layout.labels, placement)?)))
        .collect();

    for (address, content) in cells? {
        debug!(address, content = %content, "Filling cell");
        let cell = memory.get_mut(address).unwrap();
        *cell = content;
    }

    Ok(memory)
}
