use std::convert::TryFrom;
use std::{collections::HashMap, convert::TryInto};

use thiserror::Error;
use tracing::{debug, span, trace, Level};

use crate::{
    constants as C,
    parser::expression::EvaluationError as ExpressionEvaluationError,
    parser::line::LineContent,
    parser::{
        location::Located,
        value::{ComputeError, DirectiveArgument, DirectiveKind, InstructionKind},
    },
    runtime::{
        arguments::{ArgConversionError, ImmRegDirIndIdx},
        Cell, Instruction, Memory,
    },
};

use super::layout::{Labels, Layout, Placement};

#[derive(Debug, Error)]
pub enum MemoryFillError<L> {
    #[error("could not evaluate expression")]
    Evaluation {
        location: L,
        source: ExpressionEvaluationError<L>,
    },

    #[error("could not compute instruction argument")]
    Compute {
        location: L,
        source: ComputeError<L>,
    },

    #[error("could not compile instruction")]
    InstructionCompilation {
        location: L,
        source: InstructionCompilationError,
    },
}

impl<L> MemoryFillError<L> {
    pub fn location(&self) -> &L {
        match self {
            MemoryFillError::Evaluation { location, .. } => location,
            MemoryFillError::Compute { location, .. } => location,
            MemoryFillError::InstructionCompilation { location, .. } => location,
        }
    }
}

#[derive(Debug, Error)]
pub enum InstructionCompilationError {
    #[error("invalid number of arguments: expected {expected}, got {got}")]
    InvalidArgumentNumber { expected: usize, got: usize },

    #[error(transparent)]
    ArgumentConversion(#[from] ArgConversionError),
}

impl From<std::convert::Infallible> for InstructionCompilationError {
    fn from(_: std::convert::Infallible) -> Self {
        unreachable!()
    }
}

fn get_tuple<X, Y>(args: Vec<ImmRegDirIndIdx>) -> Result<(X, Y), InstructionCompilationError>
where
    X: TryFrom<ImmRegDirIndIdx>,
    Y: TryFrom<ImmRegDirIndIdx>,
    X::Error: Into<InstructionCompilationError>,
    Y::Error: Into<InstructionCompilationError>,
{
    let [x, y]: [ImmRegDirIndIdx; 2] = args.try_into().map_err(|args: Vec<_>| {
        InstructionCompilationError::InvalidArgumentNumber {
            expected: 2,
            got: args.len(),
        }
    })?;

    Ok((
        X::try_from(x).map_err(Into::into)?,
        Y::try_from(y).map_err(Into::into)?,
    ))
}

fn get_singleton<X>(args: Vec<ImmRegDirIndIdx>) -> Result<X, InstructionCompilationError>
where
    X: TryFrom<ImmRegDirIndIdx>,
    X::Error: Into<InstructionCompilationError>,
{
    let [x]: [ImmRegDirIndIdx; 1] = args.try_into().map_err(|args: Vec<_>| {
        InstructionCompilationError::InvalidArgumentNumber {
            expected: 1,
            got: args.len(),
        }
    })?;

    X::try_from(x).map_err(Into::into)
}

fn get_none(args: Vec<ImmRegDirIndIdx>) -> Result<(), InstructionCompilationError> {
    if !args.is_empty() {
        return Err(InstructionCompilationError::InvalidArgumentNumber {
            expected: 0,
            got: args.len(),
        });
    }

    Ok(())
}

#[tracing::instrument]
fn compile_instruction(
    kind: &InstructionKind,
    arguments: Vec<ImmRegDirIndIdx>,
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

        Swap => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Swap(a, b))
        }

        Trap => {
            get_none(arguments)?;
            Ok(Instruction::Trap)
        }

        Xor => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Xor(a, b))
        }

        DebugReg => {
            get_none(arguments)?;
            Ok(Instruction::DebugReg)
        }
    }
}

#[tracing::instrument(skip(placement, labels))]
fn compile_placement<L: Clone>(
    labels: &Labels,
    placement: &Placement<L>,
) -> Result<Cell, MemoryFillError<L>> {
    use DirectiveKind::*;
    use Placement::*;

    match placement {
        // Reserved placements are created by .space directives
        Reserved => Ok(Cell::Empty),

        // Char placements are create by .string directives
        Char(c) => Ok(Cell::Char(*c)),

        // A .word directive (don't mind the weird destructuring)
        Line(LineContent::Directive {
            kind: Located { inner: Word, .. },
            argument:
                Located {
                    inner: DirectiveArgument::Expression(expression),
                    location,
                },
        }) => {
            debug!(%expression, "Evaluating directive");
            let value =
                expression
                    .evaluate(labels)
                    .map_err(|source| MemoryFillError::Evaluation {
                        source,
                        location: location.clone(),
                    })?;
            Ok(Cell::Word(value))
        }

        // We should not have any other directives other than "word" at this point
        Line(LineContent::Directive { .. }) => {
            unreachable!();
        }

        Line(LineContent::Instruction { kind, arguments }) => {
            let span = span!(Level::TRACE, "line", %kind);
            let _guard = span.enter();
            let arguments: Result<Vec<_>, _> = arguments
                .iter()
                .enumerate()
                .map(|(index, argument)| {
                    trace!("argument {} evaluation: {}", index, argument);
                    argument
                        .inner
                        .evaluate(labels)
                        .map_err(|source| MemoryFillError::Compute {
                            location: argument.location.clone(),
                            source,
                        })
                })
                .collect();
            let arguments = arguments?;
            let instruction = compile_instruction(&kind.inner, arguments).map_err(|source| {
                MemoryFillError::InstructionCompilation {
                    location: kind.location.clone(),
                    source,
                }
            })?;
            Ok(Cell::Instruction(Box::new(instruction)))
        }
    }
}

#[tracing::instrument(skip(layout))]
pub(crate) fn fill_memory<L: Clone>(layout: &Layout<L>) -> Result<Memory, MemoryFillError<L>> {
    debug!(
        placements = layout.memory.len(),
        labels = ?layout.labels,
        "Filling memory"
    );
    let mut memory = Memory::default();

    let cells: Result<HashMap<C::Address, Cell>, MemoryFillError<L>> = layout
        .memory
        .iter()
        .map(|(index, placement)| {
            let span = span!(Level::TRACE, "placement", index);
            let _guard = span.enter();
            let cell = compile_placement(&layout.labels, placement)?;
            Ok((*index, cell))
        })
        .collect();

    for (address, content) in cells? {
        trace!(address, content = %content, "Filling cell");
        let cell = memory.get_mut(address).unwrap();
        *cell = content;
    }

    Ok(memory)
}
