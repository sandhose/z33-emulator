use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::ops::Range;

use thiserror::Error;
use tracing::{debug, span, trace, Level};

use super::layout::{Labels, Layout, Placement};
use crate::constants as C;
use crate::parser::expression::EvaluationError as ExpressionEvaluationError;
use crate::parser::line::LineContent;
use crate::parser::location::Located;
use crate::parser::value::{ComputeError, DirectiveArgument, DirectiveKind, InstructionKind};
use crate::runtime::arguments::{ArgConversionError, ImmRegDirIndIdx};
use crate::runtime::{Cell, Instruction, Memory};

#[derive(Debug, Error)]
pub enum MemoryFillError {
    #[error("could not evaluate expression")]
    Evaluation {
        location: Range<usize>,
        source: ExpressionEvaluationError,
    },

    #[error("could not compute instruction argument")]
    Compute {
        location: Range<usize>,
        source: ComputeError,
    },

    #[error("could not compile instruction")]
    InstructionCompilation {
        location: Range<usize>,
        source: InstructionCompilationError,
    },
}

impl MemoryFillError {
    pub fn location(&self) -> &Range<usize> {
        match self {
            MemoryFillError::Evaluation { location, .. }
            | MemoryFillError::Compute { location, .. }
            | MemoryFillError::InstructionCompilation { location, .. } => location,
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

#[allow(clippy::needless_pass_by_value)]
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
#[expect(clippy::too_many_lines)]
fn compile_instruction(
    kind: InstructionKind,
    arguments: Vec<ImmRegDirIndIdx>,
) -> Result<Instruction, InstructionCompilationError> {
    use InstructionKind as K;

    match kind {
        K::Add => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Add(a, b))
        }

        K::And => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::And(a, b))
        }

        K::Call => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Call(a))
        }

        K::Cmp => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Cmp(a, b))
        }

        K::Div => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Div(a, b))
        }

        K::Fas => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Fas(a, b))
        }

        K::In => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::In(a, b))
        }

        K::Jmp => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jmp(a))
        }

        K::Jeq => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jeq(a))
        }

        K::Jne => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jne(a))
        }

        K::Jle => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jle(a))
        }

        K::Jlt => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jlt(a))
        }

        K::Jge => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jge(a))
        }

        K::Jgt => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jgt(a))
        }

        K::Ld => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Ld(a, b))
        }

        K::Mul => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Mul(a, b))
        }

        K::Neg => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Neg(a))
        }

        K::Nop => {
            get_none(arguments)?;
            Ok(Instruction::Nop)
        }

        K::Not => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Not(a))
        }

        K::Or => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Or(a, b))
        }

        K::Out => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Out(a, b))
        }

        K::Pop => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Pop(a))
        }

        K::Push => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Push(a))
        }

        K::Reset => {
            get_none(arguments)?;
            Ok(Instruction::Reset)
        }

        K::Rti => {
            get_none(arguments)?;
            Ok(Instruction::Rti)
        }

        K::Rtn => {
            get_none(arguments)?;
            Ok(Instruction::Rtn)
        }

        K::Shl => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Shl(a, b))
        }

        K::Shr => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Shr(a, b))
        }

        K::St => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::St(a, b))
        }

        K::Sub => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Sub(a, b))
        }

        K::Swap => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Swap(a, b))
        }

        K::Trap => {
            get_none(arguments)?;
            Ok(Instruction::Trap)
        }

        K::Xor => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Xor(a, b))
        }

        K::DebugReg => {
            get_none(arguments)?;
            Ok(Instruction::DebugReg)
        }
    }
}

#[tracing::instrument(skip(placement, labels))]
fn compile_placement(labels: &Labels, placement: &Placement) -> Result<Cell, MemoryFillError> {
    use Placement as P;

    match placement {
        // Reserved placements are created by .space directives
        // Empty placements at the end of .string directives
        P::Reserved | P::Nul => Ok(Cell::Empty),

        // Char placements are created by .string directives
        P::Char(c) => Ok(Cell::Word(u32::from(*c).into())),

        // A .word directive (don't mind the weird destructuring)
        P::Line(Located {
            inner:
                LineContent::Directive {
                    kind:
                        Located {
                            inner: DirectiveKind::Word,
                            ..
                        },
                    argument:
                        Located {
                            inner: DirectiveArgument::Expression(expression),
                            location,
                        },
                },
            location: line_location,
        }) => {
            debug!(%expression, "Evaluating directive");
            let value =
                expression
                    .evaluate(labels)
                    .map_err(|source| MemoryFillError::Evaluation {
                        source,
                        location: Range {
                            start: location.start + line_location.start,
                            end: location.end + line_location.start,
                        },
                    })?;
            Ok(Cell::Word(value))
        }

        // We should not have any other directives other than "word" at this point
        P::Line(Located {
            inner: LineContent::Directive { .. },
            ..
        }) => {
            unreachable!();
        }

        P::Line(Located {
            inner: LineContent::Instruction { kind, arguments },
            location: line_location,
        }) => {
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
                            location: Range {
                                start: argument.location.start + line_location.start,
                                end: argument.location.end + line_location.start,
                            },
                            source,
                        })
                })
                .collect();
            let arguments = arguments?;
            let instruction = compile_instruction(kind.inner, arguments).map_err(|source| {
                MemoryFillError::InstructionCompilation {
                    location: Range {
                        start: kind.location.start + line_location.start,
                        end: kind.location.end + line_location.start,
                    },
                    source,
                }
            })?;
            Ok(Cell::Instruction(Box::new(instruction)))
        }
    }
}

#[tracing::instrument(skip(layout))]
pub(crate) fn fill_memory(layout: &Layout) -> Result<Memory, MemoryFillError> {
    debug!(
        placements = layout.memory.len(),
        labels = ?layout.labels,
        "Filling memory"
    );
    let mut memory = Memory::default();

    let cells: Result<HashMap<C::Address, Cell>, MemoryFillError> = layout
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
