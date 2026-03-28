use std::convert::TryFrom;
use std::ops::Range;

use smallvec::SmallVec;
use thiserror::Error;
use tracing::{debug, span, trace, Level};

use super::layout::{Labels, Layout, Placement};
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
        /// Span of the argument that failed evaluation
        location: Range<usize>,
        source: ComputeError,
    },

    #[error("{source}")]
    InstructionCompilation {
        /// Span of the instruction mnemonic
        instruction_span: Range<usize>,
        /// Spans of each argument (indexed same as the instruction arguments)
        argument_spans: SmallVec<[Range<usize>; 2]>,
        source: InstructionCompilationError,
    },
}

impl MemoryFillError {
    /// The primary location for this error (for backward compat).
    #[must_use]
    pub fn location(&self) -> &Range<usize> {
        match self {
            MemoryFillError::Evaluation { location, .. }
            | MemoryFillError::Compute { location, .. } => location,
            MemoryFillError::InstructionCompilation {
                instruction_span, ..
            } => instruction_span,
        }
    }
}

#[derive(Debug, Error)]
pub enum InstructionCompilationError {
    #[error("'{instruction}' takes {expected} argument(s), got {got}")]
    InvalidArgumentCount {
        instruction: InstructionKind,
        expected: usize,
        got: usize,
    },

    #[error("invalid argument for '{instruction}'")]
    InvalidArgumentType {
        instruction: InstructionKind,
        /// 0-indexed position of the argument that failed
        argument_index: usize,
        source: ArgConversionError,
    },
}

/// Check argument count, returning the instruction kind in errors.
fn check_arg_count(
    kind: InstructionKind,
    args: &[ImmRegDirIndIdx],
    expected: usize,
) -> Result<(), InstructionCompilationError> {
    if args.len() != expected {
        return Err(InstructionCompilationError::InvalidArgumentCount {
            instruction: kind,
            expected,
            got: args.len(),
        });
    }
    Ok(())
}

/// Get an argument at a given position without type conversion.
fn get_arg(args: &[ImmRegDirIndIdx], index: usize) -> ImmRegDirIndIdx {
    args[index].clone()
}

/// Try to convert an argument at a given position, wrapping errors with
/// context.
fn convert_arg<T: TryFrom<ImmRegDirIndIdx, Error = ArgConversionError>>(
    kind: InstructionKind,
    args: &[ImmRegDirIndIdx],
    index: usize,
) -> Result<T, InstructionCompilationError> {
    T::try_from(args[index].clone()).map_err(|source| {
        InstructionCompilationError::InvalidArgumentType {
            instruction: kind,
            argument_index: index,
            source,
        }
    })
}

#[tracing::instrument]
#[expect(clippy::too_many_lines)]
fn compile_instruction(
    kind: InstructionKind,
    arguments: &[ImmRegDirIndIdx],
) -> Result<Instruction, InstructionCompilationError> {
    use InstructionKind as K;

    match kind {
        // Error instructions should be skipped during layout
        K::Error => unreachable!("error instructions should be skipped during layout"),

        // Binary ops: arg1 = ImmRegDirIndIdx, arg2 = Reg
        K::Add => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::Add(
                get_arg(arguments, 0),
                convert_arg(kind, arguments, 1)?,
            ))
        }
        K::And => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::And(
                get_arg(arguments, 0),
                convert_arg(kind, arguments, 1)?,
            ))
        }
        K::Cmp => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::Cmp(
                get_arg(arguments, 0),
                convert_arg(kind, arguments, 1)?,
            ))
        }
        K::Div => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::Div(
                get_arg(arguments, 0),
                convert_arg(kind, arguments, 1)?,
            ))
        }
        K::Ld => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::Ld(
                get_arg(arguments, 0),
                convert_arg(kind, arguments, 1)?,
            ))
        }
        K::Mul => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::Mul(
                get_arg(arguments, 0),
                convert_arg(kind, arguments, 1)?,
            ))
        }
        K::Or => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::Or(
                get_arg(arguments, 0),
                convert_arg(kind, arguments, 1)?,
            ))
        }
        K::Shl => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::Shl(
                get_arg(arguments, 0),
                convert_arg(kind, arguments, 1)?,
            ))
        }
        K::Shr => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::Shr(
                get_arg(arguments, 0),
                convert_arg(kind, arguments, 1)?,
            ))
        }
        K::Sub => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::Sub(
                get_arg(arguments, 0),
                convert_arg(kind, arguments, 1)?,
            ))
        }
        K::Xor => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::Xor(
                get_arg(arguments, 0),
                convert_arg(kind, arguments, 1)?,
            ))
        }

        // DirIndIdx + Reg
        K::Fas => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::Fas(
                convert_arg(kind, arguments, 0)?,
                convert_arg(kind, arguments, 1)?,
            ))
        }
        K::In => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::In(
                convert_arg(kind, arguments, 0)?,
                convert_arg(kind, arguments, 1)?,
            ))
        }

        // Branches: 1 arg ImmRegDirIndIdx
        K::Call => {
            check_arg_count(kind, arguments, 1)?;
            Ok(Instruction::Call(get_arg(arguments, 0)))
        }
        K::Jmp => {
            check_arg_count(kind, arguments, 1)?;
            Ok(Instruction::Jmp(get_arg(arguments, 0)))
        }
        K::Jeq => {
            check_arg_count(kind, arguments, 1)?;
            Ok(Instruction::Jeq(get_arg(arguments, 0)))
        }
        K::Jne => {
            check_arg_count(kind, arguments, 1)?;
            Ok(Instruction::Jne(get_arg(arguments, 0)))
        }
        K::Jle => {
            check_arg_count(kind, arguments, 1)?;
            Ok(Instruction::Jle(get_arg(arguments, 0)))
        }
        K::Jlt => {
            check_arg_count(kind, arguments, 1)?;
            Ok(Instruction::Jlt(get_arg(arguments, 0)))
        }
        K::Jge => {
            check_arg_count(kind, arguments, 1)?;
            Ok(Instruction::Jge(get_arg(arguments, 0)))
        }
        K::Jgt => {
            check_arg_count(kind, arguments, 1)?;
            Ok(Instruction::Jgt(get_arg(arguments, 0)))
        }

        // Unary register ops
        K::Neg => {
            check_arg_count(kind, arguments, 1)?;
            Ok(Instruction::Neg(convert_arg(kind, arguments, 0)?))
        }
        K::Not => {
            check_arg_count(kind, arguments, 1)?;
            Ok(Instruction::Not(convert_arg(kind, arguments, 0)?))
        }
        K::Pop => {
            check_arg_count(kind, arguments, 1)?;
            Ok(Instruction::Pop(convert_arg(kind, arguments, 0)?))
        }

        // ImmReg source
        K::Push => {
            check_arg_count(kind, arguments, 1)?;
            Ok(Instruction::Push(convert_arg(kind, arguments, 0)?))
        }

        // ImmReg + DirIndIdx
        K::Out => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::Out(
                convert_arg(kind, arguments, 0)?,
                convert_arg(kind, arguments, 1)?,
            ))
        }

        // Reg + DirIndIdx
        K::St => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::St(
                convert_arg(kind, arguments, 0)?,
                convert_arg(kind, arguments, 1)?,
            ))
        }

        // RegDirIndIdx + Reg
        K::Swap => {
            check_arg_count(kind, arguments, 2)?;
            Ok(Instruction::Swap(
                convert_arg(kind, arguments, 0)?,
                convert_arg(kind, arguments, 1)?,
            ))
        }

        // No arguments
        K::Nop => {
            check_arg_count(kind, arguments, 0)?;
            Ok(Instruction::Nop)
        }
        K::Reset => {
            check_arg_count(kind, arguments, 0)?;
            Ok(Instruction::Reset)
        }
        K::Rti => {
            check_arg_count(kind, arguments, 0)?;
            Ok(Instruction::Rti)
        }
        K::Rtn => {
            check_arg_count(kind, arguments, 0)?;
            Ok(Instruction::Rtn)
        }
        K::Trap => {
            check_arg_count(kind, arguments, 0)?;
            Ok(Instruction::Trap)
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

        // Error recovery placeholders should never reach memory fill
        P::Line(Located {
            inner: LineContent::Error,
            ..
        }) => {
            unreachable!("error nodes should be skipped during layout");
        }

        P::Line(Located {
            inner: LineContent::Instruction { kind, arguments },
            location: line_location,
        }) => {
            let span = span!(Level::TRACE, "line", %kind);
            let _guard = span.enter();

            // Evaluate each argument, collecting (value, absolute_span) pairs.
            // If evaluation fails, we bail immediately with the argument span.
            let mut arg_values = SmallVec::<[_; 2]>::with_capacity(arguments.len());
            let mut arg_spans = SmallVec::<[_; 2]>::with_capacity(arguments.len());
            for (index, argument) in arguments.iter().enumerate() {
                trace!("argument {} evaluation: {}", index, argument);
                let value =
                    argument
                        .inner
                        .evaluate(labels)
                        .map_err(|source| MemoryFillError::Compute {
                            location: Range {
                                start: argument.location.start + line_location.start,
                                end: argument.location.end + line_location.start,
                            },
                            source,
                        })?;
                arg_values.push(value);
                arg_spans.push(Range {
                    start: argument.location.start + line_location.start,
                    end: argument.location.end + line_location.start,
                });
            }

            let instruction_span = Range {
                start: kind.location.start + line_location.start,
                end: kind.location.end + line_location.start,
            };

            let instruction = compile_instruction(kind.inner, &arg_values).map_err(|source| {
                MemoryFillError::InstructionCompilation {
                    instruction_span: instruction_span.clone(),
                    argument_spans: arg_spans,
                    source,
                }
            })?;
            Ok(Cell::Instruction(Box::new(instruction)))
        }
    }
}

/// Fill memory from the layout, collecting all errors instead of stopping at
/// the first.
///
/// Always produces a `Memory` (with `Cell::Empty` for failed placements) plus
/// a vector of errors.
#[tracing::instrument(skip(layout))]
pub(crate) fn fill_memory(layout: &Layout) -> (Memory, Vec<MemoryFillError>) {
    debug!(
        placements = layout.memory.len(),
        labels = ?layout.labels,
        "Filling memory"
    );
    let mut memory = Memory::default();
    let mut errors = Vec::new();

    for (index, (placement, _location)) in &layout.memory {
        let span = span!(Level::TRACE, "placement", index);
        let _guard = span.enter();
        match compile_placement(&layout.labels, placement) {
            Ok(cell) => {
                trace!(address = index, content = %cell, "Filling cell");
                let dest = memory.get_mut(*index).unwrap();
                *dest = cell;
            }
            Err(e) => {
                errors.push(e);
            }
        }
    }

    (memory, errors)
}
