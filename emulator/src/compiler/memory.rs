use std::collections::HashMap;

use thiserror::Error;

use crate::{
    parser::expression::EvaluationError as ExpressionEvaluationError,
    parser::line::LineContent,
    parser::value::{ComputeError, DirectiveArgument},
    processor::{Arg, ArgConversionError, Cell, Instruction, Memory, TryFromArg},
};

use super::layout::{Labels, Layout, Placement};

#[derive(Debug, Error)]
pub(crate) enum MemoryFillError<'a> {
    #[error("could not compile: {0}")]
    CompilationError(CompilationError<'a>),
}

impl<'a> From<CompilationError<'a>> for MemoryFillError<'a> {
    fn from(inner: CompilationError<'a>) -> Self {
        Self::CompilationError(inner)
    }
}

#[derive(Debug, Error)]
pub(crate) enum CompilationError<'a> {
    #[error("unsupported directive {directive}")]
    UnsupportedDirective { directive: &'a str },

    #[error("could not evaluate expression: {0}")]
    Evaluation(ExpressionEvaluationError<'a>),

    #[error("could not compute instruction argument: {0}")]
    Compute(ComputeError<'a>),

    #[error("could not compile instruction: {0}")]
    InstructionCompilation(InstructionCompilationError<'a>),
}

#[derive(Debug, Error)]
pub(crate) enum InstructionCompilationError<'a> {
    #[error("invalid opcode {0}")]
    InvalidOpcode(&'a str),

    #[error("invalid number of arguments: expected {expected}, got {got}")]
    InvalidArgumentNumber { expected: usize, got: usize },

    #[error("{0}")]
    ArgumentConversion(#[from] ArgConversionError),
}

fn get_tuple<'a, X, Y>(mut args: Vec<Arg>) -> Result<(X, Y), InstructionCompilationError<'a>>
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

fn get_singleton<'a, X>(mut args: Vec<Arg>) -> Result<X, InstructionCompilationError<'a>>
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

fn get_none<'a>(args: Vec<Arg>) -> Result<(), InstructionCompilationError<'a>> {
    if !args.is_empty() {
        return Err(InstructionCompilationError::InvalidArgumentNumber {
            expected: 2,
            got: args.len(),
        });
    }

    Ok(())
}

fn compile_instruction<'a>(
    opcode: &'a str,
    arguments: Vec<Arg>,
) -> Result<Instruction, InstructionCompilationError<'a>> {
    use InstructionCompilationError::*;

    match opcode.to_lowercase().as_str() {
        "add" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Add(a, b))
        }

        "and" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::And(a, b))
        }

        "call" => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Call(a))
        }

        "cmp" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Cmp(a, b))
        }

        "div" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Div(a, b))
        }

        "fas" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Fas(a, b))
        }

        "in" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::In(a, b))
        }

        "jmp" => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jmp(a))
        }

        "jeq" => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jeq(a))
        }

        "jne" => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jne(a))
        }

        "jle" => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jle(a))
        }

        "jlt" => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jlt(a))
        }

        "jge" => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jge(a))
        }

        "jgt" => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Jgt(a))
        }

        "ld" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Ld(a, b))
        }

        "mul" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Mul(a, b))
        }

        "neg" => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Neg(a))
        }

        "nop" => {
            get_none(arguments)?;
            Ok(Instruction::Nop)
        }

        "not" => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Not(a))
        }

        "or" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Or(a, b))
        }

        "out" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Out(a, b))
        }

        "pop" => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Pop(a))
        }

        "push" => {
            let a = get_singleton(arguments)?;
            Ok(Instruction::Push(a))
        }

        "reset" => {
            get_none(arguments)?;
            Ok(Instruction::Reset)
        }

        "rti" => {
            get_none(arguments)?;
            Ok(Instruction::Rti)
        }

        "rtn" => {
            get_none(arguments)?;
            Ok(Instruction::Rtn)
        }

        "shl" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Shl(a, b))
        }

        "shr" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Shr(a, b))
        }

        "st" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::St(a, b))
        }

        "sub" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Sub(a, b))
        }

        "trap" => {
            get_none(arguments)?;
            Ok(Instruction::Trap)
        }

        "xor" => {
            let (a, b) = get_tuple(arguments)?;
            Ok(Instruction::Xor(a, b))
        }

        _ => Err(InvalidOpcode(opcode)),
    }
}

fn compile_placement<'a>(
    labels: &Labels<'a>,
    placement: &Placement<'a>,
) -> Result<Cell, CompilationError<'a>> {
    use CompilationError::*;
    match placement {
        Placement::Reserved => Ok(Cell::Empty),
        Placement::Char(c) => Ok(Cell::Char(*c)),
        Placement::Line(LineContent::Directive {
            directive: "word",
            argument: DirectiveArgument::Expression(expression),
        }) => {
            let value = expression.evaluate(labels).map_err(Evaluation)?;
            Ok(Cell::Word(value))
        }
        Placement::Line(LineContent::Instruction { opcode, arguments }) => {
            let arguments: Result<Vec<_>, _> = arguments
                .iter()
                .map(|argument| argument.evaluate(labels))
                .collect();
            let arguments = arguments.map_err(Compute)?;
            let instruction =
                compile_instruction(opcode, arguments).map_err(InstructionCompilation)?;
            Ok(Cell::Instruction(Box::new(instruction)))
        }
        Placement::Line(LineContent::Directive { directive, .. }) => {
            Err(CompilationError::UnsupportedDirective { directive })
        }
    }
}

pub(crate) fn fill_memory<'a>(layout: &Layout<'a>) -> Result<Memory, CompilationError<'a>> {
    let mut memory = Memory::default();

    let cells: Result<HashMap<u64, Cell>, CompilationError> = layout
        .memory
        .iter()
        .map(|(index, placement)| Ok((*index, compile_placement(&layout.labels, placement)?)))
        .collect();

    for (address, content) in cells? {
        let cell = memory.get_mut(address).unwrap();
        *cell = content;
    }

    Ok(memory)
}
