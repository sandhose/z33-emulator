use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::one_of,
    character::complete::space0,
    combinator::map,
    sequence::{delimited, preceded, terminated},
    IResult,
};
use thiserror::Error;

use super::{
    expression::{parse_expression, Context, EvaluationError, Node},
    literal::parse_string_literal,
    parse_identifier,
};
use crate::runtime::{Address, Arg, Reg, Value};

/// Represents an instruction argument
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum InstructionArgument<'a> {
    /// An immediate value
    Value(Node<'a>),

    /// The content of a register
    Register(&'a str),

    /// A direct memory access
    Direct(Node<'a>),

    /// An indirect memory access (register)
    Indirect(&'a str),

    /// An indexed memory access (register + offset)
    Indexed { register: &'a str, value: Node<'a> },
}

/// Parse an instruction argument
pub(crate) fn parse_instruction_argument<'a>(
    input: &'a str,
) -> IResult<&'a str, InstructionArgument<'a>> {
    alt((parse_direct, parse_indirect))(input)
}

/// Represents a directive argument
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum DirectiveArgument<'a> {
    /// A string literal (`.string` directive)
    StringLiteral(String),

    /// An expression (`.addr`, `.word`, `.space` directives)
    Expression(Node<'a>),
}

/// Parse a directive argument
pub(crate) fn parse_directive_argument(input: &str) -> IResult<&str, DirectiveArgument> {
    alt((
        map(parse_string_literal, DirectiveArgument::StringLiteral),
        map(parse_expression, DirectiveArgument::Expression),
    ))(input)
}

impl<'a> From<&str> for DirectiveArgument<'a> {
    fn from(literal: &str) -> Self {
        Self::StringLiteral(literal.to_string())
    }
}

impl<'a> From<i128> for DirectiveArgument<'a> {
    fn from(value: i128) -> Self {
        Self::Expression(Node::Literal(value))
    }
}

#[derive(Error, Debug)]
pub(crate) enum ComputeError<'a> {
    #[error("could not evaluate argument: {0}")]
    Evaluation(EvaluationError<'a>),

    #[error("invalid register {0:?}")]
    InvalidRegister(&'a str),
}

fn convert_register(register: &str) -> Result<Reg, ComputeError> {
    match register.to_lowercase().as_str() {
        "a" => Ok(Reg::A),
        "b" => Ok(Reg::B),
        "pc" => Ok(Reg::PC),
        "sp" => Ok(Reg::SP),
        "sr" => Ok(Reg::SR),
        _ => Err(ComputeError::InvalidRegister(register)),
    }
}

impl<'a> InstructionArgument<'a> {
    pub(crate) fn evaluate<C: Context>(&self, context: &C) -> Result<Arg, ComputeError<'a>> {
        match self {
            InstructionArgument::Value(v) => {
                let value = v.evaluate(context).map_err(ComputeError::Evaluation)?;
                Ok(Arg::Value(Value::Imm(value)))
            }
            InstructionArgument::Register(r) => {
                let register = convert_register(r)?;
                Ok(Arg::Value(Value::Reg(register)))
            }
            InstructionArgument::Direct(v) => {
                let value = v.evaluate(context).map_err(ComputeError::Evaluation)?;
                Ok(Arg::Address(Address::Dir(value)))
            }
            InstructionArgument::Indirect(r) => {
                let register = convert_register(r)?;
                Ok(Arg::Address(Address::Ind(register)))
            }
            InstructionArgument::Indexed { register, value } => {
                let value = value.evaluate(context).map_err(ComputeError::Evaluation)?;
                let register = convert_register(register)?;
                Ok(Arg::Address(Address::Idx(register, value)))
            }
        }
    }
}

fn parse_register<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    preceded(tag("%"), parse_identifier)(input)
}

fn parse_indirect_indexed_inner<'a>(input: &'a str) -> IResult<&'a str, InstructionArgument<'a>> {
    let (input, register) = parse_register(input)?;
    let (input, _) = space0(input)?;
    let (input, sign) = one_of("+-")(input)?;
    let (input, _) = space0(input)?;
    let (input, value) = parse_expression(input)?;

    let value = match sign {
        '+' => value,
        '-' => Node::Invert(Box::new(value)),
        _ => unreachable!(),
    };

    Ok((input, InstructionArgument::Indexed { register, value }))
}

pub(crate) fn parse_indirect_inner<'a>(
    input: &'a str,
) -> IResult<&'a str, InstructionArgument<'a>> {
    alt((
        parse_indirect_indexed_inner,
        map(parse_register, InstructionArgument::Indirect),
        map(parse_expression, InstructionArgument::Direct),
    ))(input)
}

fn parse_indirect<'a>(input: &'a str) -> IResult<&'a str, InstructionArgument<'a>> {
    delimited(
        terminated(tag("["), space0),
        parse_indirect_inner,
        preceded(space0, tag("]")),
    )(input)
}

fn parse_direct<'a>(input: &'a str) -> IResult<&'a str, InstructionArgument<'a>> {
    alt((
        map(parse_register, InstructionArgument::Register),
        map(parse_expression, InstructionArgument::Value),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_register_test() {
        let (input, register) = parse_register("%a").unwrap();
        assert_eq!(input, "");
        assert_eq!(register, "a");
    }
}
