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
    expression::{Context, EvaluationError, Node},
    parse_expression, parse_identifier,
};
use crate::processor::{Address, Arg, Reg, Value};

#[derive(Clone, Debug, PartialEq)]
pub enum Argument<'a> {
    Value(Node<'a>),
    Register(&'a str),
    Direct(Node<'a>),
    Indirect(&'a str),
    Indexed { register: &'a str, value: Node<'a> },
}

#[derive(Error, Debug)]
pub enum ComputeError<'a> {
    #[error("could not evaluate argument: {0}")]
    EvaluationError(EvaluationError<'a>),

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

impl<'a> Argument<'a> {
    pub fn compute<C: Context>(&self, context: &C) -> Result<Arg, ComputeError<'a>> {
        match self {
            Argument::Value(v) => {
                let value = v.evaluate(context).map_err(ComputeError::EvaluationError)?;
                Ok(Arg::Value(Value::Imm(value)))
            }
            Argument::Register(r) => {
                let register = convert_register(r)?;
                Ok(Arg::Value(Value::Reg(register)))
            }
            Argument::Direct(v) => {
                let value = v.evaluate(context).map_err(ComputeError::EvaluationError)?;
                Ok(Arg::Address(Address::Dir(value)))
            }
            Argument::Indirect(r) => {
                let register = convert_register(r)?;
                Ok(Arg::Address(Address::Ind(register)))
            }
            Argument::Indexed { register, value } => {
                let value = value
                    .evaluate(context)
                    .map_err(ComputeError::EvaluationError)?;
                let register = convert_register(register)?;
                Ok(Arg::Address(Address::Idx(register, value)))
            }
        }
    }
}

fn parse_register<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    preceded(tag("%"), parse_identifier)(input)
}

fn parse_indirect_indexed_inner<'a>(input: &'a str) -> IResult<&'a str, Argument<'a>> {
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

    Ok((input, Argument::Indexed { register, value }))
}

pub fn parse_indirect_inner<'a>(input: &'a str) -> IResult<&'a str, Argument<'a>> {
    alt((
        parse_indirect_indexed_inner,
        map(parse_register, Argument::Indirect),
        map(parse_expression, Argument::Direct),
    ))(input)
}

fn parse_indirect<'a>(input: &'a str) -> IResult<&'a str, Argument<'a>> {
    delimited(
        terminated(tag("["), space0),
        parse_indirect_inner,
        preceded(space0, tag("]")),
    )(input)
}

fn parse_direct<'a>(input: &'a str) -> IResult<&'a str, Argument<'a>> {
    alt((
        map(parse_register, Argument::Register),
        map(parse_expression, Argument::Value),
    ))(input)
}

pub fn parse_argument<'a>(input: &'a str) -> IResult<&'a str, Argument<'a>> {
    alt((parse_direct, parse_indirect))(input)
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
