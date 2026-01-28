use std::str::FromStr;

use nom::branch::alt;
use nom::character::complete::char;
use nom::combinator::{all_consuming, map, value};
use nom::{Finish, IResult, Offset, Parser};
use nom_language::error::{convert_error, VerboseError};
use thiserror::Error;
use z33_emulator::parser::location::Locatable;
use z33_emulator::parser::{parse_expression, parse_register, ExpressionContext, ExpressionNode};
use z33_emulator::runtime::{Computer, ExtractValue, Reg};

#[derive(Debug, Clone)]
pub enum Argument {
    Direct(ExpressionNode),
    Indirect(Reg),
    Indexed(Reg, ExpressionNode),
}

impl Argument {
    pub fn evaluate<Ctx: ExpressionContext, V: TryFrom<i128>>(
        self,
        computer: &Computer,
        context: &Ctx,
    ) -> Result<V, anyhow::Error> {
        let node = match self {
            Argument::Direct(node) => node,
            Argument::Indirect(reg) => {
                ExpressionNode::Literal(i128::from(reg.extract_address(computer)?))
            }
            Argument::Indexed(reg, node) => ExpressionNode::Sum(
                Box::new(ExpressionNode::Literal(i128::from(
                    reg.extract_word(computer)?,
                )))
                .with_location(0..0),
                Box::new(node).with_location(0..0),
            ),
        };

        let val = node.evaluate(context)?;
        Ok(val)
    }
}

#[derive(Debug, Error)]
#[error("could not parse expression: {0}")]
pub struct ParseError(String);

impl FromStr for Argument {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_address(s).map_err(|e| ParseError(convert_error(s, e)))
    }
}

fn parse_indexed(input: &str) -> IResult<&str, Argument, VerboseError<&str>> {
    #[derive(Clone, Copy)]
    enum Sign {
        Plus,
        Minus,
    }
    use Sign::{Minus, Plus};

    let (rest, reg) = parse_register(input)?;

    let (rest, sign) = alt((
        value(Plus, char('+')),  // "%a + …"
        value(Minus, char('-')), // "%a - …"
    ))
    .parse(rest)?;

    let start = rest;
    let (rest, mut expr) = parse_expression(rest)?;

    if let Minus = sign {
        expr = ExpressionNode::Invert(
            Box::new(expr).with_location(input.offset(start)..input.offset(rest)),
        );
    }

    Ok((rest, Argument::Indexed(reg, expr)))
}

fn parse_address_inner(input: &str) -> IResult<&str, Argument, VerboseError<&str>> {
    alt((
        map(parse_expression, Argument::Direct),
        parse_indexed,
        map(parse_register, Argument::Indirect),
    ))
    .parse(input)
}

fn parse_address(input: &str) -> Result<Argument, VerboseError<&str>> {
    let (_, ret) = all_consuming(parse_address_inner).parse(input).finish()?;
    Ok(ret)
}

#[derive(Debug, Clone)]
pub enum AssignmentTarget {
    Address(ExpressionNode),
    Register(Reg),
}

impl FromStr for AssignmentTarget {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_assignment_target(s).map_err(|e| ParseError(convert_error(s, e)))
    }
}

fn parse_assignment_target_inner(
    input: &str,
) -> IResult<&str, AssignmentTarget, VerboseError<&str>> {
    alt((
        map(parse_expression, AssignmentTarget::Address),
        map(parse_register, AssignmentTarget::Register),
    ))
    .parse(input)
}

fn parse_assignment_target(input: &str) -> Result<AssignmentTarget, VerboseError<&str>> {
    let (_, ret) = all_consuming(parse_assignment_target_inner)
        .parse(input)
        .finish()?;
    Ok(ret)
}
