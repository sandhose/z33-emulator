use std::str::FromStr;

use nom::branch::alt;
use nom::character::complete::char;
use nom::combinator::{all_consuming, map, value};
use nom::error::{convert_error, VerboseError};
use nom::{Finish, IResult, Offset};
use thiserror::Error;
use z33_emulator::constants as C;
use z33_emulator::parser::location::Locatable;
use z33_emulator::parser::{parse_expression, parse_register, ExpressionContext, ExpressionNode};
use z33_emulator::runtime::{Computer, ExtractValue, Reg};

#[derive(Debug, Clone)]
pub enum Address {
    Direct(ExpressionNode),
    Indirect(Reg),
    Indexed(Reg, ExpressionNode),
}

impl Address {
    pub fn evaluate<Ctx: ExpressionContext>(
        self,
        computer: &Computer,
        context: &Ctx,
    ) -> Result<C::Address, anyhow::Error> {
        let node = match self {
            Address::Direct(node) => node,
            Address::Indirect(reg) => {
                ExpressionNode::Literal(i128::from(reg.extract_address(computer)?))
            }
            Address::Indexed(reg, node) => ExpressionNode::Sum(
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
pub struct ParseAddressError(String);

impl FromStr for Address {
    type Err = ParseAddressError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_address(s).map_err(|e| ParseAddressError(convert_error(s, e)))
    }
}

fn parse_indexed(input: &str) -> IResult<&str, Address, VerboseError<&str>> {
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
    ))(rest)?;

    let start = rest;
    let (rest, mut expr) = parse_expression(rest)?;

    if let Minus = sign {
        expr = ExpressionNode::Invert(
            Box::new(expr).with_location(input.offset(start)..input.offset(rest)),
        );
    };

    Ok((rest, Address::Indexed(reg, expr)))
}

fn parse_address_inner(input: &str) -> IResult<&str, Address, VerboseError<&str>> {
    alt((
        map(parse_expression, Address::Direct),
        parse_indexed,
        map(parse_register, Address::Indirect),
    ))(input)
}

fn parse_address(input: &str) -> Result<Address, VerboseError<&str>> {
    let (_, ret) = all_consuming(parse_address_inner)(input).finish()?;
    Ok(ret)
}
