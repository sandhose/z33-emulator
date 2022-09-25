use std::str::FromStr;

use nom::{
    branch::alt,
    character::complete::char,
    combinator::{all_consuming, map, value},
    error::{convert_error, VerboseError},
    Finish, IResult,
};
use thiserror::Error;

use z33_emulator::{
    constants as C,
    parser::{
        location::Locatable, parse_expression, parse_register, ExpressionContext, ExpressionNode,
    },
    runtime::{Computer, ExtractValue, Reg},
};

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
                .with_location(()),
                Box::new(node).with_location(()),
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
        expr = ExpressionNode::Invert(Box::new(expr).with_location((input, start, rest)));
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
