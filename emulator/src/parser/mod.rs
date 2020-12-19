//! Program parsing logic
//!
//! This module is splitted in multiple submodules to make things easier to read. The parsing is
//! handled by the `nom` library.

use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_while1},
    character::complete::one_of,
    combinator::{map, value, verify},
    error::context,
    IResult,
};

use crate::processor::{Address, Reg};

pub(crate) mod condition;
pub(crate) mod expression;
pub(crate) mod line;
pub(crate) mod literal;
pub(crate) mod value;

pub use condition::parse_condition;
pub use expression::{
    parse_const_expression, parse_expression, Context as ExpressionContext,
    EvaluationError as ExpressionEvaluationError, Node as Expression,
};
pub use line::{parse_program, DirectiveArgument, Line, LineContent};
pub use literal::parse_string_literal;
pub use value::Argument;

fn parse_reg(input: &str) -> IResult<&str, Reg> {
    let (input, _) = tag("%")(input)?;
    context(
        "expected a valid register",
        alt((
            value(Reg::A, tag_no_case("a")),
            value(Reg::B, tag_no_case("b")),
            value(Reg::PC, tag_no_case("pc")),
            value(Reg::SP, tag_no_case("sp")),
            value(Reg::SR, tag_no_case("sr")),
        )),
    )(input)
}

fn parse_indexed(input: &str) -> IResult<&str, (Reg, i64)> {
    let (input, reg) = parse_reg(input)?;
    let (input, sign) = one_of("+-")(input)?;
    let (input, val): (_, i64) = parse_const_expression(input)?;

    let value = if sign == '-' { -val } else { val };

    Ok((input, (reg, value)))
}

pub(crate) fn parse_inner_address(input: &str) -> IResult<&str, Address> {
    context(
        "expected a valid address",
        alt((
            map(parse_const_expression, Address::Dir),
            map(parse_indexed, |(reg, off)| Address::Idx(reg, off)),
            map(parse_reg, Address::Ind),
        )),
    )(input)
}

fn is_identifier_char(c: char) -> bool {
    is_start_identifier_char(c) || ('0'..'9').contains(&c)
}

fn is_start_identifier_char(c: char) -> bool {
    c == '_' || ('a'..'z').contains(&c) || ('A'..'Z').contains(&c)
}

pub(crate) fn parse_identifier(input: &str) -> IResult<&str, &str> {
    verify(take_while1(is_identifier_char), |f: &str| {
        f.chars()
            .next()
            .filter(|&c| is_start_identifier_char(c))
            .is_some()
    })(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_reg_test() {
        assert_eq!(parse_reg("%a"), Ok(("", Reg::A)));
        assert_eq!(parse_reg("%b"), Ok(("", Reg::B)));
        assert_eq!(parse_reg("%pc"), Ok(("", Reg::PC)));
        assert_eq!(parse_reg("%PC"), Ok(("", Reg::PC)));
        assert_eq!(parse_reg("%sp"), Ok(("", Reg::SP)));
        assert_eq!(parse_reg("%sr"), Ok(("", Reg::SR)));
        assert!(parse_reg("%c").is_err());
    }

    #[test]
    fn parse_indexed_test() {
        assert_eq!(parse_indexed("%a+2"), Ok(("", (Reg::A, 2))));
        assert_eq!(parse_indexed("%b-5"), Ok(("", (Reg::B, -5))));
    }
}
