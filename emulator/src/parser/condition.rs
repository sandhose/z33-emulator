//! Parse conditions and evaluate them on the fly.
//!
//! The grammar of expressions is defined as such:
//!
//! ```text
//! Condition := LogicalOr
//!
//! LogicalOr        := LogicalAnd ('||' LogicalAnd)*
//! LogicalAnd       := LogicalExpr ('&&' LogicalExpr)*
//! LogicalExpr      := Atom | '!' Atom
//! Literal          := 'true' | 'false'
//! Atom             := NumberComparison | '(' Condition ')' | Literal
//! NumberComparison := ConstExpr ('==' | '!=' | '>=' | '>' | '<=' | '<') ConstExpr
//! ```
//!
//! Note: to simplify a bit, it might accept some weird conditions.
//! For example, `!4 > 3` is evaluated like `!(4 > 3)`.

use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    character::complete::{char, space0},
    combinator::{map, value},
    multi::fold_many0,
    sequence::preceded,
    IResult,
};

use super::expression::{parse_const_expression, Value};

pub fn parse_condition(input: &str) -> IResult<&str, bool> {
    let (input, _) = space0(input)?;
    parse_logical_or(input)
}

#[derive(Clone)]
enum Comparison {
    Equal,
    NotEqual,
    GreaterOrEqual,
    GreaterThan,
    LesserOrEqual,
    LesserThan,
}

impl Comparison {
    fn evaluate(self, a: Value, b: Value) -> bool {
        match self {
            Comparison::Equal => a == b,
            Comparison::NotEqual => a != b,
            Comparison::GreaterOrEqual => a >= b,
            Comparison::GreaterThan => a > b,
            Comparison::LesserOrEqual => a <= b,
            Comparison::LesserThan => a < b,
        }
    }
}

#[doc(hidden)]
fn parse_logical_or_rec(input: &str) -> IResult<&str, bool> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("||")(input)?;
    let (input, _) = space0(input)?;
    parse_logical_and(input)
}

/// Parse a logical "or" operation
fn parse_logical_or(input: &str) -> IResult<&str, bool> {
    let (input, value) = parse_logical_and(input)?;
    fold_many0(parse_logical_or_rec, value, |value, arg| value || arg)(input)
}

#[doc(hidden)]
fn parse_logical_and_rec(input: &str) -> IResult<&str, bool> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("&&")(input)?;
    let (input, _) = space0(input)?;
    parse_logical_expression(input)
}

/// Parse a logical "and" operation
fn parse_logical_and(input: &str) -> IResult<&str, bool> {
    let (input, value) = parse_logical_expression(input)?;
    fold_many0(parse_logical_and_rec, value, |value, arg| value && arg)(input)
}

fn parse_logical_expression(input: &str) -> IResult<&str, bool> {
    alt((
        map(preceded(char('!'), parse_atom), |v| !v), // Negation
        parse_atom,
    ))(input)
}

fn parse_atom(input: &str) -> IResult<&str, bool> {
    let (input, _) = space0(input)?;
    alt((parse_number_comparison, parse_parenthesis, parse_literal))(input)
}

fn parse_number_comparison(input: &str) -> IResult<&str, bool> {
    let (input, _) = space0(input)?;
    let (input, a) = parse_const_expression(input)?;
    let (input, _) = space0(input)?;
    let (input, op) = alt((
        value(Comparison::Equal, tag("==")),
        value(Comparison::NotEqual, tag("!=")),
        value(Comparison::GreaterOrEqual, tag(">=")),
        value(Comparison::GreaterThan, tag(">")),
        value(Comparison::LesserOrEqual, tag("<=")),
        value(Comparison::LesserThan, tag("<")),
    ))(input)?;
    let (input, _) = space0(input)?;
    let (input, b) = parse_const_expression(input)?;
    Ok((input, op.evaluate(a, b)))
}

fn parse_literal(input: &str) -> IResult<&str, bool> {
    alt((
        value(true, tag_no_case("true")),
        value(false, tag_no_case("false")),
    ))(input)
}

fn parse_parenthesis(input: &str) -> IResult<&str, bool> {
    let (input, _) = char('(')(input)?;
    let (input, _) = space0(input)?;
    let (input, value) = parse_condition(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, value))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn number_comparison_test() {
        assert_eq!(parse_number_comparison("5 > 3"), Ok(("", true)));
        assert_eq!(parse_number_comparison("5 >= 3"), Ok(("", true)));
        assert_eq!(parse_number_comparison("3 < 5"), Ok(("", true)));
        assert_eq!(parse_number_comparison("3 <= 5"), Ok(("", true)));
        assert_eq!(parse_number_comparison("3 != 5"), Ok(("", true)));
        assert_eq!(parse_number_comparison("5 == 5"), Ok(("", true)));
        assert_eq!(parse_number_comparison("3 > 5"), Ok(("", false)));
        assert_eq!(parse_number_comparison("3 >= 5"), Ok(("", false)));
        assert_eq!(parse_number_comparison("5 < 3"), Ok(("", false)));
        assert_eq!(parse_number_comparison("5 <= 3"), Ok(("", false)));
        assert_eq!(parse_number_comparison("5 != 5"), Ok(("", false)));
        assert_eq!(parse_number_comparison("3 == 5"), Ok(("", false)));
        assert_eq!(
            parse_number_comparison("3 * 12 == 3 * 3 + 54 / 2"),
            Ok(("", true))
        );
    }

    #[test]
    fn logical_operations_test() {
        assert_eq!(parse_condition("true && true"), Ok(("", true)));
        assert_eq!(parse_condition("true && false"), Ok(("", false)));
        assert_eq!(parse_condition("true || false"), Ok(("", true)));
        assert_eq!(parse_condition("false || false"), Ok(("", false)));
        assert_eq!(
            parse_condition("(true || false) && (false || true)"),
            Ok(("", true))
        );
        assert_eq!(
            parse_condition("true || false && false || true"),
            Ok(("", true))
        );
        assert_eq!(
            parse_condition("!(true || false) && (false || true)"),
            Ok(("", false))
        );
    }
}
