//! Parse simple const expressions and calculate them on the fly.
//!
//! The grammar of expressions is defined as such:
//!
//! ```text
//! ConstExpr := Or
//!
//! Literal := Number literal (decimal, hex, octal or binary)
//! Or      := And ('|' And)*
//! And     := Shift ('&' Shift)*
//! Shift   := Sum ('<<' Sum | '>>' Sum)?
//! Sum     := Mul ('+' Mul | '-' Mul)*
//! Mul     := Unary ('*' Unary | '/' Unary)*
//! Unary   := Expr | '-' Expr | '~' Expr
//! Expr    := Literal | '(' ConstExpr ')'
//! ```
//!
//! All the calculation is done with the [`Value`](type.Value.html) type, then converted down using the
//! `TryFrom` trait.

use std::convert::TryFrom;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, space0},
    combinator::{map, map_res, opt, value},
    multi::fold_many0,
    sequence::preceded,
    IResult,
};

use super::literal::parse_literal;

/// The type of value used throughout the calculation
pub type Value = i64;

#[doc(hidden)]
fn parse_or_rec(input: &str) -> IResult<&str, Value> {
    let (input, _) = space0(input)?;
    let (input, _) = char('|')(input)?;
    let (input, _) = space0(input)?;
    parse_and(input)
}

/// Parse a bitwise "or" operation
fn parse_or(input: &str) -> IResult<&str, Value> {
    let (input, value) = parse_and(input)?;
    fold_many0(parse_or_rec, value, |value, arg| value | arg)(input)
}

#[doc(hidden)]
fn parse_and_rec(input: &str) -> IResult<&str, Value> {
    let (input, _) = space0(input)?;
    let (input, _) = char('&')(input)?;
    let (input, _) = space0(input)?;
    parse_shift(input)
}

/// Parse a bitwise "and" operation
fn parse_and(input: &str) -> IResult<&str, Value> {
    let (input, value) = parse_shift(input)?;
    fold_many0(parse_and_rec, value, |value, arg| value & arg)(input)
}

/// Represents a bit-shift operation direction
#[derive(Clone)]
enum ShiftOp {
    /// Shift to the right (`>>`)
    Right,
    /// Shift to the left (`<<`)
    Left,
}

#[doc(hidden)]
fn parse_shift_rec(input: &str) -> IResult<&str, (ShiftOp, Value)> {
    let (input, _) = space0(input)?;
    let (input, op) = alt((
        value(ShiftOp::Right, tag(">>")),
        value(ShiftOp::Left, tag("<<")),
    ))(input)?;
    let (input, _) = space0(input)?;
    let (input, value) = parse_sum(input)?;
    Ok((input, (op, value)))
}

/// Parse a bitshift operation
fn parse_shift(input: &str) -> IResult<&str, Value> {
    let (input, value) = parse_sum(input)?;
    let (input, op) = opt(parse_shift_rec)(input)?;

    let value = match op {
        Some((ShiftOp::Right, arg)) => value >> arg,
        Some((ShiftOp::Left, arg)) => value << arg,
        None => value,
    };

    Ok((input, value))
}

/// Represents a sum/sub operation
#[derive(Clone)]
enum SumOp {
    Sum,
    Sub,
}

#[doc(hidden)]
fn parse_sum_rec(input: &str) -> IResult<&str, (SumOp, Value)> {
    let (input, _) = space0(input)?;
    let (input, op) = alt((
        value(SumOp::Sum, char('+')), // Add
        value(SumOp::Sub, char('-')), // Substract
    ))(input)?;
    let (input, _) = space0(input)?;
    let (input, value) = parse_mul(input)?;
    Ok((input, (op, value)))
}

/// Parse a sum/sub operation
fn parse_sum(input: &str) -> IResult<&str, Value> {
    let (input, value) = parse_mul(input)?;
    fold_many0(parse_sum_rec, value, |value, (op, arg)| match op {
        SumOp::Sum => value + arg,
        SumOp::Sub => value - arg,
    })(input)
}

/// Represents a multiply/divide operation
#[derive(Clone)]
enum MulOp {
    Mul,
    Div,
}

#[doc(hidden)]
fn parse_mul_rec(input: &str) -> IResult<&str, (MulOp, Value)> {
    let (input, _) = space0(input)?;
    let (input, op) = alt((
        value(MulOp::Mul, char('*')), // Multiply
        value(MulOp::Div, char('/')), // Divide
    ))(input)?;
    let (input, _) = space0(input)?;
    let (input, value) = parse_unary(input)?;
    Ok((input, (op, value)))
}

/// Parse a multiply/divide operation
fn parse_mul(input: &str) -> IResult<&str, Value> {
    let (input, value) = parse_unary(input)?;
    fold_many0(parse_mul_rec, value, |value, (op, arg)| match op {
        MulOp::Mul => value * arg,
        MulOp::Div => value / arg,
    })(input)
}

/// Parse unary operations (negation and bit inversion)
fn parse_unary(input: &str) -> IResult<&str, Value> {
    let (input, _) = space0(input)?;
    // TODO: bit inversion is tricky because we're not supposed to know the word length here. It's
    // a bit opiniated, but for now it tries casting down to u16 before negating.
    alt((
        map(preceded(char('-'), parse_atom), |v| -v),
        map_res(preceded(char('~'), parse_atom), |v| {
            u16::try_from(v) // Try casting it down to u16
                .map(|v| !v) // Invert the bits
                .map(|v| v as _) // Cast it back up
        }),
        parse_atom,
    ))(input)
}

/// Parse an atom of an expression: either a literal or a full expression within parenthesis
fn parse_atom(input: &str) -> IResult<&str, Value> {
    let (input, _) = space0(input)?;
    alt((map(parse_literal, |v| v as Value), parse_parenthesis))(input)
}

/// Parse an expression surrounded by parenthesis
fn parse_parenthesis(input: &str) -> IResult<&str, Value> {
    let (input, _) = char('(')(input)?;
    let (input, _) = space0(input)?;
    let (input, value) = parse_or(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, value))
}

/// Parse a const expression, casting the value at the end
pub fn parse_const_expression<T: TryFrom<Value>>(input: &str) -> IResult<&str, T> {
    // At the end, we try converting the Value (i64) back to T (mostly u16 or i16)
    map_res(parse_or, TryFrom::try_from)(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn calculation_test() {
        assert_eq!(parse_const_expression("1 + 2"), Ok(("", 3)));
        assert_eq!(parse_const_expression("-3"), Ok(("", -3)));
        assert_eq!(parse_const_expression("5+2 * 3"), Ok(("", 11)));
        assert_eq!(parse_const_expression("(5 + 2) * 3"), Ok(("", 21)));
        assert_eq!(parse_const_expression("0xFF * 2"), Ok(("", 0x1FE)));
        assert_eq!(parse_const_expression("0x0F <<4"), Ok(("", 0xF0)));
        assert_eq!(parse_const_expression("0xF0>> 4"), Ok(("", 0x0F)));
        assert_eq!(parse_const_expression("0xAF & 0xF0"), Ok(("", 0xA0)));
        assert_eq!(parse_const_expression("0x0F | 0xF0"), Ok(("", 0xFF)));
    }
}
