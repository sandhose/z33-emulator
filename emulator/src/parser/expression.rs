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
//! All the calculation is done with the [`Value`](type.Value.html) type, then
//! converted down using the `TryFrom` trait.

use std::convert::{TryFrom, TryInto};
use std::ops::Range;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, space0};
use nom::combinator::{cut, map, not, opt, value};
use nom::error::context;
use nom::{IResult, Offset};
use thiserror::Error;

use super::literal::parse_number_literal;
use super::location::{Locatable, Located};
use super::precedence::Precedence;
use super::{parse_identifier, ParseError};
use crate::ast::{AstNode, NodeKind};

type ChildNode = Located<Box<Node>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    /// a | b
    BinaryOr(ChildNode, ChildNode),

    /// a & b
    BinaryAnd(ChildNode, ChildNode),

    /// a << b
    LeftShift(ChildNode, ChildNode),

    /// a >> b
    RightShift(ChildNode, ChildNode),

    /// a + b
    Sum(ChildNode, ChildNode),

    /// a - b
    Substract(ChildNode, ChildNode),

    /// a * b
    Multiply(ChildNode, ChildNode),

    /// a / b
    Divide(ChildNode, ChildNode),

    /// -a
    Invert(ChildNode),

    /// ~a
    BinaryNot(ChildNode),

    /// A literal value
    Literal(Value),

    /// A reference to a variable
    Variable(String),
}

impl AstNode for Node {
    fn kind(&self) -> crate::ast::NodeKind {
        match self {
            Node::BinaryOr(_, _) => NodeKind::ExpressionBinaryOr,
            Node::BinaryAnd(_, _) => NodeKind::ExpressionBinaryAnd,
            Node::LeftShift(_, _) => NodeKind::ExpressionLeftShift,
            Node::RightShift(_, _) => NodeKind::ExpressionRightShift,
            Node::Sum(_, _) => NodeKind::ExpressionSum,
            Node::Substract(_, _) => NodeKind::ExpressionSubstract,
            Node::Multiply(_, _) => NodeKind::ExpressionMultiply,
            Node::Divide(_, _) => NodeKind::ExpressionDivide,
            Node::Invert(_) => NodeKind::ExpressionInvert,
            Node::BinaryNot(_) => NodeKind::ExpressionBinaryNot,
            Node::Literal(_) => NodeKind::ExpressionLiteral,
            Node::Variable(_) => NodeKind::ExpressionVariable,
        }
    }

    fn children(&self) -> Vec<crate::ast::Node> {
        match self {
            Node::BinaryOr(a, b)
            | Node::BinaryAnd(a, b)
            | Node::LeftShift(a, b)
            | Node::RightShift(a, b)
            | Node::Sum(a, b)
            | Node::Substract(a, b)
            | Node::Multiply(a, b)
            | Node::Divide(a, b) => vec![a.to_node(), b.to_node()],
            Node::Invert(a) | Node::BinaryNot(a) => vec![a.to_node()],
            Node::Variable(_) | Node::Literal(_) => Vec::new(),
        }
    }

    fn content(&self) -> Option<String> {
        match self {
            Node::Literal(l) => Some(format!("{l}")),
            Node::Variable(v) => Some(v.clone()),
            _ => None,
        }
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.sign_plus() {
            // Special case for indexed arguments
            match self {
                Node::Invert(a) => write!(f, "- {}", a.inner),
                n => write!(f, "+ {n}"),
            }
        } else {
            match self {
                Node::BinaryOr(a, b) => write!(
                    f,
                    "{} | {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                Node::BinaryAnd(a, b) => write!(
                    f,
                    "{} & {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                Node::LeftShift(a, b) => write!(
                    f,
                    "{} << {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                Node::RightShift(a, b) => write!(
                    f,
                    "{} >> {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                Node::Sum(a, b) => write!(
                    f,
                    "{} + {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                Node::Substract(a, b) => write!(
                    f,
                    "{} - {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                Node::Multiply(a, b) => write!(
                    f,
                    "{} * {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                Node::Divide(a, b) => write!(
                    f,
                    "{} / {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                Node::Invert(a) => write!(f, "-{}", a.inner.with_parent(self)),
                Node::BinaryNot(a) => write!(f, "~{}", a.inner.with_parent(self)),
                Node::Literal(a) => write!(f, "{a}"),
                Node::Variable(a) => write!(f, "{a}"),
            }
        }
    }
}

impl Node {
    fn offset(self, offset: usize) -> Self {
        match self {
            Node::BinaryOr(a, b) => Node::BinaryOr(a.offset(offset), b.offset(offset)),
            Node::BinaryAnd(a, b) => Node::BinaryAnd(a.offset(offset), b.offset(offset)),
            Node::LeftShift(a, b) => Node::LeftShift(a.offset(offset), b.offset(offset)),
            Node::RightShift(a, b) => Node::RightShift(a.offset(offset), b.offset(offset)),
            Node::Sum(a, b) => Node::Sum(a.offset(offset), b.offset(offset)),
            Node::Substract(a, b) => Node::Substract(a.offset(offset), b.offset(offset)),
            Node::Multiply(a, b) => Node::Multiply(a.offset(offset), b.offset(offset)),
            Node::Divide(a, b) => Node::Divide(a.offset(offset), b.offset(offset)),
            Node::Invert(a) => Node::Invert(a.offset(offset)),
            Node::BinaryNot(a) => Node::BinaryNot(a.offset(offset)),
            Node::Literal(a) => Node::Literal(a),
            Node::Variable(a) => Node::Variable(a),
        }
    }
}

pub trait Context {
    // TODO: use something else than Value
    fn resolve_variable(&self, variable: &str) -> Option<Value>;
}

pub(crate) struct EmptyContext;
impl Context for EmptyContext {
    fn resolve_variable(&self, _variable: &str) -> Option<Value> {
        None
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum EvaluationError {
    #[error("undefined variable {variable:?}")]
    UndefinedVariable { variable: String },

    #[error("could not downcast value")]
    Downcast,

    #[error("invalid bitshift")]
    Shift,

    #[error("overflow")]
    Overflow,

    #[error("divide by zero")]
    DivByZero,

    #[error("evaluation")]
    Expression {
        location: Range<usize>,
        inner: Box<EvaluationError>,
    },
}

impl Node {
    /// Evaluate a constant expression with a given context, returning a value
    ///
    /// # Errors
    ///
    /// This function will return an error if the evaluation fails.
    pub fn evaluate<C: Context, V: TryFrom<Value>>(
        &self,
        context: &C,
    ) -> Result<V, EvaluationError> {
        let value: Value =
            match self {
                Node::BinaryOr(left, right) => {
                    let left: Value = left.evaluate(context)?;
                    let right: Value = right.evaluate(context)?;
                    left | right
                }

                Node::BinaryAnd(left, right) => {
                    let left: Value = left.evaluate(context)?;
                    let right: Value = right.evaluate(context)?;
                    left & right
                }

                Node::LeftShift(left, right) => {
                    let left: Value = left.evaluate(context)?;
                    let right: Value = right.evaluate(context)?;
                    left.checked_shl(right.try_into().map_err(|_| EvaluationError::Downcast)?)
                        .ok_or(EvaluationError::Shift)?
                }

                Node::RightShift(left, right) => {
                    let left: Value = left.evaluate(context)?;
                    let right: Value = right.evaluate(context)?;
                    left.checked_shr(right.try_into().map_err(|_| EvaluationError::Downcast)?)
                        .ok_or(EvaluationError::Shift)?
                }

                Node::Sum(left, right) => {
                    let left: Value = left.evaluate(context)?;
                    let right: Value = right.evaluate(context)?;
                    left.checked_add(right).ok_or(EvaluationError::Overflow)?
                }

                Node::Substract(left, right) => {
                    let left: Value = left.evaluate(context)?;
                    let right: Value = right.evaluate(context)?;
                    left.checked_sub(right).ok_or(EvaluationError::Overflow)?
                }

                Node::Multiply(left, right) => {
                    let left: Value = left.evaluate(context)?;
                    let right: Value = right.evaluate(context)?;
                    left.checked_mul(right).ok_or(EvaluationError::Overflow)?
                }

                Node::Divide(left, right) => {
                    let left: Value = left.evaluate(context)?;
                    let right: Value = right.evaluate(context)?;
                    left.checked_div(right).ok_or(EvaluationError::DivByZero)?
                }

                Node::Invert(operand) => {
                    let operand: Value = operand.evaluate(context)?;
                    operand.checked_neg().ok_or(EvaluationError::Overflow)?
                }

                Node::BinaryNot(operand) => {
                    let _operand: Value = operand.inner.evaluate(context)?;
                    // TODO: bit inversion is tricky because we're not supposed to know the word
                    // length here. It's a bit opiniated, but for now it tries
                    // casting down to u16 before negating.

                    /*
                    u16::try_from(v) // try casting it down to u16
                        .map(|v| !v) // invert the bits
                        .map(|v| v as _) // cast it back up
                    */
                    todo!()
                }

                Node::Literal(value) => *value,

                Node::Variable(variable) => context.resolve_variable(variable).ok_or(
                    EvaluationError::UndefinedVariable {
                        variable: variable.clone(),
                    },
                )?,
            };

        V::try_from(value).map_err(|_| EvaluationError::Downcast)
    }
}

impl ChildNode {
    /// Evaluate an expression with a given context, returning a value
    ///
    /// This evaluate the inner node and attach the location to the error
    ///
    /// # Errors
    ///
    /// This function will return an error if the evaluation fails.
    pub fn evaluate<C: Context, V: TryFrom<Value>>(
        &self,
        context: &C,
    ) -> Result<V, EvaluationError> {
        self.inner
            .evaluate(context)
            .map_err(|source| EvaluationError::Expression {
                location: self.location.clone(),
                inner: Box::new(source),
            })
    }
}

/// The type of value used throughout the calculation
pub type Value = i128;

#[doc(hidden)]
fn parse_or_rec<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ChildNode, Error> {
    let (rest, _) = space0(input)?;
    let (rest, _) = char('|')(rest)?;
    // Check if it's not a "||" to avoid conflict with boolean operations
    let (rest, ()) = not(char('|'))(rest)?;
    let (rest, _) = space0(rest)?;

    cut(move |rest: &'a str| {
        let start = rest;
        let (rest, node) = parse_and(rest)?;
        let node = Box::new(node).with_location(input.offset(start)..input.offset(rest));
        Ok((rest, node))
    })(rest)
}

/// Parse a bitwise "or" operation
fn parse_or<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Node, Error> {
    let (mut cursor, mut node) = parse_and(input)?;

    while let (rest, Some(right)) = opt(parse_or_rec)(cursor)? {
        let offset = input.offset(cursor);
        // Wrap the "left" node with location information
        let left = Box::new(node).with_location(0..offset);

        // The location embed in the `right` node is relative to the cursor, so we need
        // to offset it by the offset between the input and the cursor
        let right = right.offset(offset);

        node = Node::BinaryOr(left, right);
        cursor = rest;
    }

    Ok((cursor, node))
}

#[doc(hidden)]
fn parse_and_rec<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ChildNode, Error> {
    let (rest, _) = space0(input)?;
    let (rest, _) = char('&')(rest)?;
    // Check if it's not a "&&" to avoid conflict with boolean operations
    let (rest, ()) = not(char('&'))(rest)?;
    let (rest, _) = space0(rest)?;

    cut(move |rest: &'a str| {
        let start = rest;
        let (rest, node) = parse_shift(rest)?;
        let node = Box::new(node).with_location(input.offset(start)..input.offset(rest));
        Ok((rest, node))
    })(rest)
}

/// Parse a bitwise "and" operation
fn parse_and<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Node, Error> {
    let (mut cursor, mut node) = parse_shift(input)?;

    while let (rest, Some(right)) = opt(parse_and_rec)(cursor)? {
        let offset = input.offset(cursor);
        // Wrap the "left" node with location information
        let left = Box::new(node).with_location(0..offset);

        // The location embed in the `right` node is relative to the cursor, so we need
        // to offset it by the offset between the input and the cursor
        let right = right.offset(offset);

        node = Node::BinaryAnd(left, right);
        cursor = rest;
    }

    Ok((cursor, node))
}

/// Represents a bit-shift operation direction
#[derive(Clone, Copy)]
enum ShiftOp {
    /// Shift to the right (`>>`)
    Right,
    /// Shift to the left (`<<`)
    Left,
}

#[doc(hidden)]
fn parse_shift_rec<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (ShiftOp, ChildNode), Error> {
    let (rest, _) = space0(input)?;
    let (rest, op) = alt((
        context(">>", value(ShiftOp::Right, tag(">>"))),
        context("<<", value(ShiftOp::Left, tag("<<"))),
    ))(rest)?;
    let (rest, _) = space0(rest)?;

    cut(move |rest: &'a str| {
        let start = rest;
        let (rest, node) = parse_sum(rest)?;
        let node = Box::new(node).with_location(input.offset(start)..input.offset(rest));
        Ok((rest, (op, node)))
    })(rest)
}

/// Parse a bitshift operation
fn parse_shift<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Node, Error> {
    let (mut cursor, mut node) = parse_sum(input)?;

    if let (rest, Some((op, right))) = opt(parse_shift_rec)(cursor)? {
        let offset = input.offset(cursor);
        // Wrap the "left" node with location information
        let left = Box::new(node).with_location(0..offset);

        // The location embed in the `right` node is relative to the cursor, so we need
        // to offset it by the offset between the input and the cursor
        let right = right.offset(offset);

        node = match op {
            ShiftOp::Left => Node::LeftShift(left, right),
            ShiftOp::Right => Node::RightShift(left, right),
        };

        cursor = rest;
    }

    Ok((cursor, node))
}

/// Represents a sum/sub operation
#[derive(Clone, Copy)]
enum SumOp {
    Sum,
    Sub,
}

#[doc(hidden)]
fn parse_sum_rec<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (SumOp, ChildNode), Error> {
    let (rest, _) = space0(input)?;
    let (rest, op) = alt((
        value(SumOp::Sum, char('+')), // Add
        value(SumOp::Sub, char('-')), // Substract
    ))(rest)?;
    let (rest, _) = space0(rest)?;

    cut(move |rest: &'a str| {
        let start = rest;
        let (rest, node) = parse_mul(rest)?;
        let node = Box::new(node).with_location(input.offset(start)..input.offset(rest));
        Ok((rest, (op, node)))
    })(rest)
}

/// Parse a sum/sub operation
fn parse_sum<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Node, Error> {
    let (mut cursor, mut node) = parse_mul(input)?;

    while let (rest, Some((op, right))) = opt(parse_sum_rec)(cursor)? {
        let offset = input.offset(cursor);
        // Wrap the "left" node with location information
        let left = Box::new(node).with_location(0..offset);

        // The location embed in the `right` node is relative to the cursor, so we need
        // to offset it by the offset between the input and the cursor
        let right = right.offset(offset);

        node = match op {
            SumOp::Sum => Node::Sum(left, right),
            SumOp::Sub => Node::Substract(left, right),
        };

        cursor = rest;
    }

    Ok((cursor, node))
}

/// Represents a multiply/divide operation
#[derive(Clone, Copy)]
enum MulOp {
    Mul,
    Div,
}

#[doc(hidden)]
fn parse_mul_rec<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (MulOp, ChildNode), Error> {
    let (rest, _) = space0(input)?;
    let (rest, op) = alt((
        value(MulOp::Mul, char('*')), // Multiply
        value(MulOp::Div, char('/')), // Divide
    ))(rest)?;
    let (rest, _) = space0(rest)?;

    cut(move |rest: &'a str| {
        let start = rest;
        let (rest, node) = parse_unary(rest)?;
        let node = Box::new(node).with_location(input.offset(start)..input.offset(rest));
        Ok((rest, (op, node)))
    })(rest)
}

/// Parse a multiply/divide operation
fn parse_mul<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Node, Error> {
    let (mut cursor, mut node) = parse_unary(input)?;

    while let (rest, Some((op, right))) = opt(parse_mul_rec)(cursor)? {
        let offset = input.offset(cursor);
        // Wrap the "left" node with location information
        let left = Box::new(node).with_location(0..offset);

        // The location embed in the `right` node is relative to the cursor, so we need
        // to offset it by the offset between the input and the cursor
        let right = right.offset(offset);

        node = match op {
            MulOp::Mul => Node::Multiply(left, right),
            MulOp::Div => Node::Divide(left, right),
        };

        cursor = rest;
    }

    Ok((cursor, node))
}

fn parse_invert<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Node, Error> {
    let (rest, _) = char('-')(input)?;
    let (rest, _) = space0(rest)?;

    cut(move |rest: &'a str| {
        let start = rest;
        let (rest, node) = parse_atom(rest)?;
        let node = Box::new(node).with_location(input.offset(start)..input.offset(rest));
        Ok((rest, Node::Invert(node)))
    })(rest)
}

fn parse_binary_not<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Node, Error> {
    let (rest, _) = char('~')(input)?;
    let (rest, _) = space0(rest)?;

    cut(move |rest: &'a str| {
        let start = rest;
        let (rest, node) = parse_atom(rest)?;
        let node = Box::new(node).with_location(input.offset(start)..input.offset(rest));
        Ok((rest, Node::BinaryNot(node)))
    })(rest)
}

/// Parse unary operations (negation and bit inversion)
fn parse_unary<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Node, Error> {
    alt((parse_invert, parse_binary_not, parse_atom))(input)
}

/// Parse an atom of an expression: either a literal or a full expression within
/// parenthesis
fn parse_atom<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Node, Error> {
    alt((
        context(
            "number literal",
            map(parse_number_literal, |v| Node::Literal(Value::from(v))),
        ),
        context(
            "identifier",
            map(parse_identifier, |i| Node::Variable(i.into())),
        ),
        parse_parenthesis,
    ))(input)
}

/// Parse an expression surrounded by parenthesis
fn parse_parenthesis<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Node, Error> {
    let (rest, _) = char('(')(input)?;
    let (rest, _) = space0(rest)?;

    cut(move |rest: &'a str| {
        let offset = input.offset(rest);
        let (rest, value) = parse_expression(rest)?;
        // This offsets the child nodes location to compensate the parenthesis
        let value = value.offset(offset);

        let (rest, _) = space0(rest)?;
        let (rest, _) = char(')')(rest)?;
        Ok((rest, value))
    })(rest)
}

/// Parse an expression, returning its AST
///
/// # Errors
///
/// This function will return an error if the expression is invalid
pub fn parse_expression<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Node, Error> {
    parse_or(input)
}

#[cfg(test)]
mod tests {
    use nom::Finish;

    use super::*;

    #[track_caller]
    fn evaluate(res: IResult<&str, Node>) -> i128 {
        let (rest, node) = res.finish().unwrap();
        assert_eq!(rest, "");
        node.evaluate(&EmptyContext).unwrap()
    }

    #[test]
    fn calculation_test() {
        assert_eq!(evaluate(parse_expression("1 + 2")), 3);
        assert_eq!(evaluate(parse_expression("-3")), -3);
        assert_eq!(evaluate(parse_expression("5+2 * 3")), 11);
        assert_eq!(evaluate(parse_expression("(5 + 2) * 3")), 21);
        assert_eq!(evaluate(parse_expression("0xFF * 2")), 0x1FE);
        assert_eq!(evaluate(parse_expression("0x0F <<4")), 0xF0);
        assert_eq!(evaluate(parse_expression("0xF0>> 4")), 0x0F);
        assert_eq!(evaluate(parse_expression("0xAF & 0xF0")), 0xA0);
        assert_eq!(evaluate(parse_expression("0x0F | 0xF0")), 0xFF);
    }
}
