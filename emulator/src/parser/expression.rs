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
    combinator::{map, value},
    IResult, Offset,
};
use thiserror::Error;

use crate::ast::{AstNode, NodeKind};

use super::{
    literal::parse_number_literal,
    location::{AbsoluteLocation, Locatable, Located, RelativeLocation},
    parse_identifier,
    precedence::Precedence,
};

type ChildNode<L> = Located<Box<Node<L>>, L>;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Node<L> {
    /// a | b
    BinaryOr(ChildNode<L>, ChildNode<L>),

    /// a & b
    BinaryAnd(ChildNode<L>, ChildNode<L>),

    /// a << b
    LeftShift(ChildNode<L>, ChildNode<L>),

    /// a >> b
    RightShift(ChildNode<L>, ChildNode<L>),

    /// a + b
    Sum(ChildNode<L>, ChildNode<L>),

    /// a - b
    Substract(ChildNode<L>, ChildNode<L>),

    /// a * b
    Multiply(ChildNode<L>, ChildNode<L>),

    /// a / b
    Divide(ChildNode<L>, ChildNode<L>),

    /// -a
    Invert(ChildNode<L>),

    /// ~a
    BinaryNot(ChildNode<L>),

    /// A literal value
    Literal(Value),

    /// A reference to a variable
    Variable(String),
}

impl<L: Clone> AstNode<L> for Node<L> {
    fn kind(&self) -> crate::ast::NodeKind {
        use Node::*;
        use NodeKind::*;
        match self {
            BinaryOr(_, _) => ExpressionBinaryOr,
            BinaryAnd(_, _) => ExpressionBinaryAnd,
            LeftShift(_, _) => ExpressionLeftShift,
            RightShift(_, _) => ExpressionRightShift,
            Sum(_, _) => ExpressionSum,
            Substract(_, _) => ExpressionSubstract,
            Multiply(_, _) => ExpressionMultiply,
            Divide(_, _) => ExpressionDivide,
            Invert(_) => ExpressionInvert,
            BinaryNot(_) => ExpressionBinaryNot,
            Literal(_) => ExpressionLiteral,
            Variable(_) => ExpressionVariable,
        }
    }

    fn children(&self) -> Vec<crate::ast::Node<L>> {
        use Node::*;

        match self {
            BinaryOr(a, b) => vec![a.to_node(), b.to_node()],
            BinaryAnd(a, b) => vec![a.to_node(), b.to_node()],
            LeftShift(a, b) => vec![a.to_node(), b.to_node()],
            RightShift(a, b) => vec![a.to_node(), b.to_node()],
            Sum(a, b) => vec![a.to_node(), b.to_node()],
            Substract(a, b) => vec![a.to_node(), b.to_node()],
            Multiply(a, b) => vec![a.to_node(), b.to_node()],
            Divide(a, b) => vec![a.to_node(), b.to_node()],
            Invert(a) => vec![a.to_node()],
            BinaryNot(a) => vec![a.to_node()],
            Variable(_) | Literal(_) => Vec::new(),
        }
    }

    fn content(&self) -> Option<String> {
        use Node::*;
        match self {
            Literal(l) => Some(format!("{}", l)),
            Variable(v) => Some(v.clone()),
            _ => None,
        }
    }
}

impl<L> std::fmt::Display for Node<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Node::*;
        if f.sign_plus() {
            // Special case for indexed arguments
            match self {
                Invert(a) => write!(f, "- {}", a.inner),
                n => write!(f, "+ {}", n),
            }
        } else {
            match self {
                BinaryOr(a, b) => write!(
                    f,
                    "{} | {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                BinaryAnd(a, b) => write!(
                    f,
                    "{} & {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                LeftShift(a, b) => write!(
                    f,
                    "{} << {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                RightShift(a, b) => write!(
                    f,
                    "{} >> {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                Sum(a, b) => write!(
                    f,
                    "{} + {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                Substract(a, b) => write!(
                    f,
                    "{} - {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                Multiply(a, b) => write!(
                    f,
                    "{} * {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                Divide(a, b) => write!(
                    f,
                    "{} / {}",
                    a.inner.with_parent(self),
                    b.inner.with_parent(self)
                ),
                Invert(a) => write!(f, "-{}", a.inner.with_parent(self)),
                BinaryNot(a) => write!(f, "~{}", a.inner.with_parent(self)),
                Literal(a) => write!(f, "{}", a),
                Variable(a) => write!(f, "{}", a),
            }
        }
    }
}

impl Node<RelativeLocation> {
    #[allow(dead_code)]
    fn offset(self, offset: usize) -> Self {
        use Node::*;
        match self {
            BinaryOr(a, b) => BinaryOr(a.offset(offset), b.offset(offset)),
            BinaryAnd(a, b) => BinaryAnd(a.offset(offset), b.offset(offset)),
            LeftShift(a, b) => LeftShift(a.offset(offset), b.offset(offset)),
            RightShift(a, b) => RightShift(a.offset(offset), b.offset(offset)),
            Sum(a, b) => Sum(a.offset(offset), b.offset(offset)),
            Substract(a, b) => Substract(a.offset(offset), b.offset(offset)),
            Multiply(a, b) => Multiply(a.offset(offset), b.offset(offset)),
            Divide(a, b) => Divide(a.offset(offset), b.offset(offset)),
            Invert(a) => Invert(a.offset(offset)),
            BinaryNot(a) => BinaryNot(a.offset(offset)),
            Literal(a) => Literal(a),
            Variable(a) => Variable(a),
        }
    }

    pub(crate) fn into_absolute(self, location: &AbsoluteLocation) -> Node<AbsoluteLocation> {
        use Node::*;

        let mapper = |node: Box<Node<RelativeLocation>>, parent: &AbsoluteLocation| {
            Box::new(node.into_absolute(parent))
        };

        match self {
            BinaryOr(a, b) => BinaryOr(
                a.into_absolute(location, mapper),
                b.into_absolute(location, mapper),
            ),
            BinaryAnd(a, b) => BinaryAnd(
                a.into_absolute(location, mapper),
                b.into_absolute(location, mapper),
            ),
            LeftShift(a, b) => LeftShift(
                a.into_absolute(location, mapper),
                b.into_absolute(location, mapper),
            ),
            RightShift(a, b) => RightShift(
                a.into_absolute(location, mapper),
                b.into_absolute(location, mapper),
            ),
            Sum(a, b) => Sum(
                a.into_absolute(location, mapper),
                b.into_absolute(location, mapper),
            ),
            Substract(a, b) => Substract(
                a.into_absolute(location, mapper),
                b.into_absolute(location, mapper),
            ),
            Multiply(a, b) => Multiply(
                a.into_absolute(location, mapper),
                b.into_absolute(location, mapper),
            ),
            Divide(a, b) => Divide(
                a.into_absolute(location, mapper),
                b.into_absolute(location, mapper),
            ),
            Invert(a) => Invert(a.into_absolute(location, mapper)),
            BinaryNot(a) => BinaryNot(a.into_absolute(location, mapper)),
            Literal(a) => Literal(a),
            Variable(a) => Variable(a),
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
}

impl<L> Node<L> {
    pub fn evaluate<C: Context, V: TryFrom<Value>>(
        &self,
        context: &C,
    ) -> Result<V, EvaluationError> {
        let value: Value =
            match self {
                Node::BinaryOr(left, right) => {
                    let left: Value = left.inner.evaluate(context)?;
                    let right: Value = right.inner.evaluate(context)?;
                    left | right
                }

                Node::BinaryAnd(left, right) => {
                    let left: Value = left.inner.evaluate(context)?;
                    let right: Value = right.inner.evaluate(context)?;
                    left & right
                }

                Node::LeftShift(left, right) => {
                    let left: Value = left.inner.evaluate(context)?;
                    let right: Value = right.inner.evaluate(context)?;
                    left << right
                }

                Node::RightShift(left, right) => {
                    let left: Value = left.inner.evaluate(context)?;
                    let right: Value = right.inner.evaluate(context)?;
                    left >> right
                }

                Node::Sum(left, right) => {
                    let left: Value = left.inner.evaluate(context)?;
                    let right: Value = right.inner.evaluate(context)?;
                    left + right
                }

                Node::Substract(left, right) => {
                    let left: Value = left.inner.evaluate(context)?;
                    let right: Value = right.inner.evaluate(context)?;
                    left - right
                }

                Node::Multiply(left, right) => {
                    let left: Value = left.inner.evaluate(context)?;
                    let right: Value = right.inner.evaluate(context)?;
                    left * right
                }

                Node::Divide(left, right) => {
                    let left: Value = left.inner.evaluate(context)?;
                    let right: Value = right.inner.evaluate(context)?;
                    left / right
                }

                Node::Invert(operand) => {
                    let operand: Value = operand.inner.evaluate(context)?;
                    -operand
                }

                Node::BinaryNot(operand) => {
                    let _operand: Value = operand.inner.evaluate(context)?;
                    // TODO: bit inversion is tricky because we're not supposed to know the word length
                    // here. It's a bit opiniated, but for now it tries casting down to u16 before
                    // negating.

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

/// The type of value used throughout the calculation
pub type Value = i128;

#[doc(hidden)]
fn parse_or_rec(input: &str) -> IResult<&str, ChildNode<RelativeLocation>> {
    let (rest, _) = space0(input)?;
    let (rest, _) = char('|')(rest)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, node) = parse_and(rest)?;
    let node = Box::new(node).with_location((input.offset(start), start.offset(rest)));
    Ok((rest, node))
}

/// Parse a bitwise "or" operation
fn parse_or(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    let (mut cursor, mut node) = parse_and(input)?;

    while let Ok((rest, right)) = parse_or_rec(cursor) {
        let offset = input.offset(cursor);
        // Wrap the "left" node with location information
        let left = Box::new(node).with_location((0, offset));

        // The location embed in the `right` node is relative to the cursor, so we need to offset
        // it by the offset between the input and the cursor
        let right = right.offset(offset);

        node = Node::BinaryOr(left, right);
        cursor = rest;
    }

    Ok((cursor, node))
}

#[doc(hidden)]
fn parse_and_rec(input: &str) -> IResult<&str, ChildNode<RelativeLocation>> {
    let (rest, _) = space0(input)?;
    let (rest, _) = char('&')(rest)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, node) = parse_shift(rest)?;
    let node = Box::new(node).with_location((input.offset(start), start.offset(rest)));
    Ok((rest, node))
}

/// Parse a bitwise "and" operation
fn parse_and(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    let (mut cursor, mut node) = parse_shift(input)?;

    while let Ok((rest, right)) = parse_and_rec(cursor) {
        let offset = input.offset(cursor);
        // Wrap the "left" node with location information
        let left = Box::new(node).with_location((0, offset));

        // The location embed in the `right` node is relative to the cursor, so we need to offset
        // it by the offset between the input and the cursor
        let right = right.offset(offset);

        node = Node::BinaryAnd(left, right);
        cursor = rest;
    }

    Ok((cursor, node))
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
fn parse_shift_rec(input: &str) -> IResult<&str, (ShiftOp, ChildNode<RelativeLocation>)> {
    let (rest, _) = space0(input)?;
    let (rest, op) = alt((
        value(ShiftOp::Right, tag(">>")),
        value(ShiftOp::Left, tag("<<")),
    ))(rest)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, node) = parse_sum(rest)?;
    let node = Box::new(node).with_location((input, start, rest));
    Ok((rest, (op, node)))
}

/// Parse a bitshift operation
fn parse_shift(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    let (mut cursor, mut node) = parse_sum(input)?;

    if let Ok((rest, (op, right))) = parse_shift_rec(cursor) {
        let offset = input.offset(cursor);
        // Wrap the "left" node with location information
        let left = Box::new(node).with_location((0, offset));

        // The location embed in the `right` node is relative to the cursor, so we need to offset
        // it by the offset between the input and the cursor
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
#[derive(Clone)]
enum SumOp {
    Sum,
    Sub,
}

#[doc(hidden)]
fn parse_sum_rec(input: &str) -> IResult<&str, (SumOp, ChildNode<RelativeLocation>)> {
    let (rest, _) = space0(input)?;
    let (rest, op) = alt((
        value(SumOp::Sum, char('+')), // Add
        value(SumOp::Sub, char('-')), // Substract
    ))(rest)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, node) = parse_mul(rest)?;
    let node = Box::new(node).with_location((input, start, rest));
    Ok((rest, (op, node)))
}

/// Parse a sum/sub operation
fn parse_sum(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    let (mut cursor, mut node) = parse_mul(input)?;

    while let Ok((rest, (op, right))) = parse_sum_rec(cursor) {
        let offset = input.offset(cursor);
        // Wrap the "left" node with location information
        let left = Box::new(node).with_location((0, offset));

        // The location embed in the `right` node is relative to the cursor, so we need to offset
        // it by the offset between the input and the cursor
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
#[derive(Clone)]
enum MulOp {
    Mul,
    Div,
}

#[doc(hidden)]
fn parse_mul_rec(input: &str) -> IResult<&str, (MulOp, ChildNode<RelativeLocation>)> {
    let (rest, _) = space0(input)?;
    let (rest, op) = alt((
        value(MulOp::Mul, char('*')), // Multiply
        value(MulOp::Div, char('/')), // Divide
    ))(rest)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, node) = parse_unary(rest)?;
    let node = Box::new(node).with_location((input, start, rest));
    Ok((rest, (op, node)))
}

/// Parse a multiply/divide operation
fn parse_mul(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    let (mut cursor, mut node) = parse_unary(input)?;

    while let Ok((rest, (op, right))) = parse_mul_rec(cursor) {
        let offset = input.offset(cursor);
        // Wrap the "left" node with location information
        let left = Box::new(node).with_location((0, offset));

        // The location embed in the `right` node is relative to the cursor, so we need to offset
        // it by the offset between the input and the cursor
        let right = right.offset(offset);

        node = match op {
            MulOp::Mul => Node::Multiply(left, right),
            MulOp::Div => Node::Divide(left, right),
        };

        cursor = rest;
    }

    Ok((cursor, node))
}

fn parse_invert(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    let (rest, _) = char('-')(input)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, node) = parse_atom(rest)?;
    let node = Box::new(node).with_location((input, start, rest));
    Ok((rest, Node::Invert(node)))
}

fn parse_binary_not(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    let (rest, _) = char('~')(input)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, node) = parse_atom(rest)?;
    let node = Box::new(node).with_location((input, start, rest));
    Ok((rest, Node::BinaryNot(node)))
}

/// Parse unary operations (negation and bit inversion)
fn parse_unary(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    alt((parse_invert, parse_binary_not, parse_atom))(input)
}

/// Parse an atom of an expression: either a literal or a full expression within parenthesis
fn parse_atom(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    alt((
        map(parse_number_literal, |v| Node::Literal(v as Value)),
        map(parse_identifier, |i| Node::Variable(i.into())),
        parse_parenthesis,
    ))(input)
}

/// Parse an expression surrounded by parenthesis
fn parse_parenthesis(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    let (rest, _) = char('(')(input)?;
    let (rest, _) = space0(rest)?;

    let offset = input.offset(rest);
    let (rest, value) = parse_expression(rest)?;
    // This offsets the child nodes location to compensate the parenthesis
    let value = value.offset(offset);

    let (rest, _) = space0(rest)?;
    let (rest, _) = char(')')(rest)?;
    Ok((rest, value))
}

/// Parse an expression, returning its AST
pub(crate) fn parse_expression(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    parse_or(input)
}

#[cfg(test)]
mod tests {
    use nom::Finish;

    use super::*;

    #[track_caller]
    fn evaluate<L>(res: IResult<&str, Node<L>>) -> i128 {
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
