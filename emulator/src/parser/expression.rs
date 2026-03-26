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

use thiserror::Error;

use super::location::Located;
use super::precedence::Precedence;
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

    /// A parse error that was recovered from
    Error,
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
            Node::Error => NodeKind::Error,
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
            Node::Variable(_) | Node::Literal(_) | Node::Error => Vec::new(),
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
                Node::Error => write!(f, "<error>"),
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
            Node::Error => Node::Error,
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
    #[error("undefined label or macro {variable:?}")]
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

    #[error("cannot evaluate error placeholder")]
    Error,
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

                Node::Error => return Err(EvaluationError::Error),
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
