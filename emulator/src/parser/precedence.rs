//! Specify a node's precedence for display purposes
//!
//! This module helps correctly displaying parenthesis only when needed. This is
//! done by knowing the parent node's precedence and comparing it to the current
//! node. To do that, the node is wrapped in a [`ChildTree`] structure
//! temporarily just during output.

use super::condition::Node as ConditionNode;
use super::expression::Node as ExpressionNode;

pub(crate) struct ChildTree<'a, T: Precedence> {
    parent_precedence: usize,
    inner: &'a T,
}

pub(crate) trait Precedence: Sized {
    fn precedence(&self) -> usize;

    /// Wrap the node in a [`ChildTree`] for display
    fn with_parent<T: Precedence>(&self, parent: &T) -> ChildTree<Self> {
        ChildTree {
            parent_precedence: parent.precedence(),
            inner: self,
        }
    }
}

impl<'a, T: Precedence> ChildTree<'a, T> {
    fn needs_parenthesis(&self) -> bool {
        self.parent_precedence < self.inner.precedence()
    }
}

impl<'a, T: std::fmt::Display + Precedence> std::fmt::Display for ChildTree<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.needs_parenthesis() {
            write!(f, "({})", self.inner)
        } else {
            write!(f, "{}", self.inner)
        }
    }
}

// Precedence values are taken from here: https://en.cppreference.com/w/c/language/operator_precedence

impl Precedence for ExpressionNode {
    fn precedence(&self) -> usize {
        match self {
            Self::Literal(_) | Self::Variable(_) => 0,
            Self::Invert(_) | Self::BinaryNot(_) => 2,
            Self::Multiply(_, _) | Self::Divide(_, _) => 3,
            Self::Sum(_, _) | Self::Substract(_, _) => 4,
            Self::LeftShift(_, _) | Self::RightShift(_, _) => 5,
            Self::BinaryAnd(_, _) => 8,
            Self::BinaryOr(_, _) => 10,
        }
    }
}

impl Precedence for ConditionNode {
    fn precedence(&self) -> usize {
        match self {
            Self::Literal(_) | Self::Defined(_) => 0,
            Self::Not(_) => 2,
            Self::GreaterOrEqual(_, _)
            | Self::GreaterThan(_, _)
            | Self::LesserOrEqual(_, _)
            | Self::LesserThan(_, _) => 6,
            Self::Equal(_, _) | Self::NotEqual(_, _) => 7,
            Self::And(_, _) => 11,
            Self::Or(_, _) => 12,
        }
    }
}
