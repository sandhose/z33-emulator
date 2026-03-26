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
//! Atom             := NumberComparison | '(' Condition ')' | 'defined(' Identifier ')' | Literal
//! NumberComparison := ConstExpr ('==' | '!=' | '>=' | '>' | '<=' | '<') ConstExpr
//! ```
//!
//! Note: to simplify a bit, it might accept some weird conditions.
//! For example, `!4 > 3` is evaluated like `!(4 > 3)`.

use std::ops::Range;

use chumsky::prelude::*;
use thiserror::Error;

use super::expression::{
    Context as ExpressionContext, EmptyContext as EmptyExpressionContext,
    EvaluationError as ExpressionEvaluationError, Node as ENode,
};
use super::location::{Locatable, Located};
use super::precedence::Precedence;
use super::shared::{bool_literal, expression, hspace, identifier, span_to_range, Extra};

type ChildNode = Located<Box<Node>>;
type ExpressionNode = Located<ENode>;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Node {
    /// a == b
    Equal(ExpressionNode, ExpressionNode),

    /// a != b
    NotEqual(ExpressionNode, ExpressionNode),

    /// a >= b
    GreaterOrEqual(ExpressionNode, ExpressionNode),

    /// a > b
    GreaterThan(ExpressionNode, ExpressionNode),

    /// a <= b
    LesserOrEqual(ExpressionNode, ExpressionNode),

    /// a < b
    LesserThan(ExpressionNode, ExpressionNode),

    /// A || B
    Or(ChildNode, ChildNode),

    /// A && B
    And(ChildNode, ChildNode),

    /// !A
    Not(ChildNode),

    /// true or false
    Literal(bool),

    /// defined(N)
    Defined(Located<String>),
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Equal(a, b) => write!(
                f,
                "{} == {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            Node::NotEqual(a, b) => write!(
                f,
                "{} != {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            Node::GreaterOrEqual(a, b) => write!(
                f,
                "{} >= {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            Node::GreaterThan(a, b) => write!(
                f,
                "{} > {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            Node::LesserOrEqual(a, b) => write!(
                f,
                "{} <= {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            Node::LesserThan(a, b) => write!(
                f,
                "{} < {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            Node::Or(a, b) => write!(
                f,
                "{} || {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            Node::And(a, b) => write!(
                f,
                "{} && {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            Node::Not(a) => write!(f, "!{}", a.inner.with_parent(self)),
            Node::Literal(a) => write!(f, "{a}"),
            Node::Defined(a) => write!(f, "defined({})", a.inner),
        }
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum EvaluationError {
    #[error("could not evaluate expression")]
    ExpressionEvaluation {
        location: Range<usize>,
        source: ExpressionEvaluationError,
    },
}

impl EvaluationError {
    pub fn location(&self) -> &Range<usize> {
        match self {
            EvaluationError::ExpressionEvaluation { location, .. } => location,
        }
    }
}

/// A context holds definitions and expression variables
pub trait Context {
    type ExpressionContext: ExpressionContext;

    /// Check if a variable is defined
    fn is_defined(&self, _variable: &str) -> bool {
        false
    }

    /// Get the expression context with which expression get evaluated
    fn get_expression_context(&self) -> &Self::ExpressionContext;
}

/// An empty context that has no variable defined
#[allow(unused)]
pub(crate) struct EmptyContext;
impl Context for EmptyContext {
    type ExpressionContext = EmptyExpressionContext;

    fn get_expression_context(&self) -> &Self::ExpressionContext {
        &EmptyExpressionContext
    }
}

impl Node {
    /// Evaluate a condition AST node with a given context
    pub fn evaluate<C: Context>(&self, context: &C) -> Result<bool, ExpressionEvaluationError> {
        let value = match self {
            Node::Equal(a, b) => {
                let context = context.get_expression_context();
                let a: i128 = a.inner.evaluate(context)?;
                let b: i128 = b.inner.evaluate(context)?;
                a == b
            }

            Node::NotEqual(a, b) => {
                let context = context.get_expression_context();
                let a: i128 = a.inner.evaluate(context)?;
                let b: i128 = b.inner.evaluate(context)?;
                a != b
            }

            Node::GreaterOrEqual(a, b) => {
                let context = context.get_expression_context();
                let a: i128 = a.inner.evaluate(context)?;
                let b: i128 = b.inner.evaluate(context)?;
                a >= b
            }

            Node::GreaterThan(a, b) => {
                let context = context.get_expression_context();
                let a: i128 = a.inner.evaluate(context)?;
                let b: i128 = b.inner.evaluate(context)?;
                a > b
            }

            Node::LesserOrEqual(a, b) => {
                let context = context.get_expression_context();
                let a: i128 = a.inner.evaluate(context)?;
                let b: i128 = b.inner.evaluate(context)?;
                a <= b
            }

            Node::LesserThan(a, b) => {
                let context = context.get_expression_context();
                let a: i128 = a.inner.evaluate(context)?;
                let b: i128 = b.inner.evaluate(context)?;
                a < b
            }

            Node::Or(a, b) => {
                let a = a.inner.evaluate(context)?;
                let b = b.inner.evaluate(context)?;
                a || b
            }

            Node::And(a, b) => {
                let a = a.inner.evaluate(context)?;
                let b = b.inner.evaluate(context)?;
                a && b
            }

            Node::Not(a) => {
                let a = a.inner.evaluate(context)?;
                !a
            }

            Node::Literal(l) => *l,

            Node::Defined(v) => context.is_defined(&v.inner),
        };

        Ok(value)
    }
}

impl Located<Node> {
    /// Evaluate a condition AST node with a given context, returning a boolean
    /// value
    ///
    /// # Errors
    ///
    /// This function will return an error if the evaluation fails.
    pub fn evaluate<C: Context>(&self, context: &C) -> Result<bool, EvaluationError> {
        self.inner
            .evaluate(context)
            .map_err(|source| EvaluationError::ExpressionEvaluation {
                location: self.location.clone(),
                source,
            })
    }
}

// ---------------------------------------------------------------------------
// Chumsky parsers
// ---------------------------------------------------------------------------

/// Parse a `defined(IDENTIFIER)` expression.
fn defined_parser<'a>() -> impl Parser<'a, &'a str, Located<String>, Extra<'a>> + Clone {
    just("defined")
        .or(just("DEFINED"))
        .ignore_then(hspace())
        .ignore_then(just('('))
        .ignore_then(hspace())
        .ignore_then(
            identifier()
                .map_with(|id: &str, e| id.to_string().with_location(span_to_range(e.span()))),
        )
        .then_ignore(hspace())
        .then_ignore(just(')'))
}

/// Parse a number comparison: `ConstExpr OP ConstExpr`
fn number_comparison<'a>() -> impl Parser<'a, &'a str, Node, Extra<'a>> + Clone {
    #[derive(Clone, Copy)]
    enum Cmp {
        Eq,
        Ne,
        Ge,
        Gt,
        Le,
        Lt,
    }

    let located_expr = expression()
        .map_with(|e, extra| e.with_location(span_to_range(extra.span())));

    let op = choice((
        just("==").to(Cmp::Eq),
        just("!=").to(Cmp::Ne),
        just(">=").to(Cmp::Ge),
        just(">").to(Cmp::Gt),
        just("<=").to(Cmp::Le),
        just("<").to(Cmp::Lt),
    ));

    located_expr
        .clone()
        .then_ignore(hspace())
        .then(op)
        .then_ignore(hspace())
        .then(located_expr)
        .map(|((a, op), b)| match op {
            Cmp::Eq => Node::Equal(a, b),
            Cmp::Ne => Node::NotEqual(a, b),
            Cmp::Ge => Node::GreaterOrEqual(a, b),
            Cmp::Gt => Node::GreaterThan(a, b),
            Cmp::Le => Node::LesserOrEqual(a, b),
            Cmp::Lt => Node::LesserThan(a, b),
        })
}

/// Parse a condition expression.
pub(crate) fn condition_parser<'a>() -> impl Parser<'a, &'a str, Node, Extra<'a>> + Clone {
    recursive(|condition| {
        // Atoms: parenthesized, defined(), bool literal, or number comparison
        let paren = condition
            .delimited_by(just('(').then(hspace()), hspace().then(just(')')));

        let atom = choice((
            paren,
            defined_parser().map(Node::Defined),
            bool_literal().map(Node::Literal),
            number_comparison(),
        ))
        .padded_by(hspace());

        // Logical NOT
        let logical_expr = just('!')
            .ignore_then(hspace())
            .ignore_then(atom.clone())
            .map_with(|node, e| {
                Node::Not(Box::new(node).with_location(span_to_range(e.span())))
            })
            .or(atom);

        // Logical AND
        let logical_and = logical_expr.clone().foldl_with(
            hspace()
                .ignore_then(just("&&"))
                .then_ignore(hspace())
                .ignore_then(logical_expr)
                .repeated(),
            |lhs, rhs, e| {
                let span = e.span();
                Node::And(
                    Box::new(lhs).with_location(span.start..span.start),
                    Box::new(rhs).with_location(span.end..span.end),
                )
            },
        );

        // Logical OR
        logical_and.clone().foldl_with(
            hspace()
                .ignore_then(just("||"))
                .then_ignore(hspace())
                .ignore_then(logical_and)
                .repeated(),
            |lhs, rhs, e| {
                let span = e.span();
                Node::Or(
                    Box::new(lhs).with_location(span.start..span.start),
                    Box::new(rhs).with_location(span.end..span.end),
                )
            },
        )
    })
    .boxed()
}

/// Parse a condition expression from a complete string.
///
/// Returns the condition AST node, or an error string on parse failure.
pub(crate) fn parse_condition(input: &str) -> Result<Node, String> {
    let result = condition_parser().then_ignore(end()).parse(input);
    result.into_result().map_err(|errs| {
        errs.into_iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("; ")
    })
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[track_caller]
    fn evaluate(input: &str) -> bool {
        evaluate_with_context(input, &EmptyContext)
    }

    #[track_caller]
    fn evaluate_with_context<C: Context>(input: &str, context: &C) -> bool {
        let node = parse_condition(input).expect("parse failed");
        node.evaluate(context).unwrap()
    }

    #[test]
    fn syntax_tree_test() {
        assert_eq!(
            parse_condition("defined(HELLO)"),
            Ok(Node::Defined("HELLO".to_string().with_location(8..13)))
        );
    }

    #[test]
    fn number_comparison_test() {
        assert!(evaluate("5 > 3"));
        assert!(evaluate("5 >= 3"));
        assert!(evaluate("3 < 5"));
        assert!(evaluate("3 <= 5"));
        assert!(evaluate("3 != 5"));
        assert!(evaluate("5 == 5"));
        assert!(!evaluate("3 > 5"));
        assert!(!evaluate("3 >= 5"));
        assert!(!evaluate("5 < 3"));
        assert!(!evaluate("5 <= 3"));
        assert!(!evaluate("5 != 5"));
        assert!(!evaluate("3 == 5"));
        assert!(evaluate("3 * 12 == 3 * 3 + 54 / 2"));
    }

    #[test]
    fn logical_operations_test() {
        assert!(evaluate("true && true"));
        assert!(!evaluate("true && false"));
        assert!(evaluate("true || false"));
        assert!(!evaluate("false || false"));
        assert!(evaluate("(true || false) && (false || true)"));
        assert!(evaluate("true || false && false || true"));
        assert!(!evaluate("!(true || false) && (false || true)"));
    }

    #[test]
    fn ast_location_test() {
        use super::super::expression::Node as ENode;
        let node = parse_condition("3 > 2 && true").unwrap();
        // foldl_with produces approximate spans for the And child nodes
        match &node {
            Node::And(left, right) => {
                match &*left.inner {
                    Node::GreaterThan(a, b) => {
                        assert_eq!(a.inner, ENode::Literal(3));
                        assert_eq!(b.inner, ENode::Literal(2));
                    }
                    other => panic!("expected GreaterThan, got {other:?}"),
                }
                assert_eq!(*right.inner, Node::Literal(true));
            }
            other => panic!("expected And, got {other:?}"),
        }
    }

    #[test]
    fn context_test() {
        use super::super::expression::{EvaluationError as ExpressionEvaluationError, Node as E};

        struct TestExpressionContext;
        impl super::super::expression::Context for TestExpressionContext {
            fn resolve_variable(&self, variable: &str) -> Option<i128> {
                match variable {
                    "ten" => Some(10),
                    "undefined" => None,
                    _ => unreachable!(),
                }
            }
        }

        struct TestConditionContext;
        impl Context for TestConditionContext {
            type ExpressionContext = TestExpressionContext;
            fn is_defined(&self, variable: &str) -> bool {
                match variable {
                    "yes" => true,
                    "no" => false,
                    _ => unreachable!(),
                }
            }

            fn get_expression_context(&self) -> &Self::ExpressionContext {
                &TestExpressionContext
            }
        }

        let ctx = &TestConditionContext;
        assert_eq!(
            Node::Defined("yes".to_string().with_location(0..0)).evaluate(ctx),
            Ok(true)
        );
        assert_eq!(
            Node::Defined("no".to_string().with_location(0..0)).evaluate(ctx),
            Ok(false)
        );
        assert_eq!(
            Node::Equal(
                E::Variable("ten".into()).with_location(0..0),
                E::Literal(10).with_location(0..0)
            )
            .evaluate(ctx),
            Ok(true),
        );
        assert_eq!(
            Node::Equal(
                E::Variable("undefined".into()).with_location(0..0),
                E::Literal(10).with_location(0..0)
            )
            .evaluate(ctx),
            Err(ExpressionEvaluationError::UndefinedVariable {
                variable: "undefined".into(),
            })
        );
    }
}
