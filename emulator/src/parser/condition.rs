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

use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case};
use nom::character::complete::{char, space0};
use nom::combinator::{cut, map, opt, value};
use nom::{IResult, Offset};
use thiserror::Error;

use super::expression::{
    parse_expression, Context as ExpressionContext, EmptyContext as EmptyExpressionContext,
    EvaluationError as ExpressionEvaluationError, Node as ENode,
};
use super::literal::parse_bool_literal;
use super::location::{Locatable, Located};
use super::precedence::Precedence;
use super::{parse_identifier, ParseError};

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

impl Node {
    fn offset(self, offset: usize) -> Self {
        match self {
            Node::Equal(a, b) => Node::Equal(a.offset(offset), b.offset(offset)),
            Node::NotEqual(a, b) => Node::NotEqual(a.offset(offset), b.offset(offset)),
            Node::GreaterOrEqual(a, b) => Node::GreaterOrEqual(a.offset(offset), b.offset(offset)),
            Node::GreaterThan(a, b) => Node::GreaterThan(a.offset(offset), b.offset(offset)),
            Node::LesserOrEqual(a, b) => Node::LesserOrEqual(a.offset(offset), b.offset(offset)),
            Node::LesserThan(a, b) => Node::LesserThan(a.offset(offset), b.offset(offset)),
            Node::Or(a, b) => Node::Or(a.offset(offset), b.offset(offset)),
            Node::And(a, b) => Node::And(a.offset(offset), b.offset(offset)),
            Node::Not(a) => Node::Not(a.offset(offset)),
            Node::Literal(a) => Node::Literal(a),
            Node::Defined(a) => Node::Defined(a.offset(offset)),
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

pub(crate) fn parse_condition<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Node, Error> {
    parse_logical_or(input)
}

#[doc(hidden)]
fn parse_logical_or_rec<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ChildNode, Error> {
    let (rest, _) = space0(input)?;
    let (rest, _) = tag("||")(rest)?;
    let (rest, _) = space0(rest)?;

    cut(move |rest: &'a str| {
        let start = rest;
        let (rest, node) = parse_logical_and(rest)?;
        let node = Box::new(node).with_location(input.offset(start)..input.offset(rest));
        Ok((rest, node))
    })(rest)
}

/// Parse a logical "or" operation
fn parse_logical_or<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Node, Error> {
    let (mut cursor, mut node) = parse_logical_and(input)?;

    while let (rest, Some(right)) = opt(parse_logical_or_rec)(cursor)? {
        let offset = input.offset(cursor);
        // Wrap the "left" node with location information
        let left = Box::new(node).with_location(0..offset);

        // The location embed in the `right` node is relative to the cursor, so we need
        // to offset it by the offset between the input and the cursor
        let right = right.offset(offset);

        node = Node::Or(left, right);
        cursor = rest;
    }

    Ok((cursor, node))
}

#[doc(hidden)]
fn parse_logical_and_rec<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ChildNode, Error> {
    let (rest, _) = space0(input)?;
    let (rest, _) = tag("&&")(rest)?;
    let (rest, _) = space0(rest)?;

    cut(move |rest: &'a str| {
        let start = rest;
        let (rest, node) = parse_logical_expression(rest)?;
        let node = Box::new(node).with_location(input.offset(start)..input.offset(rest));
        Ok((rest, node))
    })(rest)
}

/// Parse a logical "and" operation
fn parse_logical_and<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Node, Error> {
    let (mut cursor, mut node) = parse_logical_expression(input)?;

    while let (rest, Some(right)) = opt(parse_logical_and_rec)(cursor)? {
        let offset = input.offset(cursor);
        // Wrap the "left" node with location information
        let left = Box::new(node).with_location(0..offset);

        // The location embed in the `right` node is relative to the cursor, so we need
        // to offset it by the offset between the input and the cursor
        let right = right.offset(offset);

        node = Node::And(left, right);
        cursor = rest;
    }

    Ok((cursor, node))
}

fn parse_logical_not<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Node, Error> {
    let (rest, _) = char('!')(input)?;
    let (rest, _) = space0(rest)?;

    cut(move |rest: &'a str| {
        let start = rest;
        let (rest, node) = parse_atom(rest)?;
        let node = Box::new(node).with_location(input.offset(start)..input.offset(rest));
        Ok((rest, Node::Not(node)))
    })(rest)
}

fn parse_logical_expression<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Node, Error> {
    alt((parse_logical_not, parse_atom))(input)
}

fn parse_atom<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Node, Error> {
    let (input, _) = space0(input)?; // TODO: why does this eat leading spaces

    // Order is important here. Since in numerical expressions, opening parenthesis,
    // bool literals and the `defined` keyword would get parsed, number
    // comparison need to be last
    alt((
        parse_parenthesis,
        map(parse_defined, Node::Defined),
        map(parse_bool_literal, Node::Literal),
        parse_number_comparison,
    ))(input)
}

fn parse_number_comparison<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Node, Error> {
    #[derive(Clone, Copy)]
    enum Comparison {
        Equal,
        NotEqual,
        GreaterOrEqual,
        GreaterThan,
        LesserOrEqual,
        LesserThan,
    }

    let (rest, a) = parse_expression(input)?; // Parse the first operand
    let a = a.with_location(0..input.offset(rest));
    let (rest, _) = space0(rest)?;

    // Parse the operator
    let (rest, op) = alt((
        value(Comparison::Equal, tag("==")),
        value(Comparison::NotEqual, tag("!=")),
        value(Comparison::GreaterOrEqual, tag(">=")),
        value(Comparison::GreaterThan, tag(">")),
        value(Comparison::LesserOrEqual, tag("<=")),
        value(Comparison::LesserThan, tag("<")),
    ))(rest)?;
    let (rest, _) = space0(rest)?;

    cut(move |rest: &'a str| {
        let start = rest;
        let a = a.clone(); // Clone the first node to keep the closure FnMut
        let (rest, b) = parse_expression(rest)?; // Parse the second operand
        let b = b.with_location(input.offset(start)..input.offset(rest));

        // Create the node out of the two operands and the operator
        let node = match op {
            Comparison::Equal => Node::Equal(a, b),
            Comparison::NotEqual => Node::NotEqual(a, b),
            Comparison::GreaterOrEqual => Node::GreaterOrEqual(a, b),
            Comparison::GreaterThan => Node::GreaterThan(a, b),
            Comparison::LesserOrEqual => Node::LesserOrEqual(a, b),
            Comparison::LesserThan => Node::LesserThan(a, b),
        };

        Ok((rest, node))
    })(rest)
}

fn parse_defined<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Located<String>, Error> {
    let (rest, _) = tag_no_case("defined")(input)?;
    let (rest, _) = space0(rest)?;

    cut(move |rest: &'a str| {
        let (rest, _) = char('(')(rest)?;
        let (rest, _) = space0(rest)?;
        let start = rest;
        let (rest, identifier) = parse_identifier(rest)?;
        let identifier = identifier
            .to_string()
            .with_location(input.offset(start)..input.offset(rest));
        let (rest, _) = space0(rest)?;
        let (rest, _) = char(')')(rest)?;
        Ok((rest, identifier))
    })(rest)
}

fn parse_parenthesis<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Node, Error> {
    let (rest, _) = char('(')(input)?;
    let (rest, _) = space0(rest)?;

    cut(move |rest: &'a str| {
        let offset = input.offset(rest);
        let (rest, value) = parse_condition(rest)?;
        // This offsets the child nodes location to compensate the parenthesis
        let value = value.offset(offset);

        let (rest, _) = space0(rest)?;
        let (rest, _) = char(')')(rest)?;
        Ok((rest, value))
    })(rest)
}

#[cfg(test)]
mod tests {
    use nom::Finish;
    use pretty_assertions::assert_eq;

    use super::*;

    type R<T> = nom::IResult<&'static str, T, ()>;

    #[track_caller]
    fn evaluate(res: IResult<&str, Node>) -> bool {
        evaluate_with_context(res, &EmptyContext)
    }

    #[track_caller]
    fn evaluate_with_context<C: Context>(res: IResult<&str, Node>, context: &C) -> bool {
        let (rest, node) = res.finish().unwrap();
        assert_eq!(rest, "");
        node.evaluate(context).unwrap()
    }

    #[test]
    fn syntax_tree_test() {
        assert_eq!(
            parse_condition("defined(HELLO)"),
            R::Ok(("", Node::Defined("HELLO".to_string().with_location(8..13))))
        );
    }

    #[test]
    fn number_comparison_test() {
        assert_eq!(evaluate(parse_number_comparison("5 > 3")), true);
        assert_eq!(evaluate(parse_number_comparison("5 >= 3")), true);
        assert_eq!(evaluate(parse_number_comparison("3 < 5")), true);
        assert_eq!(evaluate(parse_number_comparison("3 <= 5")), true);
        assert_eq!(evaluate(parse_number_comparison("3 != 5")), true);
        assert_eq!(evaluate(parse_number_comparison("5 == 5")), true);
        assert_eq!(evaluate(parse_number_comparison("3 > 5")), false);
        assert_eq!(evaluate(parse_number_comparison("3 >= 5")), false);
        assert_eq!(evaluate(parse_number_comparison("5 < 3")), false);
        assert_eq!(evaluate(parse_number_comparison("5 <= 3")), false);
        assert_eq!(evaluate(parse_number_comparison("5 != 5")), false);
        assert_eq!(evaluate(parse_number_comparison("3 == 5")), false);
        assert_eq!(
            evaluate(parse_number_comparison("3 * 12 == 3 * 3 + 54 / 2")),
            true
        );
    }

    #[test]
    fn logical_operations_test() {
        assert_eq!(evaluate(parse_condition("true && true")), true);
        assert_eq!(evaluate(parse_condition("true && false")), false);
        assert_eq!(evaluate(parse_condition("true || false")), true);
        assert_eq!(evaluate(parse_condition("false || false")), false);
        assert_eq!(
            evaluate(parse_condition("(true || false) && (false || true)")),
            true
        );
        assert_eq!(
            evaluate(parse_condition("true || false && false || true")),
            true
        );
        assert_eq!(
            evaluate(parse_condition("!(true || false) && (false || true)")),
            false
        );
    }

    #[test]
    fn ast_location_test() {
        use super::super::expression::Node as ENode;
        assert_eq!(
            parse_condition("3 > 2 && true"),
            R::Ok((
                "",
                Node::And(
                    Box::new(Node::GreaterThan(
                        ENode::Literal(3).with_location(0..1),
                        ENode::Literal(2).with_location(4..5)
                    ))
                    .with_location(0..5),
                    Box::new(Node::Literal(true)).with_location(9..13)
                )
            ))
        );
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
