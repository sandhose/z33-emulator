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

use nom::{
    branch::alt,
    bytes::complete::tag,
    bytes::complete::tag_no_case,
    character::complete::{char, space0},
    combinator::{map, value},
    multi::fold_many0,
    sequence::preceded,
    IResult,
};
use thiserror::Error;

use super::expression::{
    parse_expression, Context as ExpressionContext, EmptyContext as EmptyExpressionContext,
    EvaluationError as ExpressionEvaluationError, Node as ExpressionNode,
};
use super::literal::parse_bool_literal;
use super::parse_identifier;

#[derive(Clone, Debug, PartialEq)]
pub enum Node<'a> {
    /// a == b
    Equal(ExpressionNode<'a>, ExpressionNode<'a>),

    /// a != b
    NotEqual(ExpressionNode<'a>, ExpressionNode<'a>),

    /// a >= b
    GreaterOrEqual(ExpressionNode<'a>, ExpressionNode<'a>),

    /// a > b
    GreaterThan(ExpressionNode<'a>, ExpressionNode<'a>),

    /// a <= b
    LesserOrEqual(ExpressionNode<'a>, ExpressionNode<'a>),

    /// a < b
    LesserThan(ExpressionNode<'a>, ExpressionNode<'a>),

    /// A || B
    Or(Box<Node<'a>>, Box<Node<'a>>),

    /// A && B
    And(Box<Node<'a>>, Box<Node<'a>>),

    /// !A
    Not(Box<Node<'a>>),

    /// true or false
    Literal(bool),

    /// defined(N)
    Defined(&'a str),
}

#[derive(Error, Debug, PartialEq)]
pub enum EvaluationError<'a> {
    #[error("could not evaluate expression: {0}")]
    ExpressionEvaluation(ExpressionEvaluationError<'a>),
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
pub(crate) struct EmptyContext;
impl Context for EmptyContext {
    type ExpressionContext = EmptyExpressionContext;

    fn get_expression_context(&self) -> &Self::ExpressionContext {
        &EmptyExpressionContext
    }
}

impl<'a> Node<'a> {
    /// Evaluate a condition AST node with a given context
    pub fn evaluate<C: Context>(&self, context: &C) -> Result<bool, EvaluationError> {
        use EvaluationError::*;
        match self {
            Node::Equal(a, b) => {
                let a: i128 = a
                    .evaluate(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                let b: i128 = b
                    .evaluate(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                Ok(a == b)
            }
            Node::NotEqual(a, b) => {
                let a: i128 = a
                    .evaluate(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                let b: i128 = b
                    .evaluate(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                Ok(a != b)
            }
            Node::GreaterOrEqual(a, b) => {
                let a: i128 = a
                    .evaluate(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                let b: i128 = b
                    .evaluate(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                Ok(a >= b)
            }
            Node::GreaterThan(a, b) => {
                let a: i128 = a
                    .evaluate(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                let b: i128 = b
                    .evaluate(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                Ok(a > b)
            }
            Node::LesserOrEqual(a, b) => {
                let a: i128 = a
                    .evaluate(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                let b: i128 = b
                    .evaluate(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                Ok(a <= b)
            }
            Node::LesserThan(a, b) => {
                let a: i128 = a
                    .evaluate(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                let b: i128 = b
                    .evaluate(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                Ok(a < b)
            }
            Node::Or(a, b) => {
                let a = a.evaluate(context)?;
                let b = b.evaluate(context)?;
                Ok(a || b)
            }
            Node::And(a, b) => {
                let a = a.evaluate(context)?;
                let b = b.evaluate(context)?;
                Ok(a && b)
            }
            Node::Not(a) => {
                let a = a.evaluate(context)?;
                Ok(!a)
            }
            Node::Literal(l) => Ok(*l),
            Node::Defined(v) => Ok(context.is_defined(v)),
        }
    }
}

pub fn parse_condition(input: &str) -> IResult<&str, Node> {
    let (input, _) = space0(input)?;
    parse_logical_or(input)
}

#[doc(hidden)]
fn parse_logical_or_rec(input: &str) -> IResult<&str, Node> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("||")(input)?;
    let (input, _) = space0(input)?;
    parse_logical_and(input)
}

/// Parse a logical "or" operation
fn parse_logical_or(input: &str) -> IResult<&str, Node> {
    let (input, value) = parse_logical_and(input)?;
    fold_many0(parse_logical_or_rec, value, |value, arg| {
        Node::Or(Box::new(value), Box::new(arg))
    })(input)
}

#[doc(hidden)]
fn parse_logical_and_rec(input: &str) -> IResult<&str, Node> {
    let (input, _) = space0(input)?;
    let (input, _) = tag("&&")(input)?;
    let (input, _) = space0(input)?;
    parse_logical_expression(input)
}

/// Parse a logical "and" operation
fn parse_logical_and(input: &str) -> IResult<&str, Node> {
    let (input, value) = parse_logical_expression(input)?;
    fold_many0(parse_logical_and_rec, value, |value, arg| {
        Node::And(Box::new(value), Box::new(arg))
    })(input)
}

fn parse_logical_expression(input: &str) -> IResult<&str, Node> {
    alt((
        map(preceded(char('!'), parse_atom), |v| Node::Not(Box::new(v))), // Negation
        parse_atom,
    ))(input)
}

fn parse_atom(input: &str) -> IResult<&str, Node> {
    let (input, _) = space0(input)?;
    alt((
        parse_number_comparison,
        parse_parenthesis,
        map(parse_defined, Node::Defined),
        map(parse_bool_literal, Node::Literal),
    ))(input)
}

fn parse_number_comparison(input: &str) -> IResult<&str, Node> {
    #[derive(Clone)]
    enum Comparison {
        Equal,
        NotEqual,
        GreaterOrEqual,
        GreaterThan,
        LesserOrEqual,
        LesserThan,
    }

    use Comparison::*;

    let (input, a) = parse_expression(input)?; // Parse the first operand
    let (input, _) = space0(input)?;

    // Parse the operator
    let (input, op) = alt((
        value(Equal, tag("==")),
        value(NotEqual, tag("!=")),
        value(GreaterOrEqual, tag(">=")),
        value(GreaterThan, tag(">")),
        value(LesserOrEqual, tag("<=")),
        value(LesserThan, tag("<")),
    ))(input)?;

    let (input, _) = space0(input)?;
    let (input, b) = parse_expression(input)?; // Parse the second operand

    // Create the node out of the two operands and the operator
    let node = match op {
        Equal => Node::Equal(a, b),
        NotEqual => Node::NotEqual(a, b),
        GreaterOrEqual => Node::GreaterOrEqual(a, b),
        GreaterThan => Node::GreaterThan(a, b),
        LesserOrEqual => Node::LesserOrEqual(a, b),
        LesserThan => Node::LesserThan(a, b),
    };

    Ok((input, node))
}

fn parse_defined(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag_no_case("defined")(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char('(')(input)?;
    let (input, _) = space0(input)?;
    let (input, identifier) = parse_identifier(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, identifier))
}

fn parse_parenthesis(input: &str) -> IResult<&str, Node> {
    let (input, _) = char('(')(input)?;
    let (input, _) = space0(input)?;
    let (input, value) = parse_condition(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, value))
}

#[cfg(test)]
mod tests {
    use nom::Finish;

    use super::*;

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
        use Node::*;
        assert_eq!(
            parse_condition("defined(HELLO)"),
            Ok(("", Defined("HELLO")))
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
    fn ast_test() {
        use super::super::expression::Node::Literal as ELiteral;
        use Node::{Literal as CLiteral, *};
        assert_eq!(
            parse_condition("3 > 2 && true"),
            Ok((
                "",
                And(
                    Box::new(GreaterThan(ELiteral(3), ELiteral(2))),
                    Box::new(CLiteral(true))
                )
            ))
        );
    }

    #[test]
    fn context_test() {
        use super::super::expression::{EvaluationError as ExpressionEvaluationError, Node as E};
        use Node::*;

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
        assert_eq!(Defined("yes").evaluate(ctx), Ok(true));
        assert_eq!(Defined("no").evaluate(ctx), Ok(false));
        assert_eq!(
            Equal(E::Variable("ten"), E::Literal(10)).evaluate(ctx),
            Ok(true),
        );
        assert_eq!(
            Equal(E::Variable("undefined"), E::Literal(10)).evaluate(ctx),
            Err(EvaluationError::ExpressionEvaluation(
                ExpressionEvaluationError::UndefinedVariable {
                    variable: "undefined"
                }
            ))
        );
    }
}
