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
use super::parse_identifier;

#[derive(Clone, Debug, PartialEq)]
pub enum Node<'a> {
    Equal(Box<ExpressionNode<'a>>, Box<ExpressionNode<'a>>),
    NotEqual(Box<ExpressionNode<'a>>, Box<ExpressionNode<'a>>),
    GreaterOrEqual(Box<ExpressionNode<'a>>, Box<ExpressionNode<'a>>),
    GreaterThan(Box<ExpressionNode<'a>>, Box<ExpressionNode<'a>>),
    LesserOrEqual(Box<ExpressionNode<'a>>, Box<ExpressionNode<'a>>),
    LesserThan(Box<ExpressionNode<'a>>, Box<ExpressionNode<'a>>),
    Or(Box<Node<'a>>, Box<Node<'a>>),
    And(Box<Node<'a>>, Box<Node<'a>>),
    Not(Box<Node<'a>>),
    Literal(&'a str),
    Defined(&'a str),
}

#[derive(Error, Debug)]
pub enum EvaluationError<'a> {
    #[error("could not evaluate expression: {0}")]
    ExpressionEvaluation(ExpressionEvaluationError<'a>),

    #[error("unknown literal {0:?}")]
    UnknownLiteral(&'a str),
}

pub trait Context {
    type ExpressionContext;
    fn is_defined(&self, variable: &str) -> bool;
    fn get_expression_context(&self) -> &Self::ExpressionContext;
}

pub(crate) struct EmptyContext;
impl Context for EmptyContext {
    type ExpressionContext = EmptyExpressionContext;
    fn is_defined(&self, _variable: &str) -> bool {
        false
    }
    fn get_expression_context(&self) -> &Self::ExpressionContext {
        &EmptyExpressionContext
    }
}

impl<'a> Node<'a> {
    pub fn evaluate<C: Context>(&self, context: &C) -> Result<bool, EvaluationError>
    where
        C::ExpressionContext: ExpressionContext,
    {
        use EvaluationError::*;
        match self {
            Node::Equal(a, b) => {
                let a: i128 = a
                    .compute_with_context(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                let b: i128 = b
                    .compute_with_context(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                Ok(a == b)
            }
            Node::NotEqual(a, b) => {
                let a: i128 = a
                    .compute_with_context(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                let b: i128 = b
                    .compute_with_context(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                Ok(a != b)
            }
            Node::GreaterOrEqual(a, b) => {
                let a: i128 = a
                    .compute_with_context(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                let b: i128 = b
                    .compute_with_context(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                Ok(a >= b)
            }
            Node::GreaterThan(a, b) => {
                let a: i128 = a
                    .compute_with_context(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                let b: i128 = b
                    .compute_with_context(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                Ok(a > b)
            }
            Node::LesserOrEqual(a, b) => {
                let a: i128 = a
                    .compute_with_context(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                let b: i128 = b
                    .compute_with_context(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                Ok(a <= b)
            }
            Node::LesserThan(a, b) => {
                let a: i128 = a
                    .compute_with_context(context.get_expression_context())
                    .map_err(ExpressionEvaluation)?;
                let b: i128 = b
                    .compute_with_context(context.get_expression_context())
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
            Node::Literal(l) => match l.to_lowercase().as_str() {
                "true" => Ok(true),
                "false" => Ok(false),
                _ => Err(UnknownLiteral(l)),
            },
            Node::Defined(v) => Ok(context.is_defined(v)),
        }
    }
}

pub fn parse_condition(input: &str) -> IResult<&str, Node> {
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
        map(parse_identifier, Node::Literal),
    ))(input)
}

fn parse_number_comparison(input: &str) -> IResult<&str, Node> {
    use Comparison::*;
    let (input, _) = space0(input)?;
    let (input, a) = parse_expression(input)?;
    let (input, _) = space0(input)?;
    let (input, op) = alt((
        value(Equal, tag("==")),
        value(NotEqual, tag("!=")),
        value(GreaterOrEqual, tag(">=")),
        value(GreaterThan, tag(">")),
        value(LesserOrEqual, tag("<=")),
        value(LesserThan, tag("<")),
    ))(input)?;
    let (input, _) = space0(input)?;
    let (input, b) = parse_expression(input)?;
    let node = match op {
        Equal => Node::Equal(Box::new(a), Box::new(b)),
        NotEqual => Node::NotEqual(Box::new(a), Box::new(b)),
        GreaterOrEqual => Node::GreaterOrEqual(Box::new(a), Box::new(b)),
        GreaterThan => Node::GreaterThan(Box::new(a), Box::new(b)),
        LesserOrEqual => Node::LesserOrEqual(Box::new(a), Box::new(b)),
        LesserThan => Node::LesserThan(Box::new(a), Box::new(b)),
    };
    Ok((input, node))
}

fn parse_defined(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag_no_case("defined")(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char('(')(input)?;
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
        let (rest, node) = res.finish().unwrap();
        assert_eq!(rest, "");
        node.evaluate(&EmptyContext).unwrap()
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
}
