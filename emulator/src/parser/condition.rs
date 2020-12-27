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
    IResult, Offset,
};
use thiserror::Error;

use super::parse_identifier;
use super::{
    expression::{
        parse_expression, Context as ExpressionContext, EmptyContext as EmptyExpressionContext,
        EvaluationError as ExpressionEvaluationError, Node as ENode,
    },
    location::{Locatable, Located, RelativeLocation},
    precedence::Precedence,
};
use super::{literal::parse_bool_literal, location::AbsoluteLocation};

type ChildNode<L> = Located<Box<Node<L>>, L>;
type ExpressionNode<L> = Located<ENode<L>, L>;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Node<L> {
    /// a == b
    Equal(ExpressionNode<L>, ExpressionNode<L>),

    /// a != b
    NotEqual(ExpressionNode<L>, ExpressionNode<L>),

    /// a >= b
    GreaterOrEqual(ExpressionNode<L>, ExpressionNode<L>),

    /// a > b
    GreaterThan(ExpressionNode<L>, ExpressionNode<L>),

    /// a <= b
    LesserOrEqual(ExpressionNode<L>, ExpressionNode<L>),

    /// a < b
    LesserThan(ExpressionNode<L>, ExpressionNode<L>),

    /// A || B
    Or(ChildNode<L>, ChildNode<L>),

    /// A && B
    And(ChildNode<L>, ChildNode<L>),

    /// !A
    Not(ChildNode<L>),

    /// true or false
    Literal(bool),

    /// defined(N)
    Defined(Located<String, L>),
}

impl<L> std::fmt::Display for Node<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Node::*;

        match self {
            Equal(a, b) => write!(
                f,
                "{} == {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            NotEqual(a, b) => write!(
                f,
                "{} != {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            GreaterOrEqual(a, b) => write!(
                f,
                "{} >= {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            GreaterThan(a, b) => write!(
                f,
                "{} > {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            LesserOrEqual(a, b) => write!(
                f,
                "{} <= {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            LesserThan(a, b) => write!(
                f,
                "{} < {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            Or(a, b) => write!(
                f,
                "{} || {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            And(a, b) => write!(
                f,
                "{} && {}",
                a.inner.with_parent(self),
                b.inner.with_parent(self)
            ),
            Not(a) => write!(f, "!{}", a.inner.with_parent(self)),
            Literal(a) => write!(f, "{}", a),
            Defined(a) => write!(f, "defined({})", a.inner),
        }
    }
}

impl Node<RelativeLocation> {
    fn offset(self, offset: usize) -> Self {
        use Node::*;
        match self {
            Equal(a, b) => Equal(a.offset(offset), b.offset(offset)),
            NotEqual(a, b) => NotEqual(a.offset(offset), b.offset(offset)),
            GreaterOrEqual(a, b) => GreaterOrEqual(a.offset(offset), b.offset(offset)),
            GreaterThan(a, b) => GreaterThan(a.offset(offset), b.offset(offset)),
            LesserOrEqual(a, b) => LesserOrEqual(a.offset(offset), b.offset(offset)),
            LesserThan(a, b) => LesserThan(a.offset(offset), b.offset(offset)),
            Or(a, b) => Or(a.offset(offset), b.offset(offset)),
            And(a, b) => And(a.offset(offset), b.offset(offset)),
            Not(a) => Not(a.offset(offset)),
            Literal(a) => Literal(a),
            Defined(a) => Defined(a.offset(offset)),
        }
    }

    // TODO: this should go in a trait
    #[allow(dead_code)]
    fn into_absolute(self, location: &AbsoluteLocation) -> Node<AbsoluteLocation> {
        use Node::*;

        let mapper = |node: Box<Node<RelativeLocation>>, parent: &AbsoluteLocation| {
            Box::new(node.into_absolute(parent))
        };

        match self {
            Equal(a, b) => Equal(
                a.into_absolute(location, ENode::into_absolute),
                b.into_absolute(location, ENode::into_absolute),
            ),
            NotEqual(a, b) => NotEqual(
                a.into_absolute(location, ENode::into_absolute),
                b.into_absolute(location, ENode::into_absolute),
            ),
            GreaterOrEqual(a, b) => GreaterOrEqual(
                a.into_absolute(location, ENode::into_absolute),
                b.into_absolute(location, ENode::into_absolute),
            ),
            GreaterThan(a, b) => GreaterThan(
                a.into_absolute(location, ENode::into_absolute),
                b.into_absolute(location, ENode::into_absolute),
            ),
            LesserOrEqual(a, b) => LesserOrEqual(
                a.into_absolute(location, ENode::into_absolute),
                b.into_absolute(location, ENode::into_absolute),
            ),
            LesserThan(a, b) => LesserThan(
                a.into_absolute(location, ENode::into_absolute),
                b.into_absolute(location, ENode::into_absolute),
            ),
            Or(a, b) => Or(
                a.into_absolute(location, mapper),
                b.into_absolute(location, mapper),
            ),
            And(a, b) => And(
                a.into_absolute(location, mapper),
                b.into_absolute(location, mapper),
            ),
            Not(a) => Not(a.into_absolute(location, mapper)),
            Literal(l) => Literal(l),
            Defined(d) => Defined(d.into_absolute(location, |d, _| d)),
        }
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum EvaluationError {
    #[error("could not evaluate expression: {0}")]
    ExpressionEvaluation(ExpressionEvaluationError),
}

impl From<ExpressionEvaluationError> for EvaluationError {
    fn from(e: ExpressionEvaluationError) -> Self {
        Self::ExpressionEvaluation(e)
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
pub(crate) struct EmptyContext;
impl Context for EmptyContext {
    type ExpressionContext = EmptyExpressionContext;

    fn get_expression_context(&self) -> &Self::ExpressionContext {
        &EmptyExpressionContext
    }
}

impl<L> Node<L> {
    /// Evaluate a condition AST node with a given context
    pub fn evaluate<C: Context>(&self, context: &C) -> Result<bool, EvaluationError> {
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

pub(crate) fn parse_condition(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    let (input, _) = space0(input)?;
    parse_logical_or(input)
}

#[doc(hidden)]
fn parse_logical_or_rec(input: &str) -> IResult<&str, ChildNode<RelativeLocation>> {
    let (rest, _) = space0(input)?;
    let (rest, _) = tag("||")(rest)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, node) = parse_logical_and(rest)?;
    let node = Box::new(node).with_location((input.offset(start), start.offset(rest)));
    Ok((rest, node))
}

/// Parse a logical "or" operation
fn parse_logical_or(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    let (mut cursor, mut node) = parse_logical_and(input)?;

    while let Ok((rest, right)) = parse_logical_or_rec(cursor) {
        let offset = input.offset(cursor);
        // Wrap the "left" node with location information
        let left = Box::new(node).with_location((0, offset));

        // The location embed in the `right` node is relative to the cursor, so we need to offset
        // it by the offset between the input and the cursor
        let right = right.offset(offset);

        node = Node::Or(left, right);
        cursor = rest;
    }

    Ok((cursor, node))
}

#[doc(hidden)]
fn parse_logical_and_rec(input: &str) -> IResult<&str, ChildNode<RelativeLocation>> {
    let (rest, _) = space0(input)?;
    let (rest, _) = tag("&&")(rest)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, node) = parse_logical_expression(rest)?;
    let node = Box::new(node).with_location((input.offset(start), start.offset(rest)));
    Ok((rest, node))
}

/// Parse a logical "and" operation
fn parse_logical_and(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    let (mut cursor, mut node) = parse_logical_expression(input)?;

    while let Ok((rest, right)) = parse_logical_and_rec(cursor) {
        let offset = input.offset(cursor);
        // Wrap the "left" node with location information
        let left = Box::new(node).with_location((0, offset));

        // The location embed in the `right` node is relative to the cursor, so we need to offset
        // it by the offset between the input and the cursor
        let right = right.offset(offset);

        node = Node::And(left, right);
        cursor = rest;
    }

    Ok((cursor, node))
}

fn parse_logical_not(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    let (rest, _) = char('!')(input)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, node) = parse_atom(rest)?;
    let node = Box::new(node).with_location((input.offset(start), start.offset(rest)));
    Ok((rest, Node::Not(node)))
}

fn parse_logical_expression(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    alt((parse_logical_not, parse_atom))(input)
}

fn parse_atom(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    let (input, _) = space0(input)?;
    alt((
        parse_number_comparison,
        parse_parenthesis,
        map(parse_defined, Node::Defined),
        map(parse_bool_literal, Node::Literal),
    ))(input)
}

fn parse_number_comparison(input: &str) -> IResult<&str, Node<RelativeLocation>> {
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

    let (rest, a) = parse_expression(input)?; // Parse the first operand
    let a = a.with_location((0, input.offset(rest)));
    let (rest, _) = space0(rest)?;

    // Parse the operator
    let (rest, op) = alt((
        value(Equal, tag("==")),
        value(NotEqual, tag("!=")),
        value(GreaterOrEqual, tag(">=")),
        value(GreaterThan, tag(">")),
        value(LesserOrEqual, tag("<=")),
        value(LesserThan, tag("<")),
    ))(rest)?;

    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, b) = parse_expression(rest)?; // Parse the second operand
    let b = b.with_location((input.offset(start), start.offset(rest)));

    // Create the node out of the two operands and the operator
    let node = match op {
        Equal => Node::Equal(a, b),
        NotEqual => Node::NotEqual(a, b),
        GreaterOrEqual => Node::GreaterOrEqual(a, b),
        GreaterThan => Node::GreaterThan(a, b),
        LesserOrEqual => Node::LesserOrEqual(a, b),
        LesserThan => Node::LesserThan(a, b),
    };

    Ok((rest, node))
}

fn parse_defined(input: &str) -> IResult<&str, Located<String, RelativeLocation>> {
    let (rest, _) = tag_no_case("defined")(input)?;
    let (rest, _) = space0(rest)?;
    let (rest, _) = char('(')(rest)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, identifier) = parse_identifier(rest)?;
    let identifier = identifier
        .to_string()
        .with_location((input.offset(start), start.offset(rest)));
    let (rest, _) = space0(rest)?;
    let (rest, _) = char(')')(rest)?;
    Ok((rest, identifier))
}

fn parse_parenthesis(input: &str) -> IResult<&str, Node<RelativeLocation>> {
    let (rest, _) = char('(')(input)?;
    let (rest, _) = space0(rest)?;

    let offset = input.offset(rest);
    let (rest, value) = parse_condition(rest)?;
    // This offsets the child nodes location to compensate the parenthesis
    let value = value.offset(offset);

    let (rest, _) = space0(rest)?;
    let (rest, _) = char(')')(rest)?;
    Ok((rest, value))
}

#[cfg(test)]
mod tests {
    use nom::Finish;

    use super::*;

    #[track_caller]
    fn evaluate<L>(res: IResult<&str, Node<L>>) -> bool {
        evaluate_with_context(res, &EmptyContext)
    }

    #[track_caller]
    fn evaluate_with_context<C: Context, L>(res: IResult<&str, Node<L>>, context: &C) -> bool {
        let (rest, node) = res.finish().unwrap();
        assert_eq!(rest, "");
        node.evaluate(context).unwrap()
    }

    #[test]
    fn syntax_tree_test() {
        use Node::*;
        assert_eq!(
            parse_condition("defined(HELLO)"),
            Ok(("", Defined("HELLO".to_string().with_location((8, 5)))))
        );
    }

    #[test]
    fn absolute_location_test() {
        use Node::*;
        // This tests that RelativeLocations are correctly transformed into AbsoluteLocations.
        let tree: Node<RelativeLocation> = parse_condition("true && (false || true)").unwrap().1;
        assert_eq!(
            tree,
            And(
                Box::new(Literal(true)).with_location((0, 4)),
                Box::new(Or(
                    Box::new(Literal(false)).with_location((1, 5)),
                    Box::new(Literal(true)).with_location((10, 4)),
                ),)
                .with_location((8, 15))
            )
        );

        let base = AbsoluteLocation::default();
        let tree: Node<AbsoluteLocation> = tree.into_absolute(&base);
        assert_eq!(
            tree,
            And(
                Box::new(Literal(true)).with_location((0, 4)),
                Box::new(Or(
                    Box::new(Literal(false)).with_location((9, 5)),
                    Box::new(Literal(true)).with_location((18, 4)),
                ),)
                .with_location((8, 15))
            )
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
        use super::super::expression::Node::Literal as ELiteral;
        use Node::{Literal as CLiteral, *};
        assert_eq!(
            parse_condition("3 > 2 && true"),
            Ok((
                "",
                And(
                    Box::new(GreaterThan(
                        ELiteral(3).with_location((0, 1)),
                        ELiteral(2).with_location((4, 1))
                    ))
                    .with_location((0, 5)),
                    Box::new(CLiteral(true)).with_location((9, 4))
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
        assert_eq!(
            Defined::<()>("yes".to_string().with_location(())).evaluate(ctx),
            Ok(true)
        );
        assert_eq!(
            Defined::<()>("no".to_string().with_location(())).evaluate(ctx),
            Ok(false)
        );
        assert_eq!(
            Equal::<()>(
                E::Variable("ten".into()).with_location(()),
                E::Literal(10).with_location(())
            )
            .evaluate(ctx),
            Ok(true),
        );
        assert_eq!(
            Equal::<()>(
                E::Variable("undefined".into()).with_location(()),
                E::Literal(10).with_location(())
            )
            .evaluate(ctx),
            Err(EvaluationError::ExpressionEvaluation(
                ExpressionEvaluationError::UndefinedVariable {
                    variable: "undefined".into()
                }
            ))
        );
    }
}
