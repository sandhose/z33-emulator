use std::str::FromStr;

use thiserror::Error;
use z33_emulator::parser::location::Locatable;
use z33_emulator::parser::shared::{expression, register};
use z33_emulator::parser::{ExpressionContext, ExpressionNode};
use z33_emulator::runtime::{Computer, ExtractValue, Reg};

use chumsky::prelude::*;

type Span = z33_emulator::parser::shared::Span;
type Extra<'a> = z33_emulator::parser::shared::Extra<'a>;

#[derive(Debug, Clone)]
pub enum Argument {
    Direct(ExpressionNode),
    Indirect(Reg),
    Indexed(Reg, ExpressionNode),
}

impl Argument {
    pub fn evaluate<Ctx: ExpressionContext, V: TryFrom<i128>>(
        self,
        computer: &Computer,
        context: &Ctx,
    ) -> Result<V, anyhow::Error> {
        let node = match self {
            Argument::Direct(node) => node,
            Argument::Indirect(reg) => {
                ExpressionNode::Literal(i128::from(reg.extract_address(computer)?))
            }
            Argument::Indexed(reg, node) => ExpressionNode::Sum(
                Box::new(ExpressionNode::Literal(i128::from(
                    reg.extract_word(computer)?,
                )))
                .with_location(0..0),
                Box::new(node).with_location(0..0),
            ),
        };

        let val = node.evaluate(context)?;
        Ok(val)
    }
}

#[derive(Debug, Error)]
#[error("could not parse expression: {0}")]
pub struct ParseError(String);

impl FromStr for Argument {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_address(s).map_err(ParseError)
    }
}

fn argument_parser<'a>() -> impl Parser<'a, &'a str, Argument, Extra<'a>> {
    // Try indexed first: %reg +/- expr
    let indexed = register()
        .then(
            choice((just('+').to(true), just('-').to(false)))
                .then(expression()),
        )
        .map(|(reg, (is_plus, expr))| {
            let expr = if is_plus {
                expr
            } else {
                ExpressionNode::Invert(Box::new(expr).with_location(0..0))
            };
            Argument::Indexed(reg, expr)
        });

    choice((
        expression().map(Argument::Direct),
        indexed,
        register().map(Argument::Indirect),
    ))
}

fn parse_address(input: &str) -> Result<Argument, String> {
    argument_parser()
        .then_ignore(end())
        .parse(input)
        .into_result()
        .map_err(|errs| {
            errs.into_iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("; ")
        })
}

#[derive(Debug, Clone)]
pub enum AssignmentTarget {
    Address(ExpressionNode),
    Register(Reg),
}

impl FromStr for AssignmentTarget {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_assignment_target(s).map_err(ParseError)
    }
}

fn assignment_target_parser<'a>() -> impl Parser<'a, &'a str, AssignmentTarget, Extra<'a>> {
    choice((
        expression().map(AssignmentTarget::Address),
        register().map(AssignmentTarget::Register),
    ))
}

fn parse_assignment_target(input: &str) -> Result<AssignmentTarget, String> {
    assignment_target_parser()
        .then_ignore(end())
        .parse(input)
        .into_result()
        .map_err(|errs| {
            errs.into_iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("; ")
        })
}
