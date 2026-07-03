use std::str::FromStr;

use chumsky::prelude::*;
use thiserror::Error;
use z33_emulator::parser::location::Locatable;
use z33_emulator::parser::shared::{expression, register};
use z33_emulator::parser::{ExpressionContext, ExpressionNode};
use z33_emulator::runtime::{Computer, ExtractValue, Reg};

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
        .then(choice((just('+').to(true), just('-').to(false))).then(expression()))
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

/// Run a parser to completion over `input`, joining any parse errors into a
/// single message.
fn run_parser<'a, T, P>(parser: P, input: &'a str) -> Result<T, String>
where
    P: Parser<'a, &'a str, T, Extra<'a>>,
{
    parser
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

fn parse_address(input: &str) -> Result<Argument, String> {
    run_parser(argument_parser(), input)
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
    run_parser(assignment_target_parser(), input)
}

#[cfg(test)]
mod tests {
    use z33_emulator::runtime::Reg;

    use super::{Argument, AssignmentTarget};

    #[test]
    fn argument_parses_direct_expression() {
        assert!(matches!("5".parse::<Argument>(), Ok(Argument::Direct(_))));
    }

    #[test]
    fn argument_parses_indirect_register() {
        assert!(matches!(
            "%a".parse::<Argument>(),
            Ok(Argument::Indirect(Reg::A))
        ));
    }

    #[test]
    fn argument_parses_indexed() {
        assert!(matches!(
            "%b+4".parse::<Argument>(),
            Ok(Argument::Indexed(Reg::B, _))
        ));
        // The `-` form is also indexed (with an inverted offset).
        assert!(matches!(
            "%a-2".parse::<Argument>(),
            Ok(Argument::Indexed(Reg::A, _))
        ));
    }

    #[test]
    fn argument_rejects_garbage() {
        assert!("@@@".parse::<Argument>().is_err());
        assert!("".parse::<Argument>().is_err());
    }

    #[test]
    fn assignment_target_parses_register() {
        assert!(matches!(
            "%sp".parse::<AssignmentTarget>(),
            Ok(AssignmentTarget::Register(Reg::SP))
        ));
    }

    #[test]
    fn assignment_target_parses_address() {
        assert!(matches!(
            "100".parse::<AssignmentTarget>(),
            Ok(AssignmentTarget::Address(_))
        ));
    }

    #[test]
    fn assignment_target_rejects_garbage() {
        assert!("@@@".parse::<AssignmentTarget>().is_err());
    }
}
