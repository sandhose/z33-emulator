//! Program parsing logic
//!
//! This module is splitted in multiple submodules to make things easier to
//! read. The parsing is handled by the `nom` library.

use nom::bytes::complete::take_while1;
use nom::combinator::{all_consuming, verify};
use nom::{Finish, IResult};

use self::location::{Locatable, Located};

pub(crate) mod condition;
mod errors;
pub(crate) mod expression;
pub(crate) mod line;
pub(crate) mod literal;
pub mod location;
mod precedence;
pub(crate) mod preprocessor;
pub(crate) mod value;

pub use errors::{Error, ParseError};
pub use expression::{parse_expression, Context as ExpressionContext, Node as ExpressionNode};
pub use line::Program;
pub use value::parse_register;

fn is_identifier_char(c: char) -> bool {
    is_start_identifier_char(c) || c.is_ascii_digit()
}

fn is_start_identifier_char(c: char) -> bool {
    c == '_' || c.is_ascii_lowercase() || c.is_ascii_uppercase()
}

/// Parse a C-like identifier
pub(crate) fn parse_identifier<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, &'a str, Error> {
    verify(take_while1(is_identifier_char), |f: &str| {
        f.chars()
            .next()
            .filter(|&c| is_start_identifier_char(c))
            .is_some()
    })(input)
}

/// Parse a program
///
/// # Errors
///
/// This function will return an error if the program is invalid
pub fn parse(input: &str) -> Result<Located<Program>, nom::error::VerboseError<&str>> {
    parse_new(input)
}

#[doc(hidden)]
pub fn parse_new<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> Result<Located<Program>, Error> {
    // TODO: proper error handling & wrap those steps
    let (_, program) = all_consuming(self::line::parse_program)(input).finish()?;
    let program = program.with_location(0..input.len());

    Ok(program)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_identifier_test() {
        type R<'a> = IResult<&'a str, &'a str, ()>;
        assert_eq!(parse_identifier("hello"), R::Ok(("", "hello")));
        assert_eq!(parse_identifier("abc123"), R::Ok(("", "abc123")));
        assert_eq!(parse_identifier("123abc"), R::Err(nom::Err::Error(())));
        assert_eq!(parse_identifier("abc_123"), R::Ok(("", "abc_123")));
        assert_eq!(parse_identifier("abc-123"), R::Ok(("-123", "abc")));
    }
}
