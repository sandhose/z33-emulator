//! Program parsing logic
//!
//! This module is splitted in multiple submodules to make things easier to read. The parsing is
//! handled by the `nom` library.

use nom::{
    bytes::complete::take_while1, combinator::all_consuming, combinator::verify, Finish, IResult,
};

use self::{
    line::Program,
    location::{Locatable, Located, RelativeLocation},
};

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

fn is_identifier_char(c: char) -> bool {
    is_start_identifier_char(c) || ('0'..'9').contains(&c)
}

fn is_start_identifier_char(c: char) -> bool {
    c == '_' || ('a'..'z').contains(&c) || ('A'..'Z').contains(&c)
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

pub fn parse(
    input: &str,
) -> Result<Located<Program<RelativeLocation>, RelativeLocation>, nom::error::VerboseError<&str>> {
    parse_new(input)
}

pub fn parse_new<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> Result<Located<Program<RelativeLocation>, RelativeLocation>, Error> {
    // TODO: proper error handling & wrap those steps
    let (_, program) = all_consuming(self::line::parse_program)(input).finish()?;
    let program = program.with_location((0, input.len()));

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
