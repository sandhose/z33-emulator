//! Assembly directive parsing logic
//!
//! This defines a few assembly directives, held in the [`Directive`](enum.Directive.html) type.

use nom::{
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::{char, space0, space1},
    combinator::map,
    IResult,
};

use super::expression::parse_const_expression;
use super::literal::parse_string_literal;
use super::parse_identifier;

/// Type used for memory addresses
///
/// This should be in sync with what is defined in the `processor` module.
type Address = u16;

/// Type used for words in memory
///
/// This should be in sync with what is defined in the `processor` module.
type Word = u16;

/// Represents an assembly directive
#[derive(Debug, Clone, PartialEq)]
pub enum Directive {
    /// Define a label
    ///
    /// ```asm
    /// label:
    /// ```
    LabelDefinition(String),

    /// Change the current address
    ///
    /// ```asm
    /// .addr 0x0F00
    /// ```
    AddressChange(Address),

    /// Reserve space in memory
    ///
    /// ```asm
    /// .space 0x0F00
    /// ```
    Space(Address),

    /// Store a word in memory
    ///
    /// ```asm
    /// .word 0x0F00
    /// ```
    Word(Word),

    /// Store a string in memory
    ///
    /// ```asm
    /// .string "hello, world!"
    /// ```
    StringLiteral(String),
}

/// Parse a label definition directive
fn parse_label_def_directive(input: &str) -> IResult<&str, &str> {
    let (input, label) = parse_identifier(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(':')(input)?;
    Ok((input, label))
}

/// Parse a `.addr` (address change) directive
fn parse_address_change_directive(input: &str) -> IResult<&str, Address> {
    let (input, _) = tag_no_case(".addr")(input)?;
    let (input, _) = space1(input)?;
    let (input, addr) = parse_const_expression(input)?;
    Ok((input, addr))
}

/// Parse a `.space` directive
fn parse_space_directive(input: &str) -> IResult<&str, Address> {
    let (input, _) = tag_no_case(".space")(input)?;
    let (input, _) = space1(input)?;
    let (input, addr) = parse_const_expression(input)?;
    Ok((input, addr))
}

/// Parse a `.word` directive
fn parse_word_directive(input: &str) -> IResult<&str, Word> {
    let (input, _) = tag_no_case(".word")(input)?;
    let (input, _) = space1(input)?;
    let (input, word) = parse_const_expression(input)?;
    Ok((input, word))
}

/// Parse a `.string` directive
fn parse_string_literal_directive(input: &str) -> IResult<&str, String> {
    let (input, _) = tag_no_case(".string")(input)?;
    let (input, _) = space1(input)?;
    let (input, string) = parse_string_literal(input)?;
    Ok((input, string))
}

/// Parse a directive
pub fn parse_directive(input: &str) -> IResult<&str, Directive> {
    alt((
        map(parse_label_def_directive, |l| {
            Directive::LabelDefinition(l.into())
        }),
        map(parse_address_change_directive, Directive::AddressChange),
        map(parse_space_directive, Directive::Space),
        map(parse_word_directive, Directive::Word),
        map(parse_string_literal_directive, Directive::StringLiteral),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_label_def_test() {
        assert_eq!(parse_label_def_directive("foo:"), Ok(("", "foo")));
    }

    #[test]
    fn parse_address_change_test() {
        assert_eq!(
            parse_address_change_directive(".addr 0xF00"),
            Ok(("", 0xF00))
        );
    }

    #[test]
    fn parse_space_test() {
        assert_eq!(parse_space_directive(".space 0xF00"), Ok(("", 0xF00)));
    }

    #[test]
    fn parse_word_test() {
        assert_eq!(parse_word_directive(".word 0xF00"), Ok(("", 0xF00)));
    }

    #[test]
    fn parse_string_literal_test() {
        assert_eq!(
            parse_string_literal_directive(r#".string "baz""#),
            Ok(("", "baz".into()))
        );
    }
}
