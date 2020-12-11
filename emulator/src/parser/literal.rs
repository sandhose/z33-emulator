//! Parse number and string literals.
//!
//! It parses base 10, base 16 (prefixed by `0x`), base 8 (prefixed by `0o`) and base 2 (prefixed
//! by `01`) number literals.

use std::str::FromStr;

use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag_no_case, take_while1},
    character::complete::{char, none_of},
    combinator::{cut, map_res, value},
    IResult,
};

/// Parse a string literal
pub fn parse_string_literal<'a>(input: &'a str) -> IResult<&'a str, String> {
    let (input, _) = char('"')(input)?;
    let (input, string) = escaped_transform(none_of("\"\\"), '\\', |input: &'a str| {
        alt((
            value("\\", char('\\')),
            value("\"", char('"')),
            value("\n", char('n')),
        ))(input)
    })(input)?;
    let (input, _) = char('"')(input)?;
    Ok((input, string))
}

/// Parse a decimal number
fn from_decimal(input: &str) -> Result<u64, std::num::ParseIntError> {
    u64::from_str(input)
}

/// Check if character is a decimal digit
fn is_digit(c: char) -> bool {
    c.is_digit(10)
}

/// Parse a hexadecimal number
fn from_hexadecimal(input: &str) -> Result<u64, std::num::ParseIntError> {
    u64::from_str_radix(input, 16)
}

/// Check if character is a hexadecimal digit
fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}

/// Extract a hexadecimal literal
fn parse_hexadecimal_literal(input: &str) -> IResult<&str, u64> {
    let (input, _) = tag_no_case("0x")(input)?;
    cut(map_res(take_while1(is_hex_digit), from_hexadecimal))(input)
}

/// Parse an octal number
fn from_octal(input: &str) -> Result<u64, std::num::ParseIntError> {
    u64::from_str_radix(input, 8)
}

/// Check if character is an octal digit
fn is_oct_digit(c: char) -> bool {
    c.is_digit(8)
}

/// Extract an octal literal
fn parse_octal_literal(input: &str) -> IResult<&str, u64> {
    let (input, _) = tag_no_case("0o")(input)?;
    cut(map_res(take_while1(is_oct_digit), from_octal))(input)
}

/// Parse a binary number
fn from_binary(input: &str) -> Result<u64, std::num::ParseIntError> {
    u64::from_str_radix(input, 2)
}

/// Check if character is a binary digit
fn is_bin_digit(c: char) -> bool {
    c.is_digit(2)
}

/// Extract a binary literal
fn parse_binary_literal(input: &str) -> IResult<&str, u64> {
    let (input, _) = tag_no_case("0b")(input)?;
    cut(map_res(take_while1(is_bin_digit), from_binary))(input)
}

/// Parse a number literal
pub fn parse_literal(input: &str) -> IResult<&str, u64> {
    alt((
        parse_hexadecimal_literal,
        parse_octal_literal,
        parse_binary_literal,
        map_res(take_while1(is_digit), from_decimal),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_decimal_test() {
        assert_eq!(from_decimal("16"), Ok(16));
        assert_eq!(from_decimal("65535"), Ok(65535));
        assert!(from_decimal("foo").is_err());
    }

    #[test]
    fn is_digit_test() {
        for c in '0'..='9' {
            assert!(is_digit(c));
        }

        for c in ('a'..='z').chain('A'..='Z') {
            assert!(!is_digit(c));
        }
    }

    #[test]
    fn from_hexadecimal_test() {
        assert_eq!(from_hexadecimal("4F"), Ok(0x4f));
        assert_eq!(from_hexadecimal("4f"), Ok(0x4f)); // Lower case works
        assert_eq!(from_hexadecimal("ffff"), Ok(0xffff));
        assert!(from_hexadecimal("foo").is_err());
    }

    #[test]
    fn is_hex_digit_test() {
        for c in ('0'..='9').chain('a'..='f').chain('A'..='F') {
            assert!(is_hex_digit(c));
        }

        for c in ('g'..='z').chain('G'..'Z') {
            assert!(!is_hex_digit(c));
        }
    }

    #[test]
    fn take_hexadecimal_literal_test() {
        assert_eq!(parse_hexadecimal_literal("0x4F"), Ok(("", 0x4F)));
        assert_eq!(parse_hexadecimal_literal("0X4f"), Ok(("", 0x4f)));
        assert_eq!(parse_hexadecimal_literal("0xffff"), Ok(("", 0xffff)));
        assert_eq!(parse_hexadecimal_literal("0x10000"), Ok(("", 0x10000)));
        assert!(parse_hexadecimal_literal("0xinvalid").is_err()); // Invalid
        assert!(parse_hexadecimal_literal("ffff").is_err()); // No prefix
    }

    #[test]
    fn from_octal_test() {
        assert_eq!(from_octal("24"), Ok(0o24));
        assert!(from_octal("98").is_err());
        assert_eq!(from_octal("177777"), Ok(0xffff));
        assert!(from_octal("foo").is_err());
    }

    #[test]
    fn is_oct_digit_test() {
        for c in '0'..='7' {
            assert!(is_oct_digit(c));
        }

        for c in ('8'..='9').chain('a'..='z').chain('A'..'Z') {
            assert!(!is_oct_digit(c));
        }
    }

    #[test]
    fn take_octal_literal_test() {
        assert_eq!(parse_octal_literal("0o77"), Ok(("", 0o77)));
        assert_eq!(parse_octal_literal("0O77"), Ok(("", 0o77)));
        assert_eq!(parse_octal_literal("0o177777"), Ok(("", 0o177777)));
        assert_eq!(parse_octal_literal("0o200000"), Ok(("", 0o200000)));
        assert!(parse_octal_literal("0oinvalid").is_err()); // Invalid
        assert!(parse_octal_literal("77").is_err()); // No prefix
    }

    #[test]
    fn from_binary_test() {
        assert_eq!(from_binary("10"), Ok(0b10));
        assert!(from_binary("98").is_err());
        assert_eq!(from_binary("1111111111111111"), Ok(0xffff));
        assert!(from_binary("foo").is_err());
    }

    #[test]
    fn is_bin_digit_test() {
        for c in '0'..='1' {
            assert!(is_bin_digit(c));
        }

        for c in ('2'..='9').chain('a'..='z').chain('A'..'Z') {
            assert!(!is_bin_digit(c));
        }
    }

    #[test]
    fn take_binary_literal_test() {
        assert_eq!(parse_binary_literal("0b10"), Ok(("", 0b10)));
        assert_eq!(parse_binary_literal("0B10"), Ok(("", 0b10)));
        assert_eq!(
            parse_binary_literal("0b1111111111111111"),
            Ok(("", 0b1111111111111111))
        );
        assert_eq!(
            parse_binary_literal("0b10000000000000000"),
            Ok(("", 0b10000000000000000))
        );
        assert!(parse_binary_literal("0binvalid").is_err()); // Invalid
        assert!(parse_binary_literal("10").is_err()); // No prefix
    }

    #[test]
    fn parse_literal_test() {
        // Decimal
        assert_eq!(parse_literal("100"), Ok(("", 100)));
        assert_eq!(parse_literal("42"), Ok(("", 42)));
        assert_eq!(parse_literal("65535"), Ok(("", 0xffff)));

        // Hexadecimal
        assert_eq!(parse_literal("0x4f"), Ok(("", 0x4f)));
        assert_eq!(parse_literal("0x42"), Ok(("", 0x42)));
        assert_eq!(parse_literal("0xffff"), Ok(("", 0xffff)));

        // Octal
        assert_eq!(parse_literal("0o77"), Ok(("", 0o77)));
        assert_eq!(parse_literal("0o42"), Ok(("", 0o42)));
        assert_eq!(parse_literal("0o177777"), Ok(("", 0xffff))); // Upper bound

        // Binary
        assert_eq!(parse_literal("0b10"), Ok(("", 2)));
        assert_eq!(parse_literal("0B10"), Ok(("", 2)));
        assert_eq!(parse_literal("0b1111111111111111"), Ok(("", 0xffff))); // Upper bound
    }
}
