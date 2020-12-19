//! Program line parsing
//!
//! This module parses whole program lines, including the symbol definitions, the comments and the
//! line content itself (either an instruction or a directive).
//!
//! When parsing, this module does zero copy over the original input. All members of resulting Line
//! structure reference part of the input, hence the associated lifetime on the structure tied to
//! the original input. This allows some neat tricks, especially calculating the offset of
//! a property from the input string.

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag},
    character::complete::{char, line_ending, none_of, not_line_ending, one_of, space0, space1},
    combinator::{all_consuming, eof, map, opt, peek, value},
    multi::{many0, separated_list1},
    sequence::{delimited, terminated},
    IResult,
};

use super::{parse_expression, value::Argument, Expression};
use super::{parse_identifier, parse_string_literal};

#[derive(Clone, Debug, PartialEq)]
pub enum DirectiveArgument<'a> {
    StringLiteral(String),
    Expression(Expression<'a>),
}

impl<'a> From<&str> for DirectiveArgument<'a> {
    fn from(literal: &str) -> Self {
        Self::StringLiteral(literal.to_string())
    }
}

impl<'a> From<i128> for DirectiveArgument<'a> {
    fn from(value: i128) -> Self {
        Self::Expression(Expression::Literal(value))
    }
}

/// Holds the content of a line
#[derive(Clone, Debug, PartialEq)]
pub enum LineContent<'a> {
    /// Represents an instruction, with its opcode and list of arguments
    Instruction {
        opcode: &'a str,
        arguments: Vec<Argument<'a>>,
    },
    /// Represents a directive, with its type and argument
    Directive {
        directive: &'a str,
        argument: DirectiveArgument<'a>,
    },
}

/// Holds a whole line, with the symbol definitions (if any), the content (if any) and the comment
/// (if any).
///
/// Note that the `Default::default()` implementation represents an empty line.
#[derive(Debug, PartialEq, Default)]
pub struct Line<'a> {
    pub symbols: Vec<&'a str>,
    pub content: Option<LineContent<'a>>,
    comment: Option<&'a str>,
}

impl<'a> Line<'a> {
    #[cfg(test)] // Only used in tests for now
    pub fn comment(mut self, comment: &'a str) -> Self {
        self.comment = Some(comment);
        self
    }

    #[cfg(test)] // Only used in tests for now
    pub fn symbol(mut self, symbol: &'a str) -> Self {
        self.symbols.push(symbol);
        self
    }

    #[cfg(test)] // Only used in tests for now
    pub fn directive<T: Into<DirectiveArgument<'a>>>(
        mut self,
        directive: &'a str,
        argument: T,
    ) -> Self {
        self.content = Some(LineContent::Directive {
            directive,
            argument: argument.into(),
        });
        self
    }

    #[cfg(test)] // Only used in tests for now
    pub fn instruction(mut self, opcode: &'a str, arguments: Vec<Argument<'a>>) -> Self {
        self.content = Some(LineContent::Instruction { opcode, arguments });
        self
    }
}

/// Extracts a directive argument, including string literals
fn parse_directive_argument(input: &str) -> IResult<&str, DirectiveArgument> {
    alt((
        map(parse_string_literal, DirectiveArgument::StringLiteral),
        map(parse_expression, DirectiveArgument::Expression),
    ))(input)
}

/// Parses a directive
fn parse_directive_line(input: &str) -> IResult<&str, LineContent> {
    let (input, _) = char('.')(input)?;
    let (input, directive) = parse_identifier(input)?;
    let (input, _) = space1(input)?;
    let (input, argument) = parse_directive_argument(input)?;
    Ok((
        input,
        LineContent::Directive {
            directive,
            argument,
        },
    ))
}

/// Parses an instruction
fn parse_instruction_line(input: &str) -> IResult<&str, LineContent> {
    let (input, opcode) = parse_identifier(input)?;
    let (input, _) = space1(input)?;
    let (input, arguments) = separated_list1(
        delimited(space0, char(','), space0),
        super::value::parse_argument,
    )(input)?;
    Ok((input, LineContent::Instruction { opcode, arguments }))
}

/// Parses the content of a line: an instruction or a directive
fn parse_line_content(input: &str) -> IResult<&str, LineContent> {
    alt((parse_directive_line, parse_instruction_line))(input)
}

/// Parses an inline comment
fn parse_comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = peek(tag("#"))(input)?;
    not_line_ending(input)
}

/// Parses symbol definitions
fn parse_symbol_definition(input: &str) -> IResult<&str, &str> {
    let (input, symbol) = parse_identifier(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(':')(input)?;
    Ok((input, symbol))
}

/// Parses a whole line
fn parse_line(input: &str) -> IResult<&str, Line> {
    let (input, _) = space0(input)?;
    let (input, symbols) = many0(terminated(parse_symbol_definition, space0))(input)?;
    let (input, _) = space0(input)?;
    let (input, content) = opt(parse_line_content)(input)?;
    let (input, _) = space0(input)?;
    let (input, comment) = opt(parse_comment)(input)?;
    Ok((
        input,
        Line {
            symbols,
            content,
            comment,
        },
    ))
}

fn split_lines(input: &str) -> IResult<&str, Vec<&str>> {
    let line_parser = escaped(none_of("\\\r\n"), '\\', one_of("\\\r\nrnt\""));
    let line_parser = alt((line_parser, eof, value("", peek(line_ending))));
    separated_list1(line_ending, line_parser)(input)
}

#[allow(dead_code)]
pub fn parse_program(input: &str) -> IResult<&str, Vec<Line>> {
    let (input, lines) = split_lines(input)?;
    let lines: Result<_, _> = lines
        .into_iter()
        .map(|line| all_consuming(parse_line)(line).map(|(_, line)| line))
        .collect();
    Ok((input, lines?))
}

#[cfg(test)]
mod tests {
    use crate::parser::Expression;

    use super::*;

    #[track_caller]
    fn fully_parsed<T>(result: IResult<&str, T>) -> T {
        let (input, result) = result.unwrap();
        assert_eq!(input, "");
        result
    }

    #[test]
    fn parse_empty_line_test() {
        let line = fully_parsed(parse_line(""));
        assert_eq!(line, Line::default());
    }

    #[test]
    fn parse_comment_line_test() {
        let line = fully_parsed(parse_line("# hello"));
        assert_eq!(
            line,
            Line {
                comment: Some("# hello"),
                ..Default::default()
            }
        );
    }

    #[test]
    fn parse_symbol_line_test() {
        let line = fully_parsed(parse_line("hello:world: duplicate: duplicate:  "));
        assert_eq!(
            line,
            Line {
                symbols: vec!["hello", "world", "duplicate", "duplicate"],
                ..Default::default()
            }
        );
    }

    #[test]
    fn parse_full_line_test() {
        let line = fully_parsed(parse_line("foo: bar: .space 30 + 5 # comment"));
        assert_eq!(
            line,
            Line {
                symbols: vec!["foo", "bar"],
                content: Some(LineContent::Directive {
                    directive: "space",
                    argument: DirectiveArgument::Expression(Expression::Sum(
                        Box::new(Expression::Literal(30)),
                        Box::new(Expression::Literal(5))
                    )),
                }),
                comment: Some("# comment"),
            }
        );
    }

    #[test]
    fn split_lines_test() {
        let input = r#"hello \
world
this is a new line
this has escaped chars: \r \n \t \""#;
        let lines = fully_parsed(split_lines(input));
        assert_eq!(
            lines,
            vec![
                "hello \\\nworld",
                "this is a new line",
                "this has escaped chars: \\r \\n \\t \\\""
            ]
        );
    }

    #[test]
    fn parse_program_test() {
        let input = r#"
            str: .space "some multiline \
string"
            main: # beginning of program
                add %a, %b
        "#;

        let lines = fully_parsed(parse_program(input));
        assert_eq!(
            lines,
            vec![
                Line::default(),
                Line::default().symbol("str").directive(
                    "space",
                    DirectiveArgument::StringLiteral(String::from("some multiline string"))
                ),
                Line::default()
                    .symbol("main")
                    .comment("# beginning of program"),
                Line::default().instruction(
                    "add",
                    vec![Argument::Register("a"), Argument::Register("b")]
                ),
                Line::default(),
            ]
        );
    }
}
