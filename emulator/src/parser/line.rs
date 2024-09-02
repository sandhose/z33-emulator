//! Program line parsing
//!
//! This module parses whole program lines, including the symbol definitions,
//! the comments and the line content itself (either an instruction or a
//! directive).
//!
//! When parsing, this module does zero copy over the original input. All
//! members of resulting Line structure reference part of the input, hence the
//! associated lifetime on the structure tied to the original input. This allows
//! some neat tricks, especially calculating the offset of a property from the
//! input string.

use nom::branch::alt;
use nom::bytes::complete::escaped;
use nom::character::complete::{char, line_ending, none_of, one_of, space0, space1};
use nom::combinator::{all_consuming, cut, eof, map, opt, peek, value};
use nom::error::context;
use nom::multi::separated_list1;
use nom::sequence::delimited;
use nom::{IResult, Offset};

use super::location::{Locatable, Located};
use super::value::{
    parse_directive_argument, parse_directive_kind, parse_instruction_argument,
    parse_instruction_kind, DirectiveArgument, DirectiveKind, InstructionArgument, InstructionKind,
};
use super::{parse_identifier, ParseError};
use crate::ast::{AstNode, Node, NodeKind};

/// Holds the content of a line
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum LineContent {
    /// Represents an instruction, with its opcode and list of arguments
    Instruction {
        kind: Located<InstructionKind>,
        arguments: Vec<Located<InstructionArgument>>,
    },
    /// Represents a directive, with its type and argument
    Directive {
        kind: Located<DirectiveKind>,
        argument: Located<DirectiveArgument>,
    },
}

impl LineContent {
    /// Check if the line is a directive
    pub(crate) fn is_directive(&self) -> bool {
        matches!(self, Self::Directive { .. })
    }
}

impl AstNode for LineContent {
    fn kind(&self) -> NodeKind {
        match self {
            LineContent::Instruction { .. } => NodeKind::Instruction,
            LineContent::Directive { .. } => NodeKind::Directive,
        }
    }

    fn children(&self) -> Vec<Node> {
        match self {
            LineContent::Instruction { kind, arguments } => std::iter::once(kind.to_node())
                .chain(arguments.iter().map(Located::to_node))
                .collect(),
            LineContent::Directive { kind, argument } => vec![kind.to_node(), argument.to_node()],
        }
    }
}

impl std::fmt::Display for LineContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LineContent::Instruction { kind, arguments } => {
                // First write the opcode
                write!(f, "{:4}", kind.inner)?;

                // then the list of arguments
                let mut first = true; // This is to properly show comma between arguments
                for arg in arguments {
                    if !first {
                        write!(f, ",")?;
                    }
                    write!(f, " {}", arg.inner)?;
                    first = false;
                }
                Ok(())
            }
            LineContent::Directive { kind, argument } => {
                write!(f, ".{}: {}", kind.inner, argument.inner)
            }
        }
    }
}

/// Holds a whole line, with the symbol definitions (if any), the content (if
/// any) and the comment (if any).
///
/// Note that the `Default::default()` implementation represents an empty line.
#[derive(Debug, Clone, PartialEq, Default)]
pub(crate) struct Line {
    pub symbols: Vec<Located<String>>,
    pub content: Option<Located<LineContent>>,
}

impl AstNode for Line {
    fn kind(&self) -> NodeKind {
        NodeKind::Line
    }

    fn children(&self) -> Vec<Node> {
        let mut children = Vec::new();

        children.extend(
            self.symbols
                .iter()
                .map(|s| Node::new(NodeKind::Symbol, s.location.clone()).content(s.inner.clone())),
        );

        children.extend(self.content.iter().map(Located::to_node));

        children
    }
}

impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut had_something = false;
        for symbol in &self.symbols {
            write!(f, "{}: ", symbol.inner)?;
            had_something = true;
        }

        if let Some(ref c) = self.content {
            if !c.inner.is_directive() && !had_something {
                write!(f, "    ")?;
            }
            write!(f, "{}", c.inner)?;
        }

        Ok(())
    }
}

impl Line {
    #[cfg(test)] // Only used in tests for now
    pub(crate) fn empty() -> Located<Self> {
        Self::default().with_location(0..0)
    }
}

impl Located<Line> {
    #[cfg(test)] // Only used in tests for now
    pub(crate) fn symbol(mut self, symbol: &str) -> Self {
        self.inner
            .symbols
            .push(symbol.to_string().with_location(0..0));
        self
    }

    #[cfg(test)] // Only used in tests for now
    pub(crate) fn instruction(
        mut self,
        kind: InstructionKind,
        arguments: Vec<InstructionArgument>,
    ) -> Self {
        self.inner.content = Some(
            LineContent::Instruction {
                kind: kind.with_location(0..0),
                arguments: arguments
                    .into_iter()
                    .map(|a| a.with_location(0..0))
                    .collect(),
            }
            .with_location(0..0),
        );
        self
    }

    #[cfg(test)] // Only used in tests for now
    pub(crate) fn directive<T: Into<DirectiveArgument>>(
        mut self,
        kind: DirectiveKind,
        argument: T,
    ) -> Self {
        self.inner.content = Some(
            LineContent::Directive {
                kind: kind.with_location(0..0),
                argument: argument.into().with_location(0..0),
            }
            .with_location(0..0),
        );
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub(crate) lines: Vec<Located<Line>>,
}

impl Program {
    #[must_use]
    pub fn labels(&self) -> Vec<&str> {
        self.lines
            .iter()
            .flat_map(|line| line.inner.symbols.iter().map(|s| s.inner.as_str()))
            .collect()
    }
}

impl AstNode for Program {
    fn kind(&self) -> NodeKind {
        NodeKind::Program
    }

    fn children(&self) -> Vec<Node> {
        self.lines.iter().map(Located::to_node).collect()
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in &self.lines {
            writeln!(f, "{}", line.inner)?;
        }

        Ok(())
    }
}

/// Parses a directive
fn parse_directive_line<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, LineContent, Error> {
    let (rest, _) = char('.')(input)?;

    cut(|rest: &'a str| {
        let start = rest;
        let (rest, kind) = parse_directive_kind(rest)?;
        let kind = kind.with_location(input.offset(start)..input.offset(rest));

        let (rest, _) = space1(rest)?;

        let start = rest;
        let (rest, argument) = parse_directive_argument(rest)?;
        let argument = argument.with_location(input.offset(start)..input.offset(rest));

        Ok((rest, LineContent::Directive { kind, argument }))
    })(rest)
}

/// Parses an instruction
fn parse_instruction_line<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, LineContent, Error> {
    // First parse the opcode
    let (rest, kind) = parse_instruction_kind(input)?;

    cut(move |rest: &'a str| {
        let kind = kind.with_location(input.offset(input)..input.offset(rest));
        // Then loop to parse the list of arguments
        let mut cursor = rest;
        let mut arguments = Vec::with_capacity(2); // Instructions usually have <= 2 arguments
        loop {
            // Check if we already parsed an argument or not
            // Explicit typing is necessary here
            let (rest, succeded) = if arguments.is_empty() {
                // The first argument needs at least one space before it
                // This is not done before to avoid eating spaces if the instruction takes no
                // argument
                opt(value((), space1))(cursor)?
            } else {
                // Later arguments are separated by a comma. This also eats the spaces around
                // the comma
                opt(value((), delimited(space0, char(','), space0)))(cursor)?
            };

            // First check it has the right prefix
            if succeded.is_some() {
                let start = rest; // Save the start of the argument for location information

                // Then continue parsing the argument
                if let (rest, Some(argument)) = opt(parse_instruction_argument)(rest)? {
                    let argument = argument.with_location(input.offset(start)..input.offset(rest));
                    arguments.push(argument);
                    // Only update the cursor here, in case it fails earlier
                    cursor = rest;
                    continue; // Restart the loop to parse another argument
                }
            }
            break; // We could not parse another argument, get out of the loop
        }

        Ok((cursor, LineContent::Instruction { kind, arguments }))
    })(rest)
}

/// Parses the content of a line: an instruction or a directive
fn parse_line_content<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, LineContent, Error> {
    alt((
        context("directive", parse_directive_line),
        context("instruction", parse_instruction_line),
    ))(input)
}

/// Parses symbol definitions
fn parse_symbol_definition<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, String, Error> {
    let (input, symbol) = parse_identifier(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(':')(input)?;
    Ok((input, symbol.into()))
}

/// Parses a whole line
fn parse_line<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Line, Error> {
    let (rest, _) = space0(input)?;

    // Extract the list of symbol definitions
    let mut cursor = rest;
    let mut symbols = Vec::new();
    while let (rest, Some(symbol)) = opt(parse_symbol_definition)(cursor)? {
        // TODO: symbol location includes the colon, maybe we don't want that
        let symbol = symbol.with_location(input.offset(cursor)..input.offset(rest));
        let (rest, _) = space0(rest)?;
        symbols.push(symbol);
        cursor = rest;
    }
    let rest = cursor;

    // Extract the line content
    let start = rest;
    let (rest, content) = opt(parse_line_content)(rest)?;
    let content = content.map(|line| line.with_location(input.offset(start)..input.offset(rest))); // Save location information
    let (rest, _) = space0(rest)?;

    // Build the line
    Ok((rest, Line { symbols, content }))
}

fn split_lines<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<&str>, Error> {
    let line_parser = escaped(none_of("\\\r\n"), '\\', one_of("\\\r\nrnt\""));
    let line_parser = alt((
        // either we have an escaped line
        line_parser,
        // or an EOF
        eof,
        // or an empty line (just peek for the line ending & make the result zero-length)
        map(peek(line_ending), |i: &str| &i[..0]),
    ));
    separated_list1(line_ending, line_parser)(input)
}

pub(crate) fn parse_program<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Program, Error> {
    let (rest, lines) = split_lines(input)?;
    // TODO: bubble up more detailed errors here
    let lines: Result<_, _> = lines
        .into_iter()
        .map(|start| {
            context("line", all_consuming(parse_line))(start)
                .map(|(end, line)| line.with_location(input.offset(start)..input.offset(end)))
        })
        .collect();
    let lines = lines?;
    Ok((rest, Program { lines }))
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::super::location::Locatable;
    use super::*;
    use crate::runtime::Reg;

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
    fn parse_symbol_line_test() {
        let line = fully_parsed(parse_line("hello:world : duplicate: duplicate:  "));
        assert_eq!(
            line,
            Line {
                symbols: vec![
                    "hello".to_string().with_location(0..6),
                    "world".to_string().with_location(6..13),
                    "duplicate".to_string().with_location(14..24),
                    "duplicate".to_string().with_location(25..35)
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn parse_full_line_test() {
        use super::super::expression::Node;
        let line = fully_parsed(parse_line("foo: bar: .space 30 + 5"));
        assert_eq!(
            line,
            Line {
                symbols: vec![
                    "foo".to_string().with_location(0..4),
                    "bar".to_string().with_location(5..9),
                ],
                content: Some(
                    LineContent::Directive {
                        kind: DirectiveKind::Space.with_location(1..6),
                        argument: DirectiveArgument::Expression(Node::Sum(
                            Box::new(Node::Literal(30)).with_location(0..2),
                            Box::new(Node::Literal(5)).with_location(5..6)
                        ))
                        .with_location(7..13),
                    }
                    .with_location(10..23)
                ),
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
        use DirectiveKind::Space;
        use InstructionKind::{Add, Reset};

        let input = r#"
str: .space "some multiline \
string"
main:
    add %a, %b
    reset
        "#;

        let program = fully_parsed(parse_program(input));
        assert_eq!(
            program,
            Program {
                lines: vec![
                    Line::default().with_location(0..0),
                    Line {
                        symbols: vec!["str".to_string().with_location(0..4)],
                        content: Some(
                            LineContent::Directive {
                                kind: Space.with_location(1..6),
                                argument: DirectiveArgument::StringLiteral(
                                    "some multiline string".to_string()
                                )
                                .with_location(7..32)
                            }
                            .with_location(5..37)
                        ),
                    }
                    .with_location(1..38),
                    Line {
                        symbols: vec!["main".to_string().with_location(0..5)],
                        ..Default::default()
                    }
                    .with_location(39..44),
                    Line {
                        content: Some(
                            LineContent::Instruction {
                                kind: Add.with_location(0..3),
                                arguments: vec![
                                    InstructionArgument::Register(Reg::A).with_location(4..6),
                                    InstructionArgument::Register(Reg::B).with_location(8..10),
                                ],
                            }
                            .with_location(4..14)
                        ),
                        ..Default::default()
                    }
                    .with_location(45..59),
                    Line {
                        content: Some(
                            LineContent::Instruction {
                                kind: Reset.with_location(0..5),
                                arguments: vec![],
                            }
                            .with_location(4..9)
                        ),
                        ..Default::default()
                    }
                    .with_location(60..69),
                    Line::default().with_location(70..78),
                ]
            }
        );
    }

    #[test]
    fn parse_empty_program_test() {
        let program = fully_parsed(parse_program(""));
        assert_eq!(
            program,
            Program {
                lines: vec![Line::default().with_location(0..0)]
            }
        );
    }
}
