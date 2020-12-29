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
    multi::separated_list1,
    sequence::delimited,
    IResult,
};

use crate::ast::{AstNode, Node, NodeKind};

use super::{
    location::{Locatable, Located, RelativeLocation},
    parse_identifier,
    value::{
        parse_directive_argument, parse_directive_kind, parse_instruction_argument,
        parse_instruction_kind, DirectiveArgument, DirectiveKind, InstructionArgument,
        InstructionKind,
    },
};

/// Holds the content of a line
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum LineContent<L> {
    /// Represents an instruction, with its opcode and list of arguments
    Instruction {
        kind: Located<InstructionKind, L>,
        arguments: Vec<Located<InstructionArgument<L>, L>>,
    },
    /// Represents a directive, with its type and argument
    Directive {
        kind: Located<DirectiveKind, L>,
        argument: Located<DirectiveArgument<L>, L>,
    },
}

impl<L> LineContent<L> {
    /// Check if the line is a directive
    pub(crate) fn is_directive(&self) -> bool {
        matches!(self, Self::Directive { .. })
    }
}

impl<L: Clone> AstNode<L> for LineContent<L> {
    fn kind(&self) -> NodeKind {
        match self {
            LineContent::Instruction { .. } => NodeKind::Instruction,
            LineContent::Directive { .. } => NodeKind::Directive,
        }
    }

    fn children(&self) -> Vec<Node<L>> {
        match self {
            LineContent::Instruction { kind, arguments } => std::iter::once(kind.to_node())
                .chain(arguments.iter().map(|a| a.to_node()))
                .collect(),
            LineContent::Directive { kind, argument } => vec![kind.to_node(), argument.to_node()],
        }
    }
}

impl<L> std::fmt::Display for LineContent<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LineContent::Instruction { kind, arguments } => {
                // First write the opcode
                write!(f, "{:4}", kind.inner)?;

                // then the list of arguments
                let mut first = true; // This is to properly show comma between arguments
                for arg in arguments.iter() {
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

/// Holds a whole line, with the symbol definitions (if any), the content (if any) and the comment
/// (if any).
///
/// Note that the `Default::default()` implementation represents an empty line.
#[derive(Debug, PartialEq, Default)]
pub(crate) struct Line<L> {
    pub symbols: Vec<Located<String, L>>,
    pub content: Option<Located<LineContent<L>, L>>,
    comment: Option<Located<String, L>>,
}

impl<L: Clone> AstNode<L> for Line<L> {
    fn kind(&self) -> NodeKind {
        NodeKind::Line
    }

    fn children(&self) -> Vec<Node<L>> {
        let mut children = Vec::new();

        children.extend(
            self.symbols
                .iter()
                .map(|s| Node::new(NodeKind::Symbol, s.location.clone()).content(s.inner.clone())),
        );

        children.extend(self.content.iter().map(|c| c.to_node()));

        children.extend(
            self.comment
                .iter()
                .map(|c| Node::new(NodeKind::Comment, c.location.clone()).content(c.inner.clone())),
        );

        children
    }
}

impl<L> std::fmt::Display for Line<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut had_something = false;
        for symbol in self.symbols.iter() {
            write!(f, "{}: ", symbol.inner)?;
            had_something = true;
        }

        if let Some(ref c) = self.content {
            if !c.inner.is_directive() && !had_something {
                write!(f, "    ")?;
            }
            write!(f, "{}", c.inner)?;
            had_something = true;
        }

        if let Some(ref c) = self.comment {
            if had_something {
                write!(f, "\t{}", c.inner)?;
            } else {
                write!(f, "{}", c.inner)?;
            }
        }

        Ok(())
    }
}

impl<L> Line<L>
where
    L: From<()>,
{
    #[cfg(test)] // Only used in tests for now
    pub(crate) fn symbol(mut self, symbol: &str) -> Self {
        self.symbols.push(symbol.to_string().with_location(()));
        self
    }

    #[cfg(test)] // Only used in tests for now
    pub(crate) fn directive<T: Into<DirectiveArgument<L>>>(
        mut self,
        kind: DirectiveKind,
        argument: T,
    ) -> Self {
        self.content = Some(
            LineContent::Directive {
                kind: kind.with_location(()),
                argument: argument.into().with_location(()),
            }
            .with_location(()),
        );
        self
    }

    #[cfg(test)] // Only used in tests for now
    pub(crate) fn instruction(
        mut self,
        kind: InstructionKind,
        arguments: Vec<InstructionArgument<L>>,
    ) -> Self {
        self.content = Some(
            LineContent::Instruction {
                kind: kind.with_location(()),
                arguments: arguments.into_iter().map(|a| a.with_location(())).collect(),
            }
            .with_location(()),
        );
        self
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Program<L> {
    pub(crate) lines: Vec<Located<Line<L>, L>>,
}

impl<L: Clone> AstNode<L> for Program<L> {
    fn kind(&self) -> NodeKind {
        NodeKind::Program
    }

    fn children(&self) -> Vec<Node<L>> {
        self.lines.iter().map(|l| l.to_node()).collect()
    }
}

/// Parses a directive
fn parse_directive_line(input: &str) -> IResult<&str, LineContent<RelativeLocation>> {
    let (rest, _) = char('.')(input)?;
    let start = rest;
    let (rest, kind) = parse_directive_kind(rest)?;
    let kind = kind.with_location((input, start, rest));

    let (rest, _) = space1(rest)?;

    let start = rest;
    let (rest, argument) = parse_directive_argument(rest)?;
    let argument = argument.with_location((input, start, rest));

    Ok((rest, LineContent::Directive { kind, argument }))
}

/// Parses an instruction
fn parse_instruction_line(input: &str) -> IResult<&str, LineContent<RelativeLocation>> {
    // First parse the opcode
    let (rest, kind) = parse_instruction_kind(input)?;
    let kind = kind.with_location((input, input, rest));

    // Then loop to parse the list of arguments
    let mut cursor = rest;
    let mut arguments = Vec::with_capacity(2); // Instructions usually have <= 2 arguments
    loop {
        // Check if we already parsed an argument or not
        // Explicit typing is necessary here
        let res: IResult<&str, (), ()> = if arguments.is_empty() {
            // The first argument needs at least one space before it
            // This is not done before to avoid eating spaces if the instruction takes no argument
            value((), space1)(cursor)
        } else {
            // Later arguments are separated by a comma. This also eats the spaces around the comma
            value((), delimited(space0, char(','), space0))(cursor)
        };

        // First check it has the right prefix
        if let Ok((rest, _)) = res {
            let start = rest; // Save the start of the argument for location information

            // Then continue parsing the argument
            if let Ok((rest, argument)) = parse_instruction_argument(rest) {
                let argument = argument.with_location((input, start, rest));
                arguments.push(argument);
                // Only update the cursor here, in case it fails earlier
                cursor = rest;
                continue; // Restart the loop to parse another argument
            }
        }
        break; // We could not parse another argument, get out of the loop
    }

    Ok((cursor, LineContent::Instruction { kind, arguments }))
}

/// Parses the content of a line: an instruction or a directive
fn parse_line_content(input: &str) -> IResult<&str, LineContent<RelativeLocation>> {
    alt((parse_directive_line, parse_instruction_line))(input)
}

/// Parses an inline comment
fn parse_comment(input: &str) -> IResult<&str, String> {
    let (input, _) = peek(tag("#"))(input)?;
    let (input, comment) = not_line_ending(input)?;
    Ok((input, comment.into()))
}

/// Parses symbol definitions
fn parse_symbol_definition(input: &str) -> IResult<&str, String> {
    let (input, symbol) = parse_identifier(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(':')(input)?;
    Ok((input, symbol.into()))
}

/// Parses a whole line
fn parse_line(input: &str) -> IResult<&str, Line<RelativeLocation>> {
    let (rest, _) = space0(input)?;

    // Extract the list of symbol definitions
    let mut cursor = rest;
    let mut symbols = Vec::new();
    while let Ok((rest, symbol)) = parse_symbol_definition(cursor) {
        // TODO: symbol location includes the colon, maybe we don't want that
        let symbol = symbol.with_location((input, cursor, rest));
        let (rest, _) = space0(rest)?;
        symbols.push(symbol);
        cursor = rest;
    }
    let rest = cursor;

    // Extract the line content
    let start = rest;
    let (rest, content) = opt(parse_line_content)(rest)?;
    let content = content.map(|line| line.with_location((input, start, rest))); // Save location information
    let (rest, _) = space0(rest)?;

    // Extract the comment
    let start = rest;
    let (rest, comment) = opt(parse_comment)(rest)?;
    let comment = comment.map(|comment| comment.with_location((input, start, rest))); // Save location information

    // Build the line
    Ok((
        rest,
        Line {
            symbols,
            content,
            comment,
        },
    ))
}

fn split_lines(input: &str) -> IResult<&str, Vec<&str>> {
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

pub(crate) fn parse_program(input: &str) -> IResult<&str, Program<RelativeLocation>> {
    let (rest, lines) = split_lines(input)?;
    let lines: Result<_, _> = lines
        .into_iter()
        .map(|start| {
            all_consuming(parse_line)(start)
                .map(|(end, line)| line.with_location((input, start, end)))
        })
        .collect();
    let lines = lines?;
    Ok((rest, Program { lines }))
}

#[cfg(test)]
mod tests {
    use crate::runtime::Reg;

    use super::super::location::Locatable;
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
                comment: Some("# hello".to_string().with_location((0, 7))),
                ..Default::default()
            }
        );
    }

    #[test]
    fn parse_symbol_line_test() {
        let line = fully_parsed(parse_line("hello:world : duplicate: duplicate:  "));
        assert_eq!(
            line,
            Line {
                symbols: vec![
                    "hello".to_string().with_location((0, 6)),
                    "world".to_string().with_location((6, 7)),
                    "duplicate".to_string().with_location((14, 10)),
                    "duplicate".to_string().with_location((25, 10))
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn parse_full_line_test() {
        use super::super::expression::Node;
        let line = fully_parsed(parse_line("foo: bar: .space 30 + 5 # comment"));
        assert_eq!(
            line,
            Line {
                symbols: vec![
                    "foo".to_string().with_location((0, 4)),
                    "bar".to_string().with_location((5, 4)),
                ],
                content: Some(
                    LineContent::Directive {
                        kind: DirectiveKind::Space.with_location((1, 5)),
                        argument: DirectiveArgument::Expression(Node::Sum(
                            Box::new(Node::Literal(30)).with_location((0, 2)),
                            Box::new(Node::Literal(5)).with_location((5, 1))
                        ))
                        .with_location((7, 6)),
                    }
                    .with_location((10, 13))
                ),
                comment: Some("# comment".to_string().with_location((24, 9))),
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
main: # beginning of program
    add %a, %b
    reset
        "#;

        let program = fully_parsed(parse_program(input));
        assert_eq!(
            program,
            Program {
                lines: vec![
                    Line::default().with_location((0, 0)),
                    Line {
                        symbols: vec!["str".to_string().with_location((0, 4))],
                        content: Some(
                            LineContent::Directive {
                                kind: Space.with_location((1, 5)),
                                argument: DirectiveArgument::StringLiteral(
                                    "some multiline string".to_string()
                                )
                                .with_location((7, 25))
                            }
                            .with_location((5, 32))
                        ),
                        ..Default::default()
                    }
                    .with_location((1, 37)),
                    Line {
                        symbols: vec!["main".to_string().with_location((0, 5))],
                        comment: Some("# beginning of program".to_string().with_location((6, 22))),
                        ..Default::default()
                    }
                    .with_location((39, 28)),
                    Line {
                        content: Some(
                            LineContent::Instruction {
                                kind: Add.with_location((0, 3)),
                                arguments: vec![
                                    InstructionArgument::Register(Reg::A).with_location((4, 2)),
                                    InstructionArgument::Register(Reg::B).with_location((8, 2)),
                                ],
                            }
                            .with_location((4, 10))
                        ),
                        ..Default::default()
                    }
                    .with_location((68, 14)),
                    Line {
                        content: Some(
                            LineContent::Instruction {
                                kind: Reset.with_location((0, 5)),
                                arguments: vec![],
                            }
                            .with_location((4, 5))
                        ),
                        ..Default::default()
                    }
                    .with_location((83, 9)),
                    Line::default().with_location((93, 8)),
                ]
            }
        );
    }
}
