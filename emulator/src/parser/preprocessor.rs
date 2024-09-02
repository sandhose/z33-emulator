use nom::branch::alt;
use nom::bytes::complete::{tag, take_till};
use nom::character::complete::{char, line_ending, not_line_ending, space0, space1};
use nom::combinator::{fail, map, not, opt};
use nom::sequence::preceded;
use nom::{IResult, Offset};

use super::literal::parse_string_literal;
use super::location::{Locatable, Located};
use super::{parse_identifier, ParseError};

type Children = Vec<Located<Node>>;

#[derive(Debug, PartialEq)]
pub(crate) struct ConditionBranch {
    pub condition: Located<String>,
    pub body: Located<Children>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Node {
    Raw {
        content: String,
    },
    NewLine {
        // Whether the line ending a '\r\n'
        crlf: bool,
    },
    Error {
        message: Located<String>,
    },
    Undefine {
        key: Located<String>,
    },
    Definition {
        key: Located<String>,
        content: Option<Located<String>>,
    },
    Inclusion {
        path: Located<String>,
    },
    Condition {
        branches: Vec<ConditionBranch>,
        fallback: Option<Located<Children>>,
    },
}

impl Node {
    pub(crate) fn walk<'a, F>(&'a self, f: &mut F)
    where
        F: FnMut(&'a Self),
    {
        f(self);

        if let Node::Condition { branches, fallback } = self {
            for branch in branches {
                for chunk in &branch.body.inner {
                    chunk.inner.walk(f);
                }
            }

            if let Some(fallback) = fallback {
                for chunk in &fallback.inner {
                    chunk.inner.walk(f);
                }
            }
        }
    }

    /// Returns `true` if we should record line endings after the node
    #[must_use]
    fn should_record_line_ending(&self) -> bool {
        matches!(self, Self::Raw { .. } | Self::Inclusion { .. })
    }
}

/// Eats the end of a line, including trailing spaces, inline comments and the
/// line ending
fn eat_end_of_line<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (), Error> {
    let (rest, _) = space0(input)?;
    let (rest, _) = opt(preceded(tag("//"), not_line_ending))(rest)?;
    Ok((rest, ()))
}

/// Extracts the argument of a directive
/// It tries to stop before any trailing whitespace or comment
fn parse_directive_argument<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, &'a str, Error> {
    let mut cursor = input;
    loop {
        let (rest, _) = space0(cursor)?;

        // peek at the next thing after the spaces
        if let Some("//" | "\r\n") = rest.get(..2) {
            break;
        }

        if let Some("\n") = rest.get(..1) {
            break;
        }

        if rest.is_empty() {
            break;
        }

        let (rest, _) =
            take_till(|c| c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '/')(rest)?;
        cursor = rest;
    }

    let index = input.offset(cursor);
    if index == 0 {
        return Err(nom::Err::Error(nom::error::ParseError::from_error_kind(
            input,
            nom::error::ErrorKind::Eof, // TODO: maybe not the best kind
        )));
    }

    let content = &input[..index];
    Ok((cursor, content))
}

fn parse_definition<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Node, Error> {
    let (rest, _) = char('#')(input)?;
    let (rest, _) = space0(rest)?;
    let (rest, _) = tag("define")(rest)?;
    let (rest, _) = space1(rest)?;
    let start = rest;
    let (rest, key) = parse_identifier(rest)?;
    let key = key
        .to_owned()
        .with_location(input.offset(start)..input.offset(rest));

    let (rest, content) = opt(|rest| {
        let (rest, _) = space1(rest)?;
        let start = rest;
        let (rest, content) = parse_directive_argument(rest)?;
        let content = content
            .to_owned()
            .with_location(input.offset(start)..input.offset(rest));
        Ok((rest, content))
    })(rest)?;

    let (rest, ()) = eat_end_of_line(rest)?;

    Ok((rest, Node::Definition { key, content }))
}

fn parse_undefine<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Node, Error> {
    let (rest, _) = char('#')(input)?;
    let (rest, _) = space0(rest)?;
    let (rest, _) = tag("undefine")(rest)?;
    let (rest, _) = space1(rest)?;

    let start = rest;
    let (rest, key) = parse_identifier(rest)?;
    let key = key
        .to_owned()
        .with_location(input.offset(start)..input.offset(rest));

    let (rest, ()) = eat_end_of_line(rest)?;

    Ok((rest, Node::Undefine { key }))
}

fn parse_inclusion<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Node, Error> {
    // Parse "#include"
    let (rest, _) = char('#')(input)?;
    let (rest, _) = space0(rest)?;
    let (rest, _) = tag("include")(rest)?;
    let (rest, _) = space1(rest)?;

    // Parse the argument
    let start = rest;
    let (rest, path) = parse_string_literal(rest)?;
    let path = path.with_location(input.offset(start)..input.offset(rest));

    let (rest, ()) = eat_end_of_line(rest)?;

    Ok((rest, Node::Inclusion { path }))
}

fn parse_error<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Node, Error> {
    // Parse "#error"
    let (rest, _) = char('#')(input)?;
    let (rest, _) = space0(rest)?;
    let (rest, _) = tag("error")(rest)?;
    let (rest, _) = space1(rest)?;

    // Parse the argument
    let start = rest;
    let (rest, message) = parse_string_literal(rest)?;
    let message = message.with_location(input.offset(start)..input.offset(rest));

    let (rest, ()) = eat_end_of_line(rest)?;

    Ok((rest, Node::Error { message }))
}

fn parse_condition<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Node, Error> {
    // This structure helps parsing the else/elif/end directives
    enum BranchDirective {
        Else,
        ElseIf(Located<String>),
        EndIf,
    }
    use BranchDirective::{Else, ElseIf, EndIf};

    // First, get the "#if"
    let (rest, _) = char('#')(input)?;
    let (rest, _) = space0(rest)?;
    let (rest, _) = tag("if")(rest)?;
    let (rest, _) = space1(rest)?;

    // Then parse the first condition
    let start = rest;
    let (rest, condition) = parse_directive_argument(rest)?;
    let condition = condition
        .to_string()
        .with_location(input.offset(start)..input.offset(rest));

    let (rest, ()) = eat_end_of_line(rest)?;
    let (rest, _) = line_ending(rest)?;

    // Parse its body
    let start = rest;
    let (rest, body) = parse(rest)?;
    let body = body.with_location(input.offset(start)..input.offset(rest));

    // We have the first branch parsed, let's parse the others
    let branch = ConditionBranch { condition, body };

    let mut cursor = rest; // This saves current location in the input
    let mut branches = vec![branch];
    let mut fallback = None;

    loop {
        // First, get the "#if"
        let (rest, _) = char('#')(cursor)?;
        let (rest, _) = space0(rest)?;
        let (rest, directive) = alt((
            map(tag("endif"), |_| EndIf),
            |rest| {
                // Parse a "elif" directive
                let (rest, _) = tag("elif")(rest)?;
                let (rest, _) = space1(rest)?;
                let start = rest;
                let (rest, condition) = parse_directive_argument(rest)?;
                let condition = condition
                    .to_string()
                    .with_location(input.offset(start)..input.offset(rest));
                Ok((rest, ElseIf(condition)))
            },
            map(tag("else"), |_| Else),
        ))(rest)?;

        let (rest, ()) = eat_end_of_line(rest)?;

        // We've got an "#endif" directive, get out of the loop
        // We don't update the cursor here since we will be re-parsing the #endif
        // directive afterward
        if let EndIf = directive {
            break;
        }

        let (rest, _) = line_ending(rest)?;

        let start = rest;
        let (rest, body) = parse(rest)?;
        let body = body.with_location(input.offset(start)..input.offset(rest));

        cursor = rest;

        if let ElseIf(condition) = directive {
            branches.push(ConditionBranch { condition, body });
        } else {
            fallback = Some(body);
            break;
        }
    }

    let (rest, _) = char('#')(cursor)?;
    let (rest, _) = space0(rest)?;
    let (rest, _) = tag("endif")(rest)?;

    let (rest, ()) = eat_end_of_line(rest)?;

    Ok((rest, Node::Condition { branches, fallback }))
}

fn parse_raw<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Node, Error> {
    let (rest, ()) = not(char('#'))(input)?;
    let (rest, content) = not_line_ending(rest)?;
    // Strip the comment from the content
    let content = if let Some(i) = content.find("//") {
        &content[..i]
    } else {
        content
    };
    let content = content.to_string();
    Ok((rest, Node::Raw { content }))
}

fn parse_chunk<'a, Error: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Node, Error> {
    // If we're at the end of the input, just stop parsing,
    // so that we don't emit an empty raw node
    if input.is_empty() {
        return fail(input);
    }

    alt((
        parse_definition, // #define X [Y]
        parse_undefine,   // #undefine X
        parse_inclusion,  // #include "X"
        parse_condition,  // #if X ... [#elif Y ...] [#else Z ...] #endif
        parse_error,      // #error "X"
        parse_raw,        // anything else
    ))(input)
}

pub(crate) fn parse<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Children, Error> {
    let mut chunks = Vec::new();
    let mut cursor = input;

    while let (rest, Some(chunk)) = opt(parse_chunk)(cursor)? {
        let record_line_ending = chunk.should_record_line_ending();
        let chunk = chunk.with_location(input.offset(cursor)..input.offset(rest));
        chunks.push(chunk);

        cursor = rest;

        if let Ok((rest, ending)) = line_ending::<_, nom::error::Error<_>>(rest) {
            // We want to record line ending nodes only if we got a Raw or Inclusion node
            // just before. This is because we echo those, and want to keep location
            // information for them
            if record_line_ending {
                let chunk = Node::NewLine {
                    crlf: ending.len() == 2,
                }
                .with_location(input.offset(cursor)..input.offset(rest));

                chunks.push(chunk);
            }

            cursor = rest;
        } else {
            // Got no linebreak, get out of the loop
            break;
        }
    }

    Ok((cursor, chunks))
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn parse_directive_argument_test() {
        let res = parse_directive_argument::<()>("foo").unwrap();
        assert_eq!(res, ("", "foo"));
    }

    #[test]
    fn parse_definition_test() {
        let res = parse_definition::<()>("#define foo bar").unwrap();
        assert_eq!(
            res,
            (
                "",
                Node::Definition {
                    key: "foo".to_owned().with_location(8..11),
                    content: Some("bar".to_owned().with_location(12..15)),
                }
            )
        );

        let res = parse_definition::<()>("#define foo").unwrap();
        assert_eq!(
            res,
            (
                "",
                Node::Definition {
                    key: "foo".to_owned().with_location(8..11),
                    content: None,
                }
            )
        );

        let res = parse_definition::<()>("#define trailing ").unwrap();
        assert_eq!(
            res,
            (
                "",
                Node::Definition {
                    key: "trailing".to_owned().with_location(8..16),
                    content: None,
                }
            )
        );
    }

    #[test]
    fn parse_inclusion_test() {
        let res = parse_inclusion::<()>("#include \"foo\"").unwrap();
        assert_eq!(
            res,
            (
                "",
                Node::Inclusion {
                    path: "foo".to_string().with_location(9..14)
                }
            )
        );
    }

    #[test]
    fn parse_raw_test() {
        // It extracts the line
        let (rest, body) = parse_raw::<()>("line").unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            body,
            Node::Raw {
                content: "line".to_string()
            }
        );

        // It extracts the line and discard the comment
        let (rest, body) = parse_raw::<()>("line // comment").unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            body,
            Node::Raw {
                content: "line ".to_string()
            }
        );

        // Gets only one line
        let (rest, body) = parse_raw::<()>("line\nline").unwrap();
        assert_eq!(rest, "\nline");
        assert_eq!(
            body,
            Node::Raw {
                content: "line".to_string()
            }
        );

        // Does not get directives
        assert!(parse_raw::<()>("#directive").is_err());
    }

    #[test]
    fn parse_simple_test() {
        use Node::{Definition, Error, Inclusion, NewLine, Raw, Undefine};
        let (rest, body) = parse::<()>(indoc::indoc! {r#"
            hello
            #include "foo"
            #define bar baz
            world
            #error "deprecated"
            #undefine bar
            #define test
        "#})
        .unwrap();
        assert_eq!(rest, "");

        assert_eq!(
            body,
            vec![
                Raw {
                    content: "hello".to_string()
                }
                .with_location(0..5),
                NewLine { crlf: false }.with_location(5..6),
                Inclusion {
                    path: "foo".to_string().with_location(9..14)
                }
                .with_location(6..20),
                NewLine { crlf: false }.with_location(20..21),
                Definition {
                    key: "bar".to_string().with_location(8..11),
                    content: Some("baz".to_string().with_location(12..15))
                }
                .with_location(21..36),
                Raw {
                    content: "world".to_string()
                }
                .with_location(37..42),
                NewLine { crlf: false }.with_location(42..43),
                Error {
                    message: "deprecated".to_string().with_location(7..19)
                }
                .with_location(43..62),
                Undefine {
                    key: "bar".to_string().with_location(10..13)
                }
                .with_location(63..76),
                Definition {
                    key: "test".to_string().with_location(8..12),
                    content: None,
                }
                .with_location(77..89),
            ]
        );
    }

    #[test]
    fn parse_weird_test() {
        use Node::{Definition, Error, Inclusion, NewLine, Raw, Undefine};
        let (rest, body) = parse::<()>(indoc::indoc! {r#"
            hello
            #  include   "foo"//comment
            # define   bar   baz  // comment
            world
            #   error    "deprecated"
            # undefine  bar //comment
            #   define  test // comment

            empty line
        "#})
        .unwrap();
        assert_eq!(rest, "");

        assert_eq!(
            body,
            vec![
                Raw {
                    content: "hello".to_string()
                }
                .with_location(0..5),
                NewLine { crlf: false }.with_location(5..6),
                Inclusion {
                    path: "foo".to_string().with_location(13..18)
                }
                .with_location(6..33),
                NewLine { crlf: false }.with_location(33..34),
                Definition {
                    key: "bar".to_string().with_location(11..14),
                    content: Some("baz".to_string().with_location(17..20))
                }
                .with_location(34..66),
                Raw {
                    content: "world".to_string()
                }
                .with_location(67..72),
                NewLine { crlf: false }.with_location(72..73),
                Error {
                    message: "deprecated".to_string().with_location(13..25)
                }
                .with_location(73..98),
                Undefine {
                    key: "bar".to_string().with_location(12..15),
                }
                .with_location(99..124),
                Definition {
                    key: "test".to_string().with_location(12..16),
                    content: None,
                }
                .with_location(125..152),
                Raw {
                    content: String::new()
                }
                .with_location(153..153),
                NewLine { crlf: false }.with_location(153..154),
                Raw {
                    content: "empty line".to_string()
                }
                .with_location(154..164),
                NewLine { crlf: false }.with_location(164..165),
            ]
        );
    }

    #[test]
    fn parse_condition_test() {
        use Node::{Condition, NewLine, Raw};
        let input = indoc::indoc! {r"
            #if true
            foo
            #if false
            bar
            #endif
            #elif true // with a comment
            foobar
            #else
            baz
            #endif
        "}
        .trim_end(); // Remove the trailing linebreak

        // Check a few locations used bellow
        assert_eq!(&input[4..8], "true"); // line 1
        assert_eq!(&input[9..12], "foo"); // line 2
        assert_eq!(&input[40..44], "true"); // line 6
        assert_eq!(&input[63..69], "foobar"); // line 7
        assert_eq!(&input[76..79], "baz"); // line 9

        let (rest, condition) = parse_condition::<()>(input).unwrap();

        assert_eq!(rest, "");
        assert_eq!(
            condition,
            Condition {
                branches: vec![
                    ConditionBranch {
                        condition: "true".to_string().with_location(4..8),
                        body: vec![
                            Raw {
                                content: "foo".to_string()
                            }
                            .with_location(0..3),
                            NewLine { crlf: false }.with_location(3..4),
                            Condition {
                                branches: vec![ConditionBranch {
                                    condition: "false".to_string().with_location(4..9),
                                    body: vec![
                                        Raw {
                                            content: "bar".to_string()
                                        }
                                        .with_location(0..3),
                                        NewLine { crlf: false }.with_location(3..4)
                                    ]
                                    .with_location(10..14)
                                },],
                                fallback: None,
                            }
                            .with_location(4..24),
                        ]
                        .with_location(9..34)
                    },
                    ConditionBranch {
                        condition: "true".to_string().with_location(40..44),
                        body: vec![
                            Raw {
                                content: "foobar".to_string()
                            }
                            .with_location(0..6),
                            NewLine { crlf: false }.with_location(6..7),
                        ]
                        .with_location(63..70)
                    }
                ],
                fallback: Some(
                    vec![
                        // "else" content
                        Raw {
                            content: "baz".to_string()
                        }
                        .with_location(0..3),
                        NewLine { crlf: false }.with_location(3..4),
                    ]
                    .with_location(76..80)
                )
            }
        );
    }
}
