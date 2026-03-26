//! Preprocessor directive parser.
//!
//! Parses preprocessor directives (`#define`, `#include`, `#if`, etc.) and raw
//! assembly lines from source files. Uses chumsky for individual directive
//! parsing with an imperative top-level loop to handle the recursive
//! `#if/#elif/#else/#endif` block structure.

use chumsky::prelude::*;

use super::location::{Locatable, Located};
use super::shared::{
    hspace, identifier, span_to_range, string_literal, Extra, Span,
};

pub(crate) type Children = Vec<Located<Node>>;

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
        // Whether the line ending is '\r\n'
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

// ---------------------------------------------------------------------------
// Chumsky sub-parsers for individual directive components
// ---------------------------------------------------------------------------

/// Consume trailing whitespace and an optional `//` comment (but NOT the
/// newline itself).
fn eat_end_of_line<'a>() -> impl Parser<'a, &'a str, (), Extra<'a>> + Clone {
    hspace()
        .then(just("//").then(any().and_is(just('\n').not()).repeated()).or_not())
        .ignored()
}

/// Extract a directive argument: greedily consume non-whitespace tokens
/// separated by spaces, stopping before `//`, `\r\n`, `\n`, or EOF.
/// Trailing whitespace before the terminator is NOT included.
fn directive_argument<'a>() -> impl Parser<'a, &'a str, &'a str, Extra<'a>> + Clone {
    // Match: one or more segments of non-space, non-comment text
    any()
        .filter(|c: &char| !matches!(c, ' ' | '\t' | '\r' | '\n' | '/'))
        .repeated()
        .at_least(1)
        .then(
            // Optionally continue after whitespace if the next segment isn't a
            // comment/newline/EOF
            any()
                .filter(|c: &char| *c == ' ' || *c == '\t')
                .repeated()
                .at_least(1)
                .then(
                    any()
                        .filter(|c: &char| !matches!(c, ' ' | '\t' | '\r' | '\n' | '/'))
                        .repeated()
                        .at_least(1),
                )
                .repeated(),
        )
        .to_slice()
}

/// Parse `#define KEY [VALUE]`
fn parse_definition(input: &str) -> Option<Node> {
    let parser = just('#')
        .then(hspace())
        .ignore_then(just("define"))
        .ignore_then(
            any()
                .filter(|c: &char| *c == ' ' || *c == '\t')
                .repeated()
                .at_least(1),
        )
        .ignore_then(
            identifier()
                .map_with(|id: &str, e| id.to_owned().with_location(span_to_range(e.span())))
                .then(
                    any()
                        .filter(|c: &char| *c == ' ' || *c == '\t')
                        .repeated()
                        .at_least(1)
                        .ignore_then(
                            directive_argument().map_with(|arg: &str, e| {
                                arg.to_owned().with_location(span_to_range(e.span()))
                            }),
                        )
                        .or_not(),
                ),
        )
        .then_ignore(eat_end_of_line())
        .then_ignore(end())
        .map(|(key, content)| Node::Definition { key, content });

    parser.parse(input).into_result().ok()
}

/// Parse `#undefine KEY`
fn parse_undefine(input: &str) -> Option<Node> {
    let parser = just('#')
        .then(hspace())
        .ignore_then(just("undefine"))
        .ignore_then(
            any()
                .filter(|c: &char| *c == ' ' || *c == '\t')
                .repeated()
                .at_least(1),
        )
        .ignore_then(
            identifier()
                .map_with(|id: &str, e| id.to_owned().with_location(span_to_range(e.span()))),
        )
        .then_ignore(eat_end_of_line())
        .then_ignore(end())
        .map(|key| Node::Undefine { key });

    parser.parse(input).into_result().ok()
}

/// Parse `#include "path"`
fn parse_inclusion(input: &str) -> Option<Node> {
    let parser = just('#')
        .then(hspace())
        .ignore_then(just("include"))
        .ignore_then(
            any()
                .filter(|c: &char| *c == ' ' || *c == '\t')
                .repeated()
                .at_least(1),
        )
        .ignore_then(
            string_literal()
                .map_with(|s, e| s.with_location(span_to_range(e.span()))),
        )
        .then_ignore(eat_end_of_line())
        .then_ignore(end())
        .map(|path| Node::Inclusion { path });

    parser.parse(input).into_result().ok()
}

/// Parse `#error "message"`
fn parse_error_directive(input: &str) -> Option<Node> {
    let parser = just('#')
        .then(hspace())
        .ignore_then(just("error"))
        .ignore_then(
            any()
                .filter(|c: &char| *c == ' ' || *c == '\t')
                .repeated()
                .at_least(1),
        )
        .ignore_then(
            string_literal()
                .map_with(|s, e| s.with_location(span_to_range(e.span()))),
        )
        .then_ignore(eat_end_of_line())
        .then_ignore(end())
        .map(|message| Node::Error { message });

    parser.parse(input).into_result().ok()
}

/// Parse `#if CONDITION` header, returning the condition text.
fn parse_if_header(input: &str) -> Option<Located<String>> {
    let parser = just('#')
        .then(hspace())
        .ignore_then(just("if"))
        .ignore_then(
            any()
                .filter(|c: &char| *c == ' ' || *c == '\t')
                .repeated()
                .at_least(1),
        )
        .ignore_then(
            directive_argument()
                .map_with(|arg: &str, e| arg.to_owned().with_location(span_to_range(e.span()))),
        )
        .then_ignore(eat_end_of_line())
        .then_ignore(end());

    parser.parse(input).into_result().ok()
}

/// Check if a line is `#elif CONDITION`, returning the condition if so.
fn parse_elif_header(input: &str) -> Option<Located<String>> {
    let parser = just('#')
        .then(hspace())
        .ignore_then(just("elif"))
        .ignore_then(
            any()
                .filter(|c: &char| *c == ' ' || *c == '\t')
                .repeated()
                .at_least(1),
        )
        .ignore_then(
            directive_argument()
                .map_with(|arg: &str, e| arg.to_owned().with_location(span_to_range(e.span()))),
        )
        .then_ignore(eat_end_of_line())
        .then_ignore(end());

    parser.parse(input).into_result().ok()
}

/// Check if a line is `#else`.
fn is_else_directive(input: &str) -> bool {
    let parser = just('#')
        .then(hspace())
        .ignore_then(just("else"))
        .then_ignore(eat_end_of_line())
        .then_ignore(end());

    parser.parse(input).into_result().is_ok()
}

/// Check if a line is `#endif`.
fn is_endif_directive(input: &str) -> bool {
    let parser = just('#')
        .then(hspace())
        .ignore_then(just("endif"))
        .then_ignore(eat_end_of_line())
        .then_ignore(end());

    parser.parse(input).into_result().is_ok()
}

/// Parse a raw (non-directive) line, stripping `//` comments.
fn parse_raw_line(line: &str) -> Option<Node> {
    // Raw lines must not start with '#'
    if line.starts_with('#') {
        return None;
    }

    // Strip inline comment
    let content = if let Some(i) = line.find("//") {
        &line[..i]
    } else {
        line
    };

    Some(Node::Raw {
        content: content.to_string(),
    })
}

// ---------------------------------------------------------------------------
// Top-level parse function
// ---------------------------------------------------------------------------

/// Parse preprocessor directives and raw assembly lines.
///
/// Returns a list of `Located<Node>` where:
/// - Top-level node locations are absolute byte offsets in `input`
/// - Body locations inside `#if` blocks are also absolute in `input`
/// - Body *contents* have locations relative to the body start
///
/// This matches the contract expected by `preprocessor/mod.rs`.
pub(crate) fn parse(input: &str) -> Result<Children, String> {
    let chunks = parse_chunks(input, 0)?;

    // Verify all input was consumed. parse_chunks stops at #elif/#else/#endif
    // boundaries — if we see those at the top level, it's a syntax error.
    let consumed: usize = chunks.last().map_or(0, |c| c.location.end);
    let remaining = input[consumed..].trim();
    if !remaining.is_empty() {
        let first_line = remaining.lines().next().unwrap_or(remaining);
        return Err(format!(
            "unexpected directive at byte {consumed}: {first_line}"
        ));
    }

    Ok(chunks)
}

/// Recursive chunk parser. `base_offset` is the byte offset of `input`
/// within the top-level source (used for absolute span computation in
/// `#if` condition/body wrappers).
fn parse_chunks(input: &str, base_offset: usize) -> Result<Children, String> {
    let mut chunks = Vec::new();
    let mut pos = 0;

    while pos < input.len() {
        // Find the end of the current line
        let rest = &input[pos..];
        let line_end = rest.find('\n').unwrap_or(rest.len());
        let line = if line_end > 0 && rest.as_bytes().get(line_end - 1) == Some(&b'\r') {
            &rest[..line_end - 1]
        } else {
            &rest[..line_end]
        };
        let full_line_len = line_end; // bytes consumed (not including \n)

        // Try to parse the line as a directive
        if let Some(condition) = parse_if_header(line) {
            // #if directive — condition span stays relative to the #if line.
            // The Span::push() in mod.rs will compose it with the node's
            // absolute location to get the final position.

            let node_start = pos; // position of the #if line
            let block_start = pos + full_line_len + 1; // skip past the \n
            let (node, block_end) =
                parse_conditional_block(input, block_start, base_offset, node_start, condition)?;

            chunks.push(node.with_location((pos + base_offset)..(block_end + base_offset)));
            pos = block_end;
        } else if let Some(node) = parse_definition(line)
            .or_else(|| parse_undefine(line))
            .or_else(|| parse_inclusion(line))
            .or_else(|| parse_error_directive(line))
        {
            let record_newline = node.should_record_line_ending();
            let chunk_end = pos + full_line_len;
            chunks.push(node.with_location((pos + base_offset)..(chunk_end + base_offset)));

            // Consume the newline
            pos = chunk_end;
            if pos < input.len() && input.as_bytes()[pos] == b'\n' {
                if record_newline {
                    let has_cr = line_end > 0
                        && rest.as_bytes().get(line_end - 1) == Some(&b'\r');
                    let nl_start = if has_cr { pos + base_offset - 1 } else { pos + base_offset };
                    let nl_end = pos + base_offset + 1;
                    chunks.push(
                        Node::NewLine { crlf: has_cr }.with_location(nl_start..nl_end),
                    );
                }
                pos += 1;
            }
        } else if let Some(raw) = parse_raw_line(line) {
            let chunk_end = pos + full_line_len;
            chunks.push(raw.with_location((pos + base_offset)..(chunk_end + base_offset)));

            // Consume the newline and record it (Raw always records newlines)
            pos = chunk_end;
            if pos < input.len() && input.as_bytes()[pos] == b'\n' {
                let has_cr = line_end > 0
                    && rest.as_bytes().get(line_end - 1) == Some(&b'\r');
                let nl_start = if has_cr { pos + base_offset - 1 } else { pos + base_offset };
                let nl_end = pos + base_offset + 1;
                chunks.push(
                    Node::NewLine { crlf: has_cr }.with_location(nl_start..nl_end),
                );
                pos += 1;
            }
        } else {
            // Unknown line starting with '#' that isn't a recognized
            // directive — this is a boundary for #if body parsing.
            // Return what we have; the caller will handle the rest.
            break;
        }
    }

    Ok(chunks)
}

/// Parse a `#if/#elif/#else/#endif` block starting after the first `#if`
/// header line.
///
/// Returns `(Node::Condition { ... }, end_position)` where `end_position`
/// is the byte offset in `input` right after the `#endif` line.
fn parse_conditional_block(
    input: &str,
    start: usize,
    base_offset: usize,
    node_start: usize,
    first_condition: Located<String>,
) -> Result<(Node, usize), String> {
    let mut branches = Vec::new();
    let mut fallback = None;
    let mut body_start = start;

    // Parse the body of the first branch.
    // Body location is relative to the Condition node start (node_start).
    let (body_chunks, body_end) = parse_body_until_boundary(input, body_start)?;
    let body = body_chunks.with_location(
        (body_start - node_start + base_offset)..(body_end - node_start + base_offset),
    );
    branches.push(ConditionBranch {
        condition: first_condition,
        body,
    });

    let mut pos = body_end;

    loop {
        if pos >= input.len() {
            return Err("unterminated #if block".to_string());
        }

        // Read the boundary line
        let rest = &input[pos..];
        let line_end = rest.find('\n').unwrap_or(rest.len());
        let line = if line_end > 0 && rest.as_bytes().get(line_end - 1) == Some(&b'\r') {
            &rest[..line_end - 1]
        } else {
            &rest[..line_end]
        };

        if is_endif_directive(line) {
            // Consume the #endif line
            pos += line_end;
            if pos < input.len() && input.as_bytes()[pos] == b'\n' {
                pos += 1;
            }
            break;
        } else if let Some(mut condition) = parse_elif_header(line) {
            // Offset condition span to be relative to the Condition node
            // start (the #if line), not the #elif line
            condition.location.start += pos - node_start;
            condition.location.end += pos - node_start;

            // Skip past the #elif line
            pos += line_end;
            if pos < input.len() && input.as_bytes()[pos] == b'\n' {
                pos += 1;
            }

            body_start = pos;
            let (body_chunks, body_end) = parse_body_until_boundary(input, body_start)?;
            let body = body_chunks.with_location(
                (body_start - node_start + base_offset)..(body_end - node_start + base_offset),
            );
            branches.push(ConditionBranch { condition, body });
            pos = body_end;
        } else if is_else_directive(line) {
            // Skip past the #else line
            pos += line_end;
            if pos < input.len() && input.as_bytes()[pos] == b'\n' {
                pos += 1;
            }

            body_start = pos;
            let (body_chunks, body_end) = parse_body_until_boundary(input, body_start)?;
            let body = body_chunks.with_location(
                (body_start - node_start + base_offset)..(body_end - node_start + base_offset),
            );
            fallback = Some(body);
            pos = body_end;
        } else {
            return Err(format!("unexpected line in #if block: {line}"));
        }
    }

    Ok((Node::Condition { branches, fallback }, pos))
}

/// Parse chunks from `input[start..]` until hitting a `#elif`, `#else`, or
/// `#endif` boundary. Returns the body chunks (with spans relative to the
/// body start) and the byte offset of the boundary line.
fn parse_body_until_boundary(
    input: &str,
    start: usize,
) -> Result<(Children, usize), String> {
    let body_input = &input[start..];

    // Find where the body ends — scan for the first #elif/#else/#endif at
    // the top level (accounting for nested #if blocks).
    let boundary = find_body_boundary(body_input)?;
    let body_slice = &body_input[..boundary];

    // Parse the body slice — spans will be relative to body_slice start (= 0)
    let chunks = parse_chunks(body_slice, 0)?;

    Ok((chunks, start + boundary))
}

/// Scan through `input` to find the byte offset of the first
/// `#elif`/`#else`/`#endif` at the top nesting level. Nested `#if` blocks
/// are skipped.
fn find_body_boundary(input: &str) -> Result<usize, String> {
    let mut pos = 0;
    let mut depth = 0;

    for line in input.split('\n') {
        let trimmed = line.trim_start();

        if trimmed.starts_with('#') {
            // Check for nested #if
            let after_hash = trimmed.strip_prefix('#').unwrap_or(trimmed).trim_start();
            if after_hash.starts_with("if") && after_hash[2..].starts_with([' ', '\t'])
            {
                depth += 1;
            } else if depth > 0 && after_hash.starts_with("endif") {
                depth -= 1;
            } else if depth == 0
                && (after_hash.starts_with("elif")
                    || after_hash.starts_with("else")
                    || after_hash.starts_with("endif"))
            {
                return Ok(pos);
            }
        }

        pos += line.len() + 1; // +1 for the '\n'
    }

    Err("unterminated #if block: missing #endif".to_string())
}

/// Offset the inner `Located` spans of a node by `offset` bytes.
/// This makes spans absolute when the node was parsed from a line at a
/// known position.
fn offset_node(node: Node, offset: usize) -> Node {
    match node {
        Node::Definition { key, content } => Node::Definition {
            key: Located {
                inner: key.inner,
                location: (key.location.start + offset)..(key.location.end + offset),
            },
            content: content.map(|c| Located {
                inner: c.inner,
                location: (c.location.start + offset)..(c.location.end + offset),
            }),
        },
        Node::Undefine { key } => Node::Undefine {
            key: Located {
                inner: key.inner,
                location: (key.location.start + offset)..(key.location.end + offset),
            },
        },
        Node::Inclusion { path } => Node::Inclusion {
            path: Located {
                inner: path.inner,
                location: (path.location.start + offset)..(path.location.end + offset),
            },
        },
        Node::Error { message } => Node::Error {
            message: Located {
                inner: message.inner,
                location: (message.location.start + offset)..(message.location.end + offset),
            },
        },
        // Raw, NewLine, Condition don't need inner span adjustment
        other => other,
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn parse_raw_test() {
        // It extracts the line
        assert_eq!(
            parse_raw_line("line"),
            Some(Node::Raw {
                content: "line".to_string()
            })
        );

        // It extracts the line and discards the comment
        assert_eq!(
            parse_raw_line("line // comment"),
            Some(Node::Raw {
                content: "line ".to_string()
            })
        );

        // Does not get directives
        assert_eq!(parse_raw_line("#directive"), None);
    }

    #[test]
    fn parse_definition_test() {
        assert_eq!(
            parse_definition("#define foo bar"),
            Some(Node::Definition {
                key: "foo".to_owned().with_location(8..11),
                content: Some("bar".to_owned().with_location(12..15)),
            })
        );

        assert_eq!(
            parse_definition("#define foo"),
            Some(Node::Definition {
                key: "foo".to_owned().with_location(8..11),
                content: None,
            })
        );

        assert_eq!(
            parse_definition("#define trailing "),
            Some(Node::Definition {
                key: "trailing".to_owned().with_location(8..16),
                content: None,
            })
        );
    }

    #[test]
    fn parse_inclusion_test() {
        assert_eq!(
            parse_inclusion("#include \"foo\""),
            Some(Node::Inclusion {
                path: "foo".to_string().with_location(9..14)
            })
        );
    }

    #[test]
    fn parse_simple_test() {
        use Node::{Definition, Error, Inclusion, NewLine, Raw, Undefine};
        let body = parse(indoc::indoc! {r#"
            hello
            #include "foo"
            #define bar baz
            world
            #error "deprecated"
            #undefine bar
            #define test
        "#})
        .unwrap();

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
        let body = parse(indoc::indoc! {r#"
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

        // Check a few locations used below
        assert_eq!(&input[4..8], "true"); // line 1
        assert_eq!(&input[9..12], "foo"); // line 2
        assert_eq!(&input[40..44], "true"); // line 6
        assert_eq!(&input[63..69], "foobar"); // line 7
        assert_eq!(&input[76..79], "baz"); // line 9

        let body = parse(input).unwrap();

        assert_eq!(body.len(), 1);
        assert_eq!(
            body[0],
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
                            .with_location(4..25),
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
            .with_location(0..86)
        );
    }
}
