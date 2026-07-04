//! Preprocessor directive parser.
//!
//! Parses preprocessor directives (`#define`, `#include`, `#if`, etc.) and raw
//! assembly lines from source files. Uses chumsky for individual directive
//! parsing with an imperative top-level loop to handle the recursive
//! `#if/#elif/#else/#endif` block structure.

use chumsky::prelude::*;

use super::location::{Locatable, Located};
use super::shared::{hspace, identifier, span_to_range, string_literal, Extra};

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
    pub(crate) fn walk(&self, f: &mut impl FnMut(&Node)) {
        f(self);
        match self {
            Node::Condition {
                branches, fallback, ..
            } => {
                for branch in branches {
                    for child in &branch.body.inner {
                        child.inner.walk(f);
                    }
                }
                if let Some(fallback) = fallback {
                    for child in &fallback.inner {
                        child.inner.walk(f);
                    }
                }
            }
            Node::Raw { .. }
            | Node::NewLine { .. }
            | Node::Error { .. }
            | Node::Undefine { .. }
            | Node::Definition { .. }
            | Node::Inclusion { .. } => {}
        }
    }

    fn should_record_line_ending(&self) -> bool {
        !matches!(self, Node::Definition { .. } | Node::Undefine { .. })
    }
}

// ---------------------------------------------------------------------------
// Chumsky sub-parsers
// ---------------------------------------------------------------------------

fn eat_end_of_line<'a>() -> impl Parser<'a, &'a str, (), Extra<'a>> + Clone {
    hspace()
        .then(
            just("//")
                .then(any().and_is(just('\n').not()).repeated())
                .or_not(),
        )
        .ignored()
}

fn directive_argument<'a>() -> impl Parser<'a, &'a str, &'a str, Extra<'a>> + Clone {
    any()
        .filter(|c: &char| !matches!(c, ' ' | '\t' | '\r' | '\n' | '/'))
        .repeated()
        .at_least(1)
        .then(
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

/// The common `# <hspace> <keyword> <hspace1>` prefix for directives.
fn directive_prefix(keyword: &str) -> impl Parser<'_, &str, (), Extra<'_>> + Clone + '_ {
    just('#')
        .then(hspace())
        .ignore_then(just(keyword))
        .ignore_then(
            any()
                .filter(|c: &char| *c == ' ' || *c == '\t')
                .repeated()
                .at_least(1),
        )
        .ignored()
}

fn definition_parser<'a>() -> impl Parser<'a, &'a str, Node, Extra<'a>> + Clone {
    directive_prefix("define")
        .ignore_then(
            identifier()
                .map_with(|id: &str, e| id.to_owned().with_location(span_to_range(e.span())))
                .then(
                    any()
                        .filter(|c: &char| *c == ' ' || *c == '\t')
                        .repeated()
                        .at_least(1)
                        .ignore_then(directive_argument().map_with(|arg: &str, e| {
                            arg.to_owned().with_location(span_to_range(e.span()))
                        }))
                        .or_not(),
                ),
        )
        .then_ignore(eat_end_of_line())
        .then_ignore(end())
        .map(|(key, content)| Node::Definition { key, content })
}

fn undefine_parser<'a>() -> impl Parser<'a, &'a str, Node, Extra<'a>> + Clone {
    directive_prefix("undefine")
        .ignore_then(
            identifier()
                .map_with(|id: &str, e| id.to_owned().with_location(span_to_range(e.span()))),
        )
        .then_ignore(eat_end_of_line())
        .then_ignore(end())
        .map(|key| Node::Undefine { key })
}

fn inclusion_parser<'a>() -> impl Parser<'a, &'a str, Node, Extra<'a>> + Clone {
    directive_prefix("include")
        .ignore_then(string_literal().map_with(|s, e| s.with_location(span_to_range(e.span()))))
        .then_ignore(eat_end_of_line())
        .then_ignore(end())
        .map(|path| Node::Inclusion { path })
}

fn error_directive_parser<'a>() -> impl Parser<'a, &'a str, Node, Extra<'a>> + Clone {
    directive_prefix("error")
        .ignore_then(string_literal().map_with(|s, e| s.with_location(span_to_range(e.span()))))
        .then_ignore(eat_end_of_line())
        .then_ignore(end())
        .map(|message| Node::Error { message })
}

fn if_header_parser<'a>() -> impl Parser<'a, &'a str, Located<String>, Extra<'a>> + Clone {
    directive_prefix("if")
        .ignore_then(
            directive_argument()
                .map_with(|arg: &str, e| arg.to_owned().with_location(span_to_range(e.span()))),
        )
        .then_ignore(eat_end_of_line())
        .then_ignore(end())
}

fn elif_header_parser<'a>() -> impl Parser<'a, &'a str, Located<String>, Extra<'a>> + Clone {
    directive_prefix("elif")
        .ignore_then(
            directive_argument()
                .map_with(|arg: &str, e| arg.to_owned().with_location(span_to_range(e.span()))),
        )
        .then_ignore(eat_end_of_line())
        .then_ignore(end())
}

fn else_parser<'a>() -> impl Parser<'a, &'a str, (), Extra<'a>> + Clone {
    just('#')
        .then(hspace())
        .ignore_then(just("else"))
        .then_ignore(eat_end_of_line())
        .then_ignore(end())
        .ignored()
}

fn endif_parser<'a>() -> impl Parser<'a, &'a str, (), Extra<'a>> + Clone {
    just('#')
        .then(hspace())
        .ignore_then(just("endif"))
        .then_ignore(eat_end_of_line())
        .then_ignore(end())
        .ignored()
}

/// Parse a raw (non-directive) line. Inline `//` comments are preserved —
/// the assembly parser handles them and stores them in `Line.comment`.
fn parse_raw_line(line: &str) -> Option<Node> {
    if line.starts_with('#') {
        return None;
    }

    Some(Node::Raw {
        content: line.to_string(),
    })
}

/// Helper: append a newline node to `chunks`.
fn push_newline(
    chunks: &mut Children,
    pos: usize,
    base_offset: usize,
    line_end: usize,
    rest: &str,
    record: bool,
) {
    if !record {
        return;
    }
    let has_cr = line_end > 0 && rest.as_bytes().get(line_end - 1) == Some(&b'\r');
    let nl_start = if has_cr {
        pos + base_offset - 1
    } else {
        pos + base_offset
    };
    let nl_end = pos + base_offset + 1;
    chunks.push(Node::NewLine { crlf: has_cr }.with_location(nl_start..nl_end));
}

// ---------------------------------------------------------------------------
// Top-level parse function
// ---------------------------------------------------------------------------

/// Parse preprocessor directives and raw assembly lines.
pub(crate) fn parse(input: &str) -> Result<Children, String> {
    let chunks = parse_chunks(input, 0)?;

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

/// Recursive chunk parser.
fn parse_chunks(input: &str, base_offset: usize) -> Result<Children, String> {
    let mut chunks = Vec::new();
    let mut pos = 0;

    // Build all parsers once, reuse for every line in this chunk.
    let p_definition = definition_parser();
    let p_undefine = undefine_parser();
    let p_inclusion = inclusion_parser();
    let p_error = error_directive_parser();
    let p_if = if_header_parser();

    while pos < input.len() {
        let rest = &input[pos..];
        let line_end = rest.find('\n').unwrap_or(rest.len());
        let line = if line_end > 0 && rest.as_bytes().get(line_end - 1) == Some(&b'\r') {
            &rest[..line_end - 1]
        } else {
            &rest[..line_end]
        };
        let full_line_len = line_end;

        if let Ok(condition) = p_if.parse(line).into_result() {
            let node_start = pos;
            let block_start = pos + full_line_len + 1;
            let (node, block_end) =
                parse_conditional_block(input, block_start, base_offset, node_start, condition)?;

            chunks.push(node.with_location((pos + base_offset)..(block_end + base_offset)));
            pos = block_end;
        } else if let Some(node) = p_definition
            .parse(line)
            .into_result()
            .ok()
            .or_else(|| p_undefine.parse(line).into_result().ok())
            .or_else(|| p_inclusion.parse(line).into_result().ok())
            .or_else(|| p_error.parse(line).into_result().ok())
        {
            let record_newline = node.should_record_line_ending();
            let chunk_end = pos + full_line_len;
            chunks.push(node.with_location((pos + base_offset)..(chunk_end + base_offset)));

            pos = chunk_end;
            if pos < input.len() && input.as_bytes()[pos] == b'\n' {
                push_newline(
                    &mut chunks,
                    pos,
                    base_offset,
                    line_end,
                    rest,
                    record_newline,
                );
                pos += 1;
            }
        } else if let Some(raw) = parse_raw_line(line) {
            let chunk_end = pos + full_line_len;
            chunks.push(raw.with_location((pos + base_offset)..(chunk_end + base_offset)));

            pos = chunk_end;
            if pos < input.len() && input.as_bytes()[pos] == b'\n' {
                push_newline(&mut chunks, pos, base_offset, line_end, rest, true);
                pos += 1;
            }
        } else {
            break;
        }
    }

    Ok(chunks)
}

/// Parse a `#if/#elif/#else/#endif` block.
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

    // Build boundary parsers once for the block.
    let p_elif = elif_header_parser();
    let p_else = else_parser();
    let p_endif = endif_parser();

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

        let rest = &input[pos..];
        let line_end = rest.find('\n').unwrap_or(rest.len());
        let line = if line_end > 0 && rest.as_bytes().get(line_end - 1) == Some(&b'\r') {
            &rest[..line_end - 1]
        } else {
            &rest[..line_end]
        };

        if p_endif.parse(line).into_result().is_ok() {
            pos += line_end;
            if pos < input.len() && input.as_bytes()[pos] == b'\n' {
                pos += 1;
            }
            break;
        } else if let Ok(mut condition) = p_elif.parse(line).into_result() {
            condition.location.start += pos - node_start;
            condition.location.end += pos - node_start;

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
        } else if p_else.parse(line).into_result().is_ok() {
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
            return Err(format!("expected #elif, #else, or #endif, got: {line}"));
        }
    }

    Ok((Node::Condition { branches, fallback }, pos))
}

/// Parse chunks from `input[start..]` until hitting a boundary directive.
fn parse_body_until_boundary(input: &str, start: usize) -> Result<(Children, usize), String> {
    let body_input = &input[start..];
    let boundary = find_body_boundary(body_input)?;
    let body_slice = &body_input[..boundary];
    let chunks = parse_chunks(body_slice, 0)?;
    Ok((chunks, start + boundary))
}

/// Scan for the first `#elif`/`#else`/`#endif` at the top nesting level.
fn find_body_boundary(input: &str) -> Result<usize, String> {
    let mut pos = 0;
    let mut depth = 0;

    for line in input.split('\n') {
        let trimmed = line.trim_start();

        if trimmed.starts_with('#') {
            let after_hash = trimmed.strip_prefix('#').unwrap_or(trimmed).trim_start();
            if after_hash.starts_with("if") && after_hash[2..].starts_with([' ', '\t']) {
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

        pos += line.len() + 1;
    }

    Err("unterminated #if block: missing #endif".to_string())
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn parse_raw_test() {
        let input = "hello world";
        let result = parse(input).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(
            result[0].inner,
            Node::Raw {
                content: "hello world".to_string()
            }
        );
    }

    #[test]
    fn parse_simple_test() {
        let input = "hello world\n#define FOO bar\n#include \"baz.s\"";
        let result = parse(input).unwrap();
        assert_eq!(result.len(), 4);
        assert_eq!(
            result[0].inner,
            Node::Raw {
                content: "hello world".to_string()
            }
        );
        assert!(matches!(result[1].inner, Node::NewLine { crlf: false }));
        match &result[2].inner {
            Node::Definition { key, content } => {
                assert_eq!(key.inner, "FOO");
                assert_eq!(content.as_ref().map(|c| c.inner.as_str()), Some("bar"));
            }
            other => panic!("expected Definition, got {other:?}"),
        }
        match &result[3].inner {
            Node::Inclusion { path } => {
                assert_eq!(path.inner, "baz.s");
            }
            other => panic!("expected Inclusion, got {other:?}"),
        }
    }

    #[test]
    fn parse_definition_test() {
        let input = "#define FOO";
        let result = parse(input).unwrap();
        assert_eq!(result.len(), 1);
        match &result[0].inner {
            Node::Definition { key, content } => {
                assert_eq!(key.inner, "FOO");
                assert!(content.is_none());
            }
            other => panic!("expected Definition, got {other:?}"),
        }
    }

    #[test]
    fn parse_inclusion_test() {
        let input = "#include \"test.s\"";
        let result = parse(input).unwrap();
        assert_eq!(result.len(), 1);
        match &result[0].inner {
            Node::Inclusion { path } => {
                assert_eq!(path.inner, "test.s");
            }
            other => panic!("expected Inclusion, got {other:?}"),
        }
    }

    #[test]
    fn parse_condition_test() {
        let input = "#if FOO\nhello\n#endif";
        let result = parse(input).unwrap();
        assert_eq!(result.len(), 1);
        match &result[0].inner {
            Node::Condition {
                branches, fallback, ..
            } => {
                assert_eq!(branches.len(), 1);
                assert_eq!(branches[0].condition.inner, "FOO");
                assert!(fallback.is_none());
            }
            other => panic!("expected Condition, got {other:?}"),
        }
    }

    #[test]
    fn parse_weird_test() {
        let input = "#  define   FOO   bar baz  // comment";
        let result = parse(input).unwrap();
        assert_eq!(result.len(), 1);
        match &result[0].inner {
            Node::Definition { key, content } => {
                assert_eq!(key.inner, "FOO");
                assert_eq!(content.as_ref().map(|c| c.inner.as_str()), Some("bar baz"));
            }
            other => panic!("expected Definition, got {other:?}"),
        }
    }
}
