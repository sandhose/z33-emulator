//! Chumsky-based assembly parser with error recovery.
//!
//! This module replaces the nom-based assembly parser with one that can recover
//! from syntax errors, producing partial ASTs alongside accumulated diagnostics.

use chumsky::prelude::*;

use super::line::{Line, LineContent, Program};
use super::location::{Locatable, Located};
use super::shared::{
    expression, hspace, hspace1, identifier, kw, register, span_to_range, string_literal, Extra,
    ParseDiagnostic,
};
use super::value::{
    DirectiveArgument, DirectiveKind, InstructionArgument, InstructionKind,
};
use crate::parser::expression::Node as ExpressionNode;
use crate::parser::shared::rich_to_diagnostic;

/// Result of parsing: always produces a program, plus accumulated diagnostics.
pub struct ParseResult {
    pub program: Located<Program>,
    pub diagnostics: Vec<ParseDiagnostic>,
}

// ---------------------------------------------------------------------------
// Instruction arguments
// ---------------------------------------------------------------------------

fn instruction_argument<'a>() -> impl Parser<'a, &'a str, InstructionArgument, Extra<'a>> + Clone {
    let reg = register().map(InstructionArgument::Register);

    let value = expression().map(InstructionArgument::Value);

    // Bracketed forms: [expr], [%reg], [%reg +/- expr]
    let bracketed = just('[')
        .ignore_then(hspace())
        .ignore_then(
            // Try indexed first: [%reg +/- expr]
            register()
                .map_with(|r, e| r.with_location(span_to_range(e.span())))
                .then_ignore(hspace())
                .then(
                    choice((just('+').to(true), just('-').to(false)))
                        .then_ignore(hspace())
                        .then(expression())
                )
                .then_ignore(hspace())
                .then_ignore(just(']'))

                .map_with(|(register, (is_plus, expr)), e| {
                    let span = span_to_range(e.span());
                    // If minus, wrap in Invert
                    let value_node = if is_plus {
                        expr
                    } else {
                        ExpressionNode::Invert(
                            Box::new(expr).with_location(span.clone()),
                        )
                    };
                    InstructionArgument::Indexed {
                        register,
                        value: value_node.with_location(span),
                    }
                })
            .or(
                // Try indirect: [%reg]
                register()
                    .map_with(|r, e| r.with_location(span_to_range(e.span())))
                    .then_ignore(hspace())
                    .then_ignore(just(']'))
                    .map(InstructionArgument::Indirect)
            )
            .or(
                // Direct: [expr]
                expression()
                    .map_with(|n, e| n.with_location(span_to_range(e.span())))
                    .then_ignore(hspace())
                    .then_ignore(just(']'))
                    .map(InstructionArgument::Direct)
            ),
        )
        .recover_with(via_parser(
            just('[')
                .then(any().and_is(just(']').not()).repeated())
                .then(just(']'))
                .map(|_| InstructionArgument::Error),
        ));

    // Order: try bracketed first (starts with [), then register (starts with %), then value
    bracketed.or(reg).or(value)
}

// ---------------------------------------------------------------------------
// Instructions
// ---------------------------------------------------------------------------

fn instruction_kind<'a>() -> impl Parser<'a, &'a str, InstructionKind, Extra<'a>> + Clone {
    use InstructionKind as K;

    // Parse an identifier-like token and match against known mnemonics
    any()
        .filter(|c: &char| c.is_ascii_alphabetic() || *c == '_')
        .repeated()
        .at_least(1)
        .to_slice()
        .try_map(|s: &str, span| {
            // Case-insensitive matching against all instruction mnemonics
            let upper = s.to_ascii_lowercase();
            match upper.as_str() {
                "add" => Ok(K::Add),
                "and" => Ok(K::And),
                "call" => Ok(K::Call),
                "cmp" => Ok(K::Cmp),
                "div" => Ok(K::Div),
                "fas" => Ok(K::Fas),
                "in" => Ok(K::In),
                "jmp" => Ok(K::Jmp),
                "jeq" => Ok(K::Jeq),
                "jne" => Ok(K::Jne),
                "jle" => Ok(K::Jle),
                "jlt" => Ok(K::Jlt),
                "jge" => Ok(K::Jge),
                "jgt" => Ok(K::Jgt),
                "ld" => Ok(K::Ld),
                "mul" => Ok(K::Mul),
                "neg" => Ok(K::Neg),
                "nop" => Ok(K::Nop),
                "not" => Ok(K::Not),
                "or" => Ok(K::Or),
                "out" => Ok(K::Out),
                "pop" => Ok(K::Pop),
                "push" => Ok(K::Push),
                "reset" => Ok(K::Reset),
                "rti" => Ok(K::Rti),
                "rtn" => Ok(K::Rtn),
                "shl" => Ok(K::Shl),
                "shr" => Ok(K::Shr),
                "st" => Ok(K::St),
                "sub" => Ok(K::Sub),
                "swap" => Ok(K::Swap),
                "trap" => Ok(K::Trap),
                "xor" => Ok(K::Xor),
                "debugreg" => Ok(K::DebugReg),
                _ => Err(Rich::custom(
                    span,
                    format!("unknown instruction '{s}'"),
                )),
            }
        })
}

// ---------------------------------------------------------------------------
// Directives
// ---------------------------------------------------------------------------

fn directive_kind<'a>() -> impl Parser<'a, &'a str, DirectiveKind, Extra<'a>> + Clone {
    use DirectiveKind as K;

    just('.').ignore_then(choice((
        kw("string").to(K::String),
        kw("space").to(K::Space),
        kw("addr").to(K::Addr),
        kw("word").to(K::Word),
    )))
}

fn directive_argument<'a>() -> impl Parser<'a, &'a str, DirectiveArgument, Extra<'a>> + Clone {
    string_literal()
        .map(DirectiveArgument::StringLiteral)
        .or(expression().map(DirectiveArgument::Expression))
}

// ---------------------------------------------------------------------------
// Lines
// ---------------------------------------------------------------------------

fn symbol_definition<'a>() -> impl Parser<'a, &'a str, Located<String>, Extra<'a>> + Clone {
    identifier()
        .map_with(|i: &str, e| i.to_string().with_location(span_to_range(e.span())))
        .then_ignore(hspace())
        .then_ignore(just(':'))
}

fn line_content<'a>() -> impl Parser<'a, &'a str, Located<LineContent>, Extra<'a>> + Clone {
    // Directive: .kind argument
    let directive = directive_kind()
        .map_with(|k, e| k.with_location(span_to_range(e.span())))
        .then_ignore(hspace1())
        .then(
            directive_argument()
                .map_with(|a, e| a.with_location(span_to_range(e.span()))),
        )
        .map(|(kind, argument)| LineContent::Directive { kind, argument });

    // Instruction: mnemonic [arg [, arg]*]
    let instruction = instruction_kind()
        .map_with(|k, e| k.with_location(span_to_range(e.span())))
        .then(
            hspace1()
                .ignore_then(
                    instruction_argument()
                        .map_with(|a, e| a.with_location(span_to_range(e.span())))
                        .separated_by(
                            hspace()
                                .then(just(','))
                                .then(hspace()),
                        )
                        .collect::<Vec<_>>(),
                )
                .or_not()
                .map(Option::unwrap_or_default),
        )
        .map(|(kind, arguments)| LineContent::Instruction { kind, arguments });

    directive
        .or(instruction)
        .map_with(|content, e| content.with_location(span_to_range(e.span())))
}

fn line<'a>() -> impl Parser<'a, &'a str, Located<Line>, Extra<'a>> + Clone {
    let symbols = symbol_definition()
        .then_ignore(hspace())
        .repeated()
        .collect::<Vec<_>>();

    // Comments are stripped by the preprocessor, so we don't need to handle
    // them here. We just consume trailing whitespace.
    symbols
        .then_ignore(hspace())
        .then(line_content().or_not())
        .then_ignore(hspace())
        .map_with(|(symbols, content), e| {
            Line { symbols, content }.with_location(span_to_range(e.span()))
        })
}

/// Adjust all `Located` spans inside a `LineContent` so they are relative to
/// the content start instead of the line start.
///
/// Chumsky produces all spans relative to the input it received (the full
/// line string). The compiler expects inner spans (kind, arguments, and all
/// nested `Located` fields) to be relative to the content start, because it
/// computes absolute positions as `inner_span + content_absolute_start`.
/// Subtract `base` from a `Located`'s span, making it relative to
/// content start instead of line start.
fn adjust_span<T>(loc: &mut Located<T>, base: usize) {
    loc.location.start = loc.location.start.saturating_sub(base);
    loc.location.end = loc.location.end.saturating_sub(base);
}

fn make_content_relative(content: &mut Located<LineContent>) {
    let base = content.location.start;
    match &mut content.inner {
        LineContent::Instruction { kind, arguments } => {
            adjust_span(kind, base);
            for arg in arguments {
                adjust_span(arg, base);
                match &mut arg.inner {
                    InstructionArgument::Direct(node) => adjust_span(node, base),
                    InstructionArgument::Indirect(reg) => adjust_span(reg, base),
                    InstructionArgument::Indexed { register, value } => {
                        adjust_span(register, base);
                        adjust_span(value, base);
                    }
                    InstructionArgument::Value(_)
                    | InstructionArgument::Register(_)
                    | InstructionArgument::Error => {}
                }
            }
        }
        LineContent::Directive { kind, argument } => {
            adjust_span(kind, base);
            adjust_span(argument, base);
        }
        LineContent::Error => {}
    }
}

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Parse a Z33 assembly program with error recovery.
///
/// Always returns a `ParseResult` containing a (possibly partial) program AST
/// and a list of diagnostics. If `diagnostics` is empty, the program parsed
/// without errors.
#[must_use]
pub fn parse(input: &str) -> ParseResult {
    let mut diagnostics = Vec::new();
    let mut lines = Vec::new();

    // Split on newlines, keeping track of byte offsets.
    // We parse each line individually so that an error in one line doesn't
    // prevent parsing of subsequent lines.
    let mut offset = 0;
    for raw_line in input.split('\n') {
        let line_len = raw_line.len();
        // Strip trailing \r for \r\n line endings
        let raw_line = raw_line.strip_suffix('\r').unwrap_or(raw_line);

        let line_parser = line().then_ignore(end());
        let result = line_parser.parse(raw_line);

        let parsed_line = if let Some(mut l) = result.output().cloned() {
            // Adjust inner content spans to be content-relative
            if let Some(ref mut content) = l.inner.content {
                make_content_relative(content);
            }
            // Set line.location to absolute position in the full input
            l.location.start += offset;
            l.location.end += offset;
            l
        } else {
            // Total parse failure for this line — create an error line
            Line {
                symbols: Vec::new(),
                content: if raw_line.trim().is_empty() {
                    None
                } else {
                    Some(LineContent::Error.with_location(0..raw_line.len()))
                },
            }
            .with_location(offset..offset + raw_line.len())
        };

        // Collect diagnostics, adjusting spans to be absolute
        for error in result.errors() {
            let mut diag = rich_to_diagnostic(error);
            diag.span.start += offset;
            diag.span.end += offset;
            for label in &mut diag.labels {
                label.0.start += offset;
                label.0.end += offset;
            }
            diagnostics.push(diag);
        }

        lines.push(parsed_line);
        offset += line_len + 1; // +1 for the \n
    }

    let program = Program { lines }.with_location(0..input.len());

    ParseResult {
        program,
        diagnostics,
    }
}


#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn parse_empty() {
        let result = parse("");
        assert!(result.diagnostics.is_empty(), "diagnostics: {:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
        assert_eq!(result.program.inner.lines.len(), 1); // One empty line
    }

    #[test]
    fn parse_simple_instruction() {
        let result = parse("    add %a, %b");
        assert!(result.diagnostics.is_empty(), "diagnostics: {:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
        let lines = &result.program.inner.lines;
        assert_eq!(lines.len(), 1);
        let line = &lines[0].inner;
        assert!(line.symbols.is_empty());
        match &line.content {
            Some(Located {
                inner: LineContent::Instruction { kind, arguments },
                ..
            }) => {
                assert_eq!(kind.inner, InstructionKind::Add);
                assert_eq!(arguments.len(), 2);
                assert_eq!(arguments[0].inner, InstructionArgument::Register(Reg::A));
                assert_eq!(arguments[1].inner, InstructionArgument::Register(Reg::B));
            }
            other => panic!("expected instruction, got {other:?}"),
        }
    }

    #[test]
    fn parse_label_and_instruction() {
        let result = parse("main: add %a, %b");
        assert!(result.diagnostics.is_empty(), "diagnostics: {:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
        let line = &result.program.inner.lines[0].inner;
        assert_eq!(line.symbols.len(), 1);
        assert_eq!(line.symbols[0].inner, "main");
        assert!(line.content.is_some());
    }

    #[test]
    fn parse_directive() {
        let result = parse(".word 42");
        assert!(result.diagnostics.is_empty(), "diagnostics: {:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
        let line = &result.program.inner.lines[0].inner;
        match &line.content {
            Some(Located {
                inner: LineContent::Directive { kind, argument },
                ..
            }) => {
                assert_eq!(kind.inner, DirectiveKind::Word);
                assert_eq!(
                    argument.inner,
                    DirectiveArgument::Expression(ExpressionNode::Literal(42))
                );
            }
            other => panic!("expected directive, got {other:?}"),
        }
    }

    #[test]
    fn parse_string_directive() {
        let result = parse(r#".string "hello""#);
        assert!(result.diagnostics.is_empty(), "diagnostics: {:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
        let line = &result.program.inner.lines[0].inner;
        match &line.content {
            Some(Located {
                inner: LineContent::Directive { kind, argument },
                ..
            }) => {
                assert_eq!(kind.inner, DirectiveKind::String);
                assert_eq!(
                    argument.inner,
                    DirectiveArgument::StringLiteral("hello".to_string())
                );
            }
            other => panic!("expected directive, got {other:?}"),
        }
    }

    #[test]
    fn parse_multiline_program() {
        let result = parse("main:\n    add %a, %b\n    reset");
        assert!(result.diagnostics.is_empty(), "diagnostics: {:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
        assert_eq!(result.program.inner.lines.len(), 3);
    }

    #[test]
    fn parse_expression_precedence() {
        let result = parse(".word 5 + 2 * 3");
        assert!(result.diagnostics.is_empty(), "diagnostics: {:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
        let line = &result.program.inner.lines[0].inner;
        match &line.content {
            Some(Located {
                inner: LineContent::Directive { argument, .. },
                ..
            }) => {
                // Should be Sum(5, Multiply(2, 3))
                match &argument.inner {
                    DirectiveArgument::Expression(ExpressionNode::Sum(..)) => {}
                    other => panic!("expected Sum expression, got {other:?}"),
                }
            }
            other => panic!("expected directive, got {other:?}"),
        }
    }

    #[test]
    fn parse_hex_literal() {
        let result = parse(".word 0xFF");
        assert!(result.diagnostics.is_empty(), "diagnostics: {:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
    }

    #[test]
    fn parse_memory_access_modes() {
        // Direct
        let result = parse("ld %a, [42]");
        assert!(result.diagnostics.is_empty(), "diagnostics: {:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());

        // Indirect
        let result = parse("ld %a, [%b]");
        assert!(result.diagnostics.is_empty(), "diagnostics: {:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());

        // Indexed
        let result = parse("ld %a, [%b+2]");
        assert!(result.diagnostics.is_empty(), "diagnostics: {:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
    }

    #[test]
    fn parse_error_recovery() {
        // Invalid line in the middle should be recovered
        let result = parse("main:\n    $$invalid$$\n    reset");
        // Should have diagnostics for the invalid line
        assert!(!result.diagnostics.is_empty());
        // But should still parse 3 lines (the invalid one becomes an error)
        assert_eq!(result.program.inner.lines.len(), 3);
    }

    #[test]
    fn parse_trailing_whitespace() {
        // Comments are stripped by the preprocessor before reaching the
        // assembly parser, so the parser only sees trailing whitespace.
        let result = parse("    add %a, %b    ");
        assert!(result.diagnostics.is_empty(), "diagnostics: {:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
    }

    #[test]
    fn parse_no_argument_instruction() {
        let result = parse("    reset");
        assert!(result.diagnostics.is_empty(), "diagnostics: {:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
        let line = &result.program.inner.lines[0].inner;
        match &line.content {
            Some(Located {
                inner: LineContent::Instruction { kind, arguments },
                ..
            }) => {
                assert_eq!(kind.inner, InstructionKind::Reset);
                assert!(arguments.is_empty());
            }
            other => panic!("expected instruction, got {other:?}"),
        }
    }

    /// Verify that the span model matches what the compiler expects:
    ///   - line.location: absolute in the full input
    ///   - symbol.location: relative to line start
    ///   - content.location: relative to line start
    ///   - kind/argument spans: relative to content start
    #[test]
    fn span_model_multiline() {
        //            0         1         2
        //            0123456789012345678901234567
        let input = "main:\n    add %a, %b";
        let result = parse(input);
        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());

        let lines = &result.program.inner.lines;
        assert_eq!(lines.len(), 2);

        // Line 0: "main:"
        let line0 = &lines[0];
        assert_eq!(line0.location, 0..5); // absolute
        assert_eq!(line0.inner.symbols.len(), 1);
        assert_eq!(line0.inner.symbols[0].inner, "main");
        assert_eq!(line0.inner.symbols[0].location, 0..4); // relative to line start

        // Line 1: "    add %a, %b"
        // Line starts at byte 6 in the input (after "main:\n")
        let line1 = &lines[1];
        assert_eq!(line1.location, 6..20); // absolute

        let content = line1.inner.content.as_ref().unwrap();
        // content.location: relative to line start
        // "    add %a, %b" — content starts at byte 4 (after spaces)
        assert_eq!(content.location, 4..14);

        match &content.inner {
            LineContent::Instruction { kind, arguments } => {
                // kind.location: relative to content start
                // "add" is at the start of content
                assert_eq!(kind.location, 0..3);
                assert_eq!(kind.inner, InstructionKind::Add);

                // argument locations: relative to content start
                // Content is "add %a, %b"
                //             0123456789
                assert_eq!(arguments.len(), 2);
                assert_eq!(arguments[0].location, 4..6); // "%a"
                assert_eq!(arguments[1].location, 8..10); // "%b"
            }
            other => panic!("expected instruction, got {other:?}"),
        }
    }

    #[test]
    fn span_model_directive() {
        //            0         1
        //            0123456789012345
        let input = "foo: .word 42";
        let result = parse(input);
        assert!(result.diagnostics.is_empty(), "{:?}", result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());

        let line = &result.program.inner.lines[0];
        assert_eq!(line.location, 0..13); // absolute

        assert_eq!(line.inner.symbols[0].inner, "foo");
        assert_eq!(line.inner.symbols[0].location, 0..3); // relative to line

        let content = line.inner.content.as_ref().unwrap();
        // ".word 42" starts at byte 5
        assert_eq!(content.location, 5..13); // relative to line

        match &content.inner {
            LineContent::Directive { kind, argument } => {
                // Relative to content start (".word 42")
                // The directive_kind parser skips the '.', so:
                // ".word" -> kind location covers "word" part
                assert_eq!(kind.inner, DirectiveKind::Word);
                // "42" is at position 6 in ".word 42"
                assert_eq!(argument.inner, DirectiveArgument::Expression(ExpressionNode::Literal(42)));
            }
            other => panic!("expected directive, got {other:?}"),
        }
    }
}
