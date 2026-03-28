//! Chumsky-based assembly parser with error recovery.
//!
//! Parses the entire program in a single pass. All spans are absolute byte
//! offsets in the input — no relative-to-content or relative-to-line spans.

use chumsky::prelude::*;
use smallvec::SmallVec;

use super::line::{Line, LineContent, Program};
use super::location::{Locatable, Located};
use super::shared::{
    expression, hspace, hspace1, kw, register, span_to_range, string_literal, Extra,
    ParseDiagnostic,
};
use super::value::{DirectiveArgument, DirectiveKind, InstructionArgument, InstructionKind};
use crate::parser::expression::Node as ExpressionNode;
use crate::parser::shared::{is_identifier_char, is_start_identifier_char, rich_to_diagnostic};

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
                        .then(expression()),
                )
                .then_ignore(hspace())
                .then_ignore(just(']'))
                .map_with(|(register, (is_plus, expr)), e| {
                    let span = span_to_range(e.span());
                    // If minus, wrap in Invert
                    let value_node = if is_plus {
                        expr
                    } else {
                        ExpressionNode::Invert(Box::new(expr).with_location(span.clone()))
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
                        .map(InstructionArgument::Indirect),
                )
                .or(
                    // Direct: [expr]
                    expression()
                        .map_with(|n, e| n.with_location(span_to_range(e.span())))
                        .then_ignore(hspace())
                        .then_ignore(just(']'))
                        .map(InstructionArgument::Direct),
                ),
        )
        .recover_with(via_parser(
            just('[')
                .then(any().and_is(just(']').not()).repeated())
                .then(just(']'))
                .map(|_| InstructionArgument::Error),
        ));

    // Order: try bracketed first (starts with [), then register (starts with %),
    // then value
    bracketed.or(reg).or(value)
}

// ---------------------------------------------------------------------------
// Instructions
// ---------------------------------------------------------------------------

fn instruction_kind<'a>() -> impl Parser<'a, &'a str, InstructionKind, Extra<'a>> + Clone {
    use InstructionKind as K;

    any()
        .filter(|c: &char| c.is_ascii_alphabetic() || *c == '_')
        .repeated()
        .at_least(1)
        .to_slice()
        .validate(|s: &str, e, emitter| {
            let lower = s.to_ascii_lowercase();
            match lower.as_str() {
                "add" => K::Add,
                "and" => K::And,
                "call" => K::Call,
                "cmp" => K::Cmp,
                "div" => K::Div,
                "fas" => K::Fas,
                "in" => K::In,
                "jmp" => K::Jmp,
                "jeq" => K::Jeq,
                "jne" => K::Jne,
                "jle" => K::Jle,
                "jlt" => K::Jlt,
                "jge" => K::Jge,
                "jgt" => K::Jgt,
                "ld" => K::Ld,
                "mul" => K::Mul,
                "neg" => K::Neg,
                "nop" => K::Nop,
                "not" => K::Not,
                "or" => K::Or,
                "out" => K::Out,
                "pop" => K::Pop,
                "push" => K::Push,
                "reset" => K::Reset,
                "rti" => K::Rti,
                "rtn" => K::Rtn,
                "shl" => K::Shl,
                "shr" => K::Shr,
                "st" => K::St,
                "sub" => K::Sub,
                "swap" => K::Swap,
                "trap" => K::Trap,
                "xor" => K::Xor,
                _ => {
                    emitter.emit(Rich::custom(e.span(), format!("unknown instruction '{s}'")));
                    K::Error
                }
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
        .labelled("directive argument")
}

// ---------------------------------------------------------------------------
// Labels
// ---------------------------------------------------------------------------

/// Parse a label definition: `identifier [whitespace] ':'`.
///
/// The identifier is committed to the label branch only when `:` is seen,
/// avoiding ambiguity with instruction mnemonics.
fn label<'a>() -> impl Parser<'a, &'a str, Located<String>, Extra<'a>> + Clone {
    any()
        .filter(|c: &char| is_start_identifier_char(*c))
        .then(any().filter(|c: &char| is_identifier_char(*c)).repeated())
        .to_slice()
        .map(str::to_string)
        .map_with(|s, e| s.with_location(span_to_range(e.span())))
        .then_ignore(hspace())
        .then_ignore(just(':'))
}

// ---------------------------------------------------------------------------
// Lines
// ---------------------------------------------------------------------------

fn line_content<'a>() -> impl Parser<'a, &'a str, Located<LineContent>, Extra<'a>> + Clone {
    // Directive: .kind argument (starts with '.')
    let directive = directive_kind()
        .map_with(|k, e| k.with_location(span_to_range(e.span())))
        .then_ignore(hspace1())
        .then(
            directive_argument()
                .map_with(|a, e| a.with_location(span_to_range(e.span())))
                .labelled("directive argument (expression or string literal)"),
        )
        .map(|(kind, argument)| LineContent::Directive { kind, argument });

    // Instruction: mnemonic [arg [, arg]*] (starts with alpha/underscore)
    let instruction = instruction_kind()
        .map_with(|k, e| k.with_location(span_to_range(e.span())))
        .then(
            hspace1()
                .ignore_then(
                    instruction_argument()
                        .map_with(|a, e| a.with_location(span_to_range(e.span())))
                        .separated_by(hspace().then(just(',')).then(hspace()))
                        .collect::<Vec<_>>(),
                )
                .or_not()
                .map(Option::unwrap_or_default),
        )
        .map(|(kind, arguments)| LineContent::Instruction { kind, arguments });

    // Dispatch: directives start with '.', everything else is an instruction.
    let instruction = instruction.boxed();
    just('.')
        .rewind()
        .ignore_then(directive.or(instruction.clone()))
        .or(instruction)
        .map_with(|content, e| content.with_location(span_to_range(e.span())))
}

/// Shift all spans in a parsed `Line` by `offset` to convert from
/// line-relative to absolute byte offsets.
fn shift_line_spans(line: &mut Line, offset: usize) {
    for symbol in &mut line.symbols {
        symbol.location.start += offset;
        symbol.location.end += offset;
    }
    if let Some(content) = &mut line.content {
        content.location.start += offset;
        content.location.end += offset;
        match &mut content.inner {
            LineContent::Instruction { kind, arguments } => {
                kind.location.start += offset;
                kind.location.end += offset;
                for arg in arguments {
                    arg.location.start += offset;
                    arg.location.end += offset;
                    match &mut arg.inner {
                        InstructionArgument::Direct(node) => {
                            node.location.start += offset;
                            node.location.end += offset;
                        }
                        InstructionArgument::Indirect(reg) => {
                            reg.location.start += offset;
                            reg.location.end += offset;
                        }
                        InstructionArgument::Indexed { register, value } => {
                            register.location.start += offset;
                            register.location.end += offset;
                            value.location.start += offset;
                            value.location.end += offset;
                        }
                        InstructionArgument::Value(_)
                        | InstructionArgument::Register(_)
                        | InstructionArgument::Error => {}
                    }
                }
            }
            LineContent::Directive { kind, argument } => {
                kind.location.start += offset;
                kind.location.end += offset;
                argument.location.start += offset;
                argument.location.end += offset;
            }
            LineContent::Error => {}
        }
    }
}

/// Parse a single line: optional labels, then optional content, then
/// optional horizontal whitespace.
fn line<'a>() -> impl Parser<'a, &'a str, Line, Extra<'a>> + Clone {
    label()
        .then_ignore(hspace())
        .repeated()
        .collect::<Vec<_>>()
        .map(SmallVec::from_vec)
        .then_ignore(hspace())
        .then(line_content().or_not())
        .then_ignore(hspace())
        .map(|(symbols, content)| Line { symbols, content })
}

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Parse a Z33 assembly program with error recovery.
///
/// All spans in the returned AST are **absolute byte offsets** in the input.
/// This includes line locations, symbol locations, content locations, and all
/// nested spans (kind, arguments, expressions).
#[must_use]
pub fn parse(input: &str) -> ParseResult {
    // Parse line-by-line. Each line is parsed independently for reliable
    // error recovery. The line parser handles labels, instructions, and
    // directives. Chumsky spans are relative to each line's input, so we
    // shift all spans to absolute byte offsets in the full input.
    let line_parser = line().then_ignore(end());
    let mut lines = Vec::new();
    let mut diagnostics = Vec::new();
    let mut offset = 0;

    for raw_line in input.split('\n') {
        let line_len = raw_line.len();
        let raw_line = raw_line.strip_suffix('\r').unwrap_or(raw_line);

        let result = line_parser.parse(raw_line);

        let parsed_line = if let Some(mut l) = result.output().cloned() {
            // Shift all spans from line-relative to absolute
            shift_line_spans(&mut l, offset);
            l.with_location(offset..offset + raw_line.len())
        } else {
            Line {
                symbols: SmallVec::new(),
                content: if raw_line.trim().is_empty() {
                    None
                } else {
                    Some(LineContent::Error.with_location(offset..offset + raw_line.len()))
                },
            }
            .with_location(offset..offset + raw_line.len())
        };

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
        offset += line_len + 1;
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
    use crate::runtime::Reg;

    #[test]
    fn parse_empty() {
        let result = parse("");
        assert!(
            result.diagnostics.is_empty(),
            "diagnostics: {:?}",
            result
                .diagnostics
                .iter()
                .map(|d| &d.message)
                .collect::<Vec<_>>()
        );
        assert_eq!(result.program.inner.lines.len(), 1); // One empty line
    }

    #[test]
    fn parse_simple_instruction() {
        let result = parse("    add %a, %b");
        assert!(
            result.diagnostics.is_empty(),
            "diagnostics: {:?}",
            result
                .diagnostics
                .iter()
                .map(|d| &d.message)
                .collect::<Vec<_>>()
        );
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
        assert!(
            result.diagnostics.is_empty(),
            "diagnostics: {:?}",
            result
                .diagnostics
                .iter()
                .map(|d| &d.message)
                .collect::<Vec<_>>()
        );
        let line = &result.program.inner.lines[0].inner;
        assert_eq!(line.symbols.len(), 1);
        assert_eq!(line.symbols[0].inner, "main");
        assert!(line.content.is_some());
    }

    #[test]
    fn parse_directive() {
        let result = parse(".word 42");
        assert!(
            result.diagnostics.is_empty(),
            "diagnostics: {:?}",
            result
                .diagnostics
                .iter()
                .map(|d| &d.message)
                .collect::<Vec<_>>()
        );
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
        assert!(
            result.diagnostics.is_empty(),
            "diagnostics: {:?}",
            result
                .diagnostics
                .iter()
                .map(|d| &d.message)
                .collect::<Vec<_>>()
        );
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
        assert!(
            result.diagnostics.is_empty(),
            "diagnostics: {:?}",
            result
                .diagnostics
                .iter()
                .map(|d| &d.message)
                .collect::<Vec<_>>()
        );
        assert_eq!(result.program.inner.lines.len(), 3);
    }

    #[test]
    fn parse_expression_precedence() {
        let result = parse(".word 5 + 2 * 3");
        assert!(
            result.diagnostics.is_empty(),
            "diagnostics: {:?}",
            result
                .diagnostics
                .iter()
                .map(|d| &d.message)
                .collect::<Vec<_>>()
        );
        let line = &result.program.inner.lines[0].inner;
        match &line.content {
            Some(Located {
                inner: LineContent::Directive { argument, .. },
                ..
            }) => match &argument.inner {
                DirectiveArgument::Expression(ExpressionNode::Sum(..)) => {}
                other => panic!("expected Sum expression, got {other:?}"),
            },
            other => panic!("expected directive, got {other:?}"),
        }
    }

    #[test]
    fn parse_hex_literal() {
        let result = parse(".word 0xFF");
        assert!(
            result.diagnostics.is_empty(),
            "diagnostics: {:?}",
            result
                .diagnostics
                .iter()
                .map(|d| &d.message)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_memory_access_modes() {
        for input in ["ld %a, [42]", "ld %a, [%b]", "ld %a, [%b+2]"] {
            let result = parse(input);
            assert!(
                result.diagnostics.is_empty(),
                "{input}: {:?}",
                result
                    .diagnostics
                    .iter()
                    .map(|d| &d.message)
                    .collect::<Vec<_>>()
            );
        }
    }

    #[test]
    fn parse_error_recovery() {
        let result = parse("main:\n    $$invalid$$\n    reset");
        assert!(!result.diagnostics.is_empty());
        assert_eq!(result.program.inner.lines.len(), 3);
    }

    #[test]
    fn parse_trailing_whitespace() {
        let result = parse("    add %a, %b    ");
        assert!(
            result.diagnostics.is_empty(),
            "diagnostics: {:?}",
            result
                .diagnostics
                .iter()
                .map(|d| &d.message)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_no_argument_instruction() {
        let result = parse("    reset");
        assert!(
            result.diagnostics.is_empty(),
            "diagnostics: {:?}",
            result
                .diagnostics
                .iter()
                .map(|d| &d.message)
                .collect::<Vec<_>>()
        );
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

    /// All spans are now absolute byte offsets in the full input.
    #[test]
    fn span_model_multiline() {
        //            0         1         2
        //            0123456789012345678901234567
        let input = "main:\n    add %a, %b";
        let result = parse(input);
        assert!(
            result.diagnostics.is_empty(),
            "{:?}",
            result
                .diagnostics
                .iter()
                .map(|d| &d.message)
                .collect::<Vec<_>>()
        );

        let lines = &result.program.inner.lines;
        assert_eq!(lines.len(), 2);

        // Line 0: "main:"
        let first = &lines[0];
        assert_eq!(first.location, 0..5); // absolute
        assert_eq!(first.inner.symbols.len(), 1);
        assert_eq!(first.inner.symbols[0].inner, "main");
        assert_eq!(first.inner.symbols[0].location, 0..4); // absolute

        // Line 1: "    add %a, %b"
        let second = &lines[1];
        assert_eq!(second.location, 6..20); // absolute

        let content = second.inner.content.as_ref().unwrap();
        // "    add %a, %b" starts at byte 6, content starts at "add" = byte 10
        assert_eq!(content.location, 10..20); // absolute

        match &content.inner {
            LineContent::Instruction { kind, arguments } => {
                // "add" at bytes 10..13
                assert_eq!(kind.location, 10..13);
                assert_eq!(kind.inner, InstructionKind::Add);

                // "%a" at bytes 14..16, "%b" at bytes 18..20
                assert_eq!(arguments.len(), 2);
                assert_eq!(arguments[0].location, 14..16);
                assert_eq!(arguments[1].location, 18..20);
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
        assert!(
            result.diagnostics.is_empty(),
            "{:?}",
            result
                .diagnostics
                .iter()
                .map(|d| &d.message)
                .collect::<Vec<_>>()
        );

        let line = &result.program.inner.lines[0];
        assert_eq!(line.location, 0..13); // absolute

        assert_eq!(line.inner.symbols[0].inner, "foo");
        assert_eq!(line.inner.symbols[0].location, 0..3); // absolute

        let content = line.inner.content.as_ref().unwrap();
        // ".word 42" starts at byte 5
        assert_eq!(content.location, 5..13); // absolute

        match &content.inner {
            LineContent::Directive { kind, argument } => {
                assert_eq!(kind.inner, DirectiveKind::Word);
                assert_eq!(
                    argument.inner,
                    DirectiveArgument::Expression(ExpressionNode::Literal(42))
                );
            }
            other => panic!("expected directive, got {other:?}"),
        }
    }
}
