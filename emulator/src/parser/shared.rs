//! Shared chumsky parser combinators used by the assembly parser,
//! preprocessor parser, and condition parser.

use std::ops::Range;

use chumsky::prelude::*;

use super::expression::{Node as ExpressionNode, Value as ExpressionValue};
use super::location::{Locatable, Located};
use crate::runtime::Reg;

// ---------------------------------------------------------------------------
// Type aliases
// ---------------------------------------------------------------------------

pub type Span = SimpleSpan<usize>;
pub type Extra<'a> = extra::Err<Rich<'a, char, Span>>;

// ---------------------------------------------------------------------------
// Span / diagnostic helpers
// ---------------------------------------------------------------------------

/// Convert a chumsky `SimpleSpan` to a `Range<usize>`.
pub(crate) fn span_to_range(span: Span) -> Range<usize> {
    span.start..span.end
}

/// A diagnostic produced during parsing.
pub struct ParseDiagnostic {
    pub span: Range<usize>,
    pub message: String,
    pub severity: DiagnosticSeverity,
    pub labels: Vec<(Range<usize>, String)>,
}

/// Severity of a parse diagnostic.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
}

/// Convert a `Rich` error to a `ParseDiagnostic`.
pub(crate) fn rich_to_diagnostic(error: &Rich<'_, char, Span>) -> ParseDiagnostic {
    let span = span_to_range(*error.span());
    let message = error.to_string();

    let mut labels = Vec::new();
    if let Some(label) = error.reason().to_string().strip_prefix("found ") {
        labels.push((span.clone(), label.to_string()));
    }

    ParseDiagnostic {
        span,
        message,
        severity: DiagnosticSeverity::Error,
        labels,
    }
}

// ---------------------------------------------------------------------------
// Whitespace helpers
// ---------------------------------------------------------------------------

/// Parse horizontal whitespace (spaces and tabs, NOT newlines).
pub(crate) fn hspace<'a>() -> impl Parser<'a, &'a str, (), Extra<'a>> + Clone {
    any()
        .filter(|c: &char| *c == ' ' || *c == '\t')
        .repeated()
        .ignored()
}

/// Parse at least one horizontal whitespace character.
pub(crate) fn hspace1<'a>() -> impl Parser<'a, &'a str, (), Extra<'a>> + Clone {
    any()
        .filter(|c: &char| *c == ' ' || *c == '\t')
        .repeated()
        .at_least(1)
        .ignored()
}

/// Case-insensitive keyword matcher that doesn't consume trailing identifier
/// chars.
pub(crate) fn kw(
    keyword: &str,
) -> impl Parser<'_, &str, &str, Extra<'_>> + Clone + '_ {
    any()
        .filter(|c: &char| c.is_ascii_alphabetic() || *c == '_')
        .repeated()
        .at_least(1)
        .to_slice()
        .try_map(move |s: &str, span| {
            if s.eq_ignore_ascii_case(keyword) {
                Ok(s)
            } else {
                Err(Rich::custom(
                    span,
                    format!("expected '{keyword}', found '{s}'"),
                ))
            }
        })
}

// ---------------------------------------------------------------------------
// Identifiers
// ---------------------------------------------------------------------------

pub(crate) fn is_start_identifier_char(c: char) -> bool {
    c == '_' || c.is_ascii_lowercase() || c.is_ascii_uppercase()
}

pub(crate) fn is_identifier_char(c: char) -> bool {
    is_start_identifier_char(c) || c.is_ascii_digit()
}

pub(crate) fn identifier<'a>() -> impl Parser<'a, &'a str, &'a str, Extra<'a>> + Clone {
    any()
        .filter(|c: &char| is_start_identifier_char(*c))
        .then(any().filter(|c: &char| is_identifier_char(*c)).repeated())
        .to_slice()
}

// ---------------------------------------------------------------------------
// Literals
// ---------------------------------------------------------------------------

fn hex_literal<'a>() -> impl Parser<'a, &'a str, u64, Extra<'a>> + Clone {
    just("0x")
        .or(just("0X"))
        .ignore_then(
            any()
                .filter(|c: &char| c.is_ascii_hexdigit())
                .repeated()
                .at_least(1)
                .to_slice()
                .try_map(|s: &str, span| {
                    u64::from_str_radix(s, 16)
                        .map_err(|e| Rich::custom(span, format!("invalid hex literal: {e}")))
                }),
        )
}

fn octal_literal<'a>() -> impl Parser<'a, &'a str, u64, Extra<'a>> + Clone {
    just("0o")
        .or(just("0O"))
        .ignore_then(
            any()
                .filter(|c: &char| c.is_digit(8))
                .repeated()
                .at_least(1)
                .to_slice()
                .try_map(|s: &str, span| {
                    u64::from_str_radix(s, 8)
                        .map_err(|e| Rich::custom(span, format!("invalid octal literal: {e}")))
                }),
        )
}

fn binary_literal<'a>() -> impl Parser<'a, &'a str, u64, Extra<'a>> + Clone {
    just("0b")
        .or(just("0B"))
        .ignore_then(
            any()
                .filter(|c: &char| *c == '0' || *c == '1')
                .repeated()
                .at_least(1)
                .to_slice()
                .try_map(|s: &str, span| {
                    u64::from_str_radix(s, 2)
                        .map_err(|e| Rich::custom(span, format!("invalid binary literal: {e}")))
                }),
        )
}

fn decimal_literal<'a>() -> impl Parser<'a, &'a str, u64, Extra<'a>> + Clone {
    any()
        .filter(|c: &char| c.is_ascii_digit())
        .repeated()
        .at_least(1)
        .to_slice()
        .try_map(|s: &str, span| {
            s.parse::<u64>()
                .map_err(|e| Rich::custom(span, format!("invalid decimal literal: {e}")))
        })
}

pub(crate) fn number_literal<'a>() -> impl Parser<'a, &'a str, u64, Extra<'a>> + Clone {
    choice((hex_literal(), octal_literal(), binary_literal(), decimal_literal()))
}

pub(crate) fn string_literal<'a>() -> impl Parser<'a, &'a str, String, Extra<'a>> + Clone {
    let escape = just('\\').ignore_then(choice((
        just('\\').to('\\'),
        just('"').to('"'),
        just('n').to('\n'),
        // Line continuation: backslash + newline → skip
        just('\n').to('\0'), // Sentinel, filtered out below
    )));

    just('"')
        .ignore_then(
            none_of("\"\\")
                .or(escape)
                .repeated()
                .collect::<String>(),
        )
        .then_ignore(just('"'))
        .map(|s| s.replace('\0', ""))
}

/// Parse a boolean literal (`true` or `false`, case-insensitive).
pub(crate) fn bool_literal<'a>() -> impl Parser<'a, &'a str, bool, Extra<'a>> + Clone {
    kw("true").to(true).or(kw("false").to(false))
}

// ---------------------------------------------------------------------------
// Registers
// ---------------------------------------------------------------------------

#[must_use]
pub fn register<'a>() -> impl Parser<'a, &'a str, Reg, Extra<'a>> + Clone {
    just('%').ignore_then(
        // Order matters: longer matches first
        kw("pc").to(Reg::PC)
            .or(kw("sp").to(Reg::SP))
            .or(kw("sr").to(Reg::SR))
            .or(kw("a").to(Reg::A))
            .or(kw("b").to(Reg::B)),
    )
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

#[must_use]
pub fn expression<'a>() -> impl Parser<'a, &'a str, ExpressionNode, Extra<'a>> + Clone {
    // Parenthesized expressions need recursion
    let atom = recursive(|expr| {
        let number = number_literal()
            .map(|v| ExpressionNode::Literal(ExpressionValue::from(v)));

        let variable = identifier()
            .map(|i: &str| ExpressionNode::Variable(i.into()));

        let paren = expr
            .delimited_by(just('(').then(hspace()), hspace().then(just(')')));

        number.or(variable).or(paren)
    });

    // Unary operators
    let unary = choice((
        just('-')
            .ignore_then(hspace())
            .ignore_then(atom.clone())
            .map_with(|rhs, e| {
                ExpressionNode::Invert(
                    Box::new(rhs).with_location(span_to_range(e.span())),
                )
            }),
        just('~')
            .ignore_then(hspace())
            .ignore_then(atom.clone())
            .map_with(|rhs, e| {
                ExpressionNode::BinaryNot(
                    Box::new(rhs).with_location(span_to_range(e.span())),
                )
            }),
        atom,
    ));

    // Multiplication / Division
    let op = just('*').to(true).or(just('/').to(false));
    let product = unary.clone().foldl_with(
        hspace().ignore_then(op).then_ignore(hspace()).then(unary).repeated(),
        |lhs, (is_mul, rhs), e| {
            let span = e.span();
            let lhs = Box::new(lhs).with_location(span.start..span.start);
            let rhs = Box::new(rhs).with_location(span.end..span.end);
            if is_mul {
                ExpressionNode::Multiply(lhs, rhs)
            } else {
                ExpressionNode::Divide(lhs, rhs)
            }
        },
    );

    // Addition / Subtraction
    let op = just('+').to(true).or(just('-').to(false));
    let sum = product.clone().foldl_with(
        hspace().ignore_then(op).then_ignore(hspace()).then(product).repeated(),
        |lhs, (is_add, rhs), e| {
            let span = e.span();
            let lhs = Box::new(lhs).with_location(span.start..span.start);
            let rhs = Box::new(rhs).with_location(span.end..span.end);
            if is_add {
                ExpressionNode::Sum(lhs, rhs)
            } else {
                ExpressionNode::Substract(lhs, rhs)
            }
        },
    );

    // Shifts
    let op = just("<<").to(true).or(just(">>").to(false));
    let shift = sum.clone().foldl_with(
        hspace().ignore_then(op).then_ignore(hspace()).then(sum).repeated(),
        |lhs, (is_left, rhs), e| {
            let span = e.span();
            let lhs = Box::new(lhs).with_location(span.start..span.start);
            let rhs = Box::new(rhs).with_location(span.end..span.end);
            if is_left {
                ExpressionNode::LeftShift(lhs, rhs)
            } else {
                ExpressionNode::RightShift(lhs, rhs)
            }
        },
    );

    // Bitwise AND (must not match &&)
    let band = shift.clone().foldl_with(
        hspace()
            .ignore_then(just('&').then_ignore(just('&').not()))
            .then_ignore(hspace())
            .ignore_then(shift)
            .repeated(),
        |lhs, rhs, e| {
            let span = e.span();
            let lhs = Box::new(lhs).with_location(span.start..span.start);
            let rhs = Box::new(rhs).with_location(span.end..span.end);
            ExpressionNode::BinaryAnd(lhs, rhs)
        },
    );

    // Bitwise OR (must not match ||)
    band.clone().foldl_with(
        hspace()
            .ignore_then(just('|').then_ignore(just('|').not()))
            .then_ignore(hspace())
            .ignore_then(band)
            .repeated(),
        |lhs, rhs, e| {
            let span = e.span();
            let lhs = Box::new(lhs).with_location(span.start..span.start);
            let rhs = Box::new(rhs).with_location(span.end..span.end);
            ExpressionNode::BinaryOr(lhs, rhs)
        },
    )
    .boxed()
}

// ---------------------------------------------------------------------------
// Convenience wrappers for parsing complete strings
// ---------------------------------------------------------------------------

/// Parse a complete expression string, returning the AST node.
pub fn parse_expression_str(input: &str) -> Result<ExpressionNode, String> {
    let result = expression().then_ignore(end()).parse(input);
    result
        .into_result()
        .map_err(|errs| {
            errs.into_iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("; ")
        })
}

/// Parse a register name (e.g. `%a`, `%sp`), returning the register.
pub fn parse_register_str(input: &str) -> Result<Reg, String> {
    let result = register().then_ignore(end()).parse(input);
    result
        .into_result()
        .map_err(|errs| {
            errs.into_iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("; ")
        })
}
