//! Semantic token generation for Z33 assembly.
//!
//! Uses the parsed AST for precise tokenization of assembly constructs,
//! and annotations for preprocessor directives.

use tower_lsp::lsp_types::{
    SemanticToken, SemanticTokenType, SemanticTokens, SemanticTokensLegend,
};

use super::document::DocumentState;
use super::position;
use crate::parser::expression::Node as ExpressionNode;
use crate::parser::line::LineContent;
use crate::parser::value::{DirectiveArgument, InstructionArgument, InstructionKind};

/// The token types we use, in the order they appear in the legend.
pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,   // 0: instructions
    SemanticTokenType::MACRO,     // 1: directives (.word, .space, etc.)
    SemanticTokenType::VARIABLE,  // 2: registers (%a, %b, etc.)
    SemanticTokenType::FUNCTION,  // 3: label definitions (foo:)
    SemanticTokenType::PARAMETER, // 4: label references / macro references
    SemanticTokenType::NUMBER,    // 5: numeric literals
    SemanticTokenType::STRING,    // 6: string literals
    SemanticTokenType::COMMENT,   // 7: comments
];

#[must_use]
pub fn legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TOKEN_TYPES.to_vec(),
        token_modifiers: vec![],
    }
}

const TK_KEYWORD: u32 = 0;
const TK_MACRO: u32 = 1;
const TK_VARIABLE: u32 = 2;
const TK_FUNCTION: u32 = 3;
const TK_PARAMETER: u32 = 4;
const TK_NUMBER: u32 = 5;
const TK_STRING: u32 = 6;
const TK_COMMENT: u32 = 7;

/// A raw token before delta-encoding: `(start_byte, length, token_type)`.
type RawToken = (usize, usize, u32);

/// Produce semantic tokens for the document using AST data.
#[allow(clippy::too_many_lines)]
pub fn semantic_tokens(analysis: Option<&DocumentState>, source: &str) -> SemanticTokens {
    let mut raw_tokens: Vec<RawToken> = Vec::new();

    if let Some(state) = analysis {
        // AST-based tokens for assembly constructs
        if let Some(program) = state.program() {
            for line in &program.lines {
                // Label definitions
                for sym in &line.inner.symbols {
                    if let Some(span) = state.resolve_span(sym.location.clone()) {
                        raw_tokens.push((span.start, span.end - span.start, TK_FUNCTION));
                    }
                }

                // Instruction or directive content
                if let Some(content) = &line.inner.content {
                    match &content.inner {
                        LineContent::Instruction { kind, arguments } => {
                            if kind.inner != InstructionKind::Error {
                                if let Some(span) = state.resolve_span(kind.location.clone()) {
                                    raw_tokens.push((
                                        span.start,
                                        span.end - span.start,
                                        TK_KEYWORD,
                                    ));
                                }
                            }

                            for arg in arguments {
                                tokenize_argument(
                                    &arg.inner,
                                    &arg.location,
                                    state,
                                    &mut raw_tokens,
                                );
                            }
                        }
                        LineContent::Directive { kind, argument } => {
                            if let Some(span) = state.resolve_span(kind.location.clone()) {
                                // Include the '.' prefix in the token
                                let dot_start = if span.start > 0 { span.start - 1 } else { 0 };
                                raw_tokens.push((dot_start, span.end - dot_start, TK_MACRO));
                            }

                            tokenize_directive_argument(
                                &argument.inner,
                                &argument.location,
                                state,
                                &mut raw_tokens,
                            );
                        }
                        LineContent::Error => {}
                    }
                }

                // Inline // comment
                if let Some(comment) = &line.inner.comment {
                    if let Some(span) = state.resolve_span(comment.location.clone()) {
                        // Include the "//" prefix (2 bytes before the comment text)
                        let prefix_start = span.start.saturating_sub(2);
                        raw_tokens.push((prefix_start, span.end - prefix_start, TK_COMMENT));
                    }
                }
            }
        }

        // Preprocessor directive lines from annotations
        if let Some(ann) = state.annotations() {
            for def in &ann.definitions {
                raw_tokens.push((def.span.start, def.span.end - def.span.start, TK_MACRO));
            }
            for undef in &ann.undefinitions {
                raw_tokens.push((
                    undef.span.start,
                    undef.span.end - undef.span.start,
                    TK_MACRO,
                ));
            }
            for inc in &ann.inclusions {
                raw_tokens.push((inc.span.start, inc.span.end - inc.span.start, TK_MACRO));
            }
            for block in &ann.conditional_blocks {
                for branch in &block.branches {
                    raw_tokens.push((
                        branch.directive_span.start,
                        branch.directive_span.end - branch.directive_span.start,
                        TK_MACRO,
                    ));
                }
                if let Some(fallback) = &block.fallback {
                    if !fallback.directive_span.is_empty() {
                        raw_tokens.push((
                            fallback.directive_span.start,
                            fallback.directive_span.end - fallback.directive_span.start,
                            TK_MACRO,
                        ));
                    }
                }
            }
        }
    }

    // Sort by position and deduplicate overlapping tokens
    raw_tokens.sort_by_key(|t| t.0);

    // Convert to delta-encoded SemanticToken
    let mut result = Vec::with_capacity(raw_tokens.len());
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    for (start, len, token_type) in &raw_tokens {
        if *len == 0 {
            continue;
        }
        let Some(pos) = position::position(source, *start) else {
            continue;
        };

        let delta_line = pos.line - prev_line;
        let delta_start = if delta_line == 0 {
            pos.character - prev_start
        } else {
            pos.character
        };

        result.push(SemanticToken {
            delta_line,
            delta_start,
            #[allow(clippy::cast_possible_truncation)]
            length: *len as u32,
            token_type: *token_type,
            token_modifiers_bitset: 0,
        });

        prev_line = pos.line;
        prev_start = pos.character;
    }

    SemanticTokens {
        result_id: None,
        data: result,
    }
}

/// Emit tokens for an instruction argument using AST data.
fn tokenize_argument(
    arg: &InstructionArgument,
    arg_span: &std::ops::Range<usize>,
    state: &DocumentState,
    out: &mut Vec<RawToken>,
) {
    match arg {
        InstructionArgument::Register(_) => {
            if let Some(span) = state.resolve_span(arg_span.clone()) {
                out.push((span.start, span.end - span.start, TK_VARIABLE));
            }
        }
        InstructionArgument::Value(node) => {
            tokenize_expression_top(node, arg_span, state, out);
        }
        InstructionArgument::Direct(located_node) => {
            tokenize_expression(&located_node.inner, &located_node.location, state, out);
        }
        InstructionArgument::Indirect(located_reg) => {
            if let Some(span) = state.resolve_span(located_reg.location.clone()) {
                out.push((span.start, span.end - span.start, TK_VARIABLE));
            }
        }
        InstructionArgument::Indexed { register, value } => {
            if let Some(span) = state.resolve_span(register.location.clone()) {
                out.push((span.start, span.end - span.start, TK_VARIABLE));
            }
            tokenize_expression(&value.inner, &value.location, state, out);
        }
        InstructionArgument::Error => {}
    }
}

/// Emit tokens for a directive argument.
fn tokenize_directive_argument(
    arg: &DirectiveArgument,
    arg_span: &std::ops::Range<usize>,
    state: &DocumentState,
    out: &mut Vec<RawToken>,
) {
    match arg {
        DirectiveArgument::Expression(node) => {
            tokenize_expression_top(node, arg_span, state, out);
        }
        DirectiveArgument::StringLiteral(_) => {
            if let Some(span) = state.resolve_span(arg_span.clone()) {
                out.push((span.start, span.end - span.start, TK_STRING));
            }
        }
    }
}

/// Emit tokens for a top-level expression (no own Located wrapper).
fn tokenize_expression_top(
    node: &ExpressionNode,
    fallback_span: &std::ops::Range<usize>,
    state: &DocumentState,
    out: &mut Vec<RawToken>,
) {
    match node {
        ExpressionNode::Literal(_) => {
            if let Some(span) = state.resolve_span(fallback_span.clone()) {
                out.push((span.start, span.end - span.start, TK_NUMBER));
            }
        }
        ExpressionNode::Variable(_) => {
            if let Some(span) = state.resolve_span(fallback_span.clone()) {
                out.push((span.start, span.end - span.start, TK_PARAMETER));
            }
        }
        _ => tokenize_expression(node, fallback_span, state, out),
    }
}

/// Recursively emit tokens for expression nodes.
fn tokenize_expression(
    node: &ExpressionNode,
    span: &std::ops::Range<usize>,
    state: &DocumentState,
    out: &mut Vec<RawToken>,
) {
    match node {
        ExpressionNode::Literal(_) => {
            if let Some(s) = state.resolve_span(span.clone()) {
                out.push((s.start, s.end - s.start, TK_NUMBER));
            }
        }
        ExpressionNode::Variable(_) => {
            if let Some(s) = state.resolve_span(span.clone()) {
                out.push((s.start, s.end - s.start, TK_PARAMETER));
            }
        }
        ExpressionNode::BinaryOr(a, b)
        | ExpressionNode::BinaryAnd(a, b)
        | ExpressionNode::LeftShift(a, b)
        | ExpressionNode::RightShift(a, b)
        | ExpressionNode::Sum(a, b)
        | ExpressionNode::Substract(a, b)
        | ExpressionNode::Multiply(a, b)
        | ExpressionNode::Divide(a, b) => {
            tokenize_expression(&a.inner, &a.location, state, out);
            tokenize_expression(&b.inner, &b.location, state, out);
        }
        ExpressionNode::Invert(a) | ExpressionNode::BinaryNot(a) => {
            tokenize_expression(&a.inner, &a.location, state, out);
        }
        ExpressionNode::Error => {}
    }
}
