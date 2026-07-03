//! Semantic token generation for Z33 assembly.
//!
//! These tokens are a *semantic-only* overlay on top of each editor's base
//! grammar (tree-sitter in Zed, `TextMate` in VS Code, Monarch in the web IDE).
//! The grammars already classify every lexical construct — instructions,
//! directives, registers, numbers, strings, comments and label definitions — so
//! we deliberately emit **nothing** for those. We only add the two things a
//! grammar cannot know without whole-program analysis:
//!
//! - **Macro references**: a bare identifier that expands to a `#define`d
//!   macro. The grammars see it as a generic identifier; only the preprocessor
//!   knows it is a macro. Emitted as [`SemanticTokenType::MACRO`].
//! - **Resolved label references**: a label used in an expression, coloured by
//!   what it points at — [`SemanticTokenType::FUNCTION`] when it targets code
//!   and [`SemanticTokenType::VARIABLE`] when it targets data.
//!
//! Unresolved references emit nothing (diagnostics already flag undefined
//! symbols), and macro *definitions* / label *definitions* emit nothing (the
//! grammars classify those structurally).

use std::collections::HashSet;

use tower_lsp::lsp_types::{
    SemanticToken, SemanticTokenType, SemanticTokens, SemanticTokensLegend,
};

use super::document::{DocumentState, LabelKind};
use super::position;
use super::references::OccurrenceKind;
use crate::parser::line::LineContent;
use crate::parser::value::DirectiveArgument;

/// The token types we use, in the order they appear in the legend.
pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION, // 0: label reference pointing at code
    SemanticTokenType::VARIABLE, // 1: label reference pointing at data
    SemanticTokenType::MACRO,    // 2: reference to a `#define`d macro
];

#[must_use]
pub fn legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TOKEN_TYPES.to_vec(),
        token_modifiers: vec![],
    }
}

const TK_FUNCTION: u32 = 0;
const TK_VARIABLE: u32 = 1;
const TK_MACRO: u32 = 2;

/// A raw token before delta-encoding: `(start_byte, length, token_type)`.
type RawToken = (usize, usize, u32);

/// Produce semantic tokens for the document.
#[must_use]
pub fn semantic_tokens(analysis: Option<&DocumentState>, source: &str) -> SemanticTokens {
    let mut raw_tokens: Vec<RawToken> = Vec::new();

    if let Some(state) = analysis {
        // 1. Macro references (lexical scan of the original source).
        collect_macro_references(state, &mut raw_tokens);

        // Spans already claimed by macro tokens. A macro whose value is itself
        // an identifier resurfaces in the parsed AST under its *expanded* name;
        // skip those label references so we don't double-tag the same span.
        let macro_spans: Vec<(usize, usize)> =
            raw_tokens.iter().map(|t| (t.0, t.0 + t.1)).collect();

        // 2. Resolved label references, coloured code-vs-data.
        let root = state.root_file_id();
        for occ in state.occurrences() {
            if occ.kind != OccurrenceKind::Reference || occ.file_id != root {
                continue;
            }
            let (start, end) = (occ.span.start, occ.span.end);
            if macro_spans.iter().any(|(ms, me)| start < *me && *ms < end) {
                continue;
            }
            match state.label_kind(&occ.name) {
                // Unresolved reference: emit nothing (a diagnostic covers it).
                None => {}
                Some(LabelKind::Code) => raw_tokens.push((start, end - start, TK_FUNCTION)),
                Some(LabelKind::Data) => raw_tokens.push((start, end - start, TK_VARIABLE)),
            }
        }
    }

    // Sort by position (LSP requires tokens in order).
    raw_tokens.sort_by_key(|t| t.0);

    // Convert to delta-encoded SemanticToken.
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

fn is_ident_start(b: u8) -> bool {
    b.is_ascii_alphabetic() || b == b'_'
}

fn is_ident_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

/// Scan the original root source for bare identifiers that name a `#define`d
/// macro and emit a [`SemanticTokenType::MACRO`] token for each.
///
/// The preprocessor expands macros away before the AST is built (a numeric
/// macro disappears entirely, an identifier macro resurfaces under its expanded
/// name), so the only reliable place to spot a macro *usage* is the original
/// text. We therefore scan words directly, skipping any that fall inside a
/// preprocessor directive line (the definition itself), a comment or a string.
fn collect_macro_references(state: &DocumentState, out: &mut Vec<RawToken>) {
    let Some(ann) = state.annotations() else {
        return;
    };

    // Macro names are in scope regardless of which file defined them.
    let keys: HashSet<&str> = ann.definitions.iter().map(|d| d.key.as_str()).collect();
    if keys.is_empty() {
        return;
    }

    let source = state.source();
    let root = state.root_file_id();

    // Spans in the root file that must not be scanned for macro usages.
    let mut protected: Vec<(usize, usize)> = Vec::new();
    for d in ann.definitions.iter().filter(|d| d.file_id == root) {
        protected.push((d.span.start, d.span.end));
    }
    for u in ann.undefinitions.iter().filter(|u| u.file_id == root) {
        protected.push((u.span.start, u.span.end));
    }
    for block in ann.conditional_blocks.iter().filter(|b| b.file_id == root) {
        for branch in &block.branches {
            protected.push((branch.directive_span.start, branch.directive_span.end));
        }
        if let Some(fallback) = &block.fallback {
            protected.push((fallback.directive_span.start, fallback.directive_span.end));
        }
    }
    // Comments and string literals from the parsed program.
    if let Some(program) = state.program() {
        for line in &program.lines {
            if let Some(comment) = &line.inner.comment {
                if let Some(span) = state.resolve_span(comment.location.clone()) {
                    protected.push((span.start.saturating_sub(2), span.end));
                }
            }
            if let Some(content) = &line.inner.content {
                if let LineContent::Directive { argument, .. } = &content.inner {
                    if matches!(argument.inner, DirectiveArgument::StringLiteral(_)) {
                        if let Some(span) = state.resolve_span(argument.location.clone()) {
                            protected.push((span.start, span.end));
                        }
                    }
                }
            }
        }
    }

    let bytes = source.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if is_ident_start(bytes[i]) {
            let start = i;
            i += 1;
            while i < bytes.len() && is_ident_char(bytes[i]) {
                i += 1;
            }
            let word = &source[start..i];
            let protected_here = protected.iter().any(|(ps, pe)| start < *pe && *ps < i);
            if !protected_here && keys.contains(word) {
                out.push((start, i - start, TK_MACRO));
            }
        } else {
            i += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lsp::document::DocumentState;

    /// A decoded semantic token: absolute position + type name.
    #[derive(Debug, PartialEq, Eq)]
    struct Decoded {
        line: u32,
        character: u32,
        length: u32,
        ty: &'static str,
    }

    /// Decode the delta-encoded tokens into absolute positions + type names.
    fn decode(tokens: &SemanticTokens) -> Vec<Decoded> {
        let mut line = 0u32;
        let mut character = 0u32;
        let mut out = Vec::new();
        for t in &tokens.data {
            if t.delta_line == 0 {
                character += t.delta_start;
            } else {
                line += t.delta_line;
                character = t.delta_start;
            }
            out.push(Decoded {
                line,
                character,
                length: t.length,
                ty: TOKEN_TYPES[t.token_type as usize].as_str(),
            });
        }
        out
    }

    fn tokens(src: &str) -> Vec<Decoded> {
        let state = DocumentState::new(src.to_string());
        decode(&semantic_tokens(Some(&state), src))
    }

    #[test]
    fn legend_is_minimal() {
        let names: Vec<&str> = TOKEN_TYPES.iter().map(SemanticTokenType::as_str).collect();
        assert_eq!(names, vec!["function", "variable", "macro"]);
    }

    #[test]
    fn label_reference_to_code_is_function() {
        // `jmp code` targets an instruction, so the reference is `function`.
        let src = "code:\n    reset\n    jmp code\n";
        let toks = tokens(src);
        assert_eq!(
            toks,
            vec![Decoded {
                line: 2,
                character: 8,
                length: 4,
                ty: "function"
            }]
        );
    }

    #[test]
    fn label_reference_to_data_is_variable() {
        // `jmp data` targets a `.word` cell, so the reference is `variable`.
        let src = "    jmp data\ndata:\n    .word 42\n";
        let toks = tokens(src);
        assert_eq!(
            toks,
            vec![Decoded {
                line: 0,
                character: 8,
                length: 4,
                ty: "variable"
            }]
        );
    }

    #[test]
    fn label_reference_to_space_is_variable() {
        let src = "    ld buf, %a\nbuf:\n    .space 4\n";
        let toks = tokens(src);
        assert_eq!(
            toks,
            vec![Decoded {
                line: 0,
                character: 7,
                length: 3,
                ty: "variable"
            }]
        );
    }

    #[test]
    fn label_reference_to_string_is_variable() {
        let src = "    ld msg, %a\nmsg:\n    .string \"hi\"\n";
        let toks = tokens(src);
        assert_eq!(
            toks,
            vec![Decoded {
                line: 0,
                character: 7,
                length: 3,
                ty: "variable"
            }]
        );
    }

    #[test]
    fn macro_reference_is_macro() {
        // `N` expands to `5`; the grammar cannot tell it apart from a label, so
        // the LSP marks it as a macro. The `#define` line itself is not tagged.
        let src = "#define N 5\n    ld N, %a\n";
        let toks = tokens(src);
        assert_eq!(
            toks,
            vec![Decoded {
                line: 1,
                character: 7,
                length: 1,
                ty: "macro"
            }]
        );
    }

    #[test]
    fn macro_reference_to_label_value_is_macro_only() {
        // `PTR` expands to `data` (a label). It must be tagged once, as a
        // macro, not also as a (data) label reference.
        let src = "#define PTR data\ndata:\n    .word 1\n    jmp PTR\n";
        let toks = tokens(src);
        assert_eq!(
            toks,
            vec![Decoded {
                line: 3,
                character: 8,
                length: 3,
                ty: "macro"
            }]
        );
    }

    #[test]
    fn undefined_reference_emits_nothing() {
        let src = "    jmp nowhere\n";
        assert_eq!(tokens(src), vec![]);
    }

    #[test]
    fn lexical_constructs_emit_nothing() {
        // Instruction, register, number, string, comment and the label
        // definition are all left to the base grammar.
        let src = "code:\n    ld 42, %a // a comment\n    .string \"hi\"\n";
        assert_eq!(tokens(src), vec![]);
    }

    #[test]
    fn macro_name_inside_comment_is_not_tagged() {
        let src = "#define N 5\n    reset // N is five\n";
        assert_eq!(tokens(src), vec![]);
    }

    #[test]
    fn multi_token_macro_expansion_has_no_overlaps() {
        // The classic "Overlapping semantic tokens" trigger: a macro that
        // expands to several tokens, each of which used to map back onto the
        // same original span. Now it yields a single macro token, no overlaps.
        let src = "#define INC add %a, %b
code:
    INC
    jmp code
";
        let state = DocumentState::new(src.to_string());
        let toks = decode(&semantic_tokens(Some(&state), src));
        // Assert no two tokens overlap on the same line.
        for a in &toks {
            for b in &toks {
                if std::ptr::eq(a, b) {
                    continue;
                }
                if a.line == b.line {
                    let a_end = a.character + a.length;
                    let b_end = b.character + b.length;
                    assert!(
                        a.character >= b_end || b.character >= a_end,
                        "overlap between {a:?} and {b:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn macro_name_inside_string_is_not_tagged() {
        let src = "#define N 5\n    .string \"N\"\n";
        assert_eq!(tokens(src), vec![]);
    }
}
