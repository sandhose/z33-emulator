use tower_lsp::lsp_types::{
    SemanticToken, SemanticTokenType, SemanticTokens, SemanticTokensLegend,
};

use super::document::DocumentState;
use super::position;
use crate::parser::shared::is_identifier_char;

/// The token types we use, in the order they appear in the legend.
pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,   // 0: instructions
    SemanticTokenType::MACRO,     // 1: directives (.word, .space, etc.)
    SemanticTokenType::VARIABLE,  // 2: registers (%a, %b, etc.)
    SemanticTokenType::FUNCTION,  // 3: label definitions (foo:)
    SemanticTokenType::PARAMETER, // 4: label references
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

/// Produce semantic tokens for the document.
///
/// This is text-based (operates on the original source) so it works even when
/// the preprocessor or parser fails.
#[allow(clippy::too_many_lines)]
pub fn semantic_tokens(state: &DocumentState) -> SemanticTokens {
    let source = state.source();
    let labels = state.labels();
    let mut raw_tokens: Vec<(usize, usize, u32)> = Vec::new(); // (start, len, type)

    for line_text in source.split('\n') {
        let line_start = line_text.as_ptr() as usize - source.as_ptr() as usize;

        // Find comment
        if let Some(comment_offset) = find_comment(line_text) {
            let abs = line_start + comment_offset;
            let len = line_text.len() - comment_offset;
            if len > 0 {
                raw_tokens.push((abs, len, TK_COMMENT));
            }
        }

        let effective = line_text.find('#').map_or(line_text, |i| &line_text[..i]);

        let mut pos = 0;
        let bytes = effective.as_bytes();

        // Parse label definitions
        loop {
            while pos < bytes.len() && (bytes[pos] == b' ' || bytes[pos] == b'\t') {
                pos += 1;
            }
            let ident_start = pos;
            if pos < bytes.len() && (bytes[pos].is_ascii_alphabetic() || bytes[pos] == b'_') {
                pos += 1;
                while pos < bytes.len() && is_identifier_char(bytes[pos] as char) {
                    pos += 1;
                }
                let ident_end = pos;
                while pos < bytes.len() && (bytes[pos] == b' ' || bytes[pos] == b'\t') {
                    pos += 1;
                }
                if pos < bytes.len() && bytes[pos] == b':' {
                    // Label definition
                    raw_tokens.push((
                        line_start + ident_start,
                        ident_end - ident_start,
                        TK_FUNCTION,
                    ));
                    pos += 1;
                    continue;
                }
                pos = ident_start; // backtrack
            }
            break;
        }

        // Skip whitespace to content
        while pos < bytes.len() && (bytes[pos] == b' ' || bytes[pos] == b'\t') {
            pos += 1;
        }

        if pos >= bytes.len() {
            continue;
        }

        // Directive
        if bytes[pos] == b'.' {
            let dir_start = pos;
            pos += 1;
            while pos < bytes.len() && bytes[pos].is_ascii_alphabetic() {
                pos += 1;
            }
            raw_tokens.push((line_start + dir_start, pos - dir_start, TK_MACRO));

            // Tokenize the directive argument
            tokenize_value_area(effective, pos, line_start, labels, &mut raw_tokens);
            continue;
        }

        // Instruction mnemonic
        if bytes[pos].is_ascii_alphabetic() || bytes[pos] == b'_' {
            let mnemonic_start = pos;
            pos += 1;
            while pos < bytes.len() && (bytes[pos].is_ascii_alphabetic() || bytes[pos] == b'_') {
                pos += 1;
            }
            let mnemonic = &effective[mnemonic_start..pos];
            if is_instruction(mnemonic) {
                raw_tokens.push((
                    line_start + mnemonic_start,
                    pos - mnemonic_start,
                    TK_KEYWORD,
                ));
            }

            // Tokenize the argument area
            tokenize_value_area(effective, pos, line_start, labels, &mut raw_tokens);
        }
    }

    // Sort by position
    raw_tokens.sort_by_key(|t| t.0);

    // Convert to delta-encoded SemanticToken
    let mut result = Vec::with_capacity(raw_tokens.len());
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    for (start, len, token_type) in raw_tokens {
        let Some(pos) = position::position(source, start) else {
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
            length: len as u32,
            token_type,
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

/// Tokenize registers, numbers, strings, and label references in the argument
/// area of an instruction or directive.
fn tokenize_value_area(
    line: &str,
    start_pos: usize,
    line_start: usize,
    labels: &std::collections::BTreeMap<String, crate::constants::Address>,
    tokens: &mut Vec<(usize, usize, u32)>,
) {
    let bytes = line.as_bytes();
    let mut pos = start_pos;

    while pos < bytes.len() {
        match bytes[pos] {
            b' ' | b'\t' | b',' | b'[' | b']' | b'+' | b'-' | b'*' | b'/' | b'(' | b')' | b'~'
            | b'&' | b'|' | b'^' => {
                pos += 1;
            }
            b'%' => {
                let reg_start = pos;
                pos += 1;
                while pos < bytes.len() && bytes[pos].is_ascii_alphabetic() {
                    pos += 1;
                }
                tokens.push((line_start + reg_start, pos - reg_start, TK_VARIABLE));
            }
            b'"' => {
                let str_start = pos;
                pos += 1;
                while pos < bytes.len() && bytes[pos] != b'"' {
                    if bytes[pos] == b'\\' {
                        pos += 1; // skip escaped char
                    }
                    pos += 1;
                }
                if pos < bytes.len() {
                    pos += 1; // closing quote
                }
                tokens.push((line_start + str_start, pos - str_start, TK_STRING));
            }
            b'0'..=b'9' => {
                let num_start = pos;
                // Handle 0x, 0o, 0b prefixes
                if bytes[pos] == b'0' && pos + 1 < bytes.len() {
                    match bytes[pos + 1] {
                        b'x' | b'X' => {
                            pos += 2;
                            while pos < bytes.len() && bytes[pos].is_ascii_hexdigit() {
                                pos += 1;
                            }
                        }
                        b'o' | b'O' => {
                            pos += 2;
                            while pos < bytes.len() && (b'0'..=b'7').contains(&bytes[pos]) {
                                pos += 1;
                            }
                        }
                        b'b' | b'B' => {
                            pos += 2;
                            while pos < bytes.len() && (bytes[pos] == b'0' || bytes[pos] == b'1') {
                                pos += 1;
                            }
                        }
                        _ => {
                            while pos < bytes.len() && bytes[pos].is_ascii_digit() {
                                pos += 1;
                            }
                        }
                    }
                } else {
                    while pos < bytes.len() && bytes[pos].is_ascii_digit() {
                        pos += 1;
                    }
                }
                tokens.push((line_start + num_start, pos - num_start, TK_NUMBER));
            }
            c if c.is_ascii_alphabetic() || c == b'_' => {
                let ident_start = pos;
                pos += 1;
                while pos < bytes.len() && is_identifier_char(bytes[pos] as char) {
                    pos += 1;
                }
                let ident = &line[ident_start..pos];
                if labels.contains_key(ident) {
                    tokens.push((line_start + ident_start, pos - ident_start, TK_PARAMETER));
                }
            }
            _ => {
                pos += 1;
            }
        }
    }
}

fn find_comment(line: &str) -> Option<usize> {
    // Find '#' that's not inside a string
    let mut in_string = false;
    for (i, b) in line.bytes().enumerate() {
        match b {
            b'"' if !in_string => in_string = true,
            b'"' if in_string => in_string = false,
            b'\\' if in_string => {
                // skip next char (handled by iteration)
            }
            b'#' if !in_string => return Some(i),
            _ => {}
        }
    }
    None
}

fn is_instruction(word: &str) -> bool {
    matches!(
        word.to_ascii_lowercase().as_str(),
        "add"
            | "and"
            | "call"
            | "cmp"
            | "div"
            | "fas"
            | "in"
            | "jmp"
            | "jeq"
            | "jne"
            | "jle"
            | "jlt"
            | "jge"
            | "jgt"
            | "ld"
            | "mul"
            | "neg"
            | "nop"
            | "not"
            | "or"
            | "out"
            | "pop"
            | "push"
            | "reset"
            | "rti"
            | "rtn"
            | "shl"
            | "shr"
            | "st"
            | "sub"
            | "swap"
            | "trap"
            | "xor"
    )
}
