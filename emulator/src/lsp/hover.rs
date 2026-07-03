use super::document::DocumentState;
use super::instructions::{directive_meta, meta};
use crate::parser::shared::is_identifier_char;
use crate::parser::value::{DirectiveKind, InstructionKind};

/// Result of a hover lookup.
pub struct HoverResult {
    /// Markdown content to display.
    pub contents: String,
    /// Byte range in the original source that the hover applies to.
    pub span: std::ops::Range<usize>,
}

/// Look up hover documentation at the given byte offset in the original source.
///
/// `analysis` may be `None` — label hover won't work in that case.
pub fn hover(
    analysis: Option<&DocumentState>,
    source: &str,
    byte_offset: usize,
) -> Option<HoverResult> {
    if byte_offset > source.len() {
        return None;
    }

    if let Some(result) = try_register_hover(source, byte_offset) {
        return Some(result);
    }

    // Macros and labels before mnemonics — user-defined names take
    // priority over built-in instruction documentation.
    if let Some(state) = analysis {
        if let Some(result) = try_macro_hover(state, source, byte_offset) {
            return Some(result);
        }

        if let Some(result) = try_label_hover(state, source, byte_offset) {
            return Some(result);
        }
    }

    if let Some(result) = try_mnemonic_hover(source, byte_offset) {
        return Some(result);
    }

    None
}

fn try_register_hover(source: &str, offset: usize) -> Option<HoverResult> {
    let bytes = source.as_bytes();

    // Find the start of a potential register (look for %)
    let mut start = offset;
    while start > 0 && bytes[start - 1].is_ascii_alphabetic() {
        start -= 1;
    }
    if start == 0 || bytes[start - 1] != b'%' {
        // Also check if cursor is right on the %
        if offset < bytes.len() && bytes[offset] == b'%' {
            start = offset;
        } else {
            return None;
        }
    } else {
        start -= 1; // include the %
    }

    // Find the end
    let mut end = offset;
    while end < bytes.len() && bytes[end].is_ascii_alphabetic() {
        end += 1;
    }
    // If we started on %, skip it for the name
    if start < bytes.len() && bytes[start] == b'%' && end > start + 1 {
        // ok
    } else {
        return None;
    }

    let reg_text = &source[start..end];
    let doc = register_doc(reg_text)?;
    Some(HoverResult {
        contents: doc.to_string(),
        span: start..end,
    })
}

fn try_mnemonic_hover(source: &str, offset: usize) -> Option<HoverResult> {
    let bytes = source.as_bytes();

    // Find the word boundaries
    let mut start = offset;
    while start > 0 && (bytes[start - 1].is_ascii_alphabetic() || bytes[start - 1] == b'_') {
        start -= 1;
    }
    let mut end = offset;
    while end < bytes.len() && (bytes[end].is_ascii_alphabetic() || bytes[end] == b'_') {
        end += 1;
    }

    if start == end {
        return None;
    }

    // Check for directive (preceded by '.')
    if start > 0 && bytes[start - 1] == b'.' {
        let word = &source[start..end];
        let kind = word.to_ascii_lowercase().parse::<DirectiveKind>().ok()?;
        return Some(HoverResult {
            contents: directive_meta(kind).hover.to_string(),
            span: (start - 1)..end, // include the dot
        });
    }

    let word = &source[start..end];
    let kind = word.to_ascii_lowercase().parse::<InstructionKind>().ok()?;
    if kind == InstructionKind::Error {
        return None;
    }
    Some(HoverResult {
        contents: meta(kind).hover.to_string(),
        span: start..end,
    })
}

fn try_macro_hover(state: &DocumentState, source: &str, offset: usize) -> Option<HoverResult> {
    let bytes = source.as_bytes();

    let mut start = offset;
    while start > 0 && is_identifier_char(bytes[start - 1] as char) {
        start -= 1;
    }
    let mut end = offset;
    while end < bytes.len() && is_identifier_char(bytes[end] as char) {
        end += 1;
    }

    if start == end {
        return None;
    }

    let word = &source[start..end];
    let annotations = state.annotations()?;

    // Find the last #define for this name (later defines override earlier ones)
    let def = annotations
        .definitions
        .iter()
        .rev()
        .find(|d| d.key == word)?;

    let value_display = def.value.as_deref().unwrap_or("(defined without value)");

    Some(HoverResult {
        contents: format!("**{word}** — macro\n\n`#define {word} {value_display}`"),
        span: start..end,
    })
}

fn try_label_hover(state: &DocumentState, source: &str, offset: usize) -> Option<HoverResult> {
    let bytes = source.as_bytes();

    let mut start = offset;
    while start > 0 && is_identifier_char(bytes[start - 1] as char) {
        start -= 1;
    }
    let mut end = offset;
    while end < bytes.len() && is_identifier_char(bytes[end] as char) {
        end += 1;
    }

    if start == end {
        return None;
    }

    let word = &source[start..end];
    let addr = state.labels().get(word)?;

    Some(HoverResult {
        contents: format!("**{word}** — label at address {addr}"),
        span: start..end,
    })
}

fn register_doc(reg: &str) -> Option<&'static str> {
    match reg {
        "%a" => Some(
            "**%a** — General-purpose register\n\n\
             64-bit signed integer register. Used as source or destination \
             for arithmetic, logic, and data transfer instructions.",
        ),
        "%b" => Some(
            "**%b** — General-purpose register\n\n\
             64-bit signed integer register. Used as source or destination \
             for arithmetic, logic, and data transfer instructions.",
        ),
        "%pc" => Some(
            "**%pc** — Program counter\n\n\
             Points to the address of the next instruction to execute. \
             Automatically incremented after each instruction. Modified by \
             jump and call instructions. Read-only in user mode.",
        ),
        "%sp" => Some(
            "**%sp** — Stack pointer\n\n\
             Points to the top of the stack. Starts at 10000 (top of memory) \
             and grows downward. Modified by `push`, `pop`, `call`, and `rtn`.",
        ),
        "%sr" => Some(
            "**%sr** — Status register\n\n\
             Contains processor flags:\n\
             - Bit 0: **C** (Carry)\n\
             - Bit 1: **Z** (Zero)\n\
             - Bit 2: **N** (Negative)\n\
             - Bit 3: **O** (Overflow)\n\
             - Bit 4: **I** (Interrupt enable)\n\
             - Bit 5: **S** (Supervisor mode)",
        ),
        _ => None,
    }
}
