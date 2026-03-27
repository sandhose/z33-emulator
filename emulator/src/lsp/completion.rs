use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, InsertTextFormat,
};

use super::document::DocumentState;
use crate::parser::shared::is_identifier_char;
use crate::parser::value::InstructionKind;

/// What types of arguments an instruction expects at a given position.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ArgType {
    /// Only a register
    Reg,
    /// Immediate or register
    ImmReg,
    /// Any addressing mode
    ImmRegDirIndIdx,
    /// Memory addressing only (direct, indirect, indexed)
    DirIndIdx,
    /// Register or memory addressing
    RegDirIndIdx,
}

/// Returns the expected argument types for a given instruction kind.
///
/// Returns `None` for instructions that take no arguments, or for argument
/// indices beyond what the instruction accepts.
#[allow(clippy::match_same_arms)]
fn expected_arg_type(kind: InstructionKind, arg_index: usize) -> Option<ArgType> {
    use InstructionKind as K;
    match (kind, arg_index) {
        // 2-arg: (ImmRegDirIndIdx, Reg)
        (
            K::Add
            | K::And
            | K::Cmp
            | K::Div
            | K::Ld
            | K::Mul
            | K::Or
            | K::Shl
            | K::Shr
            | K::Sub
            | K::Xor,
            0,
        ) => Some(ArgType::ImmRegDirIndIdx),
        (
            K::Add
            | K::And
            | K::Cmp
            | K::Div
            | K::Ld
            | K::Mul
            | K::Or
            | K::Shl
            | K::Shr
            | K::Sub
            | K::Xor,
            1,
        ) => Some(ArgType::Reg),

        // 2-arg: (DirIndIdx, Reg)
        (K::Fas | K::In, 0) => Some(ArgType::DirIndIdx),
        (K::Fas | K::In, 1) => Some(ArgType::Reg),

        // 2-arg: (Reg, DirIndIdx)
        (K::St, 0) => Some(ArgType::Reg),
        (K::St, 1) => Some(ArgType::DirIndIdx),

        // 2-arg: (ImmReg, DirIndIdx)
        (K::Out, 0) => Some(ArgType::ImmReg),
        (K::Out, 1) => Some(ArgType::DirIndIdx),

        // 2-arg: (RegDirIndIdx, Reg)
        (K::Swap, 0) => Some(ArgType::RegDirIndIdx),
        (K::Swap, 1) => Some(ArgType::Reg),

        // 1-arg: (ImmRegDirIndIdx)
        (K::Call | K::Jmp | K::Jeq | K::Jne | K::Jle | K::Jlt | K::Jge | K::Jgt, 0) => {
            Some(ArgType::ImmRegDirIndIdx)
        }

        // 1-arg: (Reg)
        (K::Neg | K::Not | K::Pop, 0) => Some(ArgType::Reg),

        // 1-arg: (ImmReg)
        (K::Push, 0) => Some(ArgType::ImmReg),

        // 0-arg or out-of-range
        _ => None,
    }
}

/// If `prefix_typed` is true, the `%` has already been typed by the user, so
/// `insert_text` omits it to avoid doubling.
fn register_completions(prefix_typed: bool) -> Vec<CompletionItem> {
    [
        ("a", "%a"),
        ("b", "%b"),
        ("pc", "%pc"),
        ("sp", "%sp"),
        ("sr", "%sr"),
    ]
    .into_iter()
    .map(|(short, full)| CompletionItem {
        label: full.to_string(),
        kind: Some(CompletionItemKind::VARIABLE),
        detail: Some("register".to_string()),
        insert_text: if prefix_typed {
            Some(short.to_string())
        } else {
            None
        },
        filter_text: if prefix_typed {
            Some(short.to_string())
        } else {
            None
        },
        ..Default::default()
    })
    .collect()
}

fn instruction_completions() -> Vec<CompletionItem> {
    // (mnemonic, description, signature)
    // No snippets — signature help and argument completions guide the user.
    let instructions: &[(&str, &str, &str)] = &[
        ("add", "Add a value to a register", "src, %dst"),
        ("and", "Bitwise AND with a register", "src, %dst"),
        ("call", "Push PC and jump to address", "addr"),
        ("cmp", "Compare a value with a register", "src, %dst"),
        ("div", "Divide a register by a value", "src, %dst"),
        ("fas", "Fetch-and-set (atomic)", "[addr], %dst"),
        ("in", "Read from I/O controller", "[port], %dst"),
        ("jmp", "Unconditional jump", "addr"),
        ("jeq", "Jump if equal (zero flag set)", "addr"),
        ("jne", "Jump if not equal", "addr"),
        ("jle", "Jump if less or equal", "addr"),
        ("jlt", "Jump if strictly less", "addr"),
        ("jge", "Jump if greater or equal", "addr"),
        ("jgt", "Jump if strictly greater", "addr"),
        ("ld", "Load a value into a register", "src, %dst"),
        ("mul", "Multiply a register by a value", "src, %dst"),
        ("neg", "Negate a register", "%reg"),
        ("nop", "No operation", ""),
        ("not", "Bitwise NOT of a register", "%reg"),
        ("or", "Bitwise OR with a register", "src, %dst"),
        ("out", "Write to I/O controller", "src, [port]"),
        ("pop", "Pop value from stack", "%reg"),
        ("push", "Push value onto stack", "src"),
        ("reset", "Reset the computer", ""),
        ("rti", "Return from interrupt", ""),
        ("rtn", "Return from call", ""),
        ("shl", "Shift left", "src, %dst"),
        ("shr", "Shift right", "src, %dst"),
        ("st", "Store register to memory", "%src, [addr]"),
        ("sub", "Subtract a value from a register", "src, %dst"),
        ("swap", "Swap a value and a register", "src, %dst"),
        ("trap", "Trigger trap exception", ""),
        ("xor", "Bitwise XOR with a register", "src, %dst"),
    ];

    instructions
        .iter()
        .map(|(name, detail, signature)| CompletionItem {
            label: (*name).to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some((*detail).to_string()),
            label_details: if signature.is_empty() {
                None
            } else {
                Some(CompletionItemLabelDetails {
                    detail: Some(format!(" {signature}")),
                    ..Default::default()
                })
            },
            ..Default::default()
        })
        .collect()
}

fn directive_completions() -> Vec<CompletionItem> {
    let directives = [
        (".addr", "Set current address"),
        (".space", "Reserve memory cells"),
        (".string", "Store a string literal"),
        (".word", "Store a word value"),
    ];

    directives
        .into_iter()
        .map(|(name, detail)| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("directive".to_string()),
            label_details: Some(CompletionItemLabelDetails {
                description: Some(detail.to_string()),
                ..Default::default()
            }),
            ..Default::default()
        })
        .collect()
}

fn label_completions(state: &DocumentState) -> Vec<CompletionItem> {
    state
        .labels()
        .iter()
        .map(|(name, addr)| CompletionItem {
            label: name.clone(),
            kind: Some(CompletionItemKind::CONSTANT),
            detail: Some(format!("label (address {addr})")),
            ..Default::default()
        })
        .collect()
}

fn completions_for_arg_type(arg_type: ArgType, state: &DocumentState) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    match arg_type {
        ArgType::Reg => {
            items.extend(register_completions(false));
        }
        ArgType::ImmReg => {
            items.extend(register_completions(false));
            items.extend(label_completions(state));
        }
        ArgType::ImmRegDirIndIdx => {
            items.extend(register_completions(false));
            items.extend(label_completions(state));
            items.push(bracket_snippet());
        }
        ArgType::DirIndIdx => {
            items.push(bracket_snippet());
        }
        ArgType::RegDirIndIdx => {
            items.extend(register_completions(false));
            items.push(bracket_snippet());
        }
    }
    items
}

fn bracket_snippet() -> CompletionItem {
    CompletionItem {
        label: "[...]".to_string(),
        kind: Some(CompletionItemKind::SNIPPET),
        detail: Some("memory access".to_string()),
        insert_text: Some("[$1]".to_string()),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        ..Default::default()
    }
}

/// What the cursor is positioned at.
enum CursorContext {
    /// At the start of a line (after labels), expecting an instruction or
    /// directive mnemonic.
    Mnemonic,
    /// After a `%` character, expecting a register name.
    Register,
    /// In an argument position for a known instruction.
    Argument {
        kind: InstructionKind,
        arg_index: usize,
    },
    /// In an argument position but we don't know which instruction (parse
    /// error). Offer everything.
    UnknownArgument,
}

/// Text-based fallback for context detection when the parser produced an error
/// line (e.g. incomplete instruction like `add 1, `).
fn detect_context_from_text(line_text: &str, pos_in_line: usize) -> CursorContext {
    // Skip past any label definitions at the start of the line
    let mut p = 0;
    let bytes = line_text.as_bytes();
    loop {
        // Skip whitespace
        while p < bytes.len() && (bytes[p] == b' ' || bytes[p] == b'\t') {
            p += 1;
        }
        // Try to match identifier + ':'
        let ident_start = p;
        if p < bytes.len() && (bytes[p].is_ascii_alphabetic() || bytes[p] == b'_') {
            p += 1;
            while p < bytes.len() && is_identifier_char(bytes[p] as char) {
                p += 1;
            }
            while p < bytes.len() && (bytes[p] == b' ' || bytes[p] == b'\t') {
                p += 1;
            }
            if p < bytes.len() && bytes[p] == b':' {
                p += 1;
                continue; // Found a label, skip it and look for more
            }
            p = ident_start; // Not a label — this identifier is the mnemonic
        }
        break;
    }

    // If cursor is before or at the content start, offer mnemonics
    if pos_in_line <= p {
        return CursorContext::Mnemonic;
    }

    // Work with the text after labels
    let content_text = &line_text[p..pos_in_line];
    let trimmed = content_text.trim_start();

    // Try to find an instruction mnemonic at the start
    let mnemonic_end = trimmed
        .find(|c: char| !c.is_ascii_alphabetic() && c != '_')
        .unwrap_or(trimmed.len());
    let mnemonic_text = &trimmed[..mnemonic_end];

    if mnemonic_text.is_empty() {
        // Check if it starts with '.' (directive)
        if let Some(after_dot) = trimmed.strip_prefix('.') {
            // If there's content after the directive name, we're in its argument
            let directive_end = after_dot
                .find(|c: char| !c.is_ascii_alphabetic())
                .unwrap_or(after_dot.len());
            if directive_end < after_dot.len() {
                return CursorContext::UnknownArgument;
            }
        }
        return CursorContext::Mnemonic;
    }

    // Try to match the mnemonic to a known instruction
    let kind = match mnemonic_text.to_ascii_lowercase().as_str() {
        "add" => InstructionKind::Add,
        "and" => InstructionKind::And,
        "call" => InstructionKind::Call,
        "cmp" => InstructionKind::Cmp,
        "div" => InstructionKind::Div,
        "fas" => InstructionKind::Fas,
        "in" => InstructionKind::In,
        "jmp" => InstructionKind::Jmp,
        "jeq" => InstructionKind::Jeq,
        "jne" => InstructionKind::Jne,
        "jle" => InstructionKind::Jle,
        "jlt" => InstructionKind::Jlt,
        "jge" => InstructionKind::Jge,
        "jgt" => InstructionKind::Jgt,
        "ld" => InstructionKind::Ld,
        "mul" => InstructionKind::Mul,
        "neg" => InstructionKind::Neg,
        "nop" => InstructionKind::Nop,
        "not" => InstructionKind::Not,
        "or" => InstructionKind::Or,
        "out" => InstructionKind::Out,
        "pop" => InstructionKind::Pop,
        "push" => InstructionKind::Push,
        "reset" => InstructionKind::Reset,
        "rti" => InstructionKind::Rti,
        "rtn" => InstructionKind::Rtn,
        "shl" => InstructionKind::Shl,
        "shr" => InstructionKind::Shr,
        "st" => InstructionKind::St,
        "sub" => InstructionKind::Sub,
        "swap" => InstructionKind::Swap,
        "trap" => InstructionKind::Trap,
        "xor" => InstructionKind::Xor,
        _ => return CursorContext::Mnemonic,
    };

    // Cursor is still within the mnemonic
    let after_mnemonic = &trimmed[mnemonic_end..];
    if after_mnemonic.is_empty() {
        return CursorContext::Mnemonic;
    }

    // Count commas after the mnemonic to determine argument index
    let arg_index = after_mnemonic.chars().filter(|&c| c == ',').count();

    CursorContext::Argument { kind, arg_index }
}

/// Detect the cursor context using the original source text.
///
/// Fully text-based — doesn't use AST spans (which are in the preprocessed
/// source and may not match the original). Finds the current line, strips
/// comments, and delegates to `detect_context_from_text`.
fn detect_context(state: &DocumentState, byte_offset: usize) -> Option<CursorContext> {
    let source = state.source();
    if byte_offset > source.len() {
        return None;
    }

    // Find the current line in the original source
    let line_start = source[..byte_offset].rfind('\n').map_or(0, |i| i + 1);
    let line_end = source[byte_offset..]
        .find('\n')
        .map_or(source.len(), |i| byte_offset + i);
    let line_text = &source[line_start..line_end];
    let pos_in_line = byte_offset - line_start;

    // Strip comments (everything after '#' outside strings)
    let effective_len = line_text.find('#').unwrap_or(line_text.len());
    let line_text = &line_text[..effective_len];
    let pos_in_line = pos_in_line.min(effective_len);

    // Check if cursor is after a '%' (register completion)
    if pos_in_line > 0 {
        let before = &line_text[..pos_in_line];
        if let Some(pct_pos) = before.rfind('%') {
            let after_pct = &before[pct_pos + 1..];
            if after_pct.is_empty() || after_pct.chars().all(|c| c.is_ascii_alphabetic()) {
                return Some(CursorContext::Register);
            }
        }
    }

    Some(detect_context_from_text(line_text, pos_in_line))
}

/// Compute completion items for the given cursor position.
pub fn completions(state: &DocumentState, byte_offset: usize) -> Vec<CompletionItem> {
    let Some(context) = detect_context(state, byte_offset) else {
        return Vec::new();
    };

    match context {
        CursorContext::Mnemonic => {
            let mut items = instruction_completions();
            items.extend(directive_completions());
            items
        }
        CursorContext::Register => register_completions(true),
        CursorContext::Argument { kind, arg_index } => {
            if let Some(arg_type) = expected_arg_type(kind, arg_index) {
                completions_for_arg_type(arg_type, state)
            } else {
                // Unknown arg index (too many args) or zero-arg instruction
                Vec::new()
            }
        }
        CursorContext::UnknownArgument => {
            let mut items = register_completions(false);
            items.extend(label_completions(state));
            items.push(bracket_snippet());
            items
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mnemonic_at_start() {
        let state = DocumentState::new("    ".to_string());
        let items = completions(&state, 4);
        assert!(items.iter().any(|i| i.label == "add"));
        assert!(items.iter().any(|i| i.label == ".word"));
    }

    #[test]
    fn register_after_percent() {
        let state = DocumentState::new("    add %".to_string());
        let items = completions(&state, 9); // right after %
        assert!(items.iter().any(|i| i.label == "%a"));
        assert!(items.iter().any(|i| i.label == "%sp"));
        assert!(!items.iter().any(|i| i.label == "add"));
    }

    #[test]
    fn second_arg_must_be_register() {
        let state = DocumentState::new("    add 1, ".to_string());
        let items = completions(&state, 11); // after ", "
                                             // Second arg of ADD is Reg — should only offer registers
        assert!(items.iter().any(|i| i.label == "%a"));
        assert!(!items.iter().any(|i| i.label == "[...]"));
    }

    #[test]
    fn label_completion_in_arg() {
        let src = "foo:\n    jmp ";
        let state = DocumentState::new(src.to_string());
        let items = completions(&state, src.len()); // after "jmp "
                                                    // JMP arg 0 is ImmRegDirIndIdx — should offer labels
        assert!(items.iter().any(|i| i.label == "foo"));
        assert!(items.iter().any(|i| i.label == "%a"));
    }

    #[test]
    fn mnemonic_after_label() {
        let state = DocumentState::new("main: ".to_string());
        let items = completions(&state, 6); // after "main: "
        assert!(items.iter().any(|i| i.label == "add"));
    }
}
