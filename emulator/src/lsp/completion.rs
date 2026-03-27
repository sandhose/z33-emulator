use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, InsertTextFormat,
};

use super::document::DocumentState;
use crate::parser::line::LineContent;
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

fn register_completions() -> Vec<CompletionItem> {
    ["%a", "%b", "%pc", "%sp", "%sr"]
        .into_iter()
        .map(|name| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::VARIABLE),
            detail: Some("register".to_string()),
            ..Default::default()
        })
        .collect()
}

fn instruction_completions() -> Vec<CompletionItem> {
    let instructions = [
        ("add", "Add a value to a register"),
        ("and", "Bitwise AND with a register"),
        ("call", "Push PC and jump to address"),
        ("cmp", "Compare a value with a register"),
        ("div", "Divide a register by a value"),
        ("fas", "Fetch-and-set (atomic)"),
        ("in", "Read from I/O controller"),
        ("jmp", "Unconditional jump"),
        ("jeq", "Jump if equal (zero flag set)"),
        ("jne", "Jump if not equal"),
        ("jle", "Jump if less or equal"),
        ("jlt", "Jump if strictly less"),
        ("jge", "Jump if greater or equal"),
        ("jgt", "Jump if strictly greater"),
        ("ld", "Load a value into a register"),
        ("mul", "Multiply a register by a value"),
        ("neg", "Negate a register"),
        ("nop", "No operation"),
        ("not", "Bitwise NOT of a register"),
        ("or", "Bitwise OR with a register"),
        ("out", "Write to I/O controller"),
        ("pop", "Pop value from stack"),
        ("push", "Push value onto stack"),
        ("reset", "Reset the computer"),
        ("rti", "Return from interrupt"),
        ("rtn", "Return from call"),
        ("shl", "Shift left"),
        ("shr", "Shift right"),
        ("st", "Store register to memory"),
        ("sub", "Subtract a value from a register"),
        ("swap", "Swap a value and a register"),
        ("trap", "Trigger trap exception"),
        ("xor", "Bitwise XOR with a register"),
    ];

    instructions
        .into_iter()
        .map(|(name, detail)| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(detail.to_string()),
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
            items.extend(register_completions());
        }
        ArgType::ImmReg => {
            items.extend(register_completions());
            items.extend(label_completions(state));
        }
        ArgType::ImmRegDirIndIdx => {
            items.extend(register_completions());
            items.extend(label_completions(state));
            items.push(bracket_snippet());
        }
        ArgType::DirIndIdx => {
            items.push(bracket_snippet());
        }
        ArgType::RegDirIndIdx => {
            items.extend(register_completions());
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
    let before_cursor = &line_text[..pos_in_line];
    let trimmed = before_cursor.trim_start();

    // Try to find an instruction mnemonic at the start
    let mnemonic_end = trimmed
        .find(|c: char| !c.is_ascii_alphabetic() && c != '_')
        .unwrap_or(trimmed.len());
    let mnemonic_text = &trimmed[..mnemonic_end];

    if mnemonic_text.is_empty() {
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

fn detect_context(state: &DocumentState, byte_offset: usize) -> Option<CursorContext> {
    let source = state.source();
    let program = state.program();

    // Find which line the cursor is on
    let line = program
        .lines
        .iter()
        .find(|l| byte_offset >= l.location.start && byte_offset <= l.location.end)?;

    let line_start = line.location.start;
    let line_text = &source[line_start..line.location.end.min(source.len())];
    let pos_in_line = byte_offset - line_start;

    // Check if cursor is right after a '%'
    if pos_in_line > 0 && line_text.as_bytes().get(pos_in_line - 1) == Some(&b'%') {
        return Some(CursorContext::Register);
    }

    // Also check if we're in the middle of typing a register name (e.g. "%p|")
    {
        let before = &line_text[..pos_in_line];
        // Walk back to find if there's a '%' with only alpha chars between
        if let Some(pct_pos) = before.rfind('%') {
            let after_pct = &before[pct_pos + 1..];
            if after_pct.chars().all(|c| c.is_ascii_alphabetic()) {
                return Some(CursorContext::Register);
            }
        }
    }

    // Determine where content starts in the line (after labels)
    let content_start = line.inner.content.as_ref().map_or_else(
        || {
            // No content parsed — figure out where content would start
            // Skip past label definitions
            let mut p = 0;
            let bytes = line_text.as_bytes();
            loop {
                // Skip whitespace
                while p < bytes.len() && (bytes[p] == b' ' || bytes[p] == b'\t') {
                    p += 1;
                }
                // Try to match identifier + ':'
                if p < bytes.len() && (bytes[p].is_ascii_alphabetic() || bytes[p] == b'_') {
                    p += 1;
                    while p < bytes.len() && is_identifier_char(bytes[p] as char) {
                        p += 1;
                    }
                    // Skip whitespace between identifier and ':'
                    let after_ident = p;
                    while p < bytes.len() && (bytes[p] == b' ' || bytes[p] == b'\t') {
                        p += 1;
                    }
                    if p < bytes.len() && bytes[p] == b':' {
                        p += 1;
                        continue;
                    }
                    p = after_ident;
                }
                // Skip remaining whitespace
                while p < bytes.len() && (bytes[p] == b' ' || bytes[p] == b'\t') {
                    p += 1;
                }
                break;
            }
            p
        },
        |content| content.location.start,
    );

    // If cursor is at or before the content start → mnemonic position
    if pos_in_line <= content_start {
        return Some(CursorContext::Mnemonic);
    }

    // If we have parsed content, determine if we're in the mnemonic or args
    if let Some(content) = &line.inner.content {
        match &content.inner {
            LineContent::Instruction { kind, arguments } => {
                let content_abs_start = line_start + content.location.start;
                let kind_end_abs = content_abs_start + kind.location.end;

                // If cursor is within the instruction mnemonic
                if byte_offset <= kind_end_abs {
                    return Some(CursorContext::Mnemonic);
                }

                if kind.inner == InstructionKind::Error {
                    return Some(CursorContext::Mnemonic);
                }

                // We're in the argument area. Determine which argument index
                // by counting commas in the text between mnemonic end and
                // cursor.
                let after_kind = kind_end_abs;
                let text_between = &source[after_kind..byte_offset.min(source.len())];
                let comma_count = text_between.chars().filter(|&c| c == ',').count();

                // Also check: if we have parsed arguments, use their count
                let arg_index = if arguments.is_empty() {
                    comma_count
                } else {
                    comma_count.min(arguments.len())
                };

                return Some(CursorContext::Argument {
                    kind: kind.inner,
                    arg_index,
                });
            }
            LineContent::Directive { .. } => {
                // In a directive argument — offer labels
                return Some(CursorContext::UnknownArgument);
            }
            LineContent::Error => {
                // Parser failed — fall through to text-based analysis
                return Some(detect_context_from_text(line_text, pos_in_line));
            }
        }
    }

    // Cursor is past labels in an empty line → mnemonic
    Some(CursorContext::Mnemonic)
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
        CursorContext::Register => register_completions(),
        CursorContext::Argument { kind, arg_index } => {
            if let Some(arg_type) = expected_arg_type(kind, arg_index) {
                completions_for_arg_type(arg_type, state)
            } else {
                // Unknown arg index (too many args) or zero-arg instruction
                Vec::new()
            }
        }
        CursorContext::UnknownArgument => {
            let mut items = register_completions();
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
