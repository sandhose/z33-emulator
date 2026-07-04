use std::collections::BTreeMap;
use std::sync::LazyLock;

use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, InsertTextFormat,
};

use super::document::DocumentState;
use super::instructions::{directive_meta, meta, ArgType, DIRECTIVE_KINDS, INSTRUCTION_KINDS};
use super::text::{skip_labels, strip_inline_comment};
use crate::parser::value::InstructionKind;

/// Cached static completion lists — built once, cloned on use.
static INSTRUCTIONS: LazyLock<Vec<CompletionItem>> = LazyLock::new(build_instruction_completions);
static DIRECTIVES: LazyLock<Vec<CompletionItem>> = LazyLock::new(build_directive_completions);
static REGISTERS_FULL: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_register_completions(false));
static REGISTERS_PREFIX: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_register_completions(true));
static BRACKET: LazyLock<CompletionItem> = LazyLock::new(build_bracket_snippet);

/// Returns the expected argument type for a given instruction kind and
/// position.
///
/// Returns `None` for instructions that take no arguments, or for argument
/// indices beyond what the instruction accepts.
fn expected_arg_type(kind: InstructionKind, arg_index: usize) -> Option<ArgType> {
    meta(kind).args.get(arg_index).copied()
}

/// If `prefix_typed` is true, the `%` has already been typed by the user, so
/// `insert_text` omits it to avoid doubling.
fn build_register_completions(prefix_typed: bool) -> Vec<CompletionItem> {
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

fn build_instruction_completions() -> Vec<CompletionItem> {
    // No snippets — argument completions guide the user through the operands.
    INSTRUCTION_KINDS
        .iter()
        .map(|&kind| {
            let m = meta(kind);
            CompletionItem {
                label: format!("{kind}"),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some(m.summary.to_string()),
                label_details: if m.signature.is_empty() {
                    None
                } else {
                    Some(CompletionItemLabelDetails {
                        detail: Some(format!(" {}", m.signature)),
                        ..Default::default()
                    })
                },
                ..Default::default()
            }
        })
        .collect()
}

fn build_directive_completions() -> Vec<CompletionItem> {
    DIRECTIVE_KINDS
        .iter()
        .map(|&kind| {
            let short = format!("{kind}");
            CompletionItem {
                label: format!(".{kind}"),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("directive".to_string()),
                label_details: Some(CompletionItemLabelDetails {
                    description: Some(directive_meta(kind).summary.to_string()),
                    ..Default::default()
                }),
                // '.' is a trigger character and may already be typed, so
                // insert/filter without the dot to avoid doubling.
                insert_text: Some(short.clone()),
                filter_text: Some(short),
                ..Default::default()
            }
        })
        .collect()
}

fn label_completions(labels: &BTreeMap<String, crate::constants::Address>) -> Vec<CompletionItem> {
    labels
        .iter()
        .map(|(name, addr)| CompletionItem {
            label: name.clone(),
            kind: Some(CompletionItemKind::CONSTANT),
            detail: Some(format!("label (address {addr})")),
            ..Default::default()
        })
        .collect()
}

fn macro_completions(analysis: Option<&DocumentState>) -> Vec<CompletionItem> {
    let Some(state) = analysis else {
        return Vec::new();
    };
    let Some(annotations) = state.annotations() else {
        return Vec::new();
    };

    // Deduplicate: later #defines override earlier ones, show only the last value
    let mut seen = std::collections::HashSet::new();
    annotations
        .definitions
        .iter()
        .rev()
        .filter(|d| seen.insert(&d.key))
        .map(|d| {
            let detail = d
                .value
                .as_ref()
                .map_or_else(|| "macro".to_string(), |v| format!("macro ({v})"));
            CompletionItem {
                label: d.key.clone(),
                kind: Some(CompletionItemKind::CONSTANT),
                detail: Some(detail),
                ..Default::default()
            }
        })
        .collect()
}

fn completions_for_arg_type(
    arg_type: ArgType,
    labels: &BTreeMap<String, crate::constants::Address>,
    analysis: Option<&DocumentState>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    match arg_type {
        ArgType::Reg => {
            items.extend(REGISTERS_FULL.iter().cloned());
        }
        ArgType::ImmReg => {
            items.extend(REGISTERS_FULL.iter().cloned());
            items.extend(label_completions(labels));
            items.extend(macro_completions(analysis));
        }
        ArgType::ImmRegDirIndIdx => {
            items.extend(REGISTERS_FULL.iter().cloned());
            items.extend(label_completions(labels));
            items.extend(macro_completions(analysis));
            items.push(BRACKET.clone());
        }
        ArgType::DirIndIdx => {
            items.push(BRACKET.clone());
        }
        ArgType::RegDirIndIdx => {
            items.extend(REGISTERS_FULL.iter().cloned());
            items.push(BRACKET.clone());
        }
    }
    items
}

fn build_bracket_snippet() -> CompletionItem {
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
    let p = skip_labels(line_text);

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

    // Resolve the mnemonic through the derived `FromStr`. Unknown mnemonics (and
    // the `<error>` placeholder, which never matches an alphabetic word) fall
    // back to offering mnemonic completions.
    let Ok(kind) = mnemonic_text
        .to_ascii_lowercase()
        .parse::<InstructionKind>()
    else {
        return CursorContext::Mnemonic;
    };
    if kind == InstructionKind::Error {
        return CursorContext::Mnemonic;
    }

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
fn detect_context(source: &str, byte_offset: usize) -> Option<CursorContext> {
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

    // Strip the trailing `// ...` line comment. If the cursor sits inside the
    // comment, offer no completions at all.
    let line_text = strip_inline_comment(line_text);
    if pos_in_line > line_text.len() {
        return None;
    }

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
///
/// `analysis` may be `None` or slightly stale — label completions use it
/// but text-based context detection uses the current `source`.
pub fn completions(
    analysis: Option<&DocumentState>,
    source: &str,
    byte_offset: usize,
) -> Vec<CompletionItem> {
    let Some(context) = detect_context(source, byte_offset) else {
        return Vec::new();
    };

    let empty_labels = BTreeMap::new();
    let labels = analysis.map_or(&empty_labels, DocumentState::labels);

    match context {
        CursorContext::Mnemonic => {
            let mut items = INSTRUCTIONS.clone();
            items.extend(DIRECTIVES.iter().cloned());
            items
        }
        CursorContext::Register => REGISTERS_PREFIX.clone(),
        CursorContext::Argument { kind, arg_index } => {
            if let Some(arg_type) = expected_arg_type(kind, arg_index) {
                completions_for_arg_type(arg_type, labels, analysis)
            } else {
                Vec::new()
            }
        }
        CursorContext::UnknownArgument => {
            let mut items = REGISTERS_FULL.clone();
            items.extend(label_completions(labels));
            items.extend(macro_completions(analysis));
            items.push(BRACKET.clone());
            items
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mnemonic_at_start() {
        let src = "    ";
        let items = completions(None, src, 4);
        assert!(items.iter().any(|i| i.label == "add"));
        assert!(items.iter().any(|i| i.label == ".word"));
    }

    #[test]
    fn register_after_percent() {
        let src = "    add %";
        let items = completions(None, src, 9); // right after %
        assert!(items.iter().any(|i| i.label == "%a"));
        assert!(items.iter().any(|i| i.label == "%sp"));
        assert!(!items.iter().any(|i| i.label == "add"));
    }

    #[test]
    fn second_arg_must_be_register() {
        let src = "    add 1, ";
        let items = completions(None, src, 11); // after ", "
                                                // Second arg of ADD is Reg — should only offer registers
        assert!(items.iter().any(|i| i.label == "%a"));
        assert!(!items.iter().any(|i| i.label == "[...]"));
    }

    #[test]
    fn label_completion_in_arg() {
        let src = "foo:\n    jmp ";
        let analysis = DocumentState::new(src.to_string());
        let items = completions(Some(&analysis), src, src.len()); // after "jmp "
                                                                  // JMP arg 0 is ImmRegDirIndIdx — should offer labels
        assert!(items.iter().any(|i| i.label == "foo"));
        assert!(items.iter().any(|i| i.label == "%a"));
    }

    #[test]
    fn mnemonic_after_label() {
        let src = "main: ";
        let items = completions(None, src, 6); // after "main: "
        assert!(items.iter().any(|i| i.label == "add"));
    }

    #[test]
    fn comment_comma_not_arg_separator() {
        // `add %a` followed by a `//` comment that contains a comma. The comma
        // lives in the comment, so it must not be counted as an argument
        // separator — and the cursor sits inside the comment, so nothing at all
        // should be offered.
        let src = "    add %a // b, c";
        let pos = src.find(", c").unwrap() + 1; // just past the comment's comma
        let items = completions(None, src, pos);
        assert!(
            items.is_empty(),
            "the comma inside a // comment must not drive argument completion"
        );
    }

    #[test]
    fn no_completion_inside_comment() {
        // Text that looks like a register (`%a`) inside a comment must not
        // trigger register completion.
        let src = "    reset // clears %a";
        let pos = src.find("%a").unwrap() + 1; // inside the comment, after `%`
        let items = completions(None, src, pos);
        assert!(items.is_empty(), "no completions inside a // comment");
    }
}
