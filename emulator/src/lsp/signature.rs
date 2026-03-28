use tower_lsp::lsp_types::{
    ParameterInformation, ParameterLabel, SignatureHelp, SignatureInformation,
};

use crate::parser::shared::is_identifier_char;
use crate::parser::value::InstructionKind;

/// Compute signature help at the given byte offset in the original source.
pub fn signature_help(source: &str, byte_offset: usize) -> Option<SignatureHelp> {
    if byte_offset > source.len() {
        return None;
    }

    // Find the current line
    let line_start = source[..byte_offset].rfind('\n').map_or(0, |i| i + 1);
    let line_end = source[byte_offset..]
        .find('\n')
        .map_or(source.len(), |i| byte_offset + i);
    let line_text = &source[line_start..line_end];
    let pos_in_line = byte_offset - line_start;

    // Strip comments
    let effective_len = line_text.find('#').unwrap_or(line_text.len());
    let line_text = &line_text[..effective_len];
    let pos_in_line = pos_in_line.min(effective_len);

    // Skip labels
    let content_start = skip_labels(line_text);
    if pos_in_line <= content_start {
        return None;
    }

    let content = &line_text[content_start..pos_in_line];
    let trimmed = content.trim_start();

    // Check for directive (starts with '.')
    if let Some(after_dot) = trimmed.strip_prefix('.') {
        let name_end = after_dot
            .find(|c: char| !c.is_ascii_alphabetic())
            .unwrap_or(after_dot.len());
        let name = &after_dot[..name_end];
        let sig = directive_signature(name)?;
        let after_name = &after_dot[name_end..];
        if after_name.is_empty() || after_name.trim().is_empty() {
            if !after_name.is_empty() {
                return Some(make_signature_help(&sig, 0));
            }
            return None;
        }
        return Some(make_signature_help(&sig, 0));
    }

    // Find the instruction mnemonic
    let mnemonic_end = trimmed
        .find(|c: char| !c.is_ascii_alphabetic() && c != '_')
        .unwrap_or(trimmed.len());
    let mnemonic = &trimmed[..mnemonic_end];

    let kind = parse_instruction_kind(mnemonic)?;
    let sig = instruction_signature(kind)?;

    // Determine active parameter by counting commas
    let after_mnemonic = &trimmed[mnemonic_end..];
    if after_mnemonic.is_empty() || after_mnemonic.trim().is_empty() {
        if !after_mnemonic.is_empty() {
            return Some(make_signature_help(&sig, 0));
        }
        return None;
    }

    let active_param = after_mnemonic.chars().filter(|&c| c == ',').count();
    #[allow(clippy::cast_possible_truncation)]
    Some(make_signature_help(&sig, active_param as u32))
}

struct InstructionSignature {
    label: &'static str,
    doc: &'static str,
    params: &'static [(&'static str, &'static str)],
}

fn make_signature_help(sig: &InstructionSignature, active_param: u32) -> SignatureHelp {
    let parameters: Vec<ParameterInformation> = sig
        .params
        .iter()
        .map(|(label, doc)| ParameterInformation {
            label: ParameterLabel::Simple((*label).to_string()),
            documentation: Some(tower_lsp::lsp_types::Documentation::String(
                (*doc).to_string(),
            )),
        })
        .collect();

    let active = if (active_param as usize) < parameters.len() {
        Some(active_param)
    } else {
        None
    };

    SignatureHelp {
        signatures: vec![SignatureInformation {
            label: sig.label.to_string(),
            documentation: Some(tower_lsp::lsp_types::Documentation::String(
                sig.doc.to_string(),
            )),
            parameters: Some(parameters),
            active_parameter: active,
        }],
        active_signature: Some(0),
        active_parameter: active,
    }
}

fn skip_labels(line: &str) -> usize {
    let mut p = 0;
    let bytes = line.as_bytes();
    loop {
        while p < bytes.len() && (bytes[p] == b' ' || bytes[p] == b'\t') {
            p += 1;
        }
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
                continue;
            }
            p = ident_start;
        }
        break;
    }
    p
}

fn parse_instruction_kind(mnemonic: &str) -> Option<InstructionKind> {
    use InstructionKind as K;
    match mnemonic.to_ascii_lowercase().as_str() {
        "add" => Some(K::Add),
        "and" => Some(K::And),
        "call" => Some(K::Call),
        "cmp" => Some(K::Cmp),
        "div" => Some(K::Div),
        "fas" => Some(K::Fas),
        "in" => Some(K::In),
        "jmp" => Some(K::Jmp),
        "jeq" => Some(K::Jeq),
        "jne" => Some(K::Jne),
        "jle" => Some(K::Jle),
        "jlt" => Some(K::Jlt),
        "jge" => Some(K::Jge),
        "jgt" => Some(K::Jgt),
        "ld" => Some(K::Ld),
        "mul" => Some(K::Mul),
        "neg" => Some(K::Neg),
        "nop" => Some(K::Nop),
        "not" => Some(K::Not),
        "or" => Some(K::Or),
        "out" => Some(K::Out),
        "pop" => Some(K::Pop),
        "push" => Some(K::Push),
        "reset" => Some(K::Reset),
        "rti" => Some(K::Rti),
        "rtn" => Some(K::Rtn),
        "shl" => Some(K::Shl),
        "shr" => Some(K::Shr),
        "st" => Some(K::St),
        "sub" => Some(K::Sub),
        "swap" => Some(K::Swap),
        "trap" => Some(K::Trap),
        "xor" => Some(K::Xor),
        _ => None,
    }
}

fn directive_signature(name: &str) -> Option<InstructionSignature> {
    match name.to_ascii_lowercase().as_str() {
        "word" => Some(InstructionSignature {
            label: ".word expr",
            doc: "Store a word (64-bit integer) value at the current address",
            params: &[("expr", "Expression evaluating to a word value")],
        }),
        "space" => Some(InstructionSignature {
            label: ".space n",
            doc: "Reserve n memory cells (initialized to empty)",
            params: &[("n", "Number of cells to reserve")],
        }),
        "string" => Some(InstructionSignature {
            label: ".string \"text\"",
            doc: "Store a null-terminated string (each character = one cell)",
            params: &[("\"text\"", "String literal with escape sequences")],
        }),
        "addr" => Some(InstructionSignature {
            label: ".addr n",
            doc: "Set the current assembly address to n",
            params: &[("n", "Target address for subsequent instructions")],
        }),
        _ => None,
    }
}

#[allow(clippy::too_many_lines)]
fn instruction_signature(kind: InstructionKind) -> Option<InstructionSignature> {
    use InstructionKind as K;
    match kind {
        K::Add => Some(InstructionSignature {
            label: "add src, %dst",
            doc: "%dst ← %dst + src",
            params: &[
                ("src", "Immediate, register, or memory address"),
                ("%dst", "Destination register (%a or %b)"),
            ],
        }),
        K::And => Some(InstructionSignature {
            label: "and src, %dst",
            doc: "%dst ← %dst & src",
            params: &[
                ("src", "Immediate, register, or memory address"),
                ("%dst", "Destination register (%a or %b)"),
            ],
        }),
        K::Call => Some(InstructionSignature {
            label: "call addr",
            doc: "push %pc; %pc ← addr",
            params: &[("addr", "Target address (immediate, register, or memory)")],
        }),
        K::Cmp => Some(InstructionSignature {
            label: "cmp src, %dst",
            doc: "%dst - src (result discarded, sets flags)",
            params: &[
                ("src", "Value to compare against"),
                ("%dst", "Register to compare (%a or %b)"),
            ],
        }),
        K::Div => Some(InstructionSignature {
            label: "div src, %dst",
            doc: "%dst ← %dst / src",
            params: &[
                ("src", "Divisor (immediate, register, or memory)"),
                ("%dst", "Destination register (%a or %b)"),
            ],
        }),
        K::Fas => Some(InstructionSignature {
            label: "fas [addr], %dst",
            doc: "%dst ← mem[addr]; mem[addr] ← 1",
            params: &[
                ("[addr]", "Memory address (direct, indirect, or indexed)"),
                ("%dst", "Destination register (%a or %b)"),
            ],
        }),
        K::In => Some(InstructionSignature {
            label: "in [port], %dst",
            doc: "%dst ← IO[port] (privileged)",
            params: &[
                ("[port]", "I/O port address (direct, indirect, or indexed)"),
                ("%dst", "Destination register (%a or %b)"),
            ],
        }),
        K::Jmp => Some(InstructionSignature {
            label: "jmp addr",
            doc: "%pc ← addr",
            params: &[("addr", "Target address")],
        }),
        K::Jeq => Some(InstructionSignature {
            label: "jeq addr",
            doc: "if Z then %pc ← addr",
            params: &[("addr", "Target address (jumped to if equal)")],
        }),
        K::Jne => Some(InstructionSignature {
            label: "jne addr",
            doc: "if ¬Z then %pc ← addr",
            params: &[("addr", "Target address (jumped to if not equal)")],
        }),
        K::Jle => Some(InstructionSignature {
            label: "jle addr",
            doc: "if Z∨N then %pc ← addr",
            params: &[("addr", "Target address (jumped to if ≤)")],
        }),
        K::Jlt => Some(InstructionSignature {
            label: "jlt addr",
            doc: "if N then %pc ← addr",
            params: &[("addr", "Target address (jumped to if <)")],
        }),
        K::Jge => Some(InstructionSignature {
            label: "jge addr",
            doc: "if ¬N then %pc ← addr",
            params: &[("addr", "Target address (jumped to if ≥)")],
        }),
        K::Jgt => Some(InstructionSignature {
            label: "jgt addr",
            doc: "if ¬Z∧¬N then %pc ← addr",
            params: &[("addr", "Target address (jumped to if >)")],
        }),
        K::Ld => Some(InstructionSignature {
            label: "ld src, %dst",
            doc: "%dst ← src",
            params: &[
                ("src", "Value to load (immediate, register, or memory)"),
                ("%dst", "Destination register (%a or %b)"),
            ],
        }),
        K::Mul => Some(InstructionSignature {
            label: "mul src, %dst",
            doc: "%dst ← %dst × src",
            params: &[
                ("src", "Multiplier (immediate, register, or memory)"),
                ("%dst", "Destination register (%a or %b)"),
            ],
        }),
        K::Neg => Some(InstructionSignature {
            label: "neg %reg",
            doc: "%reg ← −%reg",
            params: &[("%reg", "Register to negate (%a or %b)")],
        }),
        K::Not => Some(InstructionSignature {
            label: "not %reg",
            doc: "%reg ← ~%reg",
            params: &[("%reg", "Register to invert (%a or %b)")],
        }),
        K::Or => Some(InstructionSignature {
            label: "or src, %dst",
            doc: "%dst ← %dst | src",
            params: &[
                ("src", "Immediate, register, or memory address"),
                ("%dst", "Destination register (%a or %b)"),
            ],
        }),
        K::Out => Some(InstructionSignature {
            label: "out src, [port]",
            doc: "IO[port] ← src (privileged)",
            params: &[
                ("src", "Value to output (immediate or register)"),
                ("[port]", "I/O port address (direct, indirect, or indexed)"),
            ],
        }),
        K::Pop => Some(InstructionSignature {
            label: "pop %reg",
            doc: "%reg ← mem[%sp]; %sp ← %sp + 1",
            params: &[("%reg", "Destination register")],
        }),
        K::Push => Some(InstructionSignature {
            label: "push src",
            doc: "%sp ← %sp − 1; mem[%sp] ← src",
            params: &[("src", "Value to push (immediate or register)")],
        }),
        K::Shl => Some(InstructionSignature {
            label: "shl src, %dst",
            doc: "%dst ← %dst << src",
            params: &[
                ("src", "Shift amount (immediate, register, or memory)"),
                ("%dst", "Destination register (%a or %b)"),
            ],
        }),
        K::Shr => Some(InstructionSignature {
            label: "shr src, %dst",
            doc: "%dst ← %dst >> src",
            params: &[
                ("src", "Shift amount (immediate, register, or memory)"),
                ("%dst", "Destination register (%a or %b)"),
            ],
        }),
        K::St => Some(InstructionSignature {
            label: "st %src, [addr]",
            doc: "mem[addr] ← %src",
            params: &[
                ("%src", "Source register"),
                ("[addr]", "Memory address (direct, indirect, or indexed)"),
            ],
        }),
        K::Sub => Some(InstructionSignature {
            label: "sub src, %dst",
            doc: "%dst ← %dst − src",
            params: &[
                ("src", "Value to subtract (immediate, register, or memory)"),
                ("%dst", "Destination register (%a or %b)"),
            ],
        }),
        K::Swap => Some(InstructionSignature {
            label: "swap src, %dst",
            doc: "src ↔ %dst",
            params: &[
                ("src", "Register or memory address to swap"),
                ("%dst", "Destination register (%a or %b)"),
            ],
        }),
        K::Xor => Some(InstructionSignature {
            label: "xor src, %dst",
            doc: "%dst ← %dst ^ src",
            params: &[
                ("src", "Immediate, register, or memory address"),
                ("%dst", "Destination register (%a or %b)"),
            ],
        }),

        // 0-arg instructions: no signature help
        K::Nop | K::Reset | K::Rti | K::Rtn | K::Trap | K::Error => None,
    }
}
