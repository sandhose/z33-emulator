//! Snapshot tests for parser and preprocessor error diagnostics.
//!
//! These tests verify that error messages are clear, point at the right
//! location, and remain stable across changes. Uses `insta` for snapshot
//! testing with `codespan_reporting::term` for rendering.

use z33_emulator::diagnostic::{
    FileDatabase, parse_diagnostic_to_codespan, preprocessor_error_to_diagnostics, render_to_string,
};
use z33_emulator::preprocessor::{InMemoryFilesystem, Workspace};
use z33_emulator::{compile, parse};

/// Parse a single-file program and render any diagnostics as a snapshot string.
fn check_parse_errors(input: &str) -> String {
    let mut db = FileDatabase::new();
    let fid = db.add("test.S", input);
    let result = parse(input);
    let diagnostics: Vec<_> = result
        .diagnostics
        .iter()
        .map(|d| parse_diagnostic_to_codespan(d, fid))
        .collect();
    let mut buf = String::new();
    for diag in &diagnostics {
        buf.push_str(&render_to_string(diag, &db));
    }
    buf
}

/// Run the full pipeline (preprocess → parse → compile) and render all
/// diagnostics as a snapshot string.
fn check_full_pipeline_errors(input: &str) -> String {
    let fs = InMemoryFilesystem::new([("/test.S".into(), input.into())]);
    let mut workspace = Workspace::new(&fs, "/test.S");
    let preprocess_result = match workspace.preprocess() {
        Ok(r) => r,
        Err(e) => {
            let diagnostics = preprocessor_error_to_diagnostics(&e);
            let mut buf = String::new();
            for diag in &diagnostics {
                buf.push_str(&render_to_string(diag, workspace.file_db()));
            }
            return buf;
        }
    };

    let parse_result = parse(&preprocess_result.source);

    // compile() merges parse + layout + fill diagnostics
    let compile_result = compile(
        &parse_result.program.inner,
        &parse_result.diagnostics,
        Some("main"),
        preprocess_result.preprocessed_file_id,
    );

    let mut buf = String::new();
    for diag in &compile_result.diagnostics {
        buf.push_str(&render_to_string(diag, workspace.file_db()));
    }
    buf
}

// ---------------------------------------------------------------------------
// Parser error tests
// ---------------------------------------------------------------------------

#[test]
fn unknown_instruction() {
    insta::assert_snapshot!(check_parse_errors("    xyz %a, %b"));
}

#[test]
fn unknown_instruction_with_args() {
    // "invalid" should be reported as unknown instruction, not as a label
    // missing ':'
    insta::assert_snapshot!(check_parse_errors("    invalid %a"));
}

#[test]
fn missing_register_name() {
    insta::assert_snapshot!(check_parse_errors("    ld %, %a"));
}

#[test]
fn invalid_number_literal() {
    insta::assert_snapshot!(check_parse_errors(".word 0xGG"));
}

#[test]
fn unclosed_bracket() {
    insta::assert_snapshot!(check_parse_errors("    ld [%sp+1, %a"));
}

#[test]
fn empty_directive_argument() {
    insta::assert_snapshot!(check_parse_errors(".word"));
}

#[test]
fn multiple_errors_recovery() {
    insta::assert_snapshot!(check_parse_errors(
        "main:\n    xyz\n    add %a, %b\n    !!!\n    reset"
    ));
}

// ---------------------------------------------------------------------------
// Preprocessor error tests
// ---------------------------------------------------------------------------

#[test]
fn preprocessor_error_directive() {
    insta::assert_snapshot!(check_full_pipeline_errors(r#"#error "stop here""#));
}

#[test]
fn preprocessor_missing_endif() {
    insta::assert_snapshot!(check_full_pipeline_errors("#if true\nhello\n"));
}

#[test]
fn preprocessor_bad_condition() {
    insta::assert_snapshot!(check_full_pipeline_errors("#if (1 +\n#endif"));
}

#[test]
fn preprocessor_missing_include() {
    insta::assert_snapshot!(check_full_pipeline_errors(r#"#include "missing.S""#));
}

// ---------------------------------------------------------------------------
// Compilation error tests
// ---------------------------------------------------------------------------

#[test]
fn compilation_duplicate_label() {
    insta::assert_snapshot!(check_full_pipeline_errors("main:\n    nop\nmain:\n    nop"));
}

#[test]
fn compilation_wrong_argument_type() {
    // cmp takes (ImmRegDirIndIdx, Reg) — `1` is Imm, not Reg
    insta::assert_snapshot!(check_full_pipeline_errors("main:\n    cmp %a, 1"));
}

#[test]
fn compilation_push_direct_memory() {
    // push takes ImmReg — [1234] is Dir
    insta::assert_snapshot!(check_full_pipeline_errors("main:\n    push [1234]"));
}

#[test]
fn compilation_too_many_arguments() {
    insta::assert_snapshot!(check_full_pipeline_errors("main:\n    add %a, %b, %a"));
}

#[test]
fn compilation_invalid_register() {
    // %c is not a valid register
    insta::assert_snapshot!(check_full_pipeline_errors("main:\n    jmp %c"));
}

#[test]
fn compilation_memory_overlap() {
    insta::assert_snapshot!(check_full_pipeline_errors(
        "main:\n    .addr 10\n    .string \"hello\"\n    .addr 14\n    .word 0"
    ));
}

#[test]
fn compilation_undefined_label() {
    insta::assert_snapshot!(check_full_pipeline_errors("main:\n    cmp foo, %a"));
}

#[test]
fn compilation_unknown_entrypoint() {
    insta::assert_snapshot!(check_full_pipeline_errors("start:\n    nop"));
}
#[test]
fn compilation_binary_not_out_of_range() {
    // `~` inverts all 128 bits of the evaluated value, so inverting a value
    // that already needs 64 bits leaves a result no word can hold.
    insta::assert_snapshot!(check_full_pipeline_errors(
        "main:\n    reset\nx: .word ~0xffffffffffffffff"
    ));
}

#[test]
fn compilation_addr_directive_out_of_bounds() {
    // .addr 9999 leaves only one valid cell (address 9999); the second `nop`
    // lands on address 10000, outside the 10000-cell memory.
    insta::assert_snapshot!(check_full_pipeline_errors(
        ".addr 9999\nmain:\n    nop\n    nop"
    ));
}

#[test]
fn compilation_space_directive_out_of_bounds() {
    // .space 20000 cannot fit starting at PROGRAM_START (1000) in a
    // 10000-cell memory.
    insta::assert_snapshot!(check_full_pipeline_errors(
        "main:\n    reset\nx: .space 20000"
    ));
}

#[test]
fn compilation_huge_space_directive() {
    // The bounds check runs before the loop; the unit test in
    // compiler::layout pins that no cell is inserted for an argument this
    // large.
    insta::assert_snapshot!(check_full_pipeline_errors(
        "main:\n    reset\nx: .space 50000000"
    ));
}

#[test]
fn compilation_space_directive_larger_than_an_address() {
    insta::assert_snapshot!(check_full_pipeline_errors(
        "main:\n    reset\nx: .space 3000000000"
    ));
}

#[test]
fn compilation_negative_space_directive() {
    // The argument is evaluated as an address, so a negative one cannot be
    // downcast.
    insta::assert_snapshot!(check_full_pipeline_errors("main:\n    reset\nx: .space -1"));
}

#[test]
fn compilation_addr_directive_at_the_last_address() {
    // The layout cursor sits on the highest address an expression can name;
    // the following instruction must not advance it past it.
    insta::assert_snapshot!(check_full_pipeline_errors(".addr 4294967295\nnop"));
}
