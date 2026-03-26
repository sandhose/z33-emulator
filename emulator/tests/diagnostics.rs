//! Snapshot tests for parser and preprocessor error diagnostics.
//!
//! These tests verify that error messages are clear, point at the right
//! location, and remain stable across changes. Uses `insta` for snapshot
//! testing with `codespan_reporting::term` for rendering.

use std::collections::HashMap;

use z33_emulator::diagnostic::{
    compilation_error_to_diagnostic, parse_diagnostic_to_codespan, preprocessor_error_to_diagnostics,
    render_to_string, resolve_to_original, simple_error, FileDatabase,
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

/// Preprocess and parse a program, rendering any parse diagnostics mapped
/// back to the original source file.
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

    let result = parse(&preprocess_result.source);
    let mut buf = String::new();

    for diag in &result.diagnostics {
        let codespan_diag = if let Some((file_id, range)) =
            resolve_to_original(&preprocess_result.source_map, diag.span.clone())
        {
            simple_error(&diag.message, file_id, range)
        } else {
            parse_diagnostic_to_codespan(diag, preprocess_result.preprocessed_file_id)
        };
        buf.push_str(&render_to_string(&codespan_diag, workspace.file_db()));
    }

    // If no parse errors, try compilation
    if result.diagnostics.is_empty() {
        if let Err(e) = compile(&result.program.inner, "main") {
            let diag =
                compilation_error_to_diagnostic(&e, preprocess_result.preprocessed_file_id);
            buf.push_str(&render_to_string(&diag, workspace.file_db()));
        }
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
    insta::assert_snapshot!(check_full_pipeline_errors(
        "#if true\nhello\n"
    ));
}

#[test]
fn preprocessor_bad_condition() {
    insta::assert_snapshot!(check_full_pipeline_errors(
        "#if (1 +\n#endif"
    ));
}

#[test]
fn preprocessor_missing_include() {
    insta::assert_snapshot!(check_full_pipeline_errors(
        r#"#include "missing.S""#
    ));
}

// ---------------------------------------------------------------------------
// Compilation error tests
// ---------------------------------------------------------------------------

#[test]
fn compilation_duplicate_label() {
    insta::assert_snapshot!(check_full_pipeline_errors(
        "main:\n    nop\nmain:\n    nop"
    ));
}

#[test]
fn compilation_wrong_argument_type() {
    // cmp takes (ImmRegDirIndIdx, Reg) — `1` is Imm, not Reg
    insta::assert_snapshot!(check_full_pipeline_errors(
        "main:\n    cmp %a, 1"
    ));
}

#[test]
fn compilation_push_direct_memory() {
    // push takes ImmReg — [1234] is Dir
    insta::assert_snapshot!(check_full_pipeline_errors(
        "main:\n    push [1234]"
    ));
}

#[test]
fn compilation_too_many_arguments() {
    insta::assert_snapshot!(check_full_pipeline_errors(
        "main:\n    add %a, %b, %a"
    ));
}

#[test]
fn compilation_unknown_entrypoint() {
    insta::assert_snapshot!(check_full_pipeline_errors(
        "start:\n    nop"
    ));
}
