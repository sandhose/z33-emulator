use tower_lsp::lsp_types;

use super::document::DocumentState;
use super::position;
use crate::compiler::layout::MemoryLayoutError;
use crate::compiler::memory::{InstructionCompilationError, MemoryFillError};
use crate::diagnostic::preprocessor_error_to_diagnostics;
use crate::parser::shared::{DiagnosticSeverity, ParseDiagnostic};

fn convert_severity(severity: DiagnosticSeverity) -> lsp_types::DiagnosticSeverity {
    match severity {
        DiagnosticSeverity::Error => lsp_types::DiagnosticSeverity::ERROR,
        DiagnosticSeverity::Warning => lsp_types::DiagnosticSeverity::WARNING,
    }
}

/// Create an LSP diagnostic from a span in the **original** source.
fn make_diagnostic(
    original_source: &str,
    span: std::ops::Range<usize>,
    message: String,
    severity: lsp_types::DiagnosticSeverity,
) -> Option<lsp_types::Diagnostic> {
    let range = position::range(original_source, span)?;
    Some(lsp_types::Diagnostic {
        range,
        severity: Some(severity),
        source: Some("z33".to_string()),
        message,
        ..Default::default()
    })
}

/// Create an LSP diagnostic from a span in the **preprocessed** source,
/// resolving it back to the original via the source map.
fn make_resolved_diagnostic(
    state: &DocumentState,
    span: std::ops::Range<usize>,
    message: String,
    severity: lsp_types::DiagnosticSeverity,
) -> Option<lsp_types::Diagnostic> {
    let original_span = state.resolve_span(span)?;
    make_diagnostic(state.source(), original_span, message, severity)
}

fn convert_parse_diagnostic(
    state: &DocumentState,
    diag: &ParseDiagnostic,
) -> Option<lsp_types::Diagnostic> {
    make_resolved_diagnostic(
        state,
        diag.span.clone(),
        diag.message.clone(),
        convert_severity(diag.severity),
    )
}

fn convert_layout_error(
    state: &DocumentState,
    error: &MemoryLayoutError,
) -> Option<lsp_types::Diagnostic> {
    let severity = lsp_types::DiagnosticSeverity::ERROR;
    match error {
        MemoryLayoutError::DuplicateLabel { label, location } => make_resolved_diagnostic(
            state,
            location.clone(),
            format!("duplicate label '{label}'"),
            severity,
        ),
        MemoryLayoutError::InvalidDirectiveArgument { kind, location } => make_resolved_diagnostic(
            state,
            location.clone(),
            format!("invalid argument for directive '.{kind}'"),
            severity,
        ),
        MemoryLayoutError::DirectiveArgumentEvaluation { kind, source: err } => {
            let _ = (kind, err);
            None
        }
        MemoryLayoutError::MemoryOverlap {
            address,
            new_location,
            ..
        } => make_resolved_diagnostic(
            state,
            new_location.clone(),
            format!("memory overlap at address {address}"),
            severity,
        ),
    }
}

fn convert_fill_error(
    state: &DocumentState,
    error: &MemoryFillError,
    out: &mut Vec<lsp_types::Diagnostic>,
) {
    let severity = lsp_types::DiagnosticSeverity::ERROR;
    match error {
        MemoryFillError::Evaluation {
            location,
            source: err,
        } => {
            if let Some(d) =
                make_resolved_diagnostic(state, location.clone(), err.to_string(), severity)
            {
                out.push(d);
            }
        }
        MemoryFillError::Compute {
            location,
            source: err,
        } => {
            if let Some(d) =
                make_resolved_diagnostic(state, location.clone(), err.to_string(), severity)
            {
                out.push(d);
            }
        }
        MemoryFillError::InstructionCompilation {
            instruction_span,
            argument_spans,
            source: err,
        } => match err {
            InstructionCompilationError::InvalidArgumentCount {
                instruction,
                expected,
                got,
            } => {
                if let Some(d) = make_resolved_diagnostic(
                    state,
                    instruction_span.clone(),
                    format!("'{instruction}' takes {expected} argument(s), got {got}"),
                    severity,
                ) {
                    out.push(d);
                }
            }
            InstructionCompilationError::InvalidArgumentType {
                instruction,
                argument_index,
                source: conversion_error,
            } => {
                // Primary diagnostic on the argument with the detailed type
                // error
                if let Some(arg_span) = argument_spans.get(*argument_index) {
                    if let Some(d) = make_resolved_diagnostic(
                        state,
                        arg_span.clone(),
                        conversion_error.to_string(),
                        severity,
                    ) {
                        out.push(d);
                    }
                }

                // Secondary diagnostic on the instruction name for context
                if let Some(d) = make_resolved_diagnostic(
                    state,
                    instruction_span.clone(),
                    format!("invalid argument for '{instruction}'"),
                    lsp_types::DiagnosticSeverity::HINT,
                ) {
                    out.push(d);
                }
            }
        },
    }
}

/// Produce LSP diagnostics from the full analysis state.
pub fn diagnostics(state: &DocumentState) -> Vec<lsp_types::Diagnostic> {
    let mut result = Vec::new();

    // Preprocessor errors (spans are already in the original source)
    if let Some(error) = state.preprocessor_error() {
        let codespan_diags = preprocessor_error_to_diagnostics(error);
        for diag in &codespan_diags {
            for label in &diag.labels {
                if let Some(d) = make_diagnostic(
                    state.source(),
                    label.range.clone(),
                    diag.message.clone(),
                    lsp_types::DiagnosticSeverity::ERROR,
                ) {
                    result.push(d);
                }
            }
            // If no labels, still emit the diagnostic with a zero-length range
            if diag.labels.is_empty() {
                if let Some(d) = make_diagnostic(
                    state.source(),
                    0..0,
                    diag.message.clone(),
                    lsp_types::DiagnosticSeverity::ERROR,
                ) {
                    result.push(d);
                }
            }
        }
        return result;
    }

    // Parse diagnostics (spans in preprocessed source)
    for diag in state.parse_diagnostics() {
        if let Some(d) = convert_parse_diagnostic(state, diag) {
            result.push(d);
        }
    }

    // Layout errors (spans in preprocessed source)
    for error in state.layout_errors() {
        if let Some(d) = convert_layout_error(state, error) {
            result.push(d);
        }
    }

    // Fill errors (spans in preprocessed source)
    for error in state.fill_errors() {
        convert_fill_error(state, error, &mut result);
    }

    // Inactive preprocessor regions
    if let Some(annotations) = state.annotations() {
        for block in &annotations.conditional_blocks {
            for branch in &block.branches {
                if !branch.active {
                    if let Some(d) = make_diagnostic(
                        state.source(),
                        branch.body_span.clone(),
                        "inactive preprocessor block".to_string(),
                        lsp_types::DiagnosticSeverity::HINT,
                    ) {
                        result.push(lsp_types::Diagnostic {
                            tags: Some(vec![lsp_types::DiagnosticTag::UNNECESSARY]),
                            ..d
                        });
                    }
                }
            }
            if let Some(fallback) = &block.fallback {
                if !fallback.active {
                    if let Some(d) = make_diagnostic(
                        state.source(),
                        fallback.body_span.clone(),
                        "inactive preprocessor block".to_string(),
                        lsp_types::DiagnosticSeverity::HINT,
                    ) {
                        result.push(lsp_types::Diagnostic {
                            tags: Some(vec![lsp_types::DiagnosticTag::UNNECESSARY]),
                            ..d
                        });
                    }
                }
            }
        }
    }

    result
}
