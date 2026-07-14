use super::document::DocumentState;
use super::position;
use crate::compiler::layout::MemoryLayoutError;
use crate::compiler::memory::{InstructionCompilationError, MemoryFillError};
use crate::diagnostic::{preprocessor_error_to_diagnostics, FileId};
use crate::parser::shared::{DiagnosticSeverity, ParseDiagnostic};

/// A diagnostic together with the file it belongs to.
pub(crate) type FileDiagnostic = (FileId, lsp_types::Diagnostic);

fn convert_severity(severity: DiagnosticSeverity) -> lsp_types::DiagnosticSeverity {
    match severity {
        DiagnosticSeverity::Error => lsp_types::DiagnosticSeverity::ERROR,
        DiagnosticSeverity::Warning => lsp_types::DiagnosticSeverity::WARNING,
    }
}

/// Create an LSP diagnostic from a span in a specific **original** file.
fn make_diagnostic(
    state: &DocumentState,
    file_id: FileId,
    span: std::ops::Range<usize>,
    message: String,
    severity: lsp_types::DiagnosticSeverity,
) -> Option<FileDiagnostic> {
    let source = state.file_source(file_id)?;
    let range = position::range(source, span)?;
    Some((
        file_id,
        lsp_types::Diagnostic {
            range,
            severity: Some(severity),
            source: Some("z33".to_string()),
            message,
            ..Default::default()
        },
    ))
}

/// Create an LSP diagnostic from a span in the **preprocessed** source,
/// resolving it back to the owning original file via the source map.
fn make_resolved_diagnostic(
    state: &DocumentState,
    span: std::ops::Range<usize>,
    message: String,
    severity: lsp_types::DiagnosticSeverity,
) -> Option<FileDiagnostic> {
    let (file_id, original_span) = state.resolve_span_file(span)?;
    make_diagnostic(state, file_id, original_span, message, severity)
}

fn convert_parse_diagnostic(
    state: &DocumentState,
    diag: &ParseDiagnostic,
) -> Option<FileDiagnostic> {
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
) -> Option<FileDiagnostic> {
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
    out: &mut Vec<FileDiagnostic>,
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

/// Produce LSP diagnostics from the full analysis state, each tagged with the
/// [`FileId`] of the file it belongs to.
///
/// Diagnostics originating inside `#include`d files are attributed to those
/// files, so the caller can publish them to the correct URI.
#[must_use]
pub fn diagnostics_by_file(state: &DocumentState) -> Vec<FileDiagnostic> {
    let mut result = Vec::new();

    // Preprocessor errors. The codespan labels already carry original spans and
    // the file id they belong to.
    if let Some(error) = state.preprocessor_error() {
        let codespan_diags = preprocessor_error_to_diagnostics(error);
        for diag in &codespan_diags {
            let mut had_label = false;
            for label in &diag.labels {
                had_label = true;
                if let Some(d) = make_diagnostic(
                    state,
                    label.file_id,
                    label.range.clone(),
                    diag.message.clone(),
                    lsp_types::DiagnosticSeverity::ERROR,
                ) {
                    result.push(d);
                }
            }
            // If no labels, still emit the diagnostic on the root file.
            if !had_label {
                if let Some(d) = make_diagnostic(
                    state,
                    state.root_file_id(),
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

    // Inactive preprocessor regions (annotation spans, per file)
    if let Some(annotations) = state.annotations() {
        for block in &annotations.conditional_blocks {
            for branch in &block.branches {
                if !branch.active {
                    push_inactive(state, block.file_id, branch.body_span.clone(), &mut result);
                }
            }
            if let Some(fallback) = &block.fallback {
                if !fallback.active {
                    push_inactive(
                        state,
                        block.file_id,
                        fallback.body_span.clone(),
                        &mut result,
                    );
                }
            }
        }
    }

    result
}

fn push_inactive(
    state: &DocumentState,
    file_id: FileId,
    span: std::ops::Range<usize>,
    out: &mut Vec<FileDiagnostic>,
) {
    if let Some((file_id, d)) = make_diagnostic(
        state,
        file_id,
        span,
        "inactive preprocessor block".to_string(),
        lsp_types::DiagnosticSeverity::HINT,
    ) {
        out.push((
            file_id,
            lsp_types::Diagnostic {
                tags: Some(vec![lsp_types::DiagnosticTag::UNNECESSARY]),
                ..d
            },
        ));
    }
}
