use tower_lsp::lsp_types;

use super::document::DocumentState;
use super::position;
use crate::compiler::layout::MemoryLayoutError;
use crate::compiler::memory::MemoryFillError;
use crate::parser::shared::{DiagnosticSeverity, ParseDiagnostic};

fn convert_severity(severity: DiagnosticSeverity) -> lsp_types::DiagnosticSeverity {
    match severity {
        DiagnosticSeverity::Error => lsp_types::DiagnosticSeverity::ERROR,
        DiagnosticSeverity::Warning => lsp_types::DiagnosticSeverity::WARNING,
    }
}

fn make_diagnostic(
    source: &str,
    span: std::ops::Range<usize>,
    message: String,
    severity: lsp_types::DiagnosticSeverity,
) -> Option<lsp_types::Diagnostic> {
    let range = position::range(source, span)?;
    Some(lsp_types::Diagnostic {
        range,
        severity: Some(severity),
        source: Some("z33".to_string()),
        message,
        ..Default::default()
    })
}

fn convert_parse_diagnostic(source: &str, diag: &ParseDiagnostic) -> Option<lsp_types::Diagnostic> {
    make_diagnostic(
        source,
        diag.span.clone(),
        diag.message.clone(),
        convert_severity(diag.severity),
    )
}

fn convert_layout_error(source: &str, error: &MemoryLayoutError) -> Option<lsp_types::Diagnostic> {
    let severity = lsp_types::DiagnosticSeverity::ERROR;
    match error {
        MemoryLayoutError::DuplicateLabel { label, location } => make_diagnostic(
            source,
            location.clone(),
            format!("duplicate label '{label}'"),
            severity,
        ),
        MemoryLayoutError::InvalidDirectiveArgument { kind, location } => make_diagnostic(
            source,
            location.clone(),
            format!("invalid argument for directive '.{kind}'"),
            severity,
        ),
        MemoryLayoutError::DirectiveArgumentEvaluation { kind, source: err } => {
            // No location available — skip
            let _ = (kind, err);
            None
        }
        MemoryLayoutError::MemoryOverlap {
            address,
            new_location,
            ..
        } => make_diagnostic(
            source,
            new_location.clone(),
            format!("memory overlap at address {address}"),
            severity,
        ),
    }
}

fn convert_fill_error(source: &str, error: &MemoryFillError) -> Option<lsp_types::Diagnostic> {
    let severity = lsp_types::DiagnosticSeverity::ERROR;
    match error {
        MemoryFillError::Evaluation {
            location,
            source: err,
        } => make_diagnostic(source, location.clone(), err.to_string(), severity),
        MemoryFillError::Compute {
            location,
            source: err,
        } => make_diagnostic(source, location.clone(), err.to_string(), severity),
        MemoryFillError::InstructionCompilation {
            instruction_span,
            source: err,
            ..
        } => make_diagnostic(source, instruction_span.clone(), err.to_string(), severity),
    }
}

/// Produce LSP diagnostics from the full analysis state.
pub fn diagnostics(state: &DocumentState) -> Vec<lsp_types::Diagnostic> {
    let source = state.source();
    let mut result = Vec::new();

    for diag in state.parse_diagnostics() {
        if let Some(d) = convert_parse_diagnostic(source, diag) {
            result.push(d);
        }
    }

    for error in state.layout_errors() {
        if let Some(d) = convert_layout_error(source, error) {
            result.push(d);
        }
    }

    for error in state.fill_errors() {
        if let Some(d) = convert_fill_error(source, error) {
            result.push(d);
        }
    }

    result
}
