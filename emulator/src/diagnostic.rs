//! Centralized diagnostic infrastructure.
//!
//! Provides:
//! - [`FileDatabase`] — a file storage backed by `codespan_reporting::files::SimpleFiles`
//! - Conversion functions from error types to `codespan_reporting::diagnostic::Diagnostic`
//! - JSON serialization of diagnostics for the web IDE
//! - Terminal rendering helpers for the CLI

use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;

use crate::compiler::CompilationError;
use crate::parser::shared::{DiagnosticSeverity, ParseDiagnostic};
use crate::preprocessor::PreprocessorError;

/// A handle to a file in the [`FileDatabase`].
pub type FileId = usize;

/// Central file storage for diagnostic rendering.
///
/// Wraps `SimpleFiles` and provides convenience methods. All source files
/// (original `.S` files AND the virtual preprocessed output) are registered
/// here so that diagnostics can reference them by [`FileId`].
#[derive(Clone, Default)]
pub struct FileDatabase {
    inner: SimpleFiles<String, String>,
}

impl FileDatabase {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a file and return its [`FileId`].
    pub fn add(&mut self, name: impl Into<String>, source: impl Into<String>) -> FileId {
        self.inner.add(name.into(), source.into())
    }

    /// Get a file's name.
    ///
    /// # Panics
    ///
    /// Panics if `id` is not a valid file ID.
    #[must_use]
    pub fn name(&self, id: FileId) -> String {
        use codespan_reporting::files::Files;
        self.inner.name(id).expect("valid file ID")
    }

    /// Get a file's source content.
    ///
    /// # Panics
    ///
    /// Panics if `id` is not a valid file ID.
    #[must_use]
    pub fn source(&self, id: FileId) -> &str {
        use codespan_reporting::files::Files;
        self.inner.source(id).expect("valid file ID")
    }

    /// Get the inner `SimpleFiles` for use with `codespan_reporting::term`.
    #[must_use]
    pub fn inner(&self) -> &SimpleFiles<String, String> {
        &self.inner
    }
}

// ---------------------------------------------------------------------------
// Conversion: ParseDiagnostic → codespan Diagnostic
// ---------------------------------------------------------------------------

/// Convert a parser diagnostic to a codespan diagnostic.
#[must_use]
pub fn parse_diagnostic_to_codespan(
    diag: &ParseDiagnostic,
    file_id: FileId,
) -> Diagnostic<FileId> {
    let severity = match diag.severity {
        DiagnosticSeverity::Error => Severity::Error,
        DiagnosticSeverity::Warning => Severity::Warning,
    };

    let mut labels = vec![Label::primary(file_id, diag.span.clone())
        .with_message(&diag.message)];

    for (range, msg) in &diag.labels {
        labels.push(Label::secondary(file_id, range.clone()).with_message(msg));
    }

    Diagnostic::new(severity)
        .with_message(&diag.message)
        .with_labels(labels)
}

// ---------------------------------------------------------------------------
// Conversion: PreprocessorError → codespan Diagnostic
// ---------------------------------------------------------------------------

/// Convert a preprocessor error to one or more codespan diagnostics.
#[must_use]
pub fn preprocessor_error_to_diagnostics(
    error: &PreprocessorError,
) -> Vec<Diagnostic<FileId>> {
    match error {
        PreprocessorError::InInclude {
            file_id,
            span,
            inner,
        } => {
            let mut diagnostics = preprocessor_error_to_diagnostics(inner);
            // Add a secondary label at the #include site to the first diagnostic
            if let Some(first) = diagnostics.first_mut() {
                first.labels.push(
                    Label::secondary(*file_id, span.clone())
                        .with_message("included from here"),
                );
            }
            diagnostics
        }

        PreprocessorError::LoadFile { path, inner } => {
            vec![Diagnostic::error()
                .with_message(format!("could not load file '{path}': {inner}"))]
        }

        PreprocessorError::ParseFile { file_id, inner } => inner
            .iter()
            .map(|msg| {
                Diagnostic::error()
                    .with_message(msg)
                    .with_labels(vec![Label::primary(*file_id, 0..0)])
            })
            .collect(),

        PreprocessorError::UserError {
            file_id,
            span,
            message,
        } => {
            vec![Diagnostic::error()
                .with_message(format!("#error: {message}"))
                .with_labels(vec![
                    Label::primary(*file_id, span.clone())
                        .with_message(message),
                ])]
        }

        PreprocessorError::ConditionParse {
            file_id,
            span,
            inner,
        } => {
            vec![Diagnostic::error()
                .with_message(format!("invalid condition syntax: {inner}"))
                .with_labels(vec![Label::primary(*file_id, span.clone())])]
        }

        PreprocessorError::ConditionEvaluation {
            file_id,
            span,
            inner,
        } => {
            vec![Diagnostic::error()
                .with_message(format!("condition evaluation failed: {inner}"))
                .with_labels(vec![Label::primary(*file_id, span.clone())])]
        }
    }
}

// ---------------------------------------------------------------------------
// Conversion: CompilationError → codespan Diagnostic
// ---------------------------------------------------------------------------

/// Convert a compilation error to a codespan diagnostic.
///
/// `file_id` should be the preprocessed virtual file's ID.
#[must_use]
pub fn compilation_error_to_diagnostic(
    error: &CompilationError,
    file_id: FileId,
) -> Diagnostic<FileId> {
    use crate::compiler::CompilationError as CE;

    match error {
        CE::MemoryLayout(e) => {
            let mut diag = Diagnostic::error().with_message(e.to_string());
            if let Some(loc) = e.location() {
                diag.labels.push(Label::primary(file_id, loc.clone()));
            }
            diag
        }
        CE::MemoryFill(e) => memory_fill_error_to_diagnostic(e, file_id),
        CE::UnknownEntrypoint(name) => {
            Diagnostic::error().with_message(format!("unknown entrypoint: {name}"))
        }
        CE::HasParseErrors => {
            Diagnostic::error().with_message("program contains syntax errors")
        }
    }
}

fn memory_fill_error_to_diagnostic(
    error: &crate::compiler::memory::MemoryFillError,
    file_id: FileId,
) -> Diagnostic<FileId> {
    use crate::compiler::memory::InstructionCompilationError as ICE;
    use crate::compiler::memory::MemoryFillError as MFE;

    match error {
        MFE::Evaluation { location, source } => Diagnostic::error()
            .with_message(format!("could not evaluate expression: {source}"))
            .with_labels(vec![
                Label::primary(file_id, location.clone()).with_message(source.to_string()),
            ]),

        MFE::Compute { location, source } => Diagnostic::error()
            .with_message(format!("could not compute argument: {source}"))
            .with_labels(vec![
                Label::primary(file_id, location.clone()).with_message(source.to_string()),
            ]),

        MFE::InstructionCompilation {
            instruction_span,
            argument_spans,
            source,
        } => match source {
            ICE::InvalidArgumentCount {
                instruction,
                expected,
                got,
            } => Diagnostic::error()
                .with_message(format!(
                    "'{instruction}' takes {expected} argument(s), got {got}"
                ))
                .with_labels(vec![Label::primary(file_id, instruction_span.clone())
                    .with_message(format!("expected {expected} argument(s)"))]),

            ICE::InvalidArgumentType {
                instruction,
                argument_index,
                source: conversion_error,
            } => {
                let mut labels = Vec::new();

                // Primary label on the argument that failed
                if let Some(arg_span) = argument_spans.get(*argument_index) {
                    labels.push(
                        Label::primary(file_id, arg_span.clone())
                            .with_message(conversion_error.to_string()),
                    );
                }

                // Secondary label on the instruction name for context
                labels.push(
                    Label::secondary(file_id, instruction_span.clone())
                        .with_message(format!("in instruction '{instruction}'")),
                );

                Diagnostic::error()
                    .with_message(format!("invalid argument for '{instruction}'"))
                    .with_labels(labels)
            }
        },
    }
}

/// Create a simple error diagnostic with a primary label at the given range.
#[must_use]
pub fn simple_error(
    message: impl Into<String> + std::fmt::Display,
    file_id: FileId,
    range: Range<usize>,
) -> Diagnostic<FileId> {
    Diagnostic::error()
        .with_message(message)
        .with_labels(vec![Label::primary(file_id, range)])
}

// ---------------------------------------------------------------------------
// Source map resolution
// ---------------------------------------------------------------------------

/// Resolve a byte range in the preprocessed output to the original file
/// using the preprocessor source map.
#[must_use]
pub fn resolve_to_original(
    source_map: &crate::preprocessor::SourceMap,
    range: Range<usize>,
) -> Option<(FileId, Range<usize>)> {
    let span = source_map.find(range.start)?;
    let start = span.range.start + (range.start - source_map.chunk_key(range.start)?);
    let end = span.range.start + (range.end - source_map.chunk_key(range.start)?);
    Some((span.file_id, start..end))
}

// ---------------------------------------------------------------------------
// JSON serialization for web
// ---------------------------------------------------------------------------

/// Serialize a codespan diagnostic to JSON matching the web app's report
/// schema.
#[must_use]
pub fn diagnostic_to_json(
    diag: &Diagnostic<FileId>,
    db: &FileDatabase,
) -> serde_json::Value {
    let severity = match diag.severity {
        Severity::Bug | Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Note | Severity::Help => "advice",
    };

    // Use the first label's file as the filename, or empty string
    let filename = diag
        .labels
        .first()
        .map(|l| db.name(l.file_id))
        .unwrap_or_default();

    let labels: Vec<serde_json::Value> = diag
        .labels
        .iter()
        .map(|l| {
            serde_json::json!({
                "label": l.message,
                "span": {
                    "offset": l.range.start,
                    "length": l.range.end - l.range.start,
                },
            })
        })
        .collect();

    let notes: Vec<&str> = diag.notes.iter().map(String::as_str).collect();

    serde_json::json!({
        "message": diag.message,
        "severity": severity,
        "filename": filename,
        "labels": labels,
        "related": [],
        "causes": notes,
    })
}

/// Serialize multiple diagnostics to a JSON array string.
#[must_use]
pub fn diagnostics_to_json(
    diagnostics: &[Diagnostic<FileId>],
    db: &FileDatabase,
) -> String {
    let arr: Vec<serde_json::Value> = diagnostics
        .iter()
        .map(|d| diagnostic_to_json(d, db))
        .collect();
    serde_json::Value::Array(arr).to_string()
}

// ---------------------------------------------------------------------------
// Terminal rendering
// ---------------------------------------------------------------------------

/// Render a diagnostic to a string (for snapshot testing and simple display).
///
/// # Panics
///
/// Panics if the diagnostic references an invalid file ID.
#[must_use]
pub fn render_to_string(
    diag: &Diagnostic<FileId>,
    db: &FileDatabase,
) -> String {
    let config = term::Config::default();
    let mut buf = Vec::new();
    term::emit_to_io_write(
        &mut buf,
        &config,
        db.inner(),
        diag,
    )
    .expect("diagnostic rendering failed");
    String::from_utf8(buf).expect("diagnostic output is valid UTF-8")
}
