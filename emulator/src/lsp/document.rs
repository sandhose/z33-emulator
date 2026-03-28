use std::collections::BTreeMap;

use camino::Utf8PathBuf;

use super::references::{self, OccurrenceKind, SymbolOccurrence};
use crate::compiler::layout::{self, Layout, MemoryLayoutError};
use crate::compiler::memory::{self, MemoryFillError};
use crate::constants::Address;
use crate::diagnostic::{self};
use crate::parser::assembly::ParseResult;
use crate::parser::line::Program;
use crate::parser::shared::ParseDiagnostic;
use crate::parser::{self};
use crate::preprocessor::{
    InMemoryFilesystem, PreprocessorError, ReferencingSourceMap, SourceAnnotations, Workspace,
};

/// The virtual filename used for single-file LSP documents.
const VIRTUAL_FILENAME: &str = "main.S";

/// Cached analysis state for a single document.
///
/// Runs the full pipeline (preprocess → parse → layout → fill) synchronously
/// on construction. The preprocessor handles comment stripping, macro
/// expansion, and conditional compilation.
pub struct DocumentState {
    /// The original source as provided by the editor.
    original_source: String,
    /// Maps byte offsets in the preprocessed source back to the original.
    /// `None` when preprocessing failed.
    source_map: Option<ReferencingSourceMap>,
    /// File ID of the original source in the file database (always 0 for
    /// single-file workspaces).
    original_file_id: usize,
    /// Preprocessor error, if any.
    preprocessor_error: Option<PreprocessorError>,
    /// Parse result (only available if preprocessing succeeded).
    parse_result: Option<ParseResult>,
    /// Preprocessor annotations (definitions, inclusions, conditionals).
    annotations: Option<SourceAnnotations>,
    layout: Layout,
    layout_errors: Vec<MemoryLayoutError>,
    fill_errors: Vec<MemoryFillError>,
    /// All symbol occurrences (definitions + references) with spans resolved
    /// to original source coordinates.
    occurrences: Vec<SymbolOccurrence>,
}

impl DocumentState {
    /// Analyze a document through the full pipeline.
    #[must_use]
    pub fn new(source: String) -> Self {
        let fs = InMemoryFilesystem::new([(Utf8PathBuf::from(VIRTUAL_FILENAME), source.clone())]);
        let mut workspace = Workspace::new(&fs, VIRTUAL_FILENAME);

        match workspace.preprocess() {
            Ok(result) => {
                let source_map: ReferencingSourceMap = result.source_map.into();
                let original_file_id = 0;

                let parse_result = parser::parse(&result.source);
                let (layout, layout_errors) =
                    layout::layout_memory(&parse_result.program.inner.lines);
                let (_, fill_errors) = memory::fill_memory(&layout);

                // Collect all symbol occurrences and resolve spans to
                // original source coordinates.
                let raw_occurrences = references::collect_occurrences(
                    Some(&parse_result.program.inner),
                    Some(&result.annotations),
                );

                let occurrences = raw_occurrences
                    .into_iter()
                    .filter_map(|mut occ| {
                        // Annotation spans (macro defs) are already in original
                        // coordinates. AST spans need resolve_span.
                        if occ.kind == OccurrenceKind::Definition
                            && result
                                .annotations
                                .definitions
                                .iter()
                                .any(|d| d.key == occ.name && d.span == occ.span)
                        {
                            // Already in original coordinates
                            Some(occ)
                        } else if let Some((file_id, range)) =
                            diagnostic::resolve_to_original(&source_map, occ.span.clone())
                        {
                            if file_id == original_file_id {
                                occ.span = range;
                                Some(occ)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .collect();

                Self {
                    original_source: source,
                    source_map: Some(source_map),
                    original_file_id,
                    preprocessor_error: None,
                    parse_result: Some(parse_result),
                    annotations: Some(result.annotations),
                    layout,
                    layout_errors,
                    fill_errors,
                    occurrences,
                }
            }
            Err(error) => {
                let original_file_id = 0;
                Self {
                    original_source: source,
                    source_map: None,
                    original_file_id,
                    preprocessor_error: Some(error),
                    parse_result: None,
                    annotations: None,
                    layout: Layout::default(),
                    layout_errors: Vec::new(),
                    fill_errors: Vec::new(),
                    occurrences: Vec::new(),
                }
            }
        }
    }

    /// The original source text (what the editor shows).
    #[must_use]
    pub fn source(&self) -> &str {
        &self.original_source
    }

    #[must_use]
    pub fn program(&self) -> Option<&Program> {
        self.parse_result.as_ref().map(|r| &r.program.inner)
    }

    #[must_use]
    pub fn labels(&self) -> &BTreeMap<String, Address> {
        &self.layout.labels
    }

    #[must_use]
    pub fn parse_diagnostics(&self) -> &[ParseDiagnostic] {
        self.parse_result.as_ref().map_or(&[], |r| &r.diagnostics)
    }

    #[must_use]
    pub fn layout_errors(&self) -> &[MemoryLayoutError] {
        &self.layout_errors
    }

    #[must_use]
    pub fn fill_errors(&self) -> &[MemoryFillError] {
        &self.fill_errors
    }

    #[must_use]
    pub fn preprocessor_error(&self) -> Option<&PreprocessorError> {
        self.preprocessor_error.as_ref()
    }

    #[must_use]
    pub fn annotations(&self) -> Option<&SourceAnnotations> {
        self.annotations.as_ref()
    }

    /// All symbol occurrences (definitions + references), with spans in the
    /// original source.
    #[must_use]
    pub fn occurrences(&self) -> &[SymbolOccurrence] {
        &self.occurrences
    }

    /// Find the symbol occurrence at the given byte offset in the original
    /// source, if any.
    #[must_use]
    pub fn occurrence_at(&self, offset: usize) -> Option<&SymbolOccurrence> {
        self.occurrences
            .iter()
            .find(|occ| occ.span.contains(&offset))
    }

    /// Find all occurrences of the symbol with the given name.
    pub fn occurrences_of<'a>(
        &'a self,
        name: &'a str,
    ) -> impl Iterator<Item = &'a SymbolOccurrence> {
        self.occurrences.iter().filter(move |occ| occ.name == name)
    }

    /// Map a byte range in the preprocessed source back to a byte range in
    /// the original source. Returns the span unchanged if no source map is
    /// available.
    #[must_use]
    pub fn resolve_span(&self, span: std::ops::Range<usize>) -> Option<std::ops::Range<usize>> {
        let Some(source_map) = self.source_map.as_ref() else {
            return Some(span);
        };
        let (file_id, range) = diagnostic::resolve_to_original(source_map, span)?;
        if file_id == self.original_file_id {
            Some(range)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::line::LineContent;

    /// Helper: given a source and a byte range, return the substring.
    fn slice<'a>(source: &'a str, range: &std::ops::Range<usize>) -> &'a str {
        &source[range.start..range.end]
    }

    // -----------------------------------------------------------------------
    // Basic span resolution
    // -----------------------------------------------------------------------

    #[test]
    fn label_spans_resolve_to_original() {
        let src = "main:\n    reset\n";
        let state = DocumentState::new(src.to_string());
        let program = state.program().unwrap();

        let line = &program.lines[0];
        assert_eq!(line.inner.symbols.len(), 1);

        let sym = &line.inner.symbols[0];
        assert_eq!(sym.inner, "main");

        let resolved = state.resolve_span(sym.location.clone()).unwrap();
        assert_eq!(slice(src, &resolved), "main");
    }

    #[test]
    fn instruction_kind_span_resolves() {
        let src = "    add %a, %b\n";
        let state = DocumentState::new(src.to_string());
        let program = state.program().unwrap();
        let content = program.lines[0].inner.content.as_ref().unwrap();

        match &content.inner {
            LineContent::Instruction { kind, .. } => {
                let resolved = state.resolve_span(kind.location.clone()).unwrap();
                assert_eq!(slice(src, &resolved), "add");
            }
            other => panic!("expected instruction, got {other:?}"),
        }
    }

    #[test]
    fn argument_spans_resolve() {
        let src = "    ld 42, %a\n";
        let state = DocumentState::new(src.to_string());
        let program = state.program().unwrap();
        let content = program.lines[0].inner.content.as_ref().unwrap();

        match &content.inner {
            LineContent::Instruction { arguments, .. } => {
                assert_eq!(arguments.len(), 2);
                let arg0 = state.resolve_span(arguments[0].location.clone()).unwrap();
                let arg1 = state.resolve_span(arguments[1].location.clone()).unwrap();
                assert_eq!(slice(src, &arg0), "42");
                assert_eq!(slice(src, &arg1), "%a");
            }
            other => panic!("expected instruction, got {other:?}"),
        }
    }

    // -----------------------------------------------------------------------
    // Comments
    // -----------------------------------------------------------------------

    #[test]
    fn inline_comment_preserved() {
        let src = "    reset // halt the cpu\n";
        let state = DocumentState::new(src.to_string());
        let program = state.program().unwrap();
        let line = &program.lines[0].inner;

        assert!(line.content.is_some());
        let comment = line.comment.as_ref().expect("comment should be present");
        assert_eq!(comment.inner, "halt the cpu");
    }

    #[test]
    fn comment_only_line() {
        let src = "// just a comment\n    reset\n";
        let state = DocumentState::new(src.to_string());
        let program = state.program().unwrap();

        // First line should have no content but a comment
        let line0 = &program.lines[0].inner;
        assert!(line0.content.is_none());
        assert!(line0.comment.is_some());
        assert_eq!(line0.comment.as_ref().unwrap().inner, "just a comment");

        // Second line should have content
        let line1 = &program.lines[1].inner;
        assert!(line1.content.is_some());
    }

    // -----------------------------------------------------------------------
    // Spans with comments (offsets shift)
    // -----------------------------------------------------------------------

    #[test]
    fn label_after_comment_line_resolves() {
        //                    0         1         2         3
        //                    0123456789012345678901234567890123456789
        let src = "    ld 1, %a // load\nloop:\n    jne loop\n";
        let state = DocumentState::new(src.to_string());
        let program = state.program().unwrap();

        // "loop" label is on line 1 (after the comment line)
        let line1 = &program.lines[1];
        assert_eq!(line1.inner.symbols.len(), 1);
        let sym = &line1.inner.symbols[0];
        assert_eq!(sym.inner, "loop");

        let resolved = state.resolve_span(sym.location.clone()).unwrap();
        assert_eq!(slice(src, &resolved), "loop");
        assert_eq!(resolved, 21..25);
    }

    #[test]
    fn goto_definition_target_after_comments() {
        let src = "main:\n    ld 1, %a // comment\nloop:\n    sub 1, %a\n    jne loop\n";
        let state = DocumentState::new(src.to_string());
        let program = state.program().unwrap();

        // Find the "loop" label definition
        for line in &program.lines {
            for sym in &line.inner.symbols {
                if sym.inner == "loop" {
                    let resolved = state.resolve_span(sym.location.clone()).unwrap();
                    assert_eq!(slice(src, &resolved), "loop");
                    // "loop" starts at byte 29 in the original source
                    assert_eq!(resolved.start, src.find("loop:").unwrap());
                    return;
                }
            }
        }
        panic!("label 'loop' not found");
    }

    // -----------------------------------------------------------------------
    // Preprocessor directives
    // -----------------------------------------------------------------------

    #[test]
    fn define_doesnt_break_spans() {
        let src = "#define N 5\nmain:\n    ld N, %a\n";
        let state = DocumentState::new(src.to_string());

        // Should have no errors
        assert!(state.preprocessor_error().is_none());
        assert!(state.parse_diagnostics().is_empty());

        // Label "main" should resolve correctly
        let program = state.program().unwrap();
        let line = program
            .lines
            .iter()
            .find(|l| l.inner.symbols.iter().any(|s| s.inner == "main"))
            .unwrap();
        let sym = &line.inner.symbols[0];
        let resolved = state.resolve_span(sym.location.clone()).unwrap();
        assert_eq!(slice(src, &resolved), "main");
    }

    #[test]
    fn define_annotation_recorded() {
        let src = "#define FOO 42\nmain:\n    reset\n";
        let state = DocumentState::new(src.to_string());
        let annotations = state.annotations().unwrap();

        assert_eq!(annotations.definitions.len(), 1);
        assert_eq!(annotations.definitions[0].key, "FOO");
        assert_eq!(annotations.definitions[0].value.as_deref(), Some("42"));
    }

    #[test]
    fn labels_available_with_errors() {
        let src = "main:\n    $$invalid$$\nloop:\n    reset\n";
        let state = DocumentState::new(src.to_string());

        // Should have parse errors but still have labels
        assert!(!state.parse_diagnostics().is_empty());
        assert!(state.labels().contains_key("main"));
        assert!(state.labels().contains_key("loop"));
    }

    // -----------------------------------------------------------------------
    // Diagnostic spans
    // -----------------------------------------------------------------------

    #[test]
    fn diagnostic_spans_point_to_original_source() {
        let src = "    xyz\n";
        let state = DocumentState::new(src.to_string());

        assert!(!state.parse_diagnostics().is_empty());
        let diag = &state.parse_diagnostics()[0];

        // The diagnostic span should point to "xyz" in the original source
        let resolved = state.resolve_span(diag.span.clone()).unwrap();
        assert_eq!(slice(src, &resolved), "xyz");
    }

    #[test]
    fn diagnostic_after_comment_resolves() {
        let src = "    reset // ok\n    xyz\n";
        let state = DocumentState::new(src.to_string());

        assert!(!state.parse_diagnostics().is_empty());
        let diag = &state.parse_diagnostics()[0];
        let resolved = state.resolve_span(diag.span.clone()).unwrap();
        assert_eq!(slice(src, &resolved), "xyz");
    }

    #[test]
    fn undefined_label_error_span() {
        let src = "    jmp nowhere\n";
        let state = DocumentState::new(src.to_string());

        // Should produce a fill error for undefined label
        assert!(!state.fill_errors().is_empty());
    }
}
