use std::collections::BTreeMap;
use std::ops::Range;

use camino::{Utf8Path, Utf8PathBuf};

use super::references::{self, SymbolOccurrence};
use crate::compiler::layout::{self, Layout, MemoryLayoutError, Placement};
use crate::compiler::memory::{self, MemoryFillError};
use crate::constants::Address;
use crate::diagnostic::{self, FileId};
use crate::parser::assembly::ParseResult;
use crate::parser::line::{LineContent, Program};
use crate::parser::shared::ParseDiagnostic;
use crate::parser::{self};
use crate::preprocessor::{
    Filesystem, InMemoryFilesystem, PreprocessorError, ReferencingSourceMap, SourceAnnotations,
    Workspace,
};

/// The virtual filename used for single-file LSP documents.
const VIRTUAL_FILENAME: &str = "main.S";

/// The [`FileId`] of the root document. The preprocessor always registers the
/// entrypoint first, so it is guaranteed to be `0`.
const ROOT_FILE_ID: FileId = 0;

/// What a resolved label points at in the compiled memory layout.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelKind {
    /// The label points at an instruction (executable code).
    Code,
    /// The label points at data (`.word`, `.string`, `.space`).
    Data,
}

/// Cached analysis state for a single document, treated as a preprocessing
/// root.
///
/// Runs the full pipeline (preprocess → parse → layout → fill) synchronously
/// on construction. The preprocessor handles comment stripping, macro
/// expansion, conditional compilation, and `#include` resolution against the
/// provided [`Filesystem`].
///
/// Because a document may `#include` others, analysis spans multiple files:
/// every [`SymbolOccurrence`], diagnostic, and span carries the [`FileId`] of
/// the file it belongs to. Use [`file_path`](Self::file_path) /
/// [`file_source`](Self::file_source) to map a [`FileId`] back to its path and
/// contents.
pub struct DocumentState {
    /// The original source of the root document (what the editor shows).
    original_source: String,
    /// Maps byte offsets in the preprocessed source back to the original files.
    /// `None` when preprocessing failed.
    source_map: Option<ReferencingSourceMap>,
    /// Relative path of every source file involved in this analysis.
    file_paths: BTreeMap<FileId, Utf8PathBuf>,
    /// Source text of every file involved in this analysis (keyed by id).
    file_sources: BTreeMap<FileId, String>,
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
    /// to original-source coordinates and tagged with their file id.
    occurrences: Vec<SymbolOccurrence>,
}

impl DocumentState {
    /// Analyze a single, self-contained document (no `#include` resolution).
    ///
    /// Convenience wrapper around [`analyze`](Self::analyze) used by tests and
    /// callers that don't have a workspace filesystem.
    #[must_use]
    pub fn new(source: String) -> Self {
        let fs = InMemoryFilesystem::new([(Utf8PathBuf::from(VIRTUAL_FILENAME), source)]);
        Self::analyze(&fs, Utf8Path::new(VIRTUAL_FILENAME))
    }

    /// Analyze a document through the full pipeline, using `fs` as the root of
    /// the workspace and `root_path` (relative to that root) as the
    /// entrypoint.
    ///
    /// `#include` directives are resolved against `fs`, so cross-file features
    /// work as long as the filesystem can produce the included files.
    #[must_use]
    pub fn analyze<FS: Filesystem>(fs: &FS, root_path: &Utf8Path) -> Self {
        let mut workspace = Workspace::new(fs, root_path);
        // The root file is only registered in the file database if it could be
        // read. When the filesystem fails to produce it (e.g. an unreadable or
        // missing root), `ROOT_FILE_ID` is absent and `source()` would panic —
        // fall back to empty source so the preprocessor-error path below still
        // produces diagnostics.
        let original_source = if workspace.file_ids().any(|(id, _)| id == ROOT_FILE_ID) {
            workspace.file_db().source(ROOT_FILE_ID).to_string()
        } else {
            String::new()
        };

        match workspace.preprocess() {
            Ok(result) => {
                let source_map: ReferencingSourceMap = result.source_map.into();

                // Snapshot the file registry (relative path + source) for every
                // file that took part in this analysis.
                let file_paths: BTreeMap<FileId, Utf8PathBuf> = workspace
                    .file_ids()
                    .map(|(id, path)| (id, path.to_owned()))
                    .collect();
                let file_sources: BTreeMap<FileId, String> = file_paths
                    .keys()
                    .map(|&id| (id, workspace.file_db().source(id).to_string()))
                    .collect();

                let parse_result = parser::parse(&result.source);
                let (layout, layout_errors) =
                    layout::layout_memory(&parse_result.program.inner.lines);
                let (_, fill_errors) = memory::fill_memory(&layout);

                // AST occurrences carry preprocessed spans: resolve each back
                // to (file_id, original range).
                let mut occurrences: Vec<SymbolOccurrence> =
                    references::collect_occurrences(Some(&parse_result.program.inner))
                        .into_iter()
                        .filter_map(|mut occ| {
                            let (file_id, range) =
                                diagnostic::resolve_to_original(&source_map, occ.span.clone())?;
                            occ.file_id = file_id;
                            occ.span = range;
                            Some(occ)
                        })
                        .collect();

                // Macro (#define) definitions come from annotations, already in
                // original coordinates with a file id.
                for def in &result.annotations.definitions {
                    occurrences.push(SymbolOccurrence {
                        name: def.key.clone(),
                        file_id: def.file_id,
                        span: def.span.clone(),
                        kind: references::OccurrenceKind::Definition,
                    });
                }

                Self {
                    original_source,
                    source_map: Some(source_map),
                    file_paths,
                    file_sources,
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
                // Even on failure, expose whatever files were loaded so
                // diagnostics can be attributed to the right file.
                let file_paths: BTreeMap<FileId, Utf8PathBuf> = workspace
                    .file_ids()
                    .map(|(id, path)| (id, path.to_owned()))
                    .collect();
                let mut file_sources: BTreeMap<FileId, String> = file_paths
                    .keys()
                    .map(|&id| (id, workspace.file_db().source(id).to_string()))
                    .collect();
                // When the root file itself failed to load it is absent from
                // the registry above. Register it (with whatever source we
                // have, possibly empty) so error diagnostics can still be
                // attributed to the root document instead of being dropped.
                file_sources
                    .entry(ROOT_FILE_ID)
                    .or_insert_with(|| original_source.clone());

                Self {
                    original_source,
                    source_map: None,
                    file_paths,
                    file_sources,
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

    /// The original source text of the root document (what the editor shows).
    #[must_use]
    pub fn source(&self) -> &str {
        &self.original_source
    }

    /// The [`FileId`] of the root document.
    #[must_use]
    pub fn root_file_id(&self) -> FileId {
        ROOT_FILE_ID
    }

    /// The relative path of a file that took part in this analysis.
    #[must_use]
    pub fn file_path(&self, file_id: FileId) -> Option<&Utf8Path> {
        self.file_paths.get(&file_id).map(Utf8PathBuf::as_path)
    }

    /// The source text of a file that took part in this analysis.
    #[must_use]
    pub fn file_source(&self, file_id: FileId) -> Option<&str> {
        self.file_sources.get(&file_id).map(String::as_str)
    }

    #[must_use]
    pub fn program(&self) -> Option<&Program> {
        self.parse_result.as_ref().map(|r| &r.program.inner)
    }

    #[must_use]
    pub fn labels(&self) -> &BTreeMap<String, Address> {
        &self.layout.labels
    }

    /// Classify what a label points at, using the compiled memory layout.
    ///
    /// Returns [`LabelKind::Code`] when the label's address holds an
    /// instruction and [`LabelKind::Data`] when it holds a `.word`, `.string`
    /// or `.space` cell. Returns `None` when the label is undefined (not in the
    /// layout). A defined label whose address was never filled (e.g. it sits
    /// past the last placed cell) is treated as [`LabelKind::Code`].
    #[must_use]
    pub fn label_kind(&self, name: &str) -> Option<LabelKind> {
        let address = *self.layout.labels.get(name)?;
        let kind = match self.layout.memory.get(&address) {
            Some((Placement::Line(content), _)) => match &content.inner {
                LineContent::Instruction { .. } => LabelKind::Code,
                // `.word` (the only directive that places a `Line` cell).
                LineContent::Directive { .. } | LineContent::Error => LabelKind::Data,
            },
            Some((Placement::Reserved | Placement::Char(_) | Placement::Nul, _)) => LabelKind::Data,
            // Defined but never filled: default to code.
            None => LabelKind::Code,
        };
        Some(kind)
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

    /// Find the symbol occurrence at the given byte offset in the **root**
    /// document, if any. The cursor is always in the root document, so
    /// occurrences from included files are ignored here.
    #[must_use]
    pub fn occurrence_at(&self, offset: usize) -> Option<&SymbolOccurrence> {
        self.occurrences
            .iter()
            .find(|occ| occ.file_id == ROOT_FILE_ID && occ.span.contains(&offset))
    }

    /// Find all occurrences of the symbol with the given name, across every
    /// file in the analysis.
    pub fn occurrences_of<'a>(
        &'a self,
        name: &'a str,
    ) -> impl Iterator<Item = &'a SymbolOccurrence> {
        self.occurrences.iter().filter(move |occ| occ.name == name)
    }

    /// Map a byte range in the preprocessed source back to a byte range in the
    /// **root** document. Returns `None` if the span belongs to an included
    /// file. Returns the span unchanged if no source map is available.
    #[must_use]
    pub fn resolve_span(&self, span: Range<usize>) -> Option<Range<usize>> {
        let (file_id, range) = self.resolve_span_file(span.clone())?;
        if file_id == ROOT_FILE_ID {
            Some(range)
        } else {
            None
        }
    }

    /// Map a byte range in the preprocessed source back to the file id and
    /// byte range in that original file. Returns `(ROOT_FILE_ID, span)`
    /// unchanged if no source map is available.
    #[must_use]
    pub fn resolve_span_file(&self, span: Range<usize>) -> Option<(FileId, Range<usize>)> {
        let Some(source_map) = self.source_map.as_ref() else {
            return Some((ROOT_FILE_ID, span));
        };
        diagnostic::resolve_to_original(source_map, span)
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

    // -----------------------------------------------------------------------
    // Multi-file analysis
    // -----------------------------------------------------------------------

    use crate::preprocessor::InMemoryFilesystem;

    fn fs(files: &[(&str, &str)]) -> InMemoryFilesystem {
        InMemoryFilesystem::new(
            files
                .iter()
                .map(|(p, c)| (Utf8PathBuf::from(*p), (*c).to_string()))
                .collect::<std::collections::HashMap<_, _>>(),
        )
    }

    #[test]
    fn include_resolves_across_files() {
        let fs = fs(&[
            ("main.s", "#include \"util.s\"\n    jmp target\n"),
            ("util.s", "target:\n    reset\n"),
        ]);
        let state = DocumentState::analyze(&fs, Utf8Path::new("main.s"));

        // No preprocessor error: the include resolved.
        assert!(state.preprocessor_error().is_none());

        // The label `target` is known and its definition lives in util.s.
        assert!(state.labels().contains_key("target"));
        let def = state
            .occurrences_of("target")
            .find(|o| o.kind == references::OccurrenceKind::Definition)
            .expect("definition of target");
        let def_path = state.file_path(def.file_id).unwrap();
        assert_eq!(def_path, Utf8Path::new("util.s"));
        assert_eq!(
            slice(state.file_source(def.file_id).unwrap(), &def.span),
            "target"
        );

        // The reference `jmp target` lives in the root document.
        let reference = state
            .occurrences_of("target")
            .find(|o| o.kind == references::OccurrenceKind::Reference)
            .expect("reference to target");
        assert_eq!(reference.file_id, state.root_file_id());
    }

    #[test]
    fn missing_include_is_a_preprocessor_error() {
        let fs = fs(&[("main.s", "#include \"nope.s\"\n    reset\n")]);
        let state = DocumentState::analyze(&fs, Utf8Path::new("main.s"));
        assert!(state.preprocessor_error().is_some());
    }

    #[test]
    fn unreadable_root_does_not_panic_and_yields_diagnostics() {
        // The root file does not exist in the filesystem, so it can never be
        // read. `analyze` must not panic (it used to eagerly read the root
        // source out of the file database) and must still surface the load
        // failure as a diagnostic.
        let fs = fs(&[]);
        let state = DocumentState::analyze(&fs, Utf8Path::new("main.s"));

        assert!(
            state.preprocessor_error().is_some(),
            "an unreadable root should be a preprocessor error"
        );
        assert!(state.source().is_empty());

        let diagnostics = crate::lsp::diagnostics::diagnostics_by_file(&state);
        assert!(
            !diagnostics.is_empty(),
            "the load failure should be reported as a diagnostic"
        );
    }
}
