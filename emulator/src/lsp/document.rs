use std::collections::BTreeMap;

use camino::Utf8PathBuf;

use crate::compiler::layout::{self, Layout, MemoryLayoutError};
use crate::compiler::memory::{self, MemoryFillError};
use crate::constants::Address;
use crate::diagnostic::{self};
use crate::parser::assembly::ParseResult;
use crate::parser::line::Program;
use crate::parser::shared::ParseDiagnostic;
use crate::parser::{self};
use crate::preprocessor::{InMemoryFilesystem, PreprocessorError, ReferencingSourceMap, Workspace};

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
    /// The preprocessed source fed to the parser.
    preprocessed_source: String,
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
    layout: Layout,
    layout_errors: Vec<MemoryLayoutError>,
    fill_errors: Vec<MemoryFillError>,
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
                // The first file added to the FileDatabase is the original
                // source, which gets ID 0.
                let original_file_id = 0;

                let parse_result = parser::parse(&result.source);
                let (layout, layout_errors) =
                    layout::layout_memory(&parse_result.program.inner.lines);
                let (_, fill_errors) = memory::fill_memory(&layout);

                Self {
                    original_source: source,
                    preprocessed_source: result.source,
                    source_map: Some(source_map),
                    original_file_id,
                    preprocessor_error: None,
                    parse_result: Some(parse_result),
                    layout,
                    layout_errors,
                    fill_errors,
                }
            }
            Err(error) => {
                // The first file added to the FileDatabase is the original
                // source, which gets ID 0.
                let original_file_id = 0;
                Self {
                    original_source: source,
                    preprocessed_source: String::new(),
                    source_map: None,
                    original_file_id,
                    preprocessor_error: Some(error),
                    parse_result: None,
                    layout: Layout::default(),
                    layout_errors: Vec::new(),
                    fill_errors: Vec::new(),
                }
            }
        }
    }

    /// The original source text (what the editor shows).
    #[must_use]
    pub fn source(&self) -> &str {
        &self.original_source
    }

    /// The preprocessed source (what the parser sees).
    #[must_use]
    pub fn preprocessed_source(&self) -> &str {
        &self.preprocessed_source
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

    /// Map a byte range in the preprocessed source back to a byte range in
    /// the original source.
    #[must_use]
    pub fn resolve_span(&self, span: std::ops::Range<usize>) -> Option<std::ops::Range<usize>> {
        let source_map = self.source_map.as_ref()?;
        let (file_id, range) = diagnostic::resolve_to_original(source_map, span)?;
        if file_id == self.original_file_id {
            Some(range)
        } else {
            None
        }
    }
}
