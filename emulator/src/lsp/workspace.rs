//! Multi-file workspace model for the LSP backend.
//!
//! The [`WorkspaceManager`] owns every piece of mutable state the language
//! server needs *except* the [`Client`](tower_lsp::Client): the set of open
//! documents, a base file map (pushed by the host, see
//! [`crate::lsp::Backend`]), and the workspace root. Keeping it client-free
//! makes the whole analysis and cross-file mapping logic unit-testable without
//! a live LSP connection.
//!
//! # File model
//!
//! Every file is identified by a path *relative to the workspace root*. Open
//! documents take precedence over the base file map, which in turn takes
//! precedence over files read from disk (native mode). This layering is
//! implemented by [`OverlayFilesystem`], which the [`DocumentState`] analysis
//! uses to resolve `#include` directives.
//!
//! Each open document is analyzed as its own preprocessing root. Because a root
//! can `#include` other files, spans, occurrences, and diagnostics carry the
//! [`FileId`] of the file they belong to; the manager maps those back to the
//! right [`Url`].

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use camino::{Utf8Path, Utf8PathBuf};
use tower_lsp::lsp_types::{
    Diagnostic, DocumentHighlight, DocumentHighlightKind, Location, TextEdit, Url, WorkspaceEdit,
};

use super::diagnostics;
use super::document::DocumentState;
use super::position;
use super::references::{OccurrenceKind, SymbolOccurrence};
use crate::diagnostic::FileId;
use crate::preprocessor::Filesystem;

/// A filesystem overlay: open-document / host-pushed contents on top of an
/// optional native (on-disk) root.
///
/// Paths are handled in relative space (`root()` is `""`); the native root, if
/// any, is only consulted inside [`read`](OverlayFilesystem::read) to locate
/// files on disk.
pub(crate) struct OverlayFilesystem {
    /// Absolute native root, if diagnostics should fall back to reading disk.
    native_root: Option<Utf8PathBuf>,
    /// Relative path -> content, merged from base files and open documents
    /// (open documents win).
    overlay: HashMap<Utf8PathBuf, String>,
}

impl Filesystem for OverlayFilesystem {
    fn read(&self, path: &Utf8Path) -> std::io::Result<String> {
        if let Some(content) = self.overlay.get(path) {
            return Ok(content.clone());
        }
        if let Some(root) = &self.native_root {
            return std::fs::read_to_string(root.join(path));
        }
        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "file not found",
        ))
    }
}

/// A single open document.
struct Document {
    /// Always-current source text, updated on every change.
    source: String,
    /// Path relative to the workspace root (or bare file name as a fallback).
    relative_path: Utf8PathBuf,
    /// Latest completed analysis (this document as a preprocessing root).
    analysis: Option<Arc<DocumentState>>,
}

/// Owns all client-independent LSP state.
#[derive(Default)]
pub(crate) struct WorkspaceManager {
    /// Workspace root as a URI, if the client provided one.
    root_uri: Option<Url>,
    /// Workspace root as a native path, if the root URI was a `file://` URI.
    native_root: Option<Utf8PathBuf>,
    /// Host-pushed base files (see `z33/workspaceFiles`), keyed by relative
    /// path.
    base_files: HashMap<Utf8PathBuf, String>,
    /// Currently open documents, keyed by URI.
    documents: HashMap<Url, Document>,
    /// URIs we last published *non-empty* diagnostics for, so we can clear them.
    published: HashSet<Url>,
}

impl WorkspaceManager {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// Record the workspace root captured from `initialize`.
    pub(crate) fn set_root(&mut self, root_uri: Option<Url>) {
        self.native_root = root_uri.as_ref().and_then(url_to_native_path);
        self.root_uri = root_uri;
    }

    /// Replace the host-pushed base file map and re-analyze.
    pub(crate) fn set_base_files(
        &mut self,
        files: HashMap<Utf8PathBuf, String>,
    ) -> Vec<(Url, Vec<Diagnostic>)> {
        self.base_files = files;
        self.recompute()
    }

    /// Handle `didOpen` / `didChange` for a document.
    pub(crate) fn upsert(&mut self, uri: Url, text: String) -> Vec<(Url, Vec<Diagnostic>)> {
        let relative_path = self.relative_for_uri(&uri);
        self.documents.insert(
            uri,
            Document {
                source: text,
                relative_path,
                analysis: None,
            },
        );
        self.recompute()
    }

    /// Handle `didClose`.
    pub(crate) fn close(&mut self, uri: &Url) -> Vec<(Url, Vec<Diagnostic>)> {
        self.documents.remove(uri);
        self.recompute()
    }

    /// The current source of an open document.
    pub(crate) fn source(&self, uri: &Url) -> Option<String> {
        self.documents.get(uri).map(|d| d.source.clone())
    }

    /// The latest analysis of an open document.
    pub(crate) fn analysis(&self, uri: &Url) -> Option<Arc<DocumentState>> {
        self.documents.get(uri).and_then(|d| d.analysis.clone())
    }

    // -- Cross-file feature queries ---------------------------------------

    /// Go-to-definition: resolve the symbol under the cursor to its definition,
    /// which may live in another file.
    pub(crate) fn goto_definition(&self, uri: &Url, offset: usize) -> Option<Location> {
        let doc = self.documents.get(uri)?;
        let analysis = doc.analysis.as_ref()?;
        let occ = analysis.occurrence_at(offset)?;
        let def = analysis
            .occurrences_of(&occ.name)
            .find(|o| o.kind == OccurrenceKind::Definition)?;
        self.occurrence_location(analysis, uri, def)
    }

    /// Find references to the symbol under the cursor, across every file.
    pub(crate) fn references(
        &self,
        uri: &Url,
        offset: usize,
        include_declaration: bool,
    ) -> Option<Vec<Location>> {
        let doc = self.documents.get(uri)?;
        let analysis = doc.analysis.as_ref()?;
        let occ = analysis.occurrence_at(offset)?;

        let locations: Vec<Location> = analysis
            .occurrences_of(&occ.name)
            .filter(|o| include_declaration || o.kind != OccurrenceKind::Definition)
            .filter_map(|o| self.occurrence_location(analysis, uri, o))
            .collect();

        (!locations.is_empty()).then_some(locations)
    }

    /// Highlight occurrences of the symbol under the cursor within this file.
    pub(crate) fn document_highlight(
        &self,
        uri: &Url,
        offset: usize,
    ) -> Option<Vec<DocumentHighlight>> {
        let doc = self.documents.get(uri)?;
        let analysis = doc.analysis.as_ref()?;
        let occ = analysis.occurrence_at(offset)?;
        let root = analysis.root_file_id();

        let highlights: Vec<DocumentHighlight> = analysis
            .occurrences_of(&occ.name)
            .filter(|o| o.file_id == root)
            .filter_map(|o| {
                let range = position::range(analysis.source(), o.span.clone())?;
                let kind = if o.kind == OccurrenceKind::Definition {
                    DocumentHighlightKind::WRITE
                } else {
                    DocumentHighlightKind::READ
                };
                Some(DocumentHighlight {
                    range,
                    kind: Some(kind),
                })
            })
            .collect();

        (!highlights.is_empty()).then_some(highlights)
    }

    /// Rename the symbol under the cursor everywhere, producing edits that may
    /// span multiple files.
    pub(crate) fn rename(
        &self,
        uri: &Url,
        offset: usize,
        new_name: &str,
    ) -> Option<WorkspaceEdit> {
        let doc = self.documents.get(uri)?;
        let analysis = doc.analysis.as_ref()?;
        let occ = analysis.occurrence_at(offset)?;

        let mut changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();
        for o in analysis.occurrences_of(&occ.name) {
            let Some(target_uri) = self.file_uri(analysis, uri, o.file_id) else {
                continue;
            };
            let Some(source) = analysis.file_source(o.file_id) else {
                continue;
            };
            let Some(range) = position::range(source, o.span.clone()) else {
                continue;
            };
            changes.entry(target_uri).or_default().push(TextEdit {
                range,
                new_text: new_name.to_string(),
            });
        }

        if changes.is_empty() {
            return None;
        }

        Some(WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        })
    }

    // -- Internal helpers -------------------------------------------------

    /// Build the [`Location`] for an occurrence, mapping its file id to a URI.
    fn occurrence_location(
        &self,
        analysis: &DocumentState,
        request_uri: &Url,
        occ: &SymbolOccurrence,
    ) -> Option<Location> {
        let source = analysis.file_source(occ.file_id)?;
        let range = position::range(source, occ.span.clone())?;
        let uri = self.file_uri(analysis, request_uri, occ.file_id)?;
        Some(Location { uri, range })
    }

    /// Map a file id (within `analysis`) to a URI.
    fn file_uri(
        &self,
        analysis: &DocumentState,
        request_uri: &Url,
        file_id: FileId,
    ) -> Option<Url> {
        // The root file of the analysis is the requested document.
        if file_id == analysis.root_file_id() {
            return Some(request_uri.clone());
        }
        let rel = analysis.file_path(file_id)?;
        self.uri_for_relative(rel)
    }

    /// Merge base files and open documents into a single overlay.
    fn overlay(&self) -> HashMap<Utf8PathBuf, String> {
        let mut overlay = self.base_files.clone();
        for doc in self.documents.values() {
            overlay.insert(doc.relative_path.clone(), doc.source.clone());
        }
        overlay
    }

    /// Re-analyze every open document and compute the diagnostics to publish
    /// (including empty lists to clear files that no longer have diagnostics).
    fn recompute(&mut self) -> Vec<(Url, Vec<Diagnostic>)> {
        let overlay = self.overlay();
        let fs = OverlayFilesystem {
            native_root: self.native_root.clone(),
            overlay,
        };

        for doc in self.documents.values_mut() {
            doc.analysis = Some(Arc::new(DocumentState::analyze(&fs, &doc.relative_path)));
        }

        self.collect_diagnostics()
    }

    /// Aggregate diagnostics from every open root, keyed by target URI, and
    /// reconcile with the previously published set so stale entries are
    /// cleared.
    fn collect_diagnostics(&mut self) -> Vec<(Url, Vec<Diagnostic>)> {
        let mut per_uri: HashMap<Url, Vec<Diagnostic>> = HashMap::new();

        for (uri, doc) in &self.documents {
            // Ensure every open document ends up in the map (so it is cleared
            // when it has no diagnostics).
            per_uri.entry(uri.clone()).or_default();

            let Some(analysis) = &doc.analysis else {
                continue;
            };
            for (file_id, diagnostic) in diagnostics::diagnostics_by_file(analysis) {
                let Some(target) = self.file_uri(analysis, uri, file_id) else {
                    continue;
                };
                per_uri.entry(target).or_default().push(diagnostic);
            }
        }

        // De-duplicate diagnostics within each URI (the same file may be
        // reached through several open roots).
        for diags in per_uri.values_mut() {
            dedupe_diagnostics(diags);
        }

        // Clear any URI we published to last time but not this time.
        for old in &self.published {
            per_uri.entry(old.clone()).or_default();
        }

        self.published = per_uri
            .iter()
            .filter(|(_, diags)| !diags.is_empty())
            .map(|(uri, _)| uri.clone())
            .collect();

        per_uri.into_iter().collect()
    }

    /// Compute the workspace-relative path for a document URI.
    fn relative_for_uri(&self, uri: &Url) -> Utf8PathBuf {
        // Prefer stripping the native root off the file path.
        if let (Some(root), Some(path)) = (&self.native_root, url_to_native_path(uri)) {
            if let Ok(rel) = path.strip_prefix(root) {
                return rel.to_owned();
            }
        }
        // Otherwise strip the root URI textually.
        if let Some(root_uri) = &self.root_uri {
            if let Some(rest) = uri.as_str().strip_prefix(root_uri.as_str()) {
                let rest = rest.trim_start_matches('/');
                if !rest.is_empty() {
                    return Utf8PathBuf::from(rest);
                }
            }
        }
        // Fall back to the bare file name.
        let name = uri
            .path_segments()
            .and_then(std::iter::Iterator::last)
            .filter(|s| !s.is_empty())
            .unwrap_or("main.S");
        Utf8PathBuf::from(name)
    }

    /// Map a workspace-relative path to a URI, preferring an already-open
    /// document with that path.
    fn uri_for_relative(&self, rel: &Utf8Path) -> Option<Url> {
        for (uri, doc) in &self.documents {
            if doc.relative_path == rel {
                return Some(uri.clone());
            }
        }

        if let Some(root) = &self.native_root {
            return Url::from_file_path(root.join(rel).as_std_path()).ok();
        }

        if let Some(root_uri) = &self.root_uri {
            let mut base = root_uri.clone();
            let path = base.path().to_string();
            if !path.ends_with('/') {
                base.set_path(&format!("{path}/"));
            }
            return base.join(rel.as_str()).ok();
        }

        None
    }
}

/// Convert a `file://` URI to a native path, if applicable.
fn url_to_native_path(uri: &Url) -> Option<Utf8PathBuf> {
    if uri.scheme() != "file" {
        return None;
    }
    let path = uri.to_file_path().ok()?;
    Utf8PathBuf::from_path_buf(path).ok()
}

/// Remove duplicate diagnostics (same range / severity / message) in place.
fn dedupe_diagnostics(diags: &mut Vec<Diagnostic>) {
    type Key = (u32, u32, u32, u32, i32, String);
    let mut seen: HashSet<Key> = HashSet::new();
    diags.retain(|d| {
        let severity = d.severity.map_or(0, severity_code);
        seen.insert((
            d.range.start.line,
            d.range.start.character,
            d.range.end.line,
            d.range.end.character,
            severity,
            d.message.clone(),
        ))
    });
}

/// A stable integer code for a diagnostic severity (LSP wire values).
fn severity_code(severity: tower_lsp::lsp_types::DiagnosticSeverity) -> i32 {
    use tower_lsp::lsp_types::DiagnosticSeverity as S;
    match severity {
        S::ERROR => 1,
        S::WARNING => 2,
        S::INFORMATION => 3,
        S::HINT => 4,
        _ => 0,
    }
}
