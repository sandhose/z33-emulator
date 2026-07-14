//! Multi-file workspace model for the LSP backend.
//!
//! The [`WorkspaceManager`] owns every piece of mutable state the language
//! server needs *except* the transport: the set of open documents, a base file
//! map (pushed by the host, see [`crate::lsp::LspSession`]), and the workspace
//! root. Keeping it transport-free makes the whole analysis and cross-file
//! mapping logic unit-testable without a live LSP connection.
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
//! right [`Uri`].

use std::collections::{HashMap, HashSet};
use std::str::FromStr;
use std::sync::Arc;

use camino::{Utf8Path, Utf8PathBuf};
use lsp_types::{
    Diagnostic, DocumentHighlight, DocumentHighlightKind, Location, TextEdit, Uri, WorkspaceEdit,
};

use super::document::DocumentState;
use super::references::{OccurrenceKind, SymbolOccurrence};
use super::{diagnostics, position};
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
    root_uri: Option<Uri>,
    /// Workspace root as a native path, if the root URI was a `file://` URI.
    native_root: Option<Utf8PathBuf>,
    /// Host-pushed base files (see `zorglub33/workspaceFiles`), keyed by
    /// relative path.
    base_files: HashMap<Utf8PathBuf, String>,
    /// Currently open documents, keyed by URI.
    documents: HashMap<Uri, Document>,
    /// URIs we last published *non-empty* diagnostics for, so we can clear
    /// them.
    published: HashSet<Uri>,
    /// Client-side commands the client declared it can execute (from the
    /// `experimental.commands` client capability). Server-produced commands
    /// (e.g. the run code lens) are only emitted when advertised here.
    client_commands: HashSet<String>,
}

impl WorkspaceManager {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// Record the workspace root captured from `initialize`.
    pub(crate) fn set_root(&mut self, root_uri: Option<Uri>) {
        self.native_root = root_uri.as_ref().and_then(uri_to_native_path);
        self.root_uri = root_uri;
    }

    pub(crate) fn set_client_commands(&mut self, commands: HashSet<String>) {
        self.client_commands = commands;
    }

    /// Whether the client declared it can execute the given client-side
    /// command (via the `experimental.commands` capability).
    pub(crate) fn supports_client_command(&self, command: &str) -> bool {
        self.client_commands.contains(command)
    }

    /// Replace the host-pushed base file map and re-analyze.
    pub(crate) fn set_base_files(
        &mut self,
        files: HashMap<Utf8PathBuf, String>,
    ) -> Vec<(Uri, Vec<Diagnostic>)> {
        self.base_files = files;
        self.recompute()
    }

    /// Handle `didOpen` / `didChange` for a document.
    pub(crate) fn upsert(&mut self, uri: Uri, text: String) -> Vec<(Uri, Vec<Diagnostic>)> {
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
    pub(crate) fn close(&mut self, uri: &Uri) -> Vec<(Uri, Vec<Diagnostic>)> {
        self.documents.remove(uri);
        self.recompute()
    }

    /// The current source of an open document.
    pub(crate) fn source(&self, uri: &Uri) -> Option<String> {
        self.documents.get(uri).map(|d| d.source.clone())
    }

    /// The latest analysis of an open document.
    pub(crate) fn analysis(&self, uri: &Uri) -> Option<Arc<DocumentState>> {
        self.documents.get(uri).and_then(|d| d.analysis.clone())
    }

    // -- Cross-file feature queries ---------------------------------------

    /// Go-to-definition: resolve the symbol under the cursor to its definition,
    /// which may live in another file.
    pub(crate) fn goto_definition(&self, uri: &Uri, offset: usize) -> Option<Location> {
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
        uri: &Uri,
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
        uri: &Uri,
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
    pub(crate) fn rename(&self, uri: &Uri, offset: usize, new_name: &str) -> Option<WorkspaceEdit> {
        let doc = self.documents.get(uri)?;
        let analysis = doc.analysis.as_ref()?;
        let occ = analysis.occurrence_at(offset)?;

        let mut changes: HashMap<Uri, Vec<TextEdit>> = HashMap::new();
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
        request_uri: &Uri,
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
        request_uri: &Uri,
        file_id: FileId,
    ) -> Option<Uri> {
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
    fn recompute(&mut self) -> Vec<(Uri, Vec<Diagnostic>)> {
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
    fn collect_diagnostics(&mut self) -> Vec<(Uri, Vec<Diagnostic>)> {
        let mut per_uri: HashMap<Uri, Vec<Diagnostic>> = HashMap::new();

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
    ///
    /// Relative paths live in *decoded* space (they are matched against
    /// `#include` arguments and `zorglub33/workspaceFiles` keys, which are
    /// plain strings), so any percent-encoding in the URI is undone here;
    /// [`Self::uri_for_relative`] re-encodes on the way out.
    pub(crate) fn relative_for_uri(&self, uri: &Uri) -> Utf8PathBuf {
        // Prefer stripping the native root off the file path.
        if let (Some(root), Some(path)) = (&self.native_root, uri_to_native_path(uri)) {
            if let Ok(rel) = path.strip_prefix(root) {
                return rel.to_owned();
            }
        }
        // Otherwise strip the root URI textually.
        if let Some(root_uri) = &self.root_uri {
            if let Some(rest) = uri.as_str().strip_prefix(root_uri.as_str()) {
                let rest = rest.trim_start_matches('/');
                if !rest.is_empty() {
                    return decode_path(rest);
                }
            }
        }
        // Fall back to the bare file name.
        let name = uri
            .path()
            .as_str()
            .rsplit('/')
            .next()
            .filter(|s| !s.is_empty())
            .unwrap_or("main.S");
        decode_path(name)
    }

    /// Map a workspace-relative path to a URI, preferring an already-open
    /// document with that path.
    fn uri_for_relative(&self, rel: &Utf8Path) -> Option<Uri> {
        for (uri, doc) in &self.documents {
            if doc.relative_path == rel {
                return Some(uri.clone());
            }
        }

        if let Some(root) = &self.native_root {
            return native_path_to_uri(&root.join(rel));
        }

        if let Some(root_uri) = &self.root_uri {
            let base = root_uri.as_str();
            let slash = if base.ends_with('/') { "" } else { "/" };
            let encoded = encode_path(rel.as_str());
            return Uri::from_str(&format!("{base}{slash}{encoded}")).ok();
        }

        None
    }
}

/// The set of characters percent-encoded in the path component of a `file://`
/// URI.
///
/// Unlike the old `url` crate — which escaped whatever it had to at parse
/// time — the fluent-uri parser behind [`lsp_types::Uri`] rejects invalid
/// characters outright, so this set must cover every ASCII character RFC 3986
/// forbids in a path (non-ASCII bytes are always encoded by
/// `percent-encoding`). `%` is included so raw path input round-trips instead
/// of being misread as an escape sequence.
const PATH_ENCODE_SET: &percent_encoding::AsciiSet = &percent_encoding::CONTROLS
    .add(b' ')
    .add(b'"')
    .add(b'#')
    .add(b'%')
    .add(b'<')
    .add(b'>')
    .add(b'?')
    .add(b'[')
    .add(b'\\')
    .add(b']')
    .add(b'^')
    .add(b'`')
    .add(b'{')
    .add(b'|')
    .add(b'}');

/// Percent-encode a (workspace-relative or absolute POSIX) path for use as the
/// path component of a URI. `/` separators are left as-is.
fn encode_path(path: &str) -> percent_encoding::PercentEncode<'_> {
    percent_encoding::utf8_percent_encode(path, PATH_ENCODE_SET)
}

/// Percent-decode one or more URI path segments into a relative path. Falls
/// back to the raw string when the decoded bytes are not valid UTF-8.
fn decode_path(s: &str) -> Utf8PathBuf {
    match percent_encoding::percent_decode_str(s).decode_utf8() {
        Ok(decoded) => Utf8PathBuf::from(decoded.as_ref()),
        Err(_) => Utf8PathBuf::from(s),
    }
}

/// Convert a `file://` URI to a native path, if applicable.
///
/// `lsp_types::Uri` has no `to_file_path` (the fluent-uri type is
/// scheme-agnostic), so the path component is percent-decoded manually. This
/// handles the common POSIX case used by the native CLI host; WASM hosts never
/// set a native root so this only matters natively.
fn uri_to_native_path(uri: &Uri) -> Option<Utf8PathBuf> {
    if !uri.scheme().is_some_and(|s| s.eq_lowercase("file")) {
        return None;
    }
    let decoded = percent_encoding::percent_decode_str(uri.path().as_str())
        .decode_utf8()
        .ok()?;
    Some(Utf8PathBuf::from(decoded.as_ref()))
}

/// Convert a native (absolute, POSIX) path to a `file://` URI.
///
/// Manual counterpart to the `url` crate's `Url::from_file_path`, which
/// `lsp_types::Uri` does not provide. Only exercised by the native CLI host
/// (WASM hosts have no root).
fn native_path_to_uri(path: &Utf8Path) -> Option<Uri> {
    let encoded = encode_path(path.as_str());
    // POSIX absolute paths begin with '/', yielding `file:///abs/path`.
    Uri::from_str(&format!("file://{encoded}")).ok()
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
fn severity_code(severity: lsp_types::DiagnosticSeverity) -> i32 {
    use lsp_types::DiagnosticSeverity as S;
    match severity {
        S::ERROR => 1,
        S::WARNING => 2,
        S::INFORMATION => 3,
        S::HINT => 4,
        _ => 0,
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    const ROOT: &str = "file:///ws/";

    fn uri(rel: &str) -> Uri {
        Uri::from_str(&format!("file:///ws/{rel}")).unwrap()
    }

    fn manager() -> WorkspaceManager {
        let mut m = WorkspaceManager::new();
        m.set_root(Some(Uri::from_str(ROOT).unwrap()));
        m
    }

    fn base(files: &[(&str, &str)]) -> HashMap<Utf8PathBuf, String> {
        files
            .iter()
            .map(|(p, c)| (Utf8PathBuf::from(*p), (*c).to_string()))
            .collect()
    }

    /// Byte offset of the first occurrence of `needle` in `haystack`.
    fn offset_of(haystack: &str, needle: &str) -> usize {
        haystack.find(needle).expect("needle present")
    }

    #[test]
    fn include_resolves_against_base_files() {
        let mut m = manager();
        let main = uri("main.s");
        m.upsert(
            main.clone(),
            "#include \"util.s\"\n    jmp target\n".to_string(),
        );

        // Without the included file, preprocessing fails.
        assert!(m.analysis(&main).unwrap().preprocessor_error().is_some());

        // Pushing the base file makes the include resolve.
        m.set_base_files(base(&[("util.s", "target:\n    reset\n")]));
        let analysis = m.analysis(&main).unwrap();
        assert!(analysis.preprocessor_error().is_none());
        assert!(analysis.labels().contains_key("target"));
    }

    #[test]
    fn open_document_overrides_base_file() {
        let mut m = manager();
        // Base util.s defines `base_label`.
        m.set_base_files(base(&[("util.s", "base_label:\n    .word 1\n")]));

        let main = uri("main.s");
        m.upsert(
            main.clone(),
            "#include \"util.s\"\n    jmp open_label\n".to_string(),
        );

        // Opening util.s with different content must shadow the base file.
        let util = uri("util.s");
        m.upsert(util.clone(), "open_label:\n    .word 2\n".to_string());

        let analysis = m.analysis(&main).unwrap();
        assert!(analysis.labels().contains_key("open_label"));
        assert!(!analysis.labels().contains_key("base_label"));
    }

    #[test]
    fn cross_file_goto_definition() {
        let mut m = manager();
        let main = uri("main.s");
        let util = uri("util.s");
        let main_src = "#include \"util.s\"\n    jmp target\n";
        m.upsert(util.clone(), "target:\n    reset\n".to_string());
        m.upsert(main.clone(), main_src.to_string());

        let offset = offset_of(main_src, "target");
        let location = m.goto_definition(&main, offset).expect("definition");
        assert_eq!(location.uri, util);
        // Points at `target:` in util.s (line 0).
        assert_eq!(location.range.start.line, 0);
        assert_eq!(location.range.start.character, 0);
    }

    #[test]
    fn space_in_filename_is_percent_encoded_and_decoded() {
        let mut m = manager();
        let main = uri("main.s");
        let main_src = "#include \"my util.s\"\n    jmp target\n";
        // The included file (with a space in its name) is a base file, so the
        // returned URI must be *built* from the relative path, percent-encoded.
        m.set_base_files(base(&[("my util.s", "target:\n    reset\n")]));
        m.upsert(main.clone(), main_src.to_string());

        assert!(m.analysis(&main).unwrap().preprocessor_error().is_none());

        let offset = offset_of(main_src, "target");
        let location = m.goto_definition(&main, offset).expect("definition");
        assert_eq!(location.uri.as_str(), "file:///ws/my%20util.s");

        // The client's (encoded) URI for that file maps back to the decoded
        // relative path, so opening it shadows the base file.
        let util = uri("my%20util.s");
        assert_eq!(m.relative_for_uri(&util), Utf8PathBuf::from("my util.s"));
        m.upsert(util.clone(), "target:\n    .word 1\nother:\n".to_string());
        let analysis = m.analysis(&main).unwrap();
        assert!(analysis.labels().contains_key("other"));

        // Once the document is open, its own URI is echoed back verbatim.
        let offset = offset_of(main_src, "target");
        let location = m.goto_definition(&main, offset).expect("definition");
        assert_eq!(location.uri, util);
    }

    #[test]
    fn non_file_root_builds_encoded_uris_textually() {
        // A non-`file://` root (e.g. vscode-test-web's virtual scheme) has no
        // native path; URIs are built by appending the encoded relative path
        // to the root URI.
        let mut m = WorkspaceManager::new();
        m.set_root(Some(
            Uri::from_str("vscode-test-web://mount/project").unwrap(),
        ));

        let main = Uri::from_str("vscode-test-web://mount/project/main.s").unwrap();
        let main_src = "#include \"lib/my util.s\"\n    jmp target\n";
        m.set_base_files(base(&[("lib/my util.s", "target:\n    reset\n")]));
        m.upsert(main.clone(), main_src.to_string());

        let offset = offset_of(main_src, "target");
        let location = m.goto_definition(&main, offset).expect("definition");
        assert_eq!(
            location.uri.as_str(),
            "vscode-test-web://mount/project/lib/my%20util.s"
        );
    }

    #[test]
    fn relative_for_uri_decodes_bare_file_name() {
        // With no root at all (`rootUri: null`), the fallback is the last path
        // segment, percent-decoded.
        let m = WorkspaceManager::new();
        let doc = Uri::from_str("file:///my%20file.s").unwrap();
        assert_eq!(m.relative_for_uri(&doc), Utf8PathBuf::from("my file.s"));
    }

    #[test]
    fn cross_file_rename_spans_multiple_files() {
        let mut m = manager();
        let main = uri("main.s");
        let util = uri("util.s");
        let main_src = "#include \"util.s\"\n    jmp target\n";
        m.upsert(util.clone(), "target:\n    reset\n".to_string());
        m.upsert(main.clone(), main_src.to_string());

        let offset = offset_of(main_src, "target");
        let edit = m.rename(&main, offset, "renamed").expect("rename edit");
        let changes = edit.changes.expect("changes");

        // Both the definition (util.s) and the reference (main.s) are edited.
        assert!(changes.contains_key(&util), "util.s should be edited");
        assert!(changes.contains_key(&main), "main.s should be edited");
        assert_eq!(changes[&util].len(), 1);
        assert_eq!(changes[&util][0].new_text, "renamed");
        assert_eq!(changes[&main].len(), 1);
    }

    #[test]
    fn diagnostics_attributed_to_included_file() {
        let mut m = manager();
        let main = uri("main.s");
        let util = uri("util.s");
        // The included file contains a parse error; only main is open.
        m.set_base_files(base(&[("util.s", "    $$bad$$\n")]));
        let batches = m.upsert(main.clone(), "#include \"util.s\"\n    reset\n".to_string());

        let util_diags = batches
            .iter()
            .find(|(u, _)| *u == util)
            .map(|(_, d)| d)
            .expect("diagnostics published for util.s");
        assert!(
            !util_diags.is_empty(),
            "the included file should carry the error"
        );

        // The root document itself has no diagnostics of its own.
        let main_diags = batches
            .iter()
            .find(|(u, _)| *u == main)
            .map(|(_, d)| d)
            .expect("entry for main.s");
        assert!(main_diags.is_empty(), "root should be clean");
    }

    #[test]
    fn stale_diagnostics_are_cleared() {
        let mut m = manager();
        let main = uri("main.s");
        // First: an error.
        let batches = m.upsert(main.clone(), "    $$bad$$\n".to_string());
        assert!(batches.iter().any(|(u, d)| *u == main && !d.is_empty()));

        // Then: fix it. The URI must be published again, now empty.
        let batches = m.upsert(main.clone(), "    reset\n".to_string());
        let main_entry = batches
            .iter()
            .find(|(u, _)| *u == main)
            .expect("main published");
        assert!(main_entry.1.is_empty(), "diagnostics should be cleared");
    }

    #[test]
    fn workspace_files_update_replaces_base_map() {
        let mut m = manager();
        let main = uri("main.s");
        m.upsert(
            main.clone(),
            "#include \"util.s\"\n    jmp target\n".to_string(),
        );

        // First base map provides util.s.
        m.set_base_files(base(&[("util.s", "target:\n    reset\n")]));
        assert!(m.analysis(&main).unwrap().preprocessor_error().is_none());

        // Replacing it with an empty map removes util.s again.
        m.set_base_files(HashMap::new());
        assert!(m.analysis(&main).unwrap().preprocessor_error().is_some());
    }
}
