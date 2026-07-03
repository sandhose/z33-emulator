//! Bidirectional line ↔ address index for the debug adapter.
//!
//! Built at launch time by composing the compiler's source map (address → byte
//! range in the *preprocessed* flat source) with the preprocessor's source map
//! (preprocessed byte offset → original file + byte range). The result lets the
//! adapter answer two questions:
//!
//! * given a program-counter address, where is it in the original source (file,
//!   1-based line, 1-based column)?
//! * given a source breakpoint (file + line), what address should it map to,
//!   adjusting forward to the next line that actually has code?

use std::collections::BTreeMap;
use std::ops::Range;

use crate::compiler::DebugInfo;
use crate::constants::Address;
use crate::diagnostic::FileDatabase;
use crate::preprocessor::SourceMap;

/// A resolved source location.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Location {
    pub file_index: usize,
    /// 1-based line number.
    pub line: u32,
    /// 1-based column number.
    pub column: u32,
}

struct FileInfo {
    path: String,
    /// Byte offset of the start of each line (line 1 starts at
    /// `line_starts[0]`).
    line_starts: Vec<usize>,
    /// Maps a 1-based line that has code to the first address placed on it.
    lines_with_code: BTreeMap<u32, Address>,
}

impl FileInfo {
    fn new(path: String, source: &str) -> Self {
        let mut line_starts = vec![0usize];
        for (offset, byte) in source.bytes().enumerate() {
            if byte == b'\n' {
                line_starts.push(offset + 1);
            }
        }
        Self {
            path,
            line_starts,
            lines_with_code: BTreeMap::new(),
        }
    }

    /// Convert a byte offset within this file to a (line, column) pair, both
    /// 1-based.
    fn line_col(&self, offset: usize) -> (u32, u32) {
        // Number of line starts that are <= offset is the 1-based line number.
        let line = self.line_starts.partition_point(|&start| start <= offset);
        let line = line.max(1);
        let line_start = self.line_starts[line - 1];
        let column = offset.saturating_sub(line_start) + 1;
        (to_u32(line), to_u32(column))
    }
}

/// The line ↔ address index.
pub struct LineIndex {
    files: Vec<FileInfo>,
    addr_to_loc: BTreeMap<Address, Location>,
}

impl LineIndex {
    /// Build the index from the compiler debug info, the preprocessor source
    /// map and the file database.
    #[must_use]
    pub fn build(
        debug_info: &DebugInfo,
        pre_source_map: &SourceMap,
        file_db: &FileDatabase,
    ) -> Self {
        // First pass: discover which original files are referenced and create a
        // `FileInfo` for each, keyed by its diagnostic `FileId`.
        let mut file_index_by_id: BTreeMap<usize, usize> = BTreeMap::new();
        let mut files: Vec<FileInfo> = Vec::new();

        let mut addr_to_loc = BTreeMap::new();

        for (&address, range) in &debug_info.source_map {
            let Some((original_file_id, original_range)) = resolve(pre_source_map, range) else {
                continue;
            };
            let file_index = *file_index_by_id.entry(original_file_id).or_insert_with(|| {
                let idx = files.len();
                files.push(FileInfo::new(
                    file_db.name(original_file_id),
                    file_db.source(original_file_id),
                ));
                idx
            });
            let (line, column) = files[file_index].line_col(original_range.start);

            // Record the first (lowest) address seen on each line so breakpoints
            // resolve deterministically.
            files[file_index]
                .lines_with_code
                .entry(line)
                .and_modify(|existing| {
                    if address < *existing {
                        *existing = address;
                    }
                })
                .or_insert(address);

            addr_to_loc.insert(
                address,
                Location {
                    file_index,
                    line,
                    column,
                },
            );
        }

        Self { files, addr_to_loc }
    }

    /// Resolve the source location of an address, if it maps to code.
    pub(super) fn location(&self, address: Address) -> Option<&Location> {
        self.addr_to_loc.get(&address)
    }

    /// Resolve the source location of an address into owned `(path, line,
    /// column)` values (all 1-based), if it maps to code.
    #[must_use]
    pub fn location_owned(&self, address: Address) -> Option<(String, u32, u32)> {
        let loc = self.addr_to_loc.get(&address)?;
        Some((
            self.files[loc.file_index].path.clone(),
            loc.line,
            loc.column,
        ))
    }

    /// The stored path of a file by index.
    pub(super) fn path(&self, file_index: usize) -> &str {
        &self.files[file_index].path
    }

    /// Resolve a breakpoint request (`path`, 1-based `line`) to the adjusted
    /// line (the next line with code at or after `line`) and its address.
    #[must_use]
    pub fn resolve_breakpoint(&self, path: &str, line: u32) -> Option<(u32, Address)> {
        let idx = self.find_file(path)?;
        let (&adjusted, &address) = self.files[idx].lines_with_code.range(line..).next()?;
        Some((adjusted, address))
    }

    /// Find a file by its path, matching exactly first, then by base name.
    fn find_file(&self, path: &str) -> Option<usize> {
        if let Some(idx) = self.files.iter().position(|f| f.path == path) {
            return Some(idx);
        }
        let wanted = base_name(path);
        self.files.iter().position(|f| base_name(&f.path) == wanted)
    }
}

/// Compose an address's preprocessed byte range with the preprocessor source
/// map to obtain `(original_file_id, original_range)`.
fn resolve(pre_source_map: &SourceMap, range: &Range<usize>) -> Option<(usize, Range<usize>)> {
    crate::diagnostic::resolve_to_original(pre_source_map, range.clone())
}

fn base_name(path: &str) -> &str {
    path.rsplit(['/', '\\']).next().unwrap_or(path)
}

fn to_u32(value: usize) -> u32 {
    u32::try_from(value).unwrap_or(u32::MAX)
}
