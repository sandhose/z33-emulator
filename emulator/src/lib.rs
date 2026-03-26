//! # Z33 Emulator
//!
//! Emulator for the Zorglub-33 (Z33) educational computer architecture.
//!
//! ## Compilation Pipeline
//!
//! The pipeline transforms source files into an executable program through
//! five stages:
//!
//! ```text
//! Input files (raw .S text)
//!     │
//!     ▼ [Preprocessor] Workspace::preprocess()
//! PreprocessResult {
//!     source: String,              // fully expanded flat source
//!     source_map: SourceMap,       // byte offsets → original file locations
//!     preprocessed_file_id: FileId // virtual file in FileDatabase
//! }
//!     │
//!     ▼ [Parser] parse()
//! ParseResult {
//!     program: Located<Program>,       // AST with error recovery nodes
//!     diagnostics: Vec<ParseDiagnostic> // accumulated parse errors
//! }
//!     │
//!     ▼ [Layout] layout_memory()
//! (Layout, Vec<MemoryLayoutError>)
//!     Layout {
//!         labels: BTreeMap<String, Address>,           // symbol → address
//!         memory: HashMap<Address, (Placement, Range)> // address → content + source span
//!     }
//!     │
//!     ▼ [Fill] fill_memory()
//! (Memory, Vec<MemoryFillError>)
//!     Memory: Vec<Cell>  // 10,000 cells: Empty | Word(i64) | Instruction(...)
//!     │
//!     ▼ [Compile] compile()
//! CompileResult {
//!     computer: Option<Computer>,      // only if no errors
//!     debug_info: DebugInfo,           // always available (labels + source map)
//!     errors: Vec<CompilationError>    // all accumulated errors
//! }
//! ```
//!
//! ### Error Recovery
//!
//! Each stage is designed to produce partial results even in the presence of
//! errors:
//!
//! - **Preprocessor**: Fatal — errors here prevent all subsequent stages.
//! - **Parser**: Recovers per-line. Invalid lines become `LineContent::Error`
//!   nodes. Labels on error lines are still extracted. Diagnostics are
//!   accumulated without stopping.
//! - **Layout**: Accumulates errors (duplicate labels, memory overlap) and
//!   continues. `LineContent::Error` nodes are silently skipped (no memory
//!   placement). Labels are always collected.
//! - **Fill**: Accumulates errors (undefined labels, type mismatches) and
//!   continues. Failed cells remain `Cell::Empty`.
//! - **Compile**: Always produces `DebugInfo` (labels + source map). Only
//!   produces a runnable `Computer` if there are zero errors.

mod ast;
pub mod compiler;
pub mod constants;
pub mod diagnostic;
pub mod parser;
pub mod preprocessor;
pub mod runtime;

pub use self::compiler::{check, compile, CompileResult};
pub use self::parser::{parse, ParseResult};
