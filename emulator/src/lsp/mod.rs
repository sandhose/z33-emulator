//! LSP (Language Server Protocol) support for Z33 assembly.
//!
//! This module contains the core LSP logic: document analysis, completions,
//! diagnostics, and the `LanguageServer` trait implementation. It uses
//! `tower-lsp` in runtime-agnostic mode so it can compile to WASM.

mod backend;
mod completion;
mod diagnostics;
mod document;
mod hover;
mod position;
mod semantic_tokens;
mod signature;
mod symbols;

pub use tower_lsp;

pub use self::backend::Backend;
pub use self::document::DocumentState;
