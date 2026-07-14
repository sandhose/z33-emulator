//! LSP (Language Server Protocol) support for Z33 assembly.
//!
//! This module contains the core LSP logic: document analysis, completions,
//! diagnostics, and the transport-agnostic [`LspSession`] message handler. It
//! is fully synchronous and I/O-free (mirroring [`crate::dap`]), so it runs
//! unchanged on native stdio and in WASM.

mod completion;
mod diagnostics;
mod document;
mod hover;
mod instructions;
mod position;
mod references;
mod semantic_tokens;
mod session;
mod symbols;
mod text;
mod workspace;

pub use self::document::DocumentState;
pub use self::session::{LspSession, WORKSPACE_FILES_METHOD};

#[cfg(test)]
mod tests;
