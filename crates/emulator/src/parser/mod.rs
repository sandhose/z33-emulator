//! Program parsing logic
//!
//! This module contains the chumsky-based parsers for Z33 assembly and
//! preprocessor directives.

pub mod assembly;
pub(crate) mod condition;
pub(crate) mod expression;
pub(crate) mod line;
pub mod location;
mod precedence;
pub(crate) mod preprocessor;
pub mod shared;
pub(crate) mod value;

pub use assembly::{parse, ParseResult};
pub use expression::{Context as ExpressionContext, Node as ExpressionNode};
pub use line::Program;
pub use shared::{DiagnosticSeverity, ParseDiagnostic};
