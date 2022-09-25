#![forbid(unsafe_code)]
#![deny(clippy::all, clippy::pedantic)]
#![allow(
    clippy::module_name_repetitions,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc
)]

mod ast;
pub mod compiler;
pub mod constants;
pub mod parser;
pub mod preprocessor;
pub mod runtime;

pub use self::{compiler::compile, parser::parse};
