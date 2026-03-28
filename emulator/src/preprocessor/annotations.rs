//! Structured metadata about preprocessor constructs, keyed by byte offsets
//! in the original source file.
//!
//! These annotations are populated during preprocessing and preserved for
//! downstream consumers (LSP, web IDE) that need to reason about preprocessor
//! constructs without re-parsing the source.

use std::ops::Range;

/// All preprocessor annotations for a single file.
#[derive(Debug, Default)]
pub struct SourceAnnotations {
    /// Lines starting with `#` that aren't recognized directives.
    pub comments: Vec<Range<usize>>,

    /// `#define KEY [VALUE]` directives.
    pub definitions: Vec<DefinitionAnnotation>,

    /// `#undef KEY` directives.
    pub undefinitions: Vec<UndefinitionAnnotation>,

    /// `#include "path"` directives.
    pub inclusions: Vec<InclusionAnnotation>,

    /// `#if`/`#elif`/`#else`/`#endif` block structure.
    pub conditional_blocks: Vec<ConditionalBlockAnnotation>,
}

/// A `#define KEY [VALUE]` directive.
#[derive(Debug)]
pub struct DefinitionAnnotation {
    /// Span of the entire `#define ...` line in the original source.
    pub span: Range<usize>,
    /// The macro name.
    pub key: String,
    /// The macro value, if any.
    pub value: Option<String>,
}

/// A `#undef KEY` directive.
#[derive(Debug)]
pub struct UndefinitionAnnotation {
    /// Span of the entire `#undef ...` line.
    pub span: Range<usize>,
    /// The macro name being undefined.
    pub key: String,
}

/// A `#include "path"` directive.
#[derive(Debug)]
pub struct InclusionAnnotation {
    /// Span of the entire `#include ...` line.
    pub span: Range<usize>,
    /// The include path as written in the source.
    pub path: String,
}

/// A `#if`/`#elif`/`#else`/`#endif` block.
#[derive(Debug)]
pub struct ConditionalBlockAnnotation {
    /// Full span from the `#if` line through the `#endif` line.
    pub span: Range<usize>,
    /// The conditional branches (`#if` and any `#elif`s).
    pub branches: Vec<BranchAnnotation>,
    /// The `#else` fallback branch, if any.
    pub fallback: Option<FallbackAnnotation>,
}

/// A single `#if` or `#elif` branch.
#[derive(Debug)]
pub struct BranchAnnotation {
    /// Span of the `#if ...` or `#elif ...` directive line.
    pub directive_span: Range<usize>,
    /// The condition expression text.
    pub condition: String,
    /// Span of the branch body (between this directive and the next).
    pub body_span: Range<usize>,
    /// Whether this branch was taken during preprocessing.
    pub active: bool,
}

/// The `#else` fallback branch.
#[derive(Debug)]
pub struct FallbackAnnotation {
    /// Span of the `#else` directive line.
    pub directive_span: Range<usize>,
    /// Span of the else body.
    pub body_span: Range<usize>,
    /// Whether this branch was taken (no prior branch was active).
    pub active: bool,
}
