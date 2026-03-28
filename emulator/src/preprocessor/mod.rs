use std::collections::{BTreeMap, HashMap, HashSet};
use std::ops::Range;
use std::sync::Arc;

use camino::{Utf8Path, Utf8PathBuf};
use thiserror::Error;
use tracing::warn;
use unicode_segmentation::UnicodeSegmentation;

use crate::diagnostic::{FileDatabase, FileId};
use crate::parser::condition::{parse_condition, Context as ConditionContext};

/// Compute the byte offset of `substr` within `base`.
/// Both must be slices of the same underlying string.
fn str_offset(base: &str, substr: &str) -> usize {
    let base_ptr = base.as_ptr() as usize;
    let sub_ptr = substr.as_ptr() as usize;
    debug_assert!(
        sub_ptr >= base_ptr && sub_ptr <= base_ptr + base.len(),
        "substr is not a subslice of base"
    );
    sub_ptr - base_ptr
}
use crate::parser::expression::{EmptyContext as EmptyExpressionContext, EvaluationError};
use crate::parser::location::{Locatable, Located};
use crate::parser::preprocessor::{parse, Node};

mod annotations;
mod fs;

pub use self::annotations::SourceAnnotations;
pub use self::fs::{Filesystem, InMemoryFilesystem, NativeFilesystem};

struct ProcessedFile {
    file_id: FileId,
    content: Result<FileContent, String>,
}

struct FileContent {
    chunks: Vec<Located<Node>>,
    references: HashMap<Utf8PathBuf, Utf8PathBuf>,
}

#[derive(Debug, Default)]
pub struct SourceMap {
    // A map of spans, from their starting position to the span
    spans: BTreeMap<usize, Span>,
}

impl SourceMap {
    #[must_use]
    pub fn find(&self, position: usize) -> Option<&Span> {
        self.spans
            .range(..=position)
            .next_back()
            .map(|(_, span)| span)
    }

    /// Like [`find`](Self::find), but also returns the chunk start offset.
    ///
    /// This is needed to compute the exact position within the original file:
    /// `original_offset = span.range.start + (position - chunk_key)`
    #[must_use]
    pub fn find_with_key(&self, position: usize) -> Option<(usize, &Span)> {
        self.spans
            .range(..=position)
            .next_back()
            .map(|(&key, span)| (key, span))
    }

    /// Return the chunk start offset for the chunk containing `position`.
    #[must_use]
    pub fn chunk_key(&self, position: usize) -> Option<usize> {
        self.spans
            .range(..=position)
            .next_back()
            .map(|(&key, _)| key)
    }
}

#[derive(Clone)]
pub struct ReferencingSourceMap {
    spans: BTreeMap<usize, Span>,
}

impl ReferencingSourceMap {
    #[must_use]
    pub fn find(&self, position: usize) -> Option<&Span> {
        self.spans
            .range(..=position)
            .next_back()
            .map(|(_, span)| span)
    }

    /// Like [`find`](Self::find), but also returns the chunk start offset.
    ///
    /// This is needed to compute the exact position within the original file:
    /// `original_offset = span.range.start + (position - chunk_key)`
    #[must_use]
    pub fn find_with_key(&self, position: usize) -> Option<(usize, &Span)> {
        self.spans
            .range(..=position)
            .next_back()
            .map(|(&key, span)| (key, span))
    }
}

impl From<SourceMap> for ReferencingSourceMap {
    fn from(value: SourceMap) -> Self {
        Self { spans: value.spans }
    }
}

#[derive(Debug, Clone)]
pub struct Span {
    pub file_id: FileId,
    pub range: Range<usize>,
}

impl Span {
    fn push<'t, T>(&self, node: &'t Located<T>) -> (Span, &'t T) {
        let range = Range {
            start: self.range.start + node.location.start,
            end: self.range.start + node.location.end,
        };

        if range.start > self.range.end || range.end > self.range.end {
            warn!("span out of bounds");

            // In debug mode, panic, because this should not happen
            #[cfg(debug_assertions)]
            panic!("span out of bounds");
        }

        let new = Span {
            file_id: self.file_id,
            range,
        };

        (new, &node.inner)
    }
}

/// The result of preprocessing a workspace.
#[derive(Debug)]
pub struct PreprocessResult {
    /// Maps byte offsets in the preprocessed output back to original files.
    pub source_map: SourceMap,
    /// The fully preprocessed source text.
    pub source: String,
    /// A [`FileId`] for the virtual preprocessed output, registered in the
    /// workspace's [`FileDatabase`].
    pub preprocessed_file_id: FileId,
    /// Structured metadata about preprocessor constructs, keyed by original
    /// source byte offsets.
    pub annotations: SourceAnnotations,
}

/// A [`Workspace`] holds parsed files loaded from a filesystem
pub struct Workspace {
    entrypoint: Utf8PathBuf,
    files: HashMap<Utf8PathBuf, Result<ProcessedFile, Arc<std::io::Error>>>,
    file_db: FileDatabase,
}

impl Workspace {
    /// Create a new [`Workspace`] and load the entrypoint from the given
    /// [`Filesystem`]
    pub fn new<FS: Filesystem, P: AsRef<Utf8Path>>(fs: &FS, entrypoint: P) -> Self {
        let entrypoint = fs.relative(None, entrypoint.as_ref());
        let mut this = Self {
            entrypoint: entrypoint.clone(),
            files: HashMap::new(),
            file_db: FileDatabase::new(),
        };

        this.load(fs, &entrypoint);

        this
    }

    /// Get a reference to the file database.
    #[must_use]
    pub fn file_db(&self) -> &FileDatabase {
        &self.file_db
    }

    /// Consume the workspace and return the file database.
    #[must_use]
    pub fn into_file_db(self) -> FileDatabase {
        self.file_db
    }

    fn load<FS: Filesystem>(&mut self, fs: &FS, path: &Utf8Path) -> &mut Self {
        // If we already loaded the path, return early
        if self.files.contains_key(path) {
            return self;
        }

        // Try to read the file from the FS
        let source = match fs.read(path) {
            Ok(s) => s,
            Err(e) => {
                // If it failed, store the error
                self.files.insert(path.to_owned(), Err(Arc::new(e)));
                return self;
            }
        };

        // Register the file in the database
        let file_id = self.file_db.add(path.to_string(), source.clone());

        // Parse the file content
        let source_str = source.as_str();
        let content = parse(source_str).map(|chunks| {
            // We parsed the chunks, now we find inclusions and resolve them
            let mut references = HashMap::new();
            for chunk in &chunks {
                chunk.inner.walk(&mut |node| {
                    if let Node::Inclusion { path: inclusion } = node {
                        let reference = Utf8PathBuf::from(inclusion.inner.clone());
                        let resolved = fs.relative(Some(path), &reference);
                        references.insert(reference, resolved);
                    }
                });
            }
            FileContent { chunks, references }
        });

        // Copy the list of files to load, as it is annoying to borrow it
        // after insert
        let to_load: HashSet<_> = content
            .as_ref()
            .iter()
            .flat_map(|c| c.references.values())
            .cloned()
            .collect();

        // Save the file
        let file = ProcessedFile { file_id, content };
        self.files.insert(path.to_owned(), Ok(file));

        // And load recursively files referenced by it
        for path in to_load {
            self.load(fs, &path);
        }

        self
    }

    /// Preprocess a file, given the file name
    ///
    /// # Errors
    ///
    /// This function will return an error if the file cannot be preprocessed,
    /// like if it has invalid syntax, can't be opened, includes an invalid
    /// file, etc.
    pub fn preprocess(&mut self) -> Result<PreprocessResult, PreprocessorError> {
        let mut ctx = Context::default();
        let entrypoint = self.entrypoint.clone();
        let chunks = self.preprocess_path(&entrypoint, &mut ctx)?;
        let (string, source_map, _) = chunks.into_iter().fold(
            (String::new(), SourceMap::default(), 0),
            |(mut string, mut source_map, offset), (stack, content)| {
                string.push_str(&content);
                source_map.spans.insert(offset, stack);

                (string, source_map, offset + content.len())
            },
        );

        let preprocessed_file_id = self
            .file_db
            .add("<preprocessed>".to_string(), string.clone());

        Ok(PreprocessResult {
            source_map,
            source: string,
            preprocessed_file_id,
            annotations: SourceAnnotations::default(),
        })
    }

    fn preprocess_path(
        &self,
        path: &Utf8Path,
        ctx: &mut Context,
    ) -> Result<Vec<(Span, String)>, PreprocessorError> {
        let Some(file) = self.files.get(path) else {
            // The file is not loaded, this shouldn't happen
            unreachable!("file {path} is not loaded");
        };

        let file = file.as_ref().map_err(|err| PreprocessorError::LoadFile {
            path: path.to_owned(),
            inner: err.clone(),
        })?;

        let source_len = self.file_db.source(file.file_id).len();
        let stack = Span {
            file_id: file.file_id,
            range: 0..source_len,
        };

        let content = file
            .content
            .as_ref()
            .map_err(|err| PreprocessorError::ParseFile {
                file_id: file.file_id,
                inner: vec![err.clone()],
            })?;

        let mut buf = Vec::new();
        self.process_chunks(&mut buf, &content.chunks, &content.references, ctx, &stack)?;
        Ok(buf)
    }

    fn process_chunks(
        &self,
        buf: &mut Vec<(Span, String)>,
        chunks: &[Located<Node>],
        references: &HashMap<Utf8PathBuf, Utf8PathBuf>,
        ctx: &mut Context,
        stack: &Span,
    ) -> Result<(), PreprocessorError> {
        'outer: for chunk in chunks {
            let (stack, chunk) = stack.push(chunk);

            match chunk {
                Node::Raw { ref content } => {
                    // Replace the definitions in the content
                    let replaced = ctx.replace(content);
                    buf.extend(replaced.into_iter().map(|l| {
                        let (stack, content) = stack.push(&l);
                        (stack, (*content).to_owned())
                    }));
                }

                Node::NewLine { crlf } => {
                    if *crlf {
                        buf.push((stack, "\r\n".to_owned()));
                    } else {
                        buf.push((stack, "\n".to_owned()));
                    }
                }

                // r[impl asm.preprocessor.error]
                Node::Error { ref message } => {
                    let (stack, message) = stack.push(message);

                    // Return the user-defined error
                    return Err(PreprocessorError::UserError {
                        file_id: stack.file_id,
                        span: stack.range.clone(),
                        message: message.clone(),
                    });
                }

                // r[impl asm.preprocessor.undefine]
                Node::Undefine { ref key } => {
                    let (_stack, key) = stack.push(key);

                    // Remove a definition
                    ctx.undefine(key);
                }

                // r[impl asm.preprocessor.define]
                Node::Definition {
                    ref key,
                    ref content,
                } => {
                    // Add a definition
                    let (_stack, key) = stack.push(key);
                    let content = content.as_ref().map(|i| &i.inner);
                    // First replace existing definitions in the content
                    let content =
                        content.map(|c| ctx.replace(c).into_iter().map(|l| l.inner).collect());
                    // Then add the definition
                    ctx.define(key.clone(), content);
                }

                // r[impl asm.preprocessor.include]
                Node::Inclusion { path: ref include } => {
                    // Include a file
                    let (stack, include) = stack.push(include);
                    let path = Utf8Path::new(include);

                    // Resolve the path
                    let Some(resolved) = references.get(path) else {
                        // The file references wasn't resolved, this shouldn't
                        // happen
                        unreachable!("Referenced file wasn't resolved")
                    };

                    // Then process it
                    let content = self.preprocess_path(resolved, ctx).map_err(|inner| {
                        PreprocessorError::InInclude {
                            file_id: stack.file_id,
                            span: stack.range.clone(),
                            inner: Box::new(inner),
                        }
                    })?;

                    buf.extend(content);
                }

                // r[impl asm.preprocessor.conditional]
                Node::Condition { branches, fallback } => {
                    for branch in branches {
                        let (condition_stack, condition) = stack.push(&branch.condition);
                        let condition: String = ctx
                            .replace_for_if_expression(condition)
                            .into_iter()
                            .map(|l| l.inner)
                            .collect();

                        let condition = condition.as_str();
                        let expression = parse_condition(condition).map_err(|err_msg| {
                            PreprocessorError::ConditionParse {
                                file_id: condition_stack.file_id,
                                span: condition_stack.range.clone(),
                                inner: err_msg,
                            }
                        })?;

                        let result = expression.evaluate(ctx).map_err(|inner| {
                            PreprocessorError::ConditionEvaluation {
                                file_id: condition_stack.file_id,
                                span: condition_stack.range.clone(),
                                inner,
                            }
                        })?;

                        if result {
                            let (stack, body) = stack.push(&branch.body);
                            self.process_chunks(buf, body, references, ctx, &stack)?;
                            continue 'outer;
                        }
                    }

                    if let Some(ref body) = fallback {
                        let (stack, body) = stack.push(body);
                        self.process_chunks(buf, body, references, ctx, &stack)?;
                    }
                }
            }
        }

        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum PreprocessorError {
    #[error("Failed to process included file")]
    InInclude {
        file_id: FileId,
        span: Range<usize>,
        #[source]
        inner: Box<PreprocessorError>,
    },

    #[error("Failed to load file {path:?}")]
    LoadFile {
        path: Utf8PathBuf,
        #[source]
        inner: Arc<std::io::Error>,
    },

    #[error("Failed to parse file")]
    ParseFile { file_id: FileId, inner: Vec<String> },

    #[error("User error: {message}")]
    UserError {
        file_id: FileId,
        span: Range<usize>,
        message: String,
    },

    #[error("Invalid syntax in condition")]
    ConditionParse {
        file_id: FileId,
        span: Range<usize>,
        inner: String,
    },

    #[error("Could not evaluate condition")]
    ConditionEvaluation {
        file_id: FileId,
        span: Range<usize>,
        #[source]
        inner: EvaluationError,
    },
}

#[derive(Default)]
struct Context {
    definitions: HashMap<String, Option<String>>,
}

impl ConditionContext for Context {
    type ExpressionContext = EmptyExpressionContext;

    fn get_expression_context(&self) -> &Self::ExpressionContext {
        &EmptyExpressionContext
    }

    fn is_defined(&self, variable: &str) -> bool {
        // XXX: this does not work. Because we replace in #if expressions with
        // definitions first, `defined(XXXX)` become `defined()`, which will
        // never work
        self.definitions.contains_key(variable)
    }
}

/// Check if the given string is full of spaces
fn is_space(s: &str) -> bool {
    s.chars().all(|c| c.is_ascii_whitespace())
}

/// Check if the given input is a valid `defined(XXXX)` expression
fn valid_defined_expr(input: &[&str]) -> bool {
    let mut iter = input.iter().peekable();
    let Some(&"defined") = iter.next() else {
        return false;
    };

    if iter.peek().is_some_and(|&token| is_space(token)) {
        // Skip the space
        iter.next();
    }

    let Some(&"(") = iter.next() else {
        return false;
    };

    if iter.peek().is_some_and(|&token| is_space(token)) {
        // Skip the space
        iter.next();
    }

    let Some(&token) = iter.next() else {
        return false;
    };

    if !token.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
        return false;
    }

    if iter.peek().is_some_and(|&token| is_space(token)) {
        // Skip the space
        iter.next();
    }

    let Some(&")") = iter.next() else {
        return false;
    };

    true
}

impl Context {
    fn define(&mut self, key: String, content: Option<String>) {
        self.definitions.insert(key, content);
    }

    fn undefine(&mut self, key: &str) {
        self.definitions.remove(key);
    }

    fn replace_for_if_expression<'a>(&'a self, input: &'a str) -> Vec<Located<&'a str>> {
        // We first store words in a Vec, so that it's easier to peek for
        // next/previous entries
        let words: Vec<&str> = input.split_word_bounds().collect();

        let mut result = Vec::with_capacity(words.len());

        let mut ignore_during_defined = false;
        for (index, word) in words.iter().enumerate() {
            if valid_defined_expr(&words[index..]) {
                // Go to ignore mode, until we find the closing parenthesis
                ignore_during_defined = true;
            }

            if *word == ")" {
                ignore_during_defined = false;
            }

            let location = str_offset(input, word)..word.len();
            if ignore_during_defined {
                result.push((*word).with_location(location));
            } else {
                let replaced = match self.definitions.get(*word) {
                    Some(Some(r)) => r.as_str(),
                    Some(None) => continue,
                    None => *word,
                };
                result.push(replaced.with_location(location));
            }
        }

        if ignore_during_defined {
            // We got an invalid `defined` expression but still detected it?
            unreachable!("Invalid `defined` expression");
        }

        result
    }

    fn replace<'a>(&'a self, input: &'a str) -> Vec<Located<&'a str>> {
        input
            .split_word_bounds()
            .filter_map(|word| {
                let location = str_offset(input, word)..word.len();
                let replaced = match self.definitions.get(word) {
                    Some(Some(r)) => r,
                    Some(None) => return None,
                    None => word,
                };
                Some(replaced.with_location(location))
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_snapshot;

    use super::*;
    use crate::diagnostic::{preprocessor_error_to_diagnostics, render_to_string};

    fn fs() -> InMemoryFilesystem {
        InMemoryFilesystem::new({
            let mut t = HashMap::new();
            t.insert(
                "/inclusion.S".into(),
                indoc::indoc! {r#"
                    this is before foo.S
                    #include "foo.S"
                    this is after foo.S
                "#}
                .into(),
            );
            t.insert("/foo.S".into(), "this is foo.S".into());
            t.insert(
                "/error.S".into(),
                indoc::indoc! {r#"
                    #error "custom"
                "#}
                .into(),
            );
            t.insert(
                "/define.S".into(),
                indoc::indoc! {r"
                    #define FOO world
                    hello FOO
                    helloFOO
                    #undefine FOO
                    hello FOO
                "}
                .into(),
            );
            t.insert(
                "/double-define.S".into(),
                indoc::indoc! {r"
                    #define FOO world
                    #define WHAT hello FOO
                    #define FOO toto
                    WHAT
                "}
                .into(),
            );
            t.insert(
                "/condition.S".into(),
                indoc::indoc! {r"
                    #if true
                    simple
                    #endif

                    #if false
                    no fallback
                    #endif

                    #if false
                    nothing
                    #else
                    fallback
                    #endif

                    #define FOO true
                    #if FOO
                    definition
                    #endif

                    #if true
                    #if true
                    nested
                    #endif
                    #endif
                "}
                .into(),
            );
            t
        })
    }

    fn preprocess<P: AsRef<Utf8Path>>(path: P) -> Result<String, PreprocessorError> {
        let fs = fs();
        let mut workspace = Workspace::new(&fs, path.as_ref());
        let result = workspace.preprocess()?;
        Ok(result.source)
    }

    fn render_error(workspace: &Workspace, err: &PreprocessorError) -> String {
        let diagnostics = preprocessor_error_to_diagnostics(err);
        diagnostics
            .iter()
            .map(|d| render_to_string(d, workspace.file_db()))
            .collect::<Vec<_>>()
            .join("\n")
    }

    #[test]
    fn inclusion_test() {
        let res = preprocess("/inclusion.S").unwrap();
        assert_snapshot!(res);
    }

    #[test]
    fn condition_test() {
        let res = preprocess("/condition.S").unwrap();
        assert_snapshot!(res);
    }

    #[test]
    fn definition_test() {
        let res = preprocess("/define.S").unwrap();
        assert_snapshot!(res);

        let res = preprocess("/double-define.S").unwrap();
        assert_snapshot!(res);
    }

    #[test]
    fn if_defined_test() {
        let fs = InMemoryFilesystem::new([(
            "/defined.S".into(),
            indoc::indoc! {r"
                #define FOO
                #if defined(FOO)
                _FOO_ is defined
                #endif

                #if defined(BAR)
                _BAR_ is defined
                #endif
            "}
            .into(),
        )]);
        let mut workspace = Workspace::new(&fs, "/defined.S");
        let result = workspace.preprocess();
        let result = result.expect("preprocessed");
        assert_snapshot!(result.source);
    }

    #[test]
    fn user_error_test() {
        let res = preprocess("/error.S");
        let err = res.expect_err("an error");
        if let PreprocessorError::UserError { message, .. } = err {
            assert_eq!(message, "custom".to_string());
        } else {
            panic!("not a UserError");
        }
    }

    #[test]
    fn unknown_include_error_test() {
        let fs = InMemoryFilesystem::new([("/include.S".into(), r#"#include "foo.S""#.into())]);
        let mut workspace = Workspace::new(&fs, "/include.S");
        let res = workspace.preprocess();
        let err = res.expect_err("should have failed");
        let report = render_error(&workspace, &err);
        insta::assert_snapshot!(report);
    }

    #[test]
    fn user_error_in_include_test() {
        let fs = InMemoryFilesystem::new([
            ("/include.S".into(), r#"#include "error.S""#.into()),
            ("/error.S".into(), r#"#error "message""#.into()),
        ]);
        let mut workspace = Workspace::new(&fs, "/include.S");
        let res = workspace.preprocess();
        let err = res.expect_err("should have failed");
        let report = render_error(&workspace, &err);
        insta::assert_snapshot!(report);
    }

    #[test]
    fn parse_error_in_include_test() {
        let fs = InMemoryFilesystem::new([
            ("/include.S".into(), r#"#include "invalid.S""#.into()),
            (
                "/invalid.S".into(),
                indoc::indoc! {r"
                    hello
                    #else
                "}
                .into(),
            ),
        ]);
        let mut workspace = Workspace::new(&fs, "/include.S");
        let res = workspace.preprocess();
        let err = res.expect_err("should have failed");
        let report = render_error(&workspace, &err);
        insta::assert_snapshot!(report);
    }

    #[test]
    fn condition_parse_error_test() {
        let fs = InMemoryFilesystem::new([(
            "/invalid.S".into(),
            indoc::indoc! {r"
                    hello
                    #if (1 + 5
                    #endif
                "}
            .into(),
        )]);
        let mut workspace = Workspace::new(&fs, "/invalid.S");
        let res = workspace.preprocess();
        let err = res.expect_err("should have failed");
        let report = render_error(&workspace, &err);
        insta::assert_snapshot!(report);
    }
}
