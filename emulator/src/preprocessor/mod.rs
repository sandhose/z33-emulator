use std::collections::{BTreeMap, HashMap, HashSet};
use std::ops::Range;
use std::sync::Arc;

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Diagnostic, NamedSource, SourceSpan};
use nom::combinator::all_consuming;
use nom::{Finish, Offset};
use thiserror::Error;
use tracing::warn;
use unicode_segmentation::UnicodeSegmentation;

use crate::parser::condition::{parse_condition, Context as ConditionContext};
use crate::parser::expression::{EmptyContext as EmptyExpressionContext, EvaluationError};
use crate::parser::location::{Locatable, Located};
use crate::parser::preprocessor::{parse, Node};

mod fs;

pub use self::fs::{Filesystem, InMemoryFilesystem, NativeFilesystem};

#[derive(Debug, Error, Clone)]
#[error(transparent)]
pub struct SharedParseError {
    inner: Arc<dyn Diagnostic + Send + Sync>,
}

// It's annoying, but miette doesn't look like it's implementing Diagnostic for
// many containers, including Box and Arc, so we have to do it manually if we
// want to share the errors and copy it arround
impl Diagnostic for SharedParseError {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.inner.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.inner.severity()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.inner.help()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.inner.url()
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        self.inner.source_code()
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.inner.labels()
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.inner.related()
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.inner.diagnostic_source()
    }
}

struct ProcessedFile {
    source: NamedSource<String>,
    content: Result<FileContent, SharedParseError>,
}

struct FileContent {
    chunks: Vec<Located<Node>>,
    references: HashMap<Utf8PathBuf, Utf8PathBuf>,
}

#[derive(Debug, Default)]
pub struct SourceMap<'a> {
    // A map of spans, from their starting position to the span
    spans: BTreeMap<usize, Span<'a>>,
}

impl SourceMap<'_> {
    #[must_use]
    pub fn find(&self, position: usize) -> Option<&Span<'_>> {
        self.spans
            .range(..=position)
            .next_back()
            .map(|(_, span)| span)
    }
}

#[derive(Debug)]
pub struct Span<'a> {
    pub source: &'a NamedSource<String>,
    pub span: Range<usize>,
}

impl<'a> Span<'a> {
    fn push<'t, T>(&self, node: &'t Located<T>) -> (Span<'a>, &'t T) {
        let span = Range {
            start: self.span.start + node.location.start,
            end: self.span.start + node.location.end,
        };

        if span.start > self.span.end || span.end > self.span.end {
            warn!("span out of bounds");

            // In debug mode, panic, because this should not happen
            #[cfg(debug_assertions)]
            panic!("span out of bounds");
        }

        let new = Span {
            source: self.source,
            span,
        };

        (new, &node.inner)
    }

    fn context(&self) -> (NamedSource<String>, SourceSpan) {
        (self.source.clone(), self.span.clone().into())
    }
}

/// A [`Workspace`] holds parsed files loaded from a filesystem
pub struct Workspace {
    entrypoint: Utf8PathBuf,
    files: HashMap<Utf8PathBuf, Result<ProcessedFile, Arc<std::io::Error>>>,
}

impl Workspace {
    /// Create a new [`Workspace`] and load the entrypoint from the given
    /// [`Filesystem`]
    pub fn new<FS: Filesystem, P: AsRef<Utf8Path>>(fs: &FS, entrypoint: P) -> Self {
        let entrypoint = fs.relative(None, entrypoint.as_ref());
        let mut this = Self {
            entrypoint: entrypoint.clone(),
            files: HashMap::new(),
        };

        this.load(fs, &entrypoint);

        this
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

        // Parse the file content
        let source_str = source.as_str();
        let content = all_consuming(parse)(source_str)
            .finish()
            .map(|(_rest, chunks)| {
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
            })
            .map_err(|e: crate::parser::Error<&str>| {
                let inner = e.to_miette_diagnostic(&source_str, 0).into();
                SharedParseError { inner }
            });

        // Copy the list of files to load, as it is annoying to borrow it after insert
        let to_load: HashSet<_> = content
            .as_ref()
            .iter()
            .flat_map(|c| c.references.values())
            .cloned()
            .collect();

        // Save the file
        let source = NamedSource::new(path, source);
        let file = ProcessedFile { source, content };
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
    pub fn preprocess(&self) -> Result<(SourceMap, String), PreprocessorError> {
        let mut ctx = Context::default();
        let chunks = self.preprocess_path(&self.entrypoint, &mut ctx)?;
        let (string, source_map, _) = chunks.into_iter().fold(
            (String::new(), SourceMap::default(), 0),
            |(mut string, mut source_map, offset), (stack, content)| {
                string.push_str(&content);
                source_map.spans.insert(offset, stack);

                (string, source_map, offset + content.len())
            },
        );

        Ok((source_map, string))
    }

    fn preprocess_path(
        &self,
        path: &Utf8Path,
        ctx: &mut Context,
    ) -> Result<Vec<(Span<'_>, String)>, PreprocessorError> {
        let Some(file) = self.files.get(path) else {
            // The file is not loaded, this shouldn't happen
            unreachable!("file {path} is not loaded");
        };

        let file = file.as_ref().map_err(|err| PreprocessorError::LoadFile {
            path: path.to_owned(),
            inner: err.clone(),
        })?;

        let stack = Span {
            source: &file.source,
            span: 0..file.source.inner().len(),
        };

        let content = file.content.as_ref().map_err(|err| {
            let (src, _span) = stack.context();
            PreprocessorError::ParseFile {
                src,
                inner: vec![err.clone()],
            }
        })?;

        let mut buf = Vec::new();
        self.process_chunks(&mut buf, &content.chunks, &content.references, ctx, &stack)?;
        Ok(buf)
    }

    fn process_chunks<'a>(
        &'a self,
        buf: &mut Vec<(Span<'a>, String)>,
        chunks: &[Located<Node>],
        references: &HashMap<Utf8PathBuf, Utf8PathBuf>,
        ctx: &mut Context,
        stack: &Span<'a>,
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

                Node::Error { ref message } => {
                    let (stack, message) = stack.push(message);
                    let (src, span) = stack.context();

                    // Return the user-defined error
                    return Err(PreprocessorError::UserError {
                        src,
                        span,
                        message: message.clone(),
                    });
                }

                Node::Undefine { ref key } => {
                    let (_stack, key) = stack.push(key);

                    // Remove a definition
                    ctx.undefine(key);
                }

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

                Node::Inclusion { path: ref include } => {
                    // Include a file
                    let (stack, include) = stack.push(include);
                    let path = Utf8Path::new(include);

                    // Resolve the path
                    let Some(resolved) = references.get(path) else {
                        // The file references wasn't resolved, this shouldn't happen
                        unreachable!("Referenced file wasn't resolved")
                    };

                    // Then process it
                    let content = self.preprocess_path(resolved, ctx).map_err(|inner| {
                        let (src, span) = stack.context();
                        PreprocessorError::InInclude {
                            src,
                            span,
                            inner: Box::new(inner),
                        }
                    })?;

                    buf.extend(content);
                }

                Node::Condition { branches, fallback } => {
                    for branch in branches {
                        let (condition_stack, condition) = stack.push(&branch.condition);
                        let condition: String = ctx
                            .replace_for_if_expression(condition)
                            .into_iter()
                            .map(|l| l.inner)
                            .collect();

                        let condition = condition.as_str();
                        let (_, expression) = parse_condition(condition).finish().map_err(
                            |err: crate::parser::Error<&str>| {
                                let (src, span) = condition_stack.context();
                                let inner = err.to_miette_diagnostic(&condition, span.offset());
                                PreprocessorError::ConditionParse { src, span, inner }
                            },
                        )?;

                        let result = expression.evaluate(ctx).map_err(|inner| {
                            let (src, span) = condition_stack.context();
                            PreprocessorError::ConditionEvaluation { src, span, inner }
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

#[derive(Error, Debug, Diagnostic)]
pub enum PreprocessorError {
    #[error("Failed to process included file")]
    InInclude {
        #[source_code]
        src: NamedSource<String>,

        #[label("File included here")]
        span: SourceSpan,

        #[source]
        #[diagnostic_source]
        inner: Box<dyn Diagnostic + Send + Sync>,
    },

    #[error("Failed to load file {path:?}")]
    LoadFile {
        path: Utf8PathBuf,

        #[source]
        inner: Arc<std::io::Error>,
    },

    #[error("Failed to parse file")]
    ParseFile {
        #[source_code]
        src: NamedSource<String>,

        #[related]
        inner: Vec<SharedParseError>,
    },

    #[error("User error: {message}")]
    UserError {
        #[source_code]
        src: NamedSource<String>,

        #[label = "'#error' preprocessor directive used here"]
        span: SourceSpan,

        message: String,
    },

    #[error("Invalid syntax in condition")]
    ConditionParse {
        #[source_code]
        src: NamedSource<String>,

        #[label]
        span: SourceSpan,

        #[source]
        #[diagnostic_source]
        inner: Box<dyn Diagnostic + Send + Sync>,
    },

    #[error("Could not evaluate condition")]
    ConditionEvaluation {
        #[source_code]
        src: NamedSource<String>,

        #[label("{inner}")]
        span: SourceSpan,

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
        // definitions first, `defined(XXXX)` become `defined()`, which will never work
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

    if iter.peek().map_or(false, |&token| is_space(token)) {
        // Skip the space
        iter.next();
    }

    let Some(&"(") = iter.next() else {
        return false;
    };

    if iter.peek().map_or(false, |&token| is_space(token)) {
        // Skip the space
        iter.next();
    }

    let Some(&token) = iter.next() else {
        return false;
    };

    if !token.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
        return false;
    }

    if iter.peek().map_or(false, |&token| is_space(token)) {
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
        // We first store words in a Vec, so that it's easier to peek for next/previous
        // entries
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

            let location = input.offset(word)..word.len();
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
                let location = input.offset(word)..word.len();
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
    use miette::Report;

    use super::*;

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
        let preprocessor = Workspace::new(&fs, path.as_ref());
        let (_source_map, res) = preprocessor.preprocess()?;
        Ok(res)
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
        let workspace = Workspace::new(&fs, "/defined.S");
        let res = workspace.preprocess();
        let (_source_map, res) = res.expect("preprocessed");
        assert_snapshot!(res);
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

    fn install_handler() {
        let _ = miette::set_hook(Box::new(|_| {
            Box::new(
                miette::MietteHandlerOpts::new()
                    .force_graphical(false)
                    .force_narrated(false)
                    .color(false)
                    .terminal_links(false)
                    .with_cause_chain()
                    .unicode(false)
                    .build(),
            )
        }));
    }

    #[test]
    fn unknown_include_error_test() {
        install_handler();
        let fs = InMemoryFilesystem::new([("/include.S".into(), r#"#include "foo.S""#.into())]);
        let workspace = Workspace::new(&fs, "/include.S");
        let res = workspace.preprocess();
        let err = res.expect_err("should have failed");
        let report = Report::new(err);
        let report = format!("{report:?}");
        insta::assert_snapshot!(report);
    }

    #[test]
    fn user_error_in_include_test() {
        install_handler();
        let fs = InMemoryFilesystem::new([
            ("/include.S".into(), r#"#include "error.S""#.into()),
            ("/error.S".into(), r#"#error "message""#.into()),
        ]);
        let workspace = Workspace::new(&fs, "/include.S");
        let res = workspace.preprocess();
        let err = res.expect_err("should have failed");
        let report = Report::new(err);
        let report = format!("{report:?}");
        insta::assert_snapshot!(report);
    }

    #[test]
    fn parse_error_in_include_test() {
        install_handler();
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
        let workspace = Workspace::new(&fs, "/include.S");
        let res = workspace.preprocess();
        let err = res.expect_err("should have failed");
        let report = Report::new(err);
        let report = format!("{report:?}");
        insta::assert_snapshot!(report);
    }

    #[test]
    fn condition_parse_error_test() {
        install_handler();
        let fs = InMemoryFilesystem::new([(
            "/invalid.S".into(),
            indoc::indoc! {r"
                    hello
                    #if (1 + 5
                    #endif
                "}
            .into(),
        )]);
        let workspace = Workspace::new(&fs, "/invalid.S");
        let res = workspace.preprocess();
        let err = res.expect_err("should have failed");
        let report = Report::new(err);
        let report = format!("{report:?}");
        insta::assert_snapshot!(report);
    }
}
