use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::sync::Arc;

use camino::{Utf8Path, Utf8PathBuf};
use miette::NamedSource;
use nom::combinator::all_consuming;
use nom::error::convert_error;
use nom::{Finish, Offset};
use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;

use crate::parser::condition::{
    parse_condition, Context as ConditionContext, EvaluationError as ConditionEvaluationError,
};
use crate::parser::expression::EmptyContext as EmptyExpressionContext;
use crate::parser::location::{Locatable, Located};
use crate::parser::preprocessor::{parse, Node};

mod fs;

pub use fs::{Filesystem, InMemoryFilesystem, NativeFilesystem};

#[derive(Debug, Error, Clone)]
#[error("parse error: {message}")]
pub struct ParseError {
    message: String,
}

struct ProcessedFile {
    #[allow(dead_code)]
    source: NamedSource<String>,
    content: Result<FileContent, ParseError>,
}

struct FileContent {
    chunks: Vec<Located<Node>>,
    references: HashMap<Utf8PathBuf, Utf8PathBuf>,
}

/// A [`Workspace`] holds parsed files loaded from a filesystem
pub struct Workspace {
    entrypoint: Utf8PathBuf,
    files: HashMap<Utf8PathBuf, Result<ProcessedFile, Arc<std::io::Error>>>,
}

impl Workspace {
    /// Create a new [`Workspace`] and load the entrypoint from the given
    /// [`Filesystem`]
    pub fn new<FS: Filesystem>(fs: &FS, entrypoint: Utf8PathBuf) -> Self {
        let mut this = Self {
            entrypoint,
            files: HashMap::new(),
        };

        let path = fs.relative(None, &this.entrypoint);
        this.load(fs, &path);

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
        let content = all_consuming(parse)(source.as_str())
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
            .map_err(|e| ParseError {
                message: convert_error(source.as_str(), e),
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
    pub fn preprocess(&self) -> Result<String, PreprocessorError> {
        let mut ctx = Context::default();
        let chunks = self.preprocess_path(&self.entrypoint, &mut ctx)?;

        Ok(chunks.join(""))
    }

    fn preprocess_path(
        &self,
        path: &Utf8Path,
        ctx: &mut Context,
    ) -> Result<Vec<String>, PreprocessorError> {
        let Some(file) = self.files.get(path) else {
            // The file is not loaded, this shouldn't happen
            todo!()
        };

        let Ok(file) = file else {
            // The file had an error loading it
            todo!()
        };

        let Ok(content) = &file.content else {
            // The file had an error parsing it
            todo!()
        };

        let mut buf = Vec::new();
        self.process_chunks(&mut buf, &content.chunks, &content.references, ctx)?;
        Ok(buf)
    }

    fn process_chunks(
        &self,
        buf: &mut Vec<String>,
        chunks: &[Located<Node>],
        references: &HashMap<Utf8PathBuf, Utf8PathBuf>,
        ctx: &mut Context,
    ) -> Result<(), PreprocessorError> {
        'outer: for chunk in chunks {
            match &chunk.inner {
                Node::Raw { ref content } => {
                    // Replace the definitions in the content
                    let replaced = ctx.replace(content);
                    buf.extend(replaced.into_iter().map(|l| l.inner.to_owned()));
                    buf.push("\n".to_owned());
                }

                Node::Error { ref message } => {
                    // Return the user-defined error
                    return Err(PreprocessorError::UserError {
                        message: message.inner.clone(),
                        location: message.location.clone(),
                    });
                }

                Node::Undefine { ref key } => {
                    // Remove a definition
                    let key = &key.inner;
                    ctx.undefine(key);
                }

                Node::Definition {
                    ref key,
                    ref content,
                } => {
                    // Add a definition
                    let key = key.inner.clone();
                    let content = content.as_ref().map(|i| &i.inner);
                    // First replace existing definitions in the content
                    let content =
                        content.map(|c| ctx.replace(c).into_iter().map(|l| l.inner).collect());
                    // Then add the definition
                    ctx.define(key, content);
                }

                Node::Inclusion { path: ref include } => {
                    // Include a file
                    let path = Utf8Path::new(&include.inner);

                    // Resolve the path
                    let Some(resolved) = references.get(path) else {
                        // The file references wasn't resolved, this shouldn't happen
                        todo!()
                    };

                    // Then process it
                    let content = self.preprocess_path(resolved, ctx)?;
                    buf.extend(content);
                }

                Node::Condition { branches, fallback } => {
                    for branch in branches {
                        let condition: String = ctx
                            .replace(&branch.condition.inner)
                            .into_iter()
                            .map(|l| l.inner)
                            .collect();

                        let (_, expression) =
                            parse_condition(&condition).finish().map_err(|(): ()| {
                                PreprocessorError::ConditionParse {
                                    location: branch.condition.location.clone(),
                                }
                            })?; // TODO: wrap the error

                        let expression =
                            expression.with_location(branch.condition.location.clone());

                        if expression.evaluate(ctx)? {
                            self.process_chunks(buf, &branch.body.inner, references, ctx)?;
                            continue 'outer;
                        }
                    }

                    if let Some(ref body) = fallback {
                        self.process_chunks(buf, &body.inner, references, ctx)?;
                    }
                }
            }
        }

        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum PreprocessorError {
    #[error("could not get file {path}: {inner}")]
    GetFile {
        path: Utf8PathBuf,
        inner: ParseError,
    },

    #[error("user error: {message}")]
    UserError {
        location: Range<usize>,
        message: String,
    },

    #[error("invalid syntax in condition")]
    ConditionParse { location: Range<usize> },

    #[error("could not evaluate condition")]
    ConditionEvaluation(#[from] ConditionEvaluationError),
}

impl PreprocessorError {
    #[must_use]
    pub fn location(&self) -> Option<&Range<usize>> {
        match self {
            PreprocessorError::GetFile { .. } => None,
            PreprocessorError::UserError { location, .. }
            | PreprocessorError::ConditionParse { location } => Some(location),
            PreprocessorError::ConditionEvaluation(e) => Some(e.location()),
        }
    }
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
        self.definitions.contains_key(variable)
    }
}

impl Context {
    fn define(&mut self, key: String, content: Option<String>) {
        self.definitions.insert(key, content);
    }

    fn undefine(&mut self, key: &str) {
        self.definitions.remove(key);
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
        let preprocessor = Workspace::new(&fs, path.as_ref().to_owned());
        preprocessor.preprocess()
    }

    #[test]
    fn inclusion_test() {
        let res = preprocess("/inclusion.S").unwrap();
        assert_eq!(
            res,
            indoc::indoc! {r"
                this is before foo.S
                this is foo.S
                this is after foo.S
            "}
        );
    }

    #[test]
    fn condition_test() {
        let res = preprocess("/condition.S").unwrap();
        assert_eq!(
            res,
            indoc::indoc! {r"
                simple


                fallback

                definition

                nested
            "}
        );
    }

    #[test]
    fn definition_test() {
        let res = preprocess("/define.S").unwrap();
        assert_eq!(
            res,
            indoc::indoc! {r"
                hello world
                helloFOO
                hello FOO
            "}
        );

        let res = preprocess("/double-define.S").unwrap();
        assert_eq!(
            res,
            indoc::indoc! {r"
                hello world
            "}
        );
    }

    #[test]
    fn user_error_test() {
        let res = preprocess("/error.S");
        assert!(res.is_err());
        if let Err(PreprocessorError::UserError { message, .. }) = res {
            assert_eq!(message, "custom".to_string());
        } else {
            panic!("not a UserError");
        }
    }
}
