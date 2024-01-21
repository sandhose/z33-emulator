use std::{
    collections::HashMap,
    io::Read,
    path::{Path, PathBuf},
};

use nom::{combinator::all_consuming, error::convert_error, Finish, Offset};
use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;

use crate::parser::{
    condition::{
        parse_condition, Context as ConditionContext, EvaluationError as ConditionEvaluationError,
    },
    expression::EmptyContext as EmptyExpressionContext,
    location::{AbsoluteLocation, Locatable, Located, MapLocation, RelativeLocation},
    preprocessor::{parse, Node},
};

mod fs;

pub use fs::{Filesystem, InMemoryFilesystem, NativeFilesystem};

#[derive(Debug, Error, Clone)]
pub enum GetFileError {
    /// An error from the underlying filesystem
    ///
    /// The inner error is wrapped around a std::arc::Arc because std::io::Error does not implement
    /// Clone, but this error needs to be clonable, since it is stored in the parser cache and
    /// might be returned multiple times.
    #[error("i/o error: {0}")]
    IO(#[from] std::sync::Arc<std::io::Error>),

    #[error("parse error: {message}")]
    ParseError { message: String },
}

struct ParsedFile<L> {
    chunks: Vec<Located<Node<L>, L>>,
}

impl<L> ParsedFile<L> {
    fn walk<F>(&self, mut f: F)
    where
        F: FnMut(&Node<L>),
    {
        for chunk in &self.chunks {
            chunk.inner.walk(&mut f);
        }
    }
}

impl<P, L> MapLocation<P> for ParsedFile<L>
where
    L: MapLocation<P, Mapped = P>,
{
    type Mapped = ParsedFile<P>;

    fn map_location(self, parent: &P) -> Self::Mapped {
        let chunks = self
            .chunks
            .into_iter()
            .map(|chunk| chunk.map_location(parent))
            .collect();

        ParsedFile { chunks }
    }
}

#[derive(Default)]
struct ParserCache {
    files: HashMap<PathBuf, Result<ParsedFile<AbsoluteLocation<PathBuf>>, GetFileError>>,
    sources: HashMap<PathBuf, String>,
}

impl ParserCache {
    fn new() -> Self {
        Self::default()
    }
}

impl ParserCache {
    fn get_file(
        &self,
        path: &Path,
    ) -> &Result<ParsedFile<AbsoluteLocation<PathBuf>>, GetFileError> {
        self.files.get(path).expect("file not in cache")
    }

    fn fill<FS: Filesystem>(&mut self, path: &Path, fs: &FS) {
        if self.files.contains_key(path) {
            return;
        }

        let res = (|| {
            let content = {
                let mut f = fs.open(path).map_err(std::sync::Arc::new)?;
                let mut buf = String::new();
                f.read_to_string(&mut buf).map_err(std::sync::Arc::new)?;
                buf
            };

            self.sources.insert(path.to_path_buf(), content.clone());

            let (_, chunks) = all_consuming(parse)(content.as_str())
                .finish()
                .map_err(|e| GetFileError::ParseError {
                    message: convert_error(content.as_str(), e),
                })?;

            let parent = AbsoluteLocation {
                offset: 0,
                length: 0,
                file: path.to_owned(),
            };
            Ok(ParsedFile { chunks }.map_location(&parent))
        })();

        let mut paths: Vec<PathBuf> = Vec::new();
        if let Ok(ref file) = res {
            file.walk(|node| {
                if let Node::Inclusion { path: inclusion } = node {
                    paths.push(inclusion.inner.clone().into());
                }
            });
        }

        self.files.insert(path.to_path_buf(), res);

        let parent = path;
        for path in paths {
            let path = fs.relative(Some(parent), &path);
            self.fill(&path, fs);
        }
    }
}

#[derive(Error, Debug)]
pub enum PreprocessorError<L> {
    #[error("could not get file {path}: {inner}")]
    GetFile { path: PathBuf, inner: GetFileError },

    #[error("user error: {message}")]
    UserError { location: L, message: String },

    #[error("invalid syntax in condition")]
    ConditionParse { location: L },

    #[error("could not evaluate condition")]
    ConditionEvaluation(#[from] ConditionEvaluationError<L>),
}

impl<L> PreprocessorError<L> {
    pub fn location(&self) -> Option<&L> {
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

    fn replace<'a>(&'a self, input: &'a str) -> Vec<Located<&'a str, RelativeLocation>> {
        input
            .split_word_bounds()
            .filter_map(|word| {
                let location = RelativeLocation::from((input.offset(word), word.len()));
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

pub struct Preprocessor<FS> {
    cache: ParserCache,
    fs: FS,
}

impl<FS> Preprocessor<FS> {
    pub fn new(fs: FS) -> Self {
        Self {
            cache: ParserCache::new(),
            fs,
        }
    }

    #[must_use]
    pub fn and_load(mut self, entrypoint: &Path) -> Self
    where
        FS: Filesystem,
    {
        self.load(entrypoint);
        self
    }

    pub fn sources(&self) -> &HashMap<PathBuf, String> {
        &self.cache.sources
    }

    pub fn load(&mut self, entrypoint: &Path)
    where
        FS: Filesystem,
    {
        let path = self.fs.relative(None, entrypoint);
        self.cache.fill(&path, &self.fs);
    }

    pub fn preprocess(
        &self,
        entrypoint: &Path,
    ) -> Result<String, PreprocessorError<AbsoluteLocation<PathBuf>>>
    where
        FS: Filesystem,
    {
        let path = self.fs.relative(None, entrypoint);
        let mut ctx = Context::default();
        let chunks = self.preprocess_path(&path, &mut ctx)?;

        Ok(chunks.join("\n"))
    }

    fn preprocess_path(
        &self,
        path: &Path,
        ctx: &mut Context,
    ) -> Result<Vec<String>, PreprocessorError<AbsoluteLocation<PathBuf>>>
    where
        FS: Filesystem,
    {
        let mut buf = Vec::new();
        let file =
            self.cache
                .get_file(path)
                .as_ref()
                .map_err(|inner| PreprocessorError::GetFile {
                    path: path.to_path_buf(),
                    inner: inner.clone(),
                })?;

        for chunk in &file.chunks {
            let content = self.process_chunk(chunk, ctx, path)?;
            buf.extend(content);
        }

        Ok(buf)
    }

    fn process_chunk(
        &self,
        chunk: &Located<Node<AbsoluteLocation<PathBuf>>, AbsoluteLocation<PathBuf>>,
        ctx: &mut Context,
        open_path: &Path,
    ) -> Result<Vec<String>, PreprocessorError<AbsoluteLocation<PathBuf>>>
    where
        FS: Filesystem,
    {
        match &chunk.inner {
            Node::Raw { ref content } => {
                // Replace the definitions in the content
                let replaced = ctx.replace(content);
                Ok(vec![replaced.into_iter().map(|l| l.inner).collect()])
            }

            Node::Error { ref message } => {
                // Return the user-defined error
                Err(PreprocessorError::UserError {
                    message: message.inner.clone(),
                    location: message.location.clone(),
                })
            }

            Node::Undefine { ref key } => {
                // Remove a definition
                let key = &key.inner;
                ctx.undefine(key);
                Ok(Vec::new()) // Generates no text
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
                Ok(Vec::new()) // Generates no text
            }

            Node::Inclusion { path: ref include } => {
                // Include a file
                let include: PathBuf = include.inner.clone().into();
                // First resolve its path
                let path = self.fs.relative(Some(open_path), &include);
                // Then process it
                let content = self.preprocess_path(&path, ctx)?;
                Ok(content)
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

                    let expression = expression
                        .map_location(&branch.condition.location)
                        .with_location(branch.condition.location.clone());

                    if expression.evaluate(ctx)? {
                        let mut buf = Vec::new();
                        for chunk in &branch.body.inner {
                            let chunks = self.process_chunk(chunk, ctx, open_path)?;
                            buf.extend(chunks);
                        }
                        return Ok(buf);
                    }
                }

                if let Some(ref body) = fallback {
                    let mut buf = Vec::new();
                    for chunk in &body.inner {
                        let chunks = self.process_chunk(chunk, ctx, open_path)?;
                        buf.extend(chunks);
                    }
                    return Ok(buf);
                }

                Ok(Vec::new())
            }
        }
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

    fn preprocess<P: AsRef<Path>>(
        path: P,
    ) -> Result<String, PreprocessorError<AbsoluteLocation<PathBuf>>> {
        let fs = fs();
        let mut preprocessor = Preprocessor::new(fs);
        let path = path.as_ref();
        preprocessor.load(path);
        preprocessor.preprocess(path)
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
