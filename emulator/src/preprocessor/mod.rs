use std::{
    collections::HashMap,
    io::Read,
    path::{Path, PathBuf},
};

use nom::{combinator::all_consuming, error::convert_error, Finish};
use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;

use crate::parser::{
    condition::{
        parse_condition, Context as ConditionContext, EvaluationError as ConditionEvaluationError,
    },
    expression::EmptyContext as EmptyExpressionContext,
    location::{AbsoluteLocation, Located, RelativeLocation},
    preprocessor::{parse, Node},
};

mod fs;

pub use fs::{Filesystem, InMemoryFilesystem, NativeFilesystem};

#[derive(Debug, Error, Clone)]
pub enum GetFileError {
    /// An error from the underlying filesystem
    ///
    /// The inner error is wrapped around a std::rc::Rc because std::io::Error does not implement
    /// Clone, but this error needs to be clonable, since it is stored in the parser cache and
    /// might be returned multiple times.
    #[error("i/o error: {0}")]
    IO(#[from] std::rc::Rc<std::io::Error>),

    #[error("parse error: {message}")]
    ParseError { message: String },
}

struct ParsedFile<L> {
    chunks: Vec<Located<Node<L>, L>>,
}

impl ParsedFile<RelativeLocation> {
    fn into_absolute(self) -> ParsedFile<AbsoluteLocation> {
        let root = AbsoluteLocation::default();
        let chunks = self
            .chunks
            .into_iter()
            .map(|located_chunk| {
                located_chunk.into_absolute(&root, Node::<RelativeLocation>::into_absolute)
            })
            .collect();

        ParsedFile { chunks }
    }
}

impl<L> ParsedFile<L> {
    fn walk<F>(&self, mut f: F)
    where
        F: FnMut(&Node<L>),
    {
        for chunk in self.chunks.iter() {
            chunk.inner.walk(&mut f);
        }
    }
}

struct ParserCache {
    files: HashMap<PathBuf, Result<ParsedFile<AbsoluteLocation>, GetFileError>>,
}

impl ParserCache {
    fn new() -> Self {
        Self {
            files: Default::default(),
        }
    }
}

impl ParserCache {
    fn get_file(&self, path: &Path) -> &Result<ParsedFile<AbsoluteLocation>, GetFileError> {
        self.files.get(path).expect("file not in cache")
    }

    fn fill<FS: Filesystem>(&mut self, path: &Path, fs: &FS) {
        if self.files.contains_key(path) {
            return;
        }

        let res = (|| {
            let content = {
                let mut f = fs.open(path).map_err(std::rc::Rc::new)?;
                let mut buf = String::new();
                f.read_to_string(&mut buf).map_err(std::rc::Rc::new)?;
                buf
            };

            let (_, chunks) = all_consuming(parse)(content.as_str())
                .finish()
                .map_err(|e| GetFileError::ParseError {
                    message: convert_error(content.as_str(), e),
                })?;

            Ok(ParsedFile { chunks }.into_absolute())
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
pub enum PreprocessorError {
    #[error("could not get file {path}: {inner}")]
    GetFile { path: PathBuf, inner: GetFileError },

    #[error("user error: {0}")]
    UserError(String),

    #[error("invalid syntax in condition")]
    ConditionParse,

    #[error("could not evaluate condition: {0}")]
    ConditionEvaluation(#[from] ConditionEvaluationError),
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

    fn replace(&self, input: &str) -> String {
        let words: Vec<_> = input
            .split_word_bounds()
            .map(|word| match self.definitions.get(word) {
                Some(Some(replacement)) => replacement.clone(),
                Some(None) => String::new(),
                None => word.to_string(),
            })
            .collect();

        words.join("")
    }
}

fn process_chunk<FS: Filesystem, L: Clone>(
    chunk: &Node<L>,
    context: &mut Context,
    open_path: &Path,
    cache: &ParserCache,
    fs: &FS,
) -> Result<Vec<String>, PreprocessorError> {
    use PreprocessorError::*;

    match chunk {
        Node::Raw { ref content } => {
            // Replace the definitions in the content
            let replaced = context.replace(content);
            Ok(vec![replaced])
        }

        Node::Error { ref message } => {
            // Return the user-defined error
            Err(UserError(message.inner.clone()))
        }

        Node::Undefine { ref key } => {
            // Remove a definition
            let key = &key.inner;
            context.undefine(key);
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
            let content = content.map(|c| context.replace(c));
            // Then add the definition
            context.define(key, content);
            Ok(Vec::new()) // Generates no text
        }

        Node::Inclusion { path: ref include } => {
            // Include a file
            let include: PathBuf = include.inner.clone().into();
            // First resolve its path
            let path = fs.relative(Some(open_path), &include);
            // Then process it
            let content = preprocess_path(&path, context, cache, fs)?;
            Ok(content)
        }

        Node::Condition { branches, fallback } => {
            for branch in branches.iter() {
                let condition = context.replace(&branch.condition.inner);
                let (_, expression) = parse_condition(&condition)
                    .finish()
                    .map_err(|_: ()| ConditionParse)?; // TODO: wrap the error

                if expression.evaluate(context)? {
                    let mut buf = Vec::new();
                    for chunk in branch.body.inner.iter() {
                        let chunks = process_chunk(&chunk.inner, context, open_path, cache, fs)?;
                        buf.extend(chunks);
                    }
                    return Ok(buf);
                }
            }

            if let Some(ref body) = fallback {
                let mut buf = Vec::new();
                for chunk in body.inner.iter() {
                    let chunks = process_chunk(&chunk.inner, context, open_path, cache, fs)?;
                    buf.extend(chunks);
                }
                return Ok(buf);
            }

            Ok(Vec::new())
        }
    }
}

fn preprocess_path<FS: Filesystem>(
    path: &Path,
    context: &mut Context,
    cache: &ParserCache,
    fs: &FS,
) -> Result<Vec<String>, PreprocessorError> {
    use PreprocessorError::*;

    let mut buf = Vec::new();
    let file = cache.get_file(path).as_ref().map_err(|inner| GetFile {
        path: path.to_path_buf(),
        inner: inner.clone(),
    })?;

    for chunk in file.chunks.iter() {
        let content = process_chunk(&chunk.inner, context, path, cache, fs)?;
        buf.extend(content);
    }

    Ok(buf)
}

pub fn preprocess<FS: Filesystem>(fs: &FS, entrypoint: &Path) -> Result<String, PreprocessorError> {
    let path = fs.relative(None, entrypoint);
    let mut context = Context::default();
    let cache = {
        let mut c = ParserCache::new();
        c.fill(&path, fs);
        c
    };
    let chunks = preprocess_path(&path, &mut context, &cache, fs)?;
    Ok(chunks.join("\n"))
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
                indoc::indoc! {r#"
                    #define FOO world
                    hello FOO
                    helloFOO
                    #undefine FOO
                    hello FOO
                "#}
                .into(),
            );
            t.insert(
                "/double-define.S".into(),
                indoc::indoc! {r#"
                    #define FOO world
                    #define WHAT hello FOO
                    #define FOO toto
                    WHAT
                "#}
                .into(),
            );
            t.insert(
                "/condition.S".into(),
                indoc::indoc! {r#"
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
                "#}
                .into(),
            );
            t
        })
    }

    #[test]
    fn inclusion_test() {
        let fs = fs();
        let res = preprocess(&fs, &PathBuf::from("/inclusion.S")).unwrap();
        assert_eq!(
            res,
            indoc::indoc! {r#"
                this is before foo.S
                this is foo.S
                this is after foo.S
            "#}
        );
    }

    #[test]
    fn condition_test() {
        let fs = fs();
        let res = preprocess(&fs, &PathBuf::from("/condition.S")).unwrap();
        assert_eq!(
            res,
            indoc::indoc! {r#"
                simple


                fallback

                definition

                nested
            "#}
        );
    }

    #[test]
    fn definition_test() {
        let fs = fs();
        let res = preprocess(&fs, &PathBuf::from("/define.S")).unwrap();
        assert_eq!(
            res,
            indoc::indoc! {r#"
                hello world
                helloFOO
                hello FOO
            "#}
        );

        let res = preprocess(&fs, &PathBuf::from("/double-define.S")).unwrap();
        assert_eq!(
            res,
            indoc::indoc! {r#"
                hello world
            "#}
        );
    }

    #[test]
    fn user_error_test() {
        let fs = fs();
        let res = preprocess(&fs, &PathBuf::from("/error.S"));
        assert!(res.is_err());
        if let Err(PreprocessorError::UserError(c)) = res {
            assert_eq!(c, "custom".to_string());
        } else {
            panic!("not a UserError");
        }
    }
}
