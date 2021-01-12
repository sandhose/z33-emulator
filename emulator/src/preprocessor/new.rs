use std::{collections::HashMap, io::Read, path::PathBuf};

use nom::Finish;
use thiserror::Error;

use crate::parser::{
    location::{AbsoluteLocation, Located, RelativeLocation},
    preprocessor::{parse, Node},
};

use super::fs::Filesystem;

#[derive(Debug, Error, Clone)]
enum GetFileError {
    /// An error from the underlying filesystem
    ///
    /// The inner error is wrapped around a std::rc::Rc because std::io::Error does not implement
    /// Clone, but this error needs to be clonable, since it is stored in the parser cache and
    /// might be returned multiple times.
    #[error("i/o error: {0}")]
    IO(#[from] std::rc::Rc<std::io::Error>),

    #[error("parse error")]
    ParseError,
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
    fn get_file(&self, path: &PathBuf) -> &Result<ParsedFile<AbsoluteLocation>, GetFileError> {
        self.files.get(path).expect("file not in cache")
    }

    fn fill<FS: Filesystem>(&mut self, path: &PathBuf, fs: &FS) {
        if self.files.contains_key(path) {
            return;
        }

        let res = (|| {
            let content = {
                let mut f = fs.open(&path).map_err(std::rc::Rc::new)?;
                let mut buf = String::new();
                f.read_to_string(&mut buf).map_err(std::rc::Rc::new)?;
                buf
            };

            let (_, chunks) = parse(content.as_str())
                .finish()
                .map_err(|_| GetFileError::ParseError)?;

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

        self.files.insert(path.clone(), res);

        let parent = path;
        for path in paths {
            let path = fs.relative(Some(parent), &path);
            self.fill(&path, fs);
        }
    }
}

#[derive(Error, Debug)]
enum PreprocessorError {
    #[error("could not get file {path}: {inner}")]
    GetFile { path: PathBuf, inner: GetFileError },

    #[error("user error: {0}")]
    UserError(String),
}

fn preprocess_path<FS: Filesystem>(
    path: &PathBuf,
    cache: &ParserCache,
    fs: &FS,
) -> Result<String, PreprocessorError> {
    use PreprocessorError::*;

    let mut buf = String::new();
    let file = cache.get_file(path).as_ref().map_err(|inner| GetFile {
        path: path.clone(),
        inner: inner.clone(),
    })?;

    for chunk in file.chunks.iter() {
        match &chunk.inner {
            Node::Raw { content } => buf += content, // TODO: do definition replacements
            Node::Error { message } => return Err(UserError(message.inner.clone())),
            Node::Undefine { .. } => todo!(),
            Node::Definition { .. } => todo!(),
            Node::Inclusion { path: ref include } => {
                let path = fs.relative(Some(path), &include.inner.clone().into());
                let included = preprocess_path(&path, cache, fs)?;
                buf += &included;
            }
            Node::Condition { .. } => todo!(),
        }
    }

    Ok(buf)
}

#[allow(dead_code)]
fn preprocess<FS: Filesystem>(fs: &FS, entrypoint: &PathBuf) -> Result<String, PreprocessorError> {
    let path = fs.relative(None, entrypoint);
    let cache = {
        let mut c = ParserCache::new();
        c.fill(&path, fs);
        c
    };
    preprocess_path(&path, &cache, fs)
}

#[cfg(test)]
mod tests {
    use super::super::fs::InMemoryFilesystem;

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
            t.insert(
                "/foo.S".into(),
                indoc::indoc! {r#"
                    this is foo.S
                "#}
                .into(),
            );
            t.insert(
                "/error.S".into(),
                indoc::indoc! {r#"
                    #error "custom"
                "#}
                .into(),
            );
            t
        })
    }

    #[test]
    fn inclusion_test() {
        let fs = fs();
        let res = preprocess(&fs, &"/inclusion.S".into()).unwrap();
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
    fn user_error_test() {
        let fs = fs();
        let res = preprocess(&fs, &"/error.S".into());
        assert!(res.is_err());
        if let Err(PreprocessorError::UserError(c)) = res {
            assert_eq!(c, "custom".to_string());
        } else {
            panic!("not a UserError");
        }
    }
}
