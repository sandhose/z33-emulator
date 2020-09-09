use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use nom::{
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::char,
    combinator::{all_consuming, value},
    IResult,
};
use regex::{Captures, Regex};
use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;

use crate::parser::{parse_condition, parse_identifier, parse_string_literal};

#[derive(Error, Debug)]
pub enum PreprocessorError {
    #[error("i/o error: {0}")]
    IO(#[from] std::io::Error),

    #[error("can't use keyword {0:?}")]
    Keyword(String),

    #[error("invalid directive {0}")]
    InvalidDirective(DirectiveToken),

    #[error("invalid argument {args:?} for directive {directive}")]
    InvalidArgument {
        directive: DirectiveToken,
        args: String,
    },

    #[error("parse error")]
    ParseError,
}

type Result<T> = std::result::Result<T, PreprocessorError>;

#[derive(Clone, Debug)]
pub enum DirectiveToken {
    If,
    Else,
    Elif,
    EndIf,
    Define,
    Include,
}

impl std::fmt::Display for DirectiveToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Elif => write!(f, "elif"),
            Self::EndIf => write!(f, "endif"),
            Self::Define => write!(f, "define"),
            Self::Include => write!(f, "include"),
        }
    }
}

fn parse_directive_token(input: &str) -> IResult<&str, DirectiveToken> {
    alt((
        value(DirectiveToken::If, tag_no_case("if")),
        value(DirectiveToken::Else, tag_no_case("else")),
        value(DirectiveToken::Elif, tag_no_case("elif")),
        value(DirectiveToken::EndIf, tag_no_case("endif")),
        value(DirectiveToken::Define, tag_no_case("define")),
        value(DirectiveToken::Include, tag_no_case("include")),
    ))(input)
}

fn take_directive(input: &str) -> IResult<&str, DirectiveToken> {
    let (input, _) = char('#')(input)?;
    parse_directive_token(input)
}

// TODO: filesystem abstraction to support virtual fs (like in the browser)

#[derive(Default, Debug)]
struct PreprocessorState {
    definitions: HashMap<String, String>,

    // TODO: this is naive and does not work for multiple reasons.
    // Condition stacks should be file-scoped, and checked whenever we exit a file.
    // Also, this solution does not support more than two branches on a condition block (elif).
    condition_stack: Vec<bool>,
    path_stack: Vec<PathBuf>,
}

fn is_keyword(key: &str) -> bool {
    key == "if" || key == "endif" || key == "else" || key == "define" || key == "defined"
}

impl PreprocessorState {
    fn define(&mut self, key: String, value: String) -> Result<()> {
        if is_keyword(key.as_str()) {
            return Err(PreprocessorError::Keyword(key));
        }

        if !self.is_skipping() {
            self.definitions.insert(key, value);
        }

        Ok(())
    }

    fn is_skipping(&self) -> bool {
        self.condition_stack.last().cloned().unwrap_or(false)
    }

    fn push_condition(&mut self, value: bool) {
        self.condition_stack.push(value);
    }

    fn pop_condition(&mut self) -> Option<bool> {
        self.condition_stack.pop()
    }

    fn replace_definitions(&self, source: String) -> String {
        let words: Vec<_> = source
            .split_word_bounds()
            .map(|word| self.definitions.get(word).cloned().unwrap_or(word.into()))
            .collect();
        words.join("")
    }

    /// Replace `defined(CONST)` expressions
    fn replace_defined(&self, source: &str) -> String {
        let re = Regex::new(r"defined\([[:space:]]*([[:word:]]+)[[:space:]]*\)").unwrap();
        re.replace(source, |caps: &Captures| {
            let key = caps[0].to_string();
            if self.definitions.contains_key(&key) {
                "true"
            } else {
                "false"
            }
        })
        .to_string()
    }

    fn evaluate_condition(&self, input: &str) -> Result<bool> {
        let input = self.replace_defined(input);
        let input = self.replace_definitions(input);
        let (_, value) = all_consuming(parse_condition)(input.as_str())
            // TODO: encapsulate the error
            .map_err(|_e| PreprocessorError::ParseError)?;
        Ok(value)
    }

    fn apply_directive(&mut self, directive: DirectiveToken, args: &str) -> Result<Option<String>> {
        let args = args.trim();

        match directive {
            DirectiveToken::If => {
                if self.is_skipping() {
                    self.push_condition(false);
                } else {
                    let cond = self.evaluate_condition(args)?;
                    self.push_condition(cond);
                }
                Ok(None)
            }
            DirectiveToken::Else => {
                if args != "" {
                    // Else statements should not have arguments
                    Err(PreprocessorError::InvalidArgument {
                        directive: DirectiveToken::Else,
                        args: args.into(),
                    })
                } else {
                    let val = self
                        .pop_condition()
                        .ok_or(PreprocessorError::InvalidDirective(DirectiveToken::Else))?;
                    self.push_condition(!val);
                    Ok(None)
                }
            }
            DirectiveToken::Elif => {
                // TODO: elif is harder than I thought. We now need to know if any condition was
                // already taken in the current condition block
                todo!()
            }
            DirectiveToken::EndIf => {
                self.pop_condition()
                    .ok_or(PreprocessorError::InvalidDirective(DirectiveToken::EndIf))?;
                Ok(None)
            }
            DirectiveToken::Define => {
                if self.is_skipping() {
                    Ok(None)
                } else {
                    let (args, identifier) = parse_identifier(args) // Parse the key
                        .map_err(|_| {
                            PreprocessorError::InvalidArgument {
                                directive: DirectiveToken::Define,
                                args: args.into()
                            }
                        })?;

                    let args = args.trim(); // Remove unnecessary spaces
                    let args = self.replace_definitions(args.into());
                    self.define(identifier.into(), args).map(|_| None)
                }
            }
            DirectiveToken::Include => {
                let (_, path) = all_consuming(parse_string_literal)(args)
                    .map_err(|_e| PreprocessorError::ParseError)?;
                let path = self.resolve_path(path)?;
                self.read_file(path).map(Some)
            }
        }
    }

    fn resolve_path(&self, path: String) -> Result<PathBuf> {
        Ok(self.path_stack.last().unwrap().join(path))
    }

    fn transform(&mut self, source: String) -> Result<String> {
        let mut lines = Vec::new();
        for line in source.lines() {
            if let Ok((rest, directive)) = take_directive(line) {
                let line = self.apply_directive(directive, rest)?;
                if let Some(line) = line {
                    lines.push(line);
                }
            } else {
                let line = self.replace_definitions(line.into());

                if !self.is_skipping() {
                    lines.push(line);
                }
            }
        }

        let processed = lines.join("\n");

        Ok(processed)
    }

    fn read_file(&mut self, path: PathBuf) -> Result<String> {
        self.path_stack.push(path.clone());
        let mut file = File::open(path)?;
        let mut source = String::new();
        file.read_to_string(&mut source)?;

        let processed = self.transform(source)?;

        self.path_stack.pop();

        Ok(processed)
    }
}

pub fn preprocess(path: PathBuf) -> Result<String> {
    let mut state = PreprocessorState::default();

    state.read_file(path)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn replace_definitions_test() {
        let mut preprocessor = PreprocessorState::default();
        preprocessor.define("HELLO".into(), "WORLD".into()).unwrap();
        preprocessor.define("WORLD".into(), "42".into()).unwrap();
        assert_eq!(
            preprocessor.replace_definitions("HELLO WORLD".into()),
            "WORLD 42".to_string()
        );
    }

    #[test]
    fn define_test() {
        let source = r#"
#define FOO world
hello FOO
        "#
        .trim()
        .to_string();
        let mut preprocessor = PreprocessorState::default();
        let transformed = preprocessor.transform(source).unwrap();
        assert_eq!(transformed, "hello world".to_string());
    }
}
