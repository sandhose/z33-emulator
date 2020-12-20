use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use nom::{
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::char,
    combinator::{all_consuming, value},
    IResult, Offset,
};
use regex::{Captures, Regex};
use thiserror::Error;
use tracing::debug;
use unicode_segmentation::UnicodeSegmentation;

use crate::parser::condition::{parse_condition, Context as ConditionContext};
use crate::parser::expression::EmptyContext as EmptyExpressionContext;
use crate::parser::{parse_identifier, parse_string_literal};

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

    #[error("invalid preprocessor state")]
    InvalidState,

    #[error("file not in a clean state {0:?}")]
    FileNotClean(SourceFileState),

    #[error("parse error")]
    ParseError {
        line: Option<usize>,
        col: Option<usize>,
        path: Option<PathBuf>,
    },

    // TODO
    #[error("condition evaluation error")]
    ConditionEvaluation,
}

fn nom_err_offset(e: nom::Err<nom::error::Error<&str>>, input: &str) -> Option<usize> {
    match e {
        nom::Err::Incomplete(_) => None,
        nom::Err::Error(e) | nom::Err::Failure(e) => Some(input.offset(e.input)),
    }
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

#[derive(Debug, Clone, Copy)]
enum ConditionState {
    /// The current condition evaluated to "true"
    Active,
    /// The current condition block already had a condition evaluated to "true"
    Passed,
    /// The current condition evaluated to "false" and no other condition evaluated to true yet
    Missed,
    /// This is in the `else` branch and is active
    ElseActive,
    /// This is in the `else` branch but inactive
    Else,
}

impl Default for ConditionState {
    fn default() -> Self {
        Self::Missed
    }
}

impl ConditionState {
    fn new(value: bool) -> Self {
        Self::default().transition(value).unwrap()
    }

    #[inline]
    fn transition(self, value: bool) -> Option<Self> {
        match (self, value) {
            (Self::Active, _) | (Self::Passed, _) => Some(Self::Passed),
            (Self::Missed, true) => Some(Self::Active),
            (Self::Missed, false) => Some(Self::Missed),
            (Self::ElseActive, _) | (Self::Else, _) => None, // Invalid transition
        }
    }

    fn transition_else(self) -> Option<Self> {
        match self {
            Self::Active | Self::Passed => Some(Self::Else),
            Self::Missed => Some(Self::ElseActive),
            Self::ElseActive | Self::Else => None, // Invalid transition
        }
    }

    fn is_active(&self) -> bool {
        matches!(self, ConditionState::Active | ConditionState::ElseActive)
    }
}

#[derive(Default, Debug)]
pub struct SourceFileState {
    condition_stack: Vec<ConditionState>,
    path: PathBuf,
}

impl SourceFileState {
    fn new(path: PathBuf) -> Self {
        Self {
            path,
            ..Default::default()
        }
    }

    /// Check if the file is in a clean state, e.g. all conditions ended
    fn is_clean(&self) -> bool {
        self.condition_stack.is_empty()
    }
}

#[derive(Default, Debug)]
struct PreprocessorState {
    definitions: HashMap<String, String>,
    file_stack: Vec<SourceFileState>,
}

fn is_keyword(key: &str) -> bool {
    key == "if" || key == "endif" || key == "else" || key == "define" || key == "defined"
}

impl PreprocessorState {
    #[tracing::instrument]
    fn define(&mut self, key: String, value: String) -> Result<()> {
        if is_keyword(key.as_str()) {
            return Err(PreprocessorError::Keyword(key));
        }

        self.definitions.insert(key, value);

        Ok(())
    }

    #[tracing::instrument]
    fn is_active(&self) -> bool {
        self.file_stack
            .last()
            .and_then(|file| file.condition_stack.last().map(ConditionState::is_active))
            .unwrap_or(true)
    }

    #[tracing::instrument]
    fn push_condition(&mut self, value: bool) {
        // TODO: handle error
        let f = self.file_stack.last_mut().unwrap();
        f.condition_stack.push(ConditionState::new(value));
    }

    #[tracing::instrument]
    fn pop_condition(&mut self) -> Option<ConditionState> {
        // TODO: handle error
        let f = self.file_stack.last_mut().unwrap();
        f.condition_stack.pop()
    }

    fn current_file(&self) -> Result<&SourceFileState> {
        self.file_stack
            .last()
            .ok_or(PreprocessorError::InvalidState)
    }

    #[tracing::instrument]
    fn current_condition(&mut self) -> Option<&mut ConditionState> {
        let f = self.file_stack.last_mut()?;
        f.condition_stack.last_mut()
    }

    #[tracing::instrument]
    fn replace_definitions(&self, source: &str) -> String {
        let words: Vec<_> = source
            .split_word_bounds()
            .map(|word| {
                self.definitions
                    .get(word)
                    .cloned()
                    .unwrap_or_else(|| word.into())
            })
            .collect();
        words.join("")
    }

    /// Replace `defined(CONST)` expressions
    #[tracing::instrument]
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

    #[tracing::instrument]
    fn evaluate_condition(&self, input: &str) -> Result<bool> {
        // let input = self.replace_defined(input);
        let input = self.replace_definitions(input);
        let input = input.as_str();
        let (_, node) =
            all_consuming(parse_condition)(input).map_err(|e| PreprocessorError::ParseError {
                line: None,
                col: nom_err_offset(e, input),
                path: None,
            })?;

        node.evaluate(self) // TODO: capture the error
            .map_err(|_| PreprocessorError::ConditionEvaluation)
    }

    #[tracing::instrument]
    fn apply_directive(&mut self, directive: DirectiveToken, args: &str) -> Result<Option<String>> {
        let args = args.trim();

        match directive {
            DirectiveToken::If => {
                if !self.is_active() {
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
                    self.current_condition()
                        .and_then(|cond| {
                            *cond = cond.transition_else()?;
                            Some(())
                        })
                        .ok_or(PreprocessorError::InvalidDirective(DirectiveToken::Else))?;
                    Ok(None)
                }
            }
            DirectiveToken::Elif => {
                let value = self.evaluate_condition(args)?;
                self.current_condition()
                    .and_then(|cond| {
                        *cond = cond.transition(value)?;
                        Some(())
                    })
                    .ok_or(PreprocessorError::InvalidDirective(DirectiveToken::Elif))?;
                Ok(None)
            }
            DirectiveToken::EndIf => {
                self.pop_condition()
                    .ok_or(PreprocessorError::InvalidDirective(DirectiveToken::EndIf))?;
                Ok(None)
            }
            DirectiveToken::Define => {
                if !self.is_active() {
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
                    let args = self.replace_definitions(args);
                    self.define(identifier.into(), args).map(|_| None)
                }
            }
            DirectiveToken::Include => {
                let (_, path) = all_consuming(parse_string_literal)(args).map_err(|e| {
                    PreprocessorError::ParseError {
                        line: None,
                        col: nom_err_offset(e, args),
                        path: None,
                    }
                })?;
                let path = self.resolve_path(path)?;
                self.read_file(path).map(Some)
            }
        }
    }

    #[tracing::instrument]
    fn resolve_path(&self, path: String) -> Result<PathBuf> {
        self.current_file()
            .map(|p| p.path.parent().unwrap().join(path))
    }

    #[tracing::instrument]
    fn transform(&mut self, source: String) -> Result<String> {
        let mut lines = Vec::new();
        for line in source.lines() {
            if let Ok((rest, directive)) = take_directive(line) {
                let line = self.apply_directive(directive, rest)?;
                if let Some(line) = line {
                    lines.push(line);
                }
            } else {
                let line = self.replace_definitions(line);

                if self.is_active() {
                    lines.push(line);
                }
            }
        }

        let processed = lines.join("\n");

        Ok(processed)
    }

    #[tracing::instrument]
    fn read_file(&mut self, path: PathBuf) -> Result<String> {
        debug!("Reading file {:?}", path);
        self.file_stack.push(SourceFileState::new(path.clone()));
        let mut file = File::open(path)?;
        let mut source = String::new();
        file.read_to_string(&mut source)?;

        let processed = self.transform(source)?;

        let state = self
            .file_stack
            .pop()
            .ok_or(PreprocessorError::InvalidState)?;
        if !state.is_clean() {
            return Err(PreprocessorError::FileNotClean(state));
        }

        Ok(processed)
    }
}

impl ConditionContext for PreprocessorState {
    type ExpressionContext = EmptyExpressionContext;

    fn is_defined(&self, variable: &str) -> bool {
        self.definitions.contains_key(variable)
    }

    fn get_expression_context(&self) -> &Self::ExpressionContext {
        &EmptyExpressionContext
    }
}

#[tracing::instrument]
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
            preprocessor.replace_definitions("HELLO WORLD"),
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

    #[test]
    fn if_defined_test() {
        let source = r#"
#define FOO FOO
#if defined(FOO)
FOO is defined
#endif
#if !defined(BAR)
BAR is not defined
#endif
        "#
        .trim()
        .to_string();
        let mut preprocessor = PreprocessorState::default();
        // Conditions need a file stack
        preprocessor
            .file_stack
            .push(SourceFileState::new("hello.S".into()));
        let transformed = preprocessor.transform(source).unwrap();
        assert_eq!(transformed, "FOO is defined\nBAR is not defined");
    }
}
