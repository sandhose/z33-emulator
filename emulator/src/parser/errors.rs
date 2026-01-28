#![expect(
    unused_assignments,
    reason = "Looks like miette error derive generates this"
)]

use std::num::ParseIntError;

use miette::SourceOffset;
use nom::Offset;

pub trait ParseError<I>:
    nom::error::ParseError<I>
    + nom::error::FromExternalError<I, ParseIntError>
    + nom::error::ContextError<I>
{
}

impl<I, E> ParseError<I> for E where
    E: nom::error::ParseError<I>
        + nom::error::FromExternalError<I, ParseIntError>
        + nom::error::ContextError<I>
{
}

#[derive(Debug)]
pub enum Error<I> {
    Context {
        input: I,
        ctx: &'static str,
        child: Box<Error<I>>,
    },
    Or {
        inner: Vec<Error<I>>,
    },
    Char {
        input: I,
        needed: char,
    },
    Nom {
        input: I,
        kind: nom::error::ErrorKind,
        child: Option<Box<Error<I>>>,
    },
    ParseIntError {
        input: I,
        kind: nom::error::ErrorKind,
        inner: ParseIntError,
    },
}

impl<I> Error<I> {
    // The potential panic is because of the `expect` on the `Or` variant, but we
    // never construct empty `Or` lists, so this would never panic
    #[allow(clippy::missing_panics_doc)]
    pub fn input(&self) -> &I {
        match self {
            Self::Context { input, .. }
            | Self::Char { input, .. }
            | Self::Nom { input, .. }
            | Self::ParseIntError { input, .. } => input,

            Self::Or { inner } => inner.first().expect("not empty error list").input(),
        }
    }

    pub fn to_miette_diagnostic(
        &self,
        source: &I,
        additional_offset: usize,
    ) -> Box<dyn miette::Diagnostic + Send + Sync>
    where
        I: Offset,
    {
        Box::new(self.to_diagnostic(source, additional_offset))
    }

    fn to_diagnostic(&self, source: &I, additional_offset: usize) -> Diagnostic
    where
        I: Offset,
    {
        match self {
            Error::Context { input, ctx, child } => Diagnostic::Context {
                span: (source.offset(input) + additional_offset).into(),
                ctx,
                child: child.to_miette_diagnostic(source, additional_offset),
            },
            Error::Or { inner } => Diagnostic::Or {
                inner: inner
                    .iter()
                    .map(|e| e.to_diagnostic(source, additional_offset))
                    .collect(),
            },
            Error::Char { input, needed } => Diagnostic::Char {
                span: (source.offset(input) + additional_offset).into(),
                needed: *needed,
            },
            Error::Nom { input, child, .. } => Diagnostic::Nom {
                span: (source.offset(input) + additional_offset).into(),
                child: child
                    .as_ref()
                    .map(|child| child.to_miette_diagnostic(source, additional_offset)),
            },
            Error::ParseIntError { input, inner, .. } => Diagnostic::ParseIntError {
                span: (source.offset(input) + additional_offset).into(),
                inner: inner.clone(),
            },
        }
    }
}

impl<I> nom::error::ParseError<I> for Error<I> {
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        Self::Nom {
            input,
            kind,
            child: None,
        }
    }

    fn append(input: I, kind: nom::error::ErrorKind, other: Self) -> Self {
        Self::Nom {
            input,
            kind,
            child: Some(Box::new(other)),
        }
    }

    fn from_char(input: I, needed: char) -> Self {
        Self::Char { input, needed }
    }

    fn or(self, other: Self) -> Self {
        // If any of `self` or `other` are `Or`, merge them
        // Otherwise, create a new `Or` with both as children
        match (self, other) {
            (Self::Or { inner: first }, Self::Or { inner: second }) => Self::Or {
                inner: first.into_iter().chain(second).collect(),
            },
            (Self::Or { inner: first }, second) => Self::Or {
                inner: first.into_iter().chain(std::iter::once(second)).collect(),
            },
            (first, Self::Or { inner: second }) => Self::Or {
                inner: std::iter::once(first).chain(second).collect(),
            },
            (first, second) => Self::Or {
                inner: vec![first, second],
            },
        }
    }
}

impl<I> nom::error::ContextError<I> for Error<I> {
    fn add_context(input: I, ctx: &'static str, other: Self) -> Self {
        Self::Context {
            input,
            ctx,
            child: Box::new(other),
        }
    }
}

impl<I> nom::error::FromExternalError<I, ParseIntError> for Error<I> {
    fn from_external_error(input: I, kind: nom::error::ErrorKind, inner: ParseIntError) -> Self {
        Self::ParseIntError { input, kind, inner }
    }
}

#[derive(Debug, miette::Diagnostic, thiserror::Error)]
#[error("invalid syntax")]
enum Diagnostic {
    // TODO: embed source code?
    Context {
        #[label("{ctx}")]
        span: SourceOffset,
        ctx: &'static str,
        child: Box<dyn miette::Diagnostic + Send + Sync>,
    },

    Or {
        #[related]
        inner: Vec<Diagnostic>,
    },

    Char {
        #[label("Expect '{needed}' here")]
        span: SourceOffset,

        needed: char,
    },

    Nom {
        #[label("here")]
        span: SourceOffset,
        child: Option<Box<dyn miette::Diagnostic + Send + Sync>>,
    },

    ParseIntError {
        #[label("here")]
        span: SourceOffset,

        #[source]
        inner: ParseIntError,
    },
}
