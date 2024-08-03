use std::num::ParseIntError;

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
        original: Box<Error<I>>,
        other: Box<Error<I>>,
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
        Self::Or {
            original: Box::new(self),
            other: Box::new(other),
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
