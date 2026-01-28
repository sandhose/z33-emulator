use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::character::complete::{char, space0};
use nom::combinator::{map, value};
use nom::error::context;
use nom::{Compare, IResult, Input, Offset, Parser};
use parse_display::{Display, FromStr};
use thiserror::Error;

use super::expression::{parse_expression, Context, EvaluationError, Node};
use super::literal::parse_string_literal;
use super::location::{Locatable, Located};
use super::ParseError;
use crate::ast::{AstNode, NodeKind};
use crate::runtime::arguments::{Dir, Idx, Imm, ImmRegDirIndIdx, Ind};
use crate::runtime::Reg;

#[derive(Display, FromStr, Clone, Copy, Debug, PartialEq)]
#[display(style = "lowercase")]
pub(crate) enum InstructionKind {
    Add,
    And,
    Call,
    Cmp,
    Div,
    Fas,
    In,
    Jmp,
    Jeq,
    Jne,
    Jle,
    Jlt,
    Jge,
    Jgt,
    Ld,
    Mul,
    Neg,
    Nop,
    Not,
    Or,
    Out,
    Pop,
    Push,
    Reset,
    Rti,
    Rtn,
    Shl,
    Shr,
    St,
    Sub,
    Swap,
    Trap,
    Xor,
    DebugReg,
}

impl AstNode for InstructionKind {
    fn kind(&self) -> NodeKind {
        NodeKind::InstructionKind
    }

    fn content(&self) -> Option<String> {
        Some(format!("{self}"))
    }
}

pub(crate) fn parse_instruction_kind<I, Error>(input: I) -> IResult<I, InstructionKind, Error>
where
    I: Input + Compare<&'static str>,
    Error: nom::error::ParseError<I> + nom::error::ContextError<I>,
{
    use InstructionKind as K;

    // `alt` only allows for 21-member tuples so we need to trick a bit by nesting
    // them
    alt((
        alt((
            context("add", value(K::Add, tag_no_case("add"))),
            context("and", value(K::And, tag_no_case("and"))),
            context("call", value(K::Call, tag_no_case("call"))),
            context("cmp", value(K::Cmp, tag_no_case("cmp"))),
            context("div", value(K::Div, tag_no_case("div"))),
            context("fas", value(K::Fas, tag_no_case("fas"))),
            context("in", value(K::In, tag_no_case("in"))),
            context("jmp", value(K::Jmp, tag_no_case("jmp"))),
            context("jeq", value(K::Jeq, tag_no_case("jeq"))),
            context("jne", value(K::Jne, tag_no_case("jne"))),
            context("jle", value(K::Jle, tag_no_case("jle"))),
            context("jlt", value(K::Jlt, tag_no_case("jlt"))),
            context("jge", value(K::Jge, tag_no_case("jge"))),
            context("jgt", value(K::Jgt, tag_no_case("jgt"))),
            context("ld", value(K::Ld, tag_no_case("ld"))),
            context("mul", value(K::Mul, tag_no_case("mul"))),
            context("neg", value(K::Neg, tag_no_case("neg"))),
            context("nop", value(K::Nop, tag_no_case("nop"))),
            context("not", value(K::Not, tag_no_case("not"))),
            context("or", value(K::Or, tag_no_case("or"))),
            context("out", value(K::Out, tag_no_case("out"))),
        )),
        alt((
            context("pop", value(K::Pop, tag_no_case("pop"))),
            context("push", value(K::Push, tag_no_case("push"))),
            context("reset", value(K::Reset, tag_no_case("reset"))),
            context("rti", value(K::Rti, tag_no_case("rti"))),
            context("rtn", value(K::Rtn, tag_no_case("rtn"))),
            context("shl", value(K::Shl, tag_no_case("shl"))),
            context("shr", value(K::Shr, tag_no_case("shr"))),
            context("st", value(K::St, tag_no_case("st"))),
            context("sub", value(K::Sub, tag_no_case("sub"))),
            context("swap", value(K::Swap, tag_no_case("swap"))),
            context("trap", value(K::Trap, tag_no_case("trap"))),
            context("xor", value(K::Xor, tag_no_case("xor"))),
            context("debugreg", value(K::DebugReg, tag_no_case("debugreg"))),
        )),
    ))
    .parse(input)
}

/// Represents an instruction argument
#[derive(Clone, Debug, PartialEq, Display)]
pub(crate) enum InstructionArgument {
    /// An immediate value
    #[display("{0}")]
    Value(Node),

    /// The content of a register
    #[display("{0}")]
    Register(Reg),

    /// A direct memory access
    #[display("[{0.inner}]")]
    Direct(Located<Node>),

    /// An indirect memory access (register)
    #[display("[{0.inner}]")]
    Indirect(Located<Reg>),

    /// An indexed memory access (register + offset)
    #[display("[{register.inner} {value.inner:+}]")]
    Indexed {
        register: Located<Reg>,
        value: Located<Node>,
    },
}

/// Parse an instruction argument
pub(crate) fn parse_instruction_argument<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, InstructionArgument, Error> {
    use InstructionArgument::{Register, Value};
    alt((
        context("immediate value", map(parse_expression, Value)),
        context("register", map(parse_register, Register)),
        // Order is important here: indexed must be before direct because the indirect one cuts
        // directly after square bracket
        context("indexed memory access", parse_indexed),
        context("indirect memory access", parse_indirect),
        context("direct memory access", parse_direct),
    ))
    .parse(input)
}

#[derive(Display, FromStr, Clone, Copy, Debug, PartialEq, Eq)]
#[display(style = "lowercase")]
pub enum DirectiveKind {
    Addr,
    Space,
    String,
    Word,
}

impl AstNode for DirectiveKind {
    fn kind(&self) -> NodeKind {
        NodeKind::DirectiveKind
    }

    fn content(&self) -> Option<String> {
        Some(format!("{self}"))
    }
}

pub(crate) fn parse_directive_kind<I, Error>(input: I) -> IResult<I, DirectiveKind, Error>
where
    I: Input + Compare<&'static str>,
    Error: nom::error::ParseError<I> + nom::error::ContextError<I>,
{
    use DirectiveKind as K;

    alt((
        context("addr", value(K::Addr, tag_no_case("addr"))),
        context("space", value(K::Space, tag_no_case("space"))),
        context("string", value(K::String, tag_no_case("string"))),
        context("word", value(K::Word, tag_no_case("word"))),
    ))
    .parse(input)
}

/// Represents a directive argument
#[derive(Clone, Debug, PartialEq, Display)]
pub(crate) enum DirectiveArgument {
    /// A string literal (`.string` directive)
    #[display("{0:?}")]
    StringLiteral(String),

    /// An expression (`.addr`, `.word`, `.space` directives)
    #[display("{0}")]
    Expression(Node),
}

impl AstNode for DirectiveArgument {
    fn kind(&self) -> NodeKind {
        match self {
            DirectiveArgument::StringLiteral(_) => NodeKind::StringLiteral,
            DirectiveArgument::Expression(e) => e.kind(),
        }
    }

    fn content(&self) -> Option<String> {
        match self {
            DirectiveArgument::StringLiteral(s) => Some(s.clone()),
            DirectiveArgument::Expression(e) => e.content(),
        }
    }

    fn children(&self) -> Vec<crate::ast::Node> {
        match self {
            DirectiveArgument::StringLiteral(_) => Vec::new(),
            DirectiveArgument::Expression(e) => e.children(),
        }
    }
}

/// Parse a directive argument
pub(crate) fn parse_directive_argument<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, DirectiveArgument, Error> {
    alt((
        context(
            "string literal",
            map(parse_string_literal, DirectiveArgument::StringLiteral),
        ),
        context(
            "expression",
            map(parse_expression, DirectiveArgument::Expression),
        ),
    ))
    .parse(input)
}

impl From<&str> for DirectiveArgument {
    fn from(literal: &str) -> Self {
        Self::StringLiteral(literal.to_string())
    }
}

impl From<i128> for DirectiveArgument {
    fn from(value: i128) -> Self {
        Self::Expression(Node::Literal(value))
    }
}

#[derive(Error, Debug)]
pub enum ComputeError {
    #[error("could not evaluate argument")]
    Evaluation(#[from] EvaluationError),
}

impl InstructionArgument {
    pub(crate) fn evaluate<C: Context>(
        &self,
        context: &C,
    ) -> Result<ImmRegDirIndIdx, ComputeError> {
        match self {
            Self::Value(v) => {
                let value = v.evaluate(context)?;
                Ok(ImmRegDirIndIdx::Imm(Imm(value)))
            }
            Self::Register(register) => Ok(ImmRegDirIndIdx::Reg(*register)),
            Self::Direct(v) => {
                let value = v.inner.evaluate(context)?;
                Ok(ImmRegDirIndIdx::Dir(Dir(value)))
            }
            Self::Indirect(register) => Ok(ImmRegDirIndIdx::Ind(Ind(register.inner))),
            Self::Indexed { register, value } => {
                let value = value.inner.evaluate(context)?;
                Ok(ImmRegDirIndIdx::Idx(Idx(register.inner, value)))
            }
        }
    }
}

impl AstNode for InstructionArgument {
    fn kind(&self) -> NodeKind {
        match self {
            InstructionArgument::Value(e) => e.kind(),
            InstructionArgument::Register(_) => NodeKind::Register,
            InstructionArgument::Direct(_) => NodeKind::Direct,
            InstructionArgument::Indirect(_) => NodeKind::Indirect,
            InstructionArgument::Indexed { .. } => NodeKind::Indexed,
        }
    }

    fn content(&self) -> Option<String> {
        match self {
            InstructionArgument::Register(r) => Some(format!("{r}")),
            _ => None,
        }
    }

    fn children(&self) -> Vec<crate::ast::Node> {
        match self {
            InstructionArgument::Value(e) => e.children(),
            InstructionArgument::Register(_) => Vec::new(),
            InstructionArgument::Direct(e) => vec![e.to_node()],
            InstructionArgument::Indirect(r) => vec![r.to_node()],
            InstructionArgument::Indexed { register, value } => {
                vec![register.to_node(), value.to_node()]
            }
        }
    }
}

/// Parse a register
///
/// # Errors
///
/// This function will return an error if the input isn't a valid register
pub fn parse_register<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Reg, Error> {
    alt((
        context("%a", value(Reg::A, tag_no_case("%a"))),
        context("%b", value(Reg::B, tag_no_case("%b"))),
        context("%pc", value(Reg::PC, tag_no_case("%pc"))),
        context("%sp", value(Reg::SP, tag_no_case("%sp"))),
        context("%sr", value(Reg::SR, tag_no_case("%sr"))),
    ))
    .parse(input)
}

fn parse_indexed<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, InstructionArgument, Error> {
    #[derive(Clone, Copy)]
    enum Sign {
        Plus,
        Minus,
    }
    use Sign::{Minus, Plus};

    let (rest, _) = char('[')(input)?;
    let (rest, _) = space0(rest)?;

    let start = rest;
    let (rest, register) = parse_register(rest)?;
    let register = register.with_location(input.offset(start)..input.offset(rest));
    let (rest, _) = space0(rest)?;

    let sign_start = rest;
    let (rest, sign) = alt((value(Plus, char('+')), value(Minus, char('-')))).parse(rest)?;
    let (rest, _) = space0(rest)?;

    let expression_start = rest;
    let (rest, value) = parse_expression(rest)?;

    let value = match sign {
        Plus => value,
        Minus => Node::Invert(
            Box::new(value).with_location(input.offset(expression_start)..input.offset(rest)),
        ),
    };
    let value = value.with_location(input.offset(sign_start)..input.offset(rest));

    let (rest, _) = space0(rest)?;
    let (rest, _) = char(']').parse(rest)?;

    Ok((rest, InstructionArgument::Indexed { register, value }))
}

fn parse_direct<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, InstructionArgument, Error> {
    let (rest, _) = char('[')(input)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, value) = parse_expression(rest)?;
    let value = value.with_location(input.offset(start)..input.offset(rest));
    let (rest, _) = space0(rest)?;
    let (rest, _) = char(']')(rest)?;
    Ok((rest, InstructionArgument::Direct(value)))
}

fn parse_indirect<'a, Error: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, InstructionArgument, Error> {
    let (rest, _) = char('[')(input)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, register) = parse_register(rest)?;
    let register = register.with_location(input.offset(start)..input.offset(rest));
    let (rest, _) = space0(rest)?;
    let (rest, _) = char(']')(rest)?;
    Ok((rest, InstructionArgument::Indirect(register)))
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn parse_register_test() {
        let (input, register) = parse_register::<()>("%a").unwrap();
        assert_eq!(input, "");
        assert_eq!(register, Reg::A);
    }

    #[test]
    fn parse_direct_test() {
        let (input, node) = parse_direct::<()>("[3]").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            node,
            InstructionArgument::Direct(Node::Literal(3).with_location(1..2))
        );
    }

    #[test]
    fn parse_indirect_test() {
        let (input, node) = parse_indirect::<()>("[%a]").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            node,
            InstructionArgument::Indirect(Reg::A.with_location(1..3))
        );
    }

    #[test]
    fn parse_indexed_test() {
        let (input, node) = parse_indexed::<()>("[%a+2]").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            node,
            InstructionArgument::Indexed {
                register: Reg::A.with_location(1..3),
                value: Node::Literal(2).with_location(3..5),
            }
        );
    }
}
