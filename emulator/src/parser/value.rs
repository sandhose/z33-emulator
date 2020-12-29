use nom::{
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::{char, space0},
    combinator::{map, value},
    error::ParseError,
    Compare, IResult, InputTake,
};
use thiserror::Error;

use super::{
    expression::{parse_expression, Context, EvaluationError, Node},
    literal::parse_string_literal,
    location::Locatable,
    location::{Located, RelativeLocation},
};
use crate::{
    ast::{AstNode, NodeKind},
    runtime::{Address, Arg, Reg, Value},
};

#[derive(Clone, Copy, Debug, PartialEq)]
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
}

impl<L> AstNode<L> for InstructionKind {
    fn kind(&self) -> NodeKind {
        NodeKind::InstructionKind
    }

    fn content(&self) -> Option<String> {
        Some(format!("{}", self))
    }
}

impl std::fmt::Display for InstructionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use InstructionKind::*;

        match self {
            Add => write!(f, "add"),
            And => write!(f, "and"),
            Call => write!(f, "call"),
            Cmp => write!(f, "cmp"),
            Div => write!(f, "div"),
            Fas => write!(f, "fas"),
            In => write!(f, "in"),
            Jmp => write!(f, "jmp"),
            Jeq => write!(f, "jeq"),
            Jne => write!(f, "jne"),
            Jle => write!(f, "jle"),
            Jlt => write!(f, "jlt"),
            Jge => write!(f, "jge"),
            Jgt => write!(f, "jgt"),
            Ld => write!(f, "ld"),
            Mul => write!(f, "mul"),
            Neg => write!(f, "neg"),
            Nop => write!(f, "nop"),
            Not => write!(f, "not"),
            Or => write!(f, "or"),
            Out => write!(f, "out"),
            Pop => write!(f, "pop"),
            Push => write!(f, "push"),
            Reset => write!(f, "reset"),
            Rti => write!(f, "rti"),
            Rtn => write!(f, "rtn"),
            Shl => write!(f, "shl"),
            Shr => write!(f, "shr"),
            St => write!(f, "st"),
            Sub => write!(f, "sub"),
            Swap => write!(f, "swap"),
            Trap => write!(f, "trap"),
            Xor => write!(f, "xor"),
        }
    }
}

pub(crate) fn parse_instruction_kind<Input, Error: ParseError<Input>>(
    input: Input,
) -> IResult<Input, InstructionKind, Error>
where
    Input: InputTake + Compare<&'static str> + Clone,
{
    use InstructionKind::*;

    // `alt` only allows for 21-member tuples so we need to trick a bit by nesting them
    alt((
        alt((
            value(Add, tag_no_case("add")),
            value(And, tag_no_case("and")),
            value(Call, tag_no_case("call")),
            value(Cmp, tag_no_case("cmp")),
            value(Div, tag_no_case("div")),
            value(Fas, tag_no_case("fas")),
            value(In, tag_no_case("in")),
            value(Jmp, tag_no_case("jmp")),
            value(Jeq, tag_no_case("jeq")),
            value(Jne, tag_no_case("jne")),
            value(Jle, tag_no_case("jle")),
            value(Jlt, tag_no_case("jlt")),
            value(Jge, tag_no_case("jge")),
            value(Jgt, tag_no_case("jgt")),
            value(Ld, tag_no_case("ld")),
            value(Mul, tag_no_case("mul")),
            value(Neg, tag_no_case("neg")),
            value(Nop, tag_no_case("nop")),
            value(Not, tag_no_case("not")),
            value(Or, tag_no_case("or")),
            value(Out, tag_no_case("out")),
        )),
        alt((
            value(Pop, tag_no_case("pop")),
            value(Push, tag_no_case("push")),
            value(Reset, tag_no_case("reset")),
            value(Rti, tag_no_case("rti")),
            value(Rtn, tag_no_case("rtn")),
            value(Shl, tag_no_case("shl")),
            value(Shr, tag_no_case("shr")),
            value(St, tag_no_case("st")),
            value(Sub, tag_no_case("sub")),
            value(Swap, tag_no_case("swap")),
            value(Trap, tag_no_case("trap")),
            value(Xor, tag_no_case("xor")),
        )),
    ))(input)
}

// TODO: save and embed location informations
/// Represents an instruction argument
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum InstructionArgument<L> {
    /// An immediate value
    Value(Node<L>),

    /// The content of a register
    Register(Reg),

    /// A direct memory access
    Direct(Located<Node<L>, L>),

    /// An indirect memory access (register)
    Indirect(Located<Reg, L>),

    /// An indexed memory access (register + offset)
    Indexed {
        register: Located<Reg, L>,
        value: Located<Node<L>, L>,
    },
}

impl<L> std::fmt::Display for InstructionArgument<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use InstructionArgument::*;

        match self {
            Value(v) => write!(f, "{}", v),
            Register(r) => write!(f, "{}", r),
            Direct(v) => write!(f, "[{}]", v.inner),
            Indirect(r) => write!(f, "[{}]", r.inner),
            Indexed { register, value } => write!(f, "[{} {:+}]", register.inner, value.inner),
        }
    }
}

/// Parse an instruction argument
pub(crate) fn parse_instruction_argument(
    input: &str,
) -> IResult<&str, InstructionArgument<RelativeLocation>> {
    alt((
        map(parse_expression, InstructionArgument::Value),
        map(parse_register, InstructionArgument::Register),
        parse_indexed,
        parse_direct,
        parse_indirect,
    ))(input)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum DirectiveKind {
    Addr,
    Space,
    String,
    Word,
}

impl<L> AstNode<L> for DirectiveKind {
    fn kind(&self) -> NodeKind {
        NodeKind::DirectiveKind
    }

    fn content(&self) -> Option<String> {
        Some(format!("{}", self))
    }

    // TODO: implement children
}

impl std::fmt::Display for DirectiveKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use DirectiveKind::*;

        match self {
            Addr => write!(f, "addr"),
            Space => write!(f, "space"),
            String => write!(f, "string"),
            Word => write!(f, "word"),
        }
    }
}

pub(crate) fn parse_directive_kind<Input, Error: ParseError<Input>>(
    input: Input,
) -> IResult<Input, DirectiveKind, Error>
where
    Input: InputTake + Compare<&'static str> + Clone,
{
    use DirectiveKind::*;

    alt((
        value(Addr, tag_no_case("addr")),
        value(Space, tag_no_case("space")),
        value(String, tag_no_case("string")),
        value(Word, tag_no_case("word")),
    ))(input)
}

/// Represents a directive argument
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum DirectiveArgument<L> {
    /// A string literal (`.string` directive)
    StringLiteral(String),

    /// An expression (`.addr`, `.word`, `.space` directives)
    Expression(Node<L>),
}

impl<L> AstNode<L> for DirectiveArgument<L> {
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

    fn children(&self) -> Vec<crate::ast::Node<L>> {
        match self {
            DirectiveArgument::StringLiteral(_) => Vec::new(),
            DirectiveArgument::Expression(e) => e.children(),
        }
    }
}

impl<L> std::fmt::Display for DirectiveArgument<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use DirectiveArgument::*;

        match self {
            StringLiteral(l) => write!(f, "{:?}", l),
            Expression(e) => write!(f, "{}", e),
        }
    }
}

/// Parse a directive argument
pub(crate) fn parse_directive_argument(
    input: &str,
) -> IResult<&str, DirectiveArgument<RelativeLocation>> {
    alt((
        map(parse_string_literal, DirectiveArgument::StringLiteral),
        map(parse_expression, DirectiveArgument::Expression),
    ))(input)
}

impl<L> From<&str> for DirectiveArgument<L> {
    fn from(literal: &str) -> Self {
        Self::StringLiteral(literal.to_string())
    }
}

impl<L> From<i128> for DirectiveArgument<L> {
    fn from(value: i128) -> Self {
        Self::Expression(Node::Literal(value))
    }
}

#[derive(Error, Debug)]
pub(crate) enum ComputeError {
    #[error("could not evaluate argument: {0}")]
    Evaluation(EvaluationError),
}

impl From<EvaluationError> for ComputeError {
    fn from(e: EvaluationError) -> Self {
        Self::Evaluation(e)
    }
}

impl<L> InstructionArgument<L> {
    pub(crate) fn evaluate<C: Context>(&self, context: &C) -> Result<Arg, ComputeError> {
        match self {
            Self::Value(v) => {
                let value = v.evaluate(context)?;
                Ok(Arg::Value(Value::Imm(value)))
            }
            Self::Register(register) => Ok(Arg::Value(Value::Reg(*register))),
            Self::Direct(v) => {
                let value = v.inner.evaluate(context)?;
                Ok(Arg::Address(Address::Dir(value)))
            }
            Self::Indirect(register) => Ok(Arg::Address(Address::Ind(register.inner))),
            Self::Indexed { register, value } => {
                let value = value.inner.evaluate(context)?;
                Ok(Arg::Address(Address::Idx(register.inner, value)))
            }
        }
    }
}

impl<L: Clone> AstNode<L> for InstructionArgument<L> {
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
            InstructionArgument::Register(r) => Some(format!("{}", r)),
            _ => None,
        }
    }

    fn children(&self) -> Vec<crate::ast::Node<L>> {
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

// TODO: get rid of this
pub(crate) fn parse_indirect_inner(
    _input: &str,
) -> IResult<&str, InstructionArgument<RelativeLocation>> {
    todo!()
}

fn parse_register(input: &str) -> IResult<&str, Reg> {
    use Reg::*;

    alt((
        value(A, tag_no_case("%a")),
        value(B, tag_no_case("%b")),
        value(PC, tag_no_case("%pc")),
        value(SP, tag_no_case("%sp")),
        value(SR, tag_no_case("%sr")),
    ))(input)
}

fn parse_indexed(input: &str) -> IResult<&str, InstructionArgument<RelativeLocation>> {
    #[derive(Clone)]
    enum Sign {
        Plus,
        Minus,
    };
    use Sign::*;

    let (rest, _) = char('[')(input)?;
    let (rest, _) = space0(rest)?;

    let start = rest;
    let (rest, register) = parse_register(rest)?;
    let register = register.with_location((input, start, rest));
    let (rest, _) = space0(rest)?;

    let sign_start = rest;
    let (rest, sign) = alt((value(Plus, char('+')), value(Minus, char('-'))))(rest)?;
    let (rest, _) = space0(rest)?;

    let expression_start = rest;
    let (rest, value) = parse_expression(rest)?;

    let value = match sign {
        Plus => value,
        Minus => Node::Invert(Box::new(value).with_location((input, expression_start, rest))),
    };
    let value = value.with_location((input, sign_start, rest));

    let (rest, _) = space0(rest)?;
    let (rest, _) = char(']')(rest)?;

    Ok((rest, InstructionArgument::Indexed { register, value }))
}

fn parse_direct(input: &str) -> IResult<&str, InstructionArgument<RelativeLocation>> {
    let (rest, _) = char('[')(input)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, value) = parse_expression(rest)?;
    let value = value.with_location((input, start, rest));
    let (rest, _) = space0(rest)?;
    let (rest, _) = char(']')(rest)?;
    Ok((rest, InstructionArgument::Direct(value)))
}

fn parse_indirect(input: &str) -> IResult<&str, InstructionArgument<RelativeLocation>> {
    let (rest, _) = char('[')(input)?;
    let (rest, _) = space0(rest)?;
    let start = rest;
    let (rest, register) = parse_register(rest)?;
    let register = register.with_location((input, start, rest));
    let (rest, _) = space0(rest)?;
    let (rest, _) = char(']')(rest)?;
    Ok((rest, InstructionArgument::Indirect(register)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_register_test() {
        let (input, register) = parse_register("%a").unwrap();
        assert_eq!(input, "");
        assert_eq!(register, Reg::A);
    }

    #[test]
    fn parse_direct_test() {
        let (input, node) = parse_direct("[3]").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            node,
            InstructionArgument::Direct(Node::Literal(3).with_location((1, 1)))
        );
    }

    #[test]
    fn parse_indirect_test() {
        let (input, node) = parse_indirect("[%a]").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            node,
            InstructionArgument::Indirect(Reg::A.with_location((1, 2)))
        );
    }

    #[test]
    fn parse_indexed_test() {
        let (input, node) = parse_indexed("[%a+2]").unwrap();
        assert_eq!(input, "");
        assert_eq!(
            node,
            InstructionArgument::Indexed {
                register: Reg::A.with_location((1, 2)),
                value: Node::Literal(2).with_location((3, 2)),
            }
        );
    }
}
