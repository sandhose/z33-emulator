use nom::{
    branch::alt,
    bytes::complete::tag,
    bytes::complete::tag_no_case,
    character::complete::one_of,
    character::complete::space0,
    combinator::map,
    combinator::value,
    error::ParseError,
    sequence::{delimited, preceded, terminated},
    Compare, IResult, InputTake,
};
use thiserror::Error;

use super::{
    expression::{parse_expression, Context, EvaluationError, Node},
    literal::parse_string_literal,
};
use crate::runtime::{Address, Arg, Reg, Value};

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

/// Represents an instruction argument
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum InstructionArgument<'a> {
    /// An immediate value
    Value(Node<'a>),

    /// The content of a register
    Register(Reg),

    /// A direct memory access
    Direct(Node<'a>),

    /// An indirect memory access (register)
    Indirect(Reg),

    /// An indexed memory access (register + offset)
    Indexed { register: Reg, value: Node<'a> },
}

impl<'a> std::fmt::Display for InstructionArgument<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use InstructionArgument::*;

        match self {
            Value(v) => write!(f, "{}", v),
            Register(r) => write!(f, "%{}", r),
            Direct(v) => write!(f, "[{}]", v),
            Indirect(r) => write!(f, "[%{}]", r),
            Indexed { register, value } => write!(f, "[%{} {:+}]", register, value),
        }
    }
}

/// Parse an instruction argument
pub(crate) fn parse_instruction_argument<'a>(
    input: &'a str,
) -> IResult<&'a str, InstructionArgument<'a>> {
    alt((parse_direct, parse_indirect))(input)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum DirectiveKind {
    Addr,
    Space,
    String,
    Word,
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
pub(crate) enum DirectiveArgument<'a> {
    /// A string literal (`.string` directive)
    StringLiteral(String),

    /// An expression (`.addr`, `.word`, `.space` directives)
    Expression(Node<'a>),
}

impl<'a> std::fmt::Display for DirectiveArgument<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use DirectiveArgument::*;

        match self {
            StringLiteral(l) => write!(f, "{:?}", l),
            Expression(e) => write!(f, "{}", e),
        }
    }
}

/// Parse a directive argument
pub(crate) fn parse_directive_argument(input: &str) -> IResult<&str, DirectiveArgument> {
    alt((
        map(parse_string_literal, DirectiveArgument::StringLiteral),
        map(parse_expression, DirectiveArgument::Expression),
    ))(input)
}

impl<'a> From<&str> for DirectiveArgument<'a> {
    fn from(literal: &str) -> Self {
        Self::StringLiteral(literal.to_string())
    }
}

impl<'a> From<i128> for DirectiveArgument<'a> {
    fn from(value: i128) -> Self {
        Self::Expression(Node::Literal(value))
    }
}

#[derive(Error, Debug)]
pub(crate) enum ComputeError<'a> {
    #[error("could not evaluate argument: {0}")]
    Evaluation(EvaluationError<'a>),
}

impl<'a> From<EvaluationError<'a>> for ComputeError<'a> {
    fn from(e: EvaluationError<'a>) -> Self {
        Self::Evaluation(e)
    }
}

impl<'a> InstructionArgument<'a> {
    pub(crate) fn evaluate<C: Context>(&self, context: &C) -> Result<Arg, ComputeError<'a>> {
        match self {
            Self::Value(v) => {
                let value = v.evaluate(context)?;
                Ok(Arg::Value(Value::Imm(value)))
            }
            Self::Register(register) => Ok(Arg::Value(Value::Reg(*register))),
            Self::Direct(v) => {
                let value = v.evaluate(context)?;
                Ok(Arg::Address(Address::Dir(value)))
            }
            Self::Indirect(register) => Ok(Arg::Address(Address::Ind(*register))),
            Self::Indexed { register, value } => {
                let value = value.evaluate(context)?;
                Ok(Arg::Address(Address::Idx(*register, value)))
            }
        }
    }
}

fn parse_register<'a>(input: &'a str) -> IResult<&'a str, Reg> {
    use Reg::*;

    alt((
        value(A, tag_no_case("%a")),
        value(B, tag_no_case("%b")),
        value(PC, tag_no_case("%pc")),
        value(SP, tag_no_case("%sp")),
        value(SR, tag_no_case("%sr")),
    ))(input)
}

fn parse_indirect_indexed_inner<'a>(input: &'a str) -> IResult<&'a str, InstructionArgument<'a>> {
    let (input, register) = parse_register(input)?;
    let (input, _) = space0(input)?;
    let (input, sign) = one_of("+-")(input)?;
    let (input, _) = space0(input)?;
    let (input, value) = parse_expression(input)?;

    let value = match sign {
        '+' => value,
        '-' => Node::Invert(Box::new(value)),
        _ => unreachable!(),
    };

    Ok((input, InstructionArgument::Indexed { register, value }))
}

pub(crate) fn parse_indirect_inner<'a>(
    input: &'a str,
) -> IResult<&'a str, InstructionArgument<'a>> {
    alt((
        parse_indirect_indexed_inner,
        map(parse_register, InstructionArgument::Indirect),
        map(parse_expression, InstructionArgument::Direct),
    ))(input)
}

fn parse_indirect<'a>(input: &'a str) -> IResult<&'a str, InstructionArgument<'a>> {
    delimited(
        terminated(tag("["), space0),
        parse_indirect_inner,
        preceded(space0, tag("]")),
    )(input)
}

fn parse_direct<'a>(input: &'a str) -> IResult<&'a str, InstructionArgument<'a>> {
    alt((
        map(parse_register, InstructionArgument::Register),
        map(parse_expression, InstructionArgument::Value),
    ))(input)
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
}
