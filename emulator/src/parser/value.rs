use parse_display::{Display, FromStr};
use thiserror::Error;

use super::expression::{Context, EvaluationError, Node};
use super::location::Located;
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

    /// A parse error that was recovered from
    #[display("<error>")]
    Error,
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

    #[error("cannot evaluate error placeholder")]
    Error,
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
            Self::Error => Err(ComputeError::Error),
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
            InstructionArgument::Error => NodeKind::Error,
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
            InstructionArgument::Direct(e) => vec![e.to_node()],
            InstructionArgument::Indirect(r) => vec![r.to_node()],
            InstructionArgument::Indexed { register, value } => {
                vec![register.to_node(), value.to_node()]
            }
            InstructionArgument::Register(_) | InstructionArgument::Error => Vec::new(),
        }
    }
}
