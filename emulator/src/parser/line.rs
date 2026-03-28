//! Program line parsing
//!
//! This module parses whole program lines, including the symbol definitions,
//! the comments and the line content itself (either an instruction or a
//! directive).
//!
//! When parsing, this module does zero copy over the original input. All
//! members of resulting Line structure reference part of the input, hence the
//! associated lifetime on the structure tied to the original input. This allows
//! some neat tricks, especially calculating the offset of a property from the
//! input string.

use smallvec::SmallVec;

#[cfg(test)]
use super::location::Locatable;
use super::location::Located;
use super::value::{DirectiveArgument, DirectiveKind, InstructionArgument, InstructionKind};
use crate::ast::{AstNode, Node, NodeKind};

/// Holds the content of a line
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum LineContent {
    /// Represents an instruction, with its opcode and list of arguments
    Instruction {
        kind: Located<InstructionKind>,
        arguments: Vec<Located<InstructionArgument>>,
    },
    /// Represents a directive, with its type and argument
    Directive {
        kind: Located<DirectiveKind>,
        argument: Located<DirectiveArgument>,
    },
    /// Represents a parse error that was recovered from
    Error,
}

impl LineContent {
    /// Check if the line is a directive
    pub(crate) fn is_directive(&self) -> bool {
        matches!(self, Self::Directive { .. })
    }
}

impl AstNode for LineContent {
    fn kind(&self) -> NodeKind {
        match self {
            LineContent::Instruction { .. } => NodeKind::Instruction,
            LineContent::Directive { .. } => NodeKind::Directive,
            LineContent::Error => NodeKind::Error,
        }
    }

    fn children(&self) -> Vec<Node> {
        match self {
            LineContent::Instruction { kind, arguments } => std::iter::once(kind.to_node())
                .chain(arguments.iter().map(Located::to_node))
                .collect(),
            LineContent::Directive { kind, argument } => vec![kind.to_node(), argument.to_node()],
            LineContent::Error => Vec::new(),
        }
    }
}

impl std::fmt::Display for LineContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LineContent::Instruction { kind, arguments } => {
                // First write the opcode
                write!(f, "{:4}", kind.inner)?;

                // then the list of arguments
                let mut first = true; // This is to properly show comma between arguments
                for arg in arguments {
                    if !first {
                        write!(f, ",")?;
                    }
                    write!(f, " {}", arg.inner)?;
                    first = false;
                }
                Ok(())
            }
            LineContent::Directive { kind, argument } => {
                write!(f, ".{}: {}", kind.inner, argument.inner)
            }
            LineContent::Error => write!(f, "<error>"),
        }
    }
}

/// Holds a whole line, with the symbol definitions (if any), the content (if
/// any) and the comment (if any).
///
/// Note that the `Default::default()` implementation represents an empty line.
#[derive(Debug, Clone, PartialEq, Default)]
pub(crate) struct Line {
    pub symbols: SmallVec<[Located<String>; 1]>,
    pub content: Option<Located<LineContent>>,
    /// Trailing inline comment (`// ...`), if any.
    pub comment: Option<Located<String>>,
}

impl AstNode for Line {
    fn kind(&self) -> NodeKind {
        NodeKind::Line
    }

    fn children(&self) -> Vec<Node> {
        let mut children = Vec::new();

        children.extend(
            self.symbols
                .iter()
                .map(|s| Node::new(NodeKind::Symbol, s.location.clone()).content(s.inner.clone())),
        );

        children.extend(self.content.iter().map(Located::to_node));

        children.extend(
            self.comment
                .iter()
                .map(|c| Node::new(NodeKind::Comment, c.location.clone()).content(c.inner.clone())),
        );

        children
    }
}

impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut had_something = false;
        for symbol in &self.symbols {
            write!(f, "{}: ", symbol.inner)?;
            had_something = true;
        }

        if let Some(ref c) = self.content {
            if !c.inner.is_directive() && !had_something {
                write!(f, "    ")?;
            }
            write!(f, "{}", c.inner)?;
            had_something = true;
        }

        if let Some(ref comment) = self.comment {
            if had_something {
                write!(f, " ")?;
            }
            write!(f, "// {}", comment.inner)?;
        }

        Ok(())
    }
}

impl Line {
    #[cfg(test)] // Only used in tests for now
    pub(crate) fn empty() -> Located<Self> {
        Self::default().with_location(0..0)
    }
}

impl Located<Line> {
    #[cfg(test)] // Only used in tests for now
    pub(crate) fn symbol(mut self, symbol: &str) -> Self {
        self.inner
            .symbols
            .push(symbol.to_string().with_location(0..0));
        self
    }

    #[cfg(test)] // Only used in tests for now
    pub(crate) fn instruction(
        mut self,
        kind: InstructionKind,
        arguments: Vec<InstructionArgument>,
    ) -> Self {
        self.inner.content = Some(
            LineContent::Instruction {
                kind: kind.with_location(0..0),
                arguments: arguments
                    .into_iter()
                    .map(|a| a.with_location(0..0))
                    .collect(),
            }
            .with_location(0..0),
        );
        self
    }

    #[cfg(test)] // Only used in tests for now
    pub(crate) fn directive<T: Into<DirectiveArgument>>(
        mut self,
        kind: DirectiveKind,
        argument: T,
    ) -> Self {
        self.inner.content = Some(
            LineContent::Directive {
                kind: kind.with_location(0..0),
                argument: argument.into().with_location(0..0),
            }
            .with_location(0..0),
        );
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub(crate) lines: Vec<Located<Line>>,
}

impl Program {
    #[must_use]
    pub fn labels(&self) -> Vec<&str> {
        self.lines
            .iter()
            .flat_map(|line| line.inner.symbols.iter().map(|s| s.inner.as_str()))
            .collect()
    }
}

impl AstNode for Program {
    fn kind(&self) -> NodeKind {
        NodeKind::Program
    }

    fn children(&self) -> Vec<Node> {
        self.lines.iter().map(Located::to_node).collect()
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in &self.lines {
            writeln!(f, "{}", line.inner)?;
        }

        Ok(())
    }
}
