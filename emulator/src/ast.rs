//! Utility AST manipulation, mainly for reporting

use std::ops::Range;

use crate::parser::location::Located;

pub trait AstNode {
    fn kind(&self) -> NodeKind;

    fn content(&self) -> Option<String> {
        None
    }

    fn children(&self) -> Vec<Node> {
        Vec::new()
    }
}

impl<N: AstNode> AstNode for Box<N> {
    fn kind(&self) -> NodeKind {
        self.as_ref().kind()
    }

    fn content(&self) -> Option<String> {
        self.as_ref().content()
    }

    fn children(&self) -> Vec<Node> {
        self.as_ref().children()
    }
}

impl<N: AstNode> Located<N> {
    pub fn to_node(&self) -> Node {
        let kind = self.inner.kind();
        let children = self.inner.children();
        let content = self.inner.content();
        let location = self.location.clone();
        Node {
            kind,
            children,
            content,
            location,
        }
    }
}

#[derive(Debug)]
pub enum NodeKind {
    Program,
    Line,

    // Children of Line
    Symbol,
    Instruction,
    Directive,
    Comment,

    // Children of LineContent
    InstructionKind,
    DirectiveKind,

    // Children of InstructionArgument
    Register,
    Direct,
    Indirect,
    Indexed,

    // Children of DirectiveArgument
    StringLiteral,
    ExpressionBinaryOr,
    ExpressionBinaryAnd,
    ExpressionBinaryNot,
    ExpressionLeftShift,
    ExpressionRightShift,
    ExpressionSum,
    ExpressionSubstract,
    ExpressionMultiply,
    ExpressionDivide,
    ExpressionInvert,
    ExpressionLiteral,
    ExpressionVariable,
}

pub struct Node {
    pub(crate) kind: NodeKind,
    pub(crate) children: Vec<Node>,
    pub(crate) content: Option<String>,
    pub(crate) location: Range<usize>,
}

impl Node {
    pub(crate) fn new(kind: NodeKind, location: Range<usize>) -> Self {
        Node {
            kind,
            children: Vec::new(),
            content: None,
            location,
        }
    }

    pub(crate) fn content(mut self, content: String) -> Self {
        self.content = Some(content);
        self
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_indent(f, 0)
    }
}

impl Node {
    fn fmt_indent(&self, f: &mut std::fmt::Formatter<'_>, level: usize) -> std::fmt::Result {
        for _ in 0..level {
            write!(f, "  ")?;
        }

        if let Some(ref content) = self.content {
            let content = if content.chars().count() > 16 {
                content
                    .chars()
                    .take(15)
                    .chain(std::iter::once('…'))
                    .collect()
            } else {
                content.clone()
            };

            writeln!(f, "{:?}({content:?}) @ {:?}", self.kind, self.location)?;
        } else {
            writeln!(f, "{:?} @ {:?}", self.kind, self.location)?;
        }

        for child in &self.children {
            child.fmt_indent(f, level + 1)?;
        }

        Ok(())
    }
}
