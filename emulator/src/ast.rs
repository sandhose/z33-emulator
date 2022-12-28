//! Utility AST manipulation, mainly for reporting

use crate::parser::location::{Located, MapLocation};

pub trait AstNode<L> {
    fn kind(&self) -> NodeKind;

    fn content(&self) -> Option<String> {
        None
    }

    fn children(&self) -> Vec<Node<L>> {
        Vec::new()
    }
}

impl<L, T: AstNode<L>> AstNode<L> for Box<T> {
    fn kind(&self) -> NodeKind {
        self.as_ref().kind()
    }

    fn content(&self) -> Option<String> {
        self.as_ref().content()
    }

    fn children(&self) -> Vec<Node<L>> {
        self.as_ref().children()
    }
}

impl<N: AstNode<L>, L: Clone> Located<N, L> {
    pub fn to_node(&self) -> Node<L> {
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

pub struct Node<L> {
    pub(crate) kind: NodeKind,
    pub(crate) children: Vec<Node<L>>,
    pub(crate) content: Option<String>,
    pub(crate) location: L,
}

impl<L, P> MapLocation<P> for Node<L>
where
    L: MapLocation<P, Mapped = P>,
{
    type Mapped = Node<P>;

    fn map_location(self, parent: &P) -> Self::Mapped {
        let location = self.location.map_location(parent);
        let children = self.children.map_location(&location);

        Node {
            kind: self.kind,
            children,
            content: self.content,
            location,
        }
    }
}

impl<L> Node<L> {
    pub(crate) fn new(kind: NodeKind, location: L) -> Self {
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

    /// Transforms the location of AST nodes relative to their parent
    pub fn transform_location<O, M>(self, parent: &O, mapper: &M) -> Node<O>
    where
        M: Fn(L, &O) -> O,
    {
        let location = mapper(self.location, parent);
        let children = self
            .children
            .into_iter()
            .map(|n| n.transform_location(&location, mapper))
            .collect();
        let kind = self.kind;
        let content = self.content;
        Node {
            kind,
            children,
            content,
            location,
        }
    }
}

impl<L: std::fmt::Display> std::fmt::Display for Node<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_indent(f, 0)
    }
}

impl<L: std::fmt::Display> Node<L> {
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

            writeln!(f, "{:?}({content:?}) @ {}", self.kind, self.location)?;
        } else {
            writeln!(f, "{:?} @ {}", self.kind, self.location)?;
        }

        for child in &self.children {
            child.fmt_indent(f, level + 1)?;
        }

        Ok(())
    }
}
