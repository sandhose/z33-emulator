use crate::parser::location::Located;

pub(crate) trait AstNode<L> {
    fn kind(&self) -> NodeKind;

    fn content(&self) -> Option<String> {
        None
    }

    fn children(&self) -> Vec<Node<L>> {
        Vec::new()
    }
}

impl<N: AstNode<L>, L: Clone> Located<N, L> {
    pub(crate) fn to_node(&self) -> Node<L> {
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
pub(crate) enum NodeKind {
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

pub(crate) struct Node<L> {
    pub(crate) kind: NodeKind,
    pub(crate) children: Vec<Node<L>>,
    pub(crate) content: Option<String>,
    pub(crate) location: L,
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

    pub(crate) fn map_location<O, M>(self, mapper: &M) -> Node<O>
    where
        M: Fn(L) -> O,
    {
        let location = mapper(self.location);
        let children = self
            .children
            .into_iter()
            .map(|n| n.map_location(mapper))
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

    pub(crate) fn transform_location<O, M>(self, parent: &O, mapper: &M) -> Node<O>
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

            writeln!(f, "{:?}({:?}) @ {}", self.kind, content, self.location)?;
        } else {
            writeln!(f, "{:?} @ {}", self.kind, self.location)?;
        }

        for child in self.children.iter() {
            child.fmt_indent(f, level + 1)?;
        }

        Ok(())
    }
}
