//! Collect all symbol references from the AST.
//!
//! Walks the parsed program and preprocessor annotations to build a list of
//! every symbol (label or macro) definition and reference with its precise
//! byte span in the original source.

use std::ops::Range;

use crate::parser::expression::Node as ExpressionNode;
use crate::parser::line::{LineContent, Program};
use crate::parser::value::{DirectiveArgument, InstructionArgument};
use crate::preprocessor::SourceAnnotations;

/// A single occurrence of a symbol in the source.
#[derive(Debug, Clone)]
pub struct SymbolOccurrence {
    /// The symbol name.
    pub name: String,
    /// Byte span in the preprocessed source (use `resolve_span` to map to
    /// original).
    pub span: Range<usize>,
    /// Whether this is a definition or a reference.
    pub kind: OccurrenceKind,
}

/// Whether a symbol occurrence is a definition or a reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OccurrenceKind {
    /// Label definition (`foo:`) or `#define`.
    Definition,
    /// Usage in an instruction argument, directive argument, etc.
    Reference,
}

/// Collect all symbol occurrences from the parsed program and annotations.
///
/// Returns occurrences with spans in the **preprocessed** source. The caller
/// should use `DocumentState::resolve_span` to map them to original-source
/// coordinates.
pub fn collect_occurrences(
    program: Option<&Program>,
    annotations: Option<&SourceAnnotations>,
) -> Vec<SymbolOccurrence> {
    let mut occurrences = Vec::new();

    if let Some(program) = program {
        for line in &program.lines {
            let line_inner = &line.inner;

            // Label definitions
            for symbol in &line_inner.symbols {
                occurrences.push(SymbolOccurrence {
                    name: symbol.inner.clone(),
                    span: symbol.location.clone(),
                    kind: OccurrenceKind::Definition,
                });
            }

            // References in instruction/directive content
            if let Some(content) = &line_inner.content {
                collect_from_content(&content.inner, &mut occurrences);
            }
        }
    }

    // Macro definitions from annotations
    if let Some(ann) = annotations {
        for def in &ann.definitions {
            occurrences.push(SymbolOccurrence {
                name: def.key.clone(),
                span: def.span.clone(),
                kind: OccurrenceKind::Definition,
            });
        }
    }

    occurrences
}

/// Collect variable references from a `LineContent`.
fn collect_from_content(content: &LineContent, out: &mut Vec<SymbolOccurrence>) {
    match content {
        LineContent::Instruction { arguments, .. } => {
            for arg in arguments {
                collect_from_argument(&arg.inner, &arg.location, out);
            }
        }
        LineContent::Directive { argument, .. } => match &argument.inner {
            DirectiveArgument::Expression(node) => {
                collect_from_expression_top(node, &argument.location, out);
            }
            DirectiveArgument::StringLiteral(_) => {}
        },
        LineContent::Error => {}
    }
}

/// Collect variable references from an instruction argument.
fn collect_from_argument(
    arg: &InstructionArgument,
    arg_span: &Range<usize>,
    out: &mut Vec<SymbolOccurrence>,
) {
    match arg {
        InstructionArgument::Value(node) => {
            // For a top-level Value, the Node is not Located — use the
            // argument's span if it's a simple Variable.
            collect_from_expression_top(node, arg_span, out);
        }
        InstructionArgument::Direct(located_node) => {
            collect_from_expression(&located_node.inner, &located_node.location, out);
        }
        InstructionArgument::Indexed { value, .. } => {
            collect_from_expression(&value.inner, &value.location, out);
        }
        InstructionArgument::Register(_)
        | InstructionArgument::Indirect(_)
        | InstructionArgument::Error => {}
    }
}

/// Collect from a top-level expression node that doesn't have its own
/// `Located` wrapper. If it's a simple `Variable`, use `fallback_span`.
fn collect_from_expression_top(
    node: &ExpressionNode,
    fallback_span: &Range<usize>,
    out: &mut Vec<SymbolOccurrence>,
) {
    match node {
        ExpressionNode::Variable(name) => {
            out.push(SymbolOccurrence {
                name: name.clone(),
                span: fallback_span.clone(),
                kind: OccurrenceKind::Reference,
            });
        }
        _ => collect_from_expression(node, fallback_span, out),
    }
}

/// Recursively collect `Variable` references from an expression tree.
fn collect_from_expression(
    node: &ExpressionNode,
    parent_span: &Range<usize>,
    out: &mut Vec<SymbolOccurrence>,
) {
    match node {
        ExpressionNode::Variable(name) => {
            out.push(SymbolOccurrence {
                name: name.clone(),
                span: parent_span.clone(),
                kind: OccurrenceKind::Reference,
            });
        }
        ExpressionNode::BinaryOr(a, b)
        | ExpressionNode::BinaryAnd(a, b)
        | ExpressionNode::LeftShift(a, b)
        | ExpressionNode::RightShift(a, b)
        | ExpressionNode::Sum(a, b)
        | ExpressionNode::Substract(a, b)
        | ExpressionNode::Multiply(a, b)
        | ExpressionNode::Divide(a, b) => {
            collect_from_expression(&a.inner, &a.location, out);
            collect_from_expression(&b.inner, &b.location, out);
        }
        ExpressionNode::Invert(a) | ExpressionNode::BinaryNot(a) => {
            collect_from_expression(&a.inner, &a.location, out);
        }
        ExpressionNode::Literal(_) | ExpressionNode::Error => {}
    }
}
