use std::collections::HashMap;

use thiserror::Error;

use crate::constants::*;
use crate::parser::expression::{
    Context as ExpressionContext, EmptyContext as EmptyExpressionContext,
    EvaluationError as ExpressionEvaluationError,
};
use crate::parser::{DirectiveArgument, Line, LineContent};

pub type Labels<'a> = HashMap<&'a str, u64>;

impl<'a> ExpressionContext for Labels<'a> {
    fn resolve_variable(&self, variable: &str) -> Option<i128> {
        self.get(variable).map(|v| *v as _)
    }
}

pub enum Placement<'a> {
    /// A memory cell filled by .space
    Reserved,

    /// A memory cell filled by .string
    Char(char),

    /// A instruction or a .word directive
    Line(&'a LineContent<'a>),
}

#[derive(Default)]
pub struct Layout<'a> {
    pub labels: Labels<'a>,
    pub memory: HashMap<u64, Placement<'a>>,
}

#[derive(Debug, Error)]
pub enum MemoryLayoutError<'a> {
    #[error("duplicate label {label}")]
    DuplicateLabel { label: &'a str },

    #[error("invalid argument for directive {directive}")]
    InvalidDirectiveArgument {
        directive: &'a str,
        argument: &'a DirectiveArgument<'a>,
    },

    #[error("unsupported directive {directive}")]
    UnsupportedDirective { directive: &'a str },

    #[error("failed to evaluate argument for directive {directive}: {inner}")]
    DirectiveArgumentEvaluation {
        directive: &'a str,
        inner: ExpressionEvaluationError<'a>,
    },
}

/// Lays out the memory
///
/// It places the labels & prepare a hashmap of cells to be filled.
pub fn layout_memory<'a>(program: &'a [Line<'a>]) -> Result<Layout<'a>, MemoryLayoutError<'a>> {
    use MemoryLayoutError::*;
    let mut layout: Layout = Default::default();
    let mut position = PROGRAM_START;

    for line in program {
        for label in line.symbols.iter() {
            if layout.labels.contains_key(label) {
                return Err(DuplicateLabel { label });
            }

            layout.labels.insert(*label, position);
        }

        if let Some(ref content) = line.content {
            match content {
                LineContent::Directive {
                    directive: "word", ..
                }
                | LineContent::Instruction { .. } => {
                    layout.memory.insert(position, Placement::Line(content));
                    position += 1; // Instructions and word directives take one memory cell
                }

                LineContent::Directive {
                    directive,
                    argument: DirectiveArgument::Expression(e),
                } if *directive == "space" => {
                    let size = e
                        .evaluate(&EmptyExpressionContext)
                        .map_err(|inner| DirectiveArgumentEvaluation { directive, inner })?;

                    for _ in 0..size {
                        layout.memory.insert(position, Placement::Reserved);
                        position += 1;
                    }
                }

                LineContent::Directive {
                    directive,
                    argument: DirectiveArgument::Expression(e),
                } if *directive == "addr" => {
                    let addr = e
                        .evaluate(&EmptyExpressionContext)
                        .map_err(|inner| DirectiveArgumentEvaluation { directive, inner })?;

                    // The ".addr N" directive changes the current address to N
                    position = addr;
                }

                LineContent::Directive {
                    directive: "string",
                    argument: DirectiveArgument::StringLiteral(literal),
                } => {
                    // Fill the memory with the chars of the string
                    for c in literal.chars() {
                        layout.memory.insert(position, Placement::Char(c));
                        position += 1;
                    }
                }

                LineContent::Directive {
                    directive,
                    argument,
                } => {
                    if matches!(*directive, "space" | "addr" | "string") {
                        return Err(InvalidDirectiveArgument {
                            directive,
                            argument,
                        });
                    } else {
                        return Err(UnsupportedDirective { directive });
                    }
                }
            }
        }
    }

    Ok(layout)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::expression::Node;
    use crate::parser::{Argument, Line};

    #[test]
    fn place_labels_simple_test() {
        let program = vec![
            Line::default().symbol("main").instruction(
                "add",
                vec![Argument::Register("a"), Argument::Register("b")],
            ),
            Line::default()
                .symbol("loop")
                .instruction("jmp", vec![Argument::Value(Node::Variable("main"))]),
        ];

        let labels = layout_memory(&program).unwrap().labels;
        let expected = {
            let mut h = HashMap::new();
            h.insert("main", PROGRAM_START);
            h.insert("loop", PROGRAM_START + 1);
            h
        };
        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_addr_test() {
        let program = vec![
            Line::default().directive("addr", 10),
            Line::default()
                .symbol("main")
                .instruction("jmp", vec![Argument::Value(Node::Variable("main"))]),
        ];

        let labels = layout_memory(&program).unwrap().labels;
        let expected = {
            let mut h = HashMap::new();
            h.insert("main", 10);
            h
        };
        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_space_test() {
        let program = vec![
            Line::default().symbol("first").directive("space", 10),
            Line::default().symbol("second").directive("space", 5),
            Line::default()
                .symbol("main")
                .instruction("jmp", vec![Argument::Value(Node::Variable("main"))]),
        ];

        let labels = layout_memory(&program).unwrap().labels;
        let expected = {
            let mut h = HashMap::new();
            h.insert("first", PROGRAM_START);
            h.insert("second", PROGRAM_START + 10);
            h.insert("main", PROGRAM_START + 15);
            h
        };

        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_word_test() {
        let program = vec![
            Line::default().symbol("first").directive("word", 123),
            Line::default().symbol("second").directive("word", 456),
            Line::default()
                .symbol("main")
                .instruction("jmp", vec![Argument::Value(Node::Variable("main"))]),
        ];

        let labels = layout_memory(&program).unwrap().labels;
        let expected = {
            let mut h = HashMap::new();
            h.insert("first", PROGRAM_START);
            h.insert("second", PROGRAM_START + 1);
            h.insert("main", PROGRAM_START + 2);
            h
        };

        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_string_test() {
        let program = vec![
            Line::default().symbol("first").directive("string", "hello"),
            Line::default()
                .symbol("second")
                .directive("string", "Émoticône: 🚙"), // length: 12 chars
            Line::default()
                .symbol("main")
                .instruction("jmp", vec![Argument::Value(Node::Variable("main"))]),
        ];

        let labels = layout_memory(&program).unwrap().labels;
        let expected = {
            let mut h = HashMap::new();
            h.insert("first", PROGRAM_START);
            h.insert("second", PROGRAM_START + 5);
            h.insert("main", PROGRAM_START + 5 + 12);
            h
        };

        assert_eq!(labels, expected);
    }
}
