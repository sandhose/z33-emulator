use std::collections::HashMap;

use thiserror::Error;
use tracing::debug;

use crate::constants::*;
use crate::parser::expression::{
    Context as ExpressionContext, EmptyContext as EmptyExpressionContext,
    EvaluationError as ExpressionEvaluationError,
};
use crate::parser::line::{Line, LineContent};
use crate::parser::value::{DirectiveArgument, DirectiveKind};

pub(crate) type Labels<'a> = HashMap<&'a str, u64>;

impl<'a> ExpressionContext for Labels<'a> {
    fn resolve_variable(&self, variable: &str) -> Option<i128> {
        self.get(variable).map(|v| *v as _)
    }
}

pub(crate) enum Placement<'a> {
    /// A memory cell filled by .space
    Reserved,

    /// A memory cell filled by .string
    Char(char),

    /// A instruction or a .word directive
    Line(&'a LineContent<'a>),
}

#[derive(Default)]
pub(crate) struct Layout<'a> {
    pub labels: Labels<'a>,
    pub memory: HashMap<u64, Placement<'a>>,
}

impl<'a> Layout<'a> {
    fn insert_placement(
        &mut self,
        address: u64,
        placement: Placement<'a>,
    ) -> Result<(), MemoryLayoutError<'a>> {
        if self.memory.contains_key(&address) {
            return Err(MemoryLayoutError::MemoryOverlap { address });
        }

        self.memory.insert(address, placement);
        Ok(())
    }

    fn insert_label(&mut self, label: &'a str, address: u64) -> Result<(), MemoryLayoutError<'a>> {
        if self.labels.contains_key(label) {
            return Err(MemoryLayoutError::DuplicateLabel { label });
        }

        self.labels.insert(label, address);
        Ok(())
    }
}

#[derive(Debug, Error, PartialEq)]
pub(crate) enum MemoryLayoutError<'a> {
    #[error("duplicate label {label}")]
    DuplicateLabel { label: &'a str },

    #[error("invalid argument for directive .{kind}")]
    InvalidDirectiveArgument {
        kind: DirectiveKind,
        argument: &'a DirectiveArgument<'a>,
    },

    #[error("failed to evaluate argument for directive .{kind}: {inner}")]
    DirectiveArgumentEvaluation {
        kind: DirectiveKind,
        inner: ExpressionEvaluationError<'a>,
    },

    #[error("address {address} is already filled")]
    MemoryOverlap { address: u64 },
}

/// Lays out the memory
///
/// It places the labels & prepare a hashmap of cells to be filled.
#[tracing::instrument(skip(program))]
pub(crate) fn layout_memory<'a>(
    program: &'a [Line<'a>],
) -> Result<Layout<'a>, MemoryLayoutError<'a>> {
    use DirectiveKind::*;
    use MemoryLayoutError::*;

    debug!("Laying out memory");
    let mut layout: Layout = Default::default();
    let mut position = PROGRAM_START;

    for line in program {
        for key in line.symbols.iter() {
            debug!(key, position, "Inserting label");
            layout.insert_label(*key, position)?;
        }

        if let Some(ref content) = line.content {
            match content {
                LineContent::Directive { kind: Word, .. } | LineContent::Instruction { .. } => {
                    layout.insert_placement(position, Placement::Line(content))?;
                    debug!(position, content = %content, "Inserting line");
                    position += 1; // Instructions and word directives take one memory cell
                }

                LineContent::Directive {
                    kind: Space,
                    argument: DirectiveArgument::Expression(e),
                } => {
                    let size = e
                        .evaluate(&EmptyExpressionContext)
                        .map_err(|inner| DirectiveArgumentEvaluation { kind: Space, inner })?;

                    debug!(size, position, "Reserving space");

                    for _ in 0..size {
                        layout.insert_placement(position, Placement::Reserved)?;
                        position += 1;
                    }
                }

                LineContent::Directive {
                    kind: Addr,
                    argument: DirectiveArgument::Expression(e),
                } => {
                    let addr = e
                        .evaluate(&EmptyExpressionContext)
                        .map_err(|inner| DirectiveArgumentEvaluation { kind: Addr, inner })?;

                    debug!(addr, "Changing address");

                    // The ".addr N" directive changes the current address to N
                    position = addr;
                }

                LineContent::Directive {
                    kind: String,
                    argument: DirectiveArgument::StringLiteral(string),
                } => {
                    debug!(position, string = string.as_str(), "Inserting string");
                    // Fill the memory with the chars of the string
                    for c in string.chars() {
                        layout.insert_placement(position, Placement::Char(c))?;
                        position += 1;
                    }
                }

                LineContent::Directive { kind, argument } => {
                    let kind = *kind;
                    return Err(InvalidDirectiveArgument { kind, argument });
                }
            }
        }
    }

    Ok(layout)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::line::Line;
    use crate::parser::value::{InstructionArgument, InstructionKind};
    use crate::{parser::expression::Node, runtime::Reg};

    use DirectiveKind::*;
    use InstructionKind::*;

    #[test]
    fn place_labels_simple_test() {
        let program = vec![
            Line::default().symbol("main").instruction(
                Add,
                vec![
                    InstructionArgument::Register(Reg::A),
                    InstructionArgument::Register(Reg::B),
                ],
            ),
            Line::default().symbol("loop").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main"))],
            ),
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
            Line::default().directive(Addr, 10),
            Line::default().symbol("main").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main"))],
            ),
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
            Line::default().symbol("first").directive(Space, 10),
            Line::default().symbol("second").directive(Space, 5),
            Line::default().symbol("main").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main"))],
            ),
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
            Line::default().symbol("first").directive(Word, 123),
            Line::default().symbol("second").directive(Word, 456),
            Line::default().symbol("main").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main"))],
            ),
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
            Line::default().symbol("first").directive(String, "hello"),
            Line::default()
                .symbol("second")
                .directive(String, "Émoticône: 🚙"), // length: 12 chars
            Line::default().symbol("main").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main"))],
            ),
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

    #[test]
    fn duplicate_label_test() {
        let program = vec![
            Line::default().symbol("hello"),
            Line::default().symbol("hello"),
        ];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::DuplicateLabel { label: "hello" })
        );
    }

    #[test]
    fn invalid_directive_argument_test() {
        let program = vec![Line::default().directive(String, 3)];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::InvalidDirectiveArgument {
                kind: String,
                argument: &3.into(),
            })
        );

        let program = vec![Line::default().directive(Space, "hello")];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::InvalidDirectiveArgument {
                kind: Space,
                argument: &"hello".into(),
            })
        );

        let program = vec![Line::default().directive(Addr, "hello")];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::InvalidDirectiveArgument {
                kind: Addr,
                argument: &"hello".into(),
            })
        );
    }

    #[test]
    fn memory_overlap_test() {
        let program = vec![
            Line::default().directive(Addr, 10),
            Line::default().directive(String, "hello"), // This takes 5 chars, so fills cells 10 to 15
            Line::default().directive(Addr, 14),
            Line::default().directive(Word, 0), // This overlaps with the second "l"
        ];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::MemoryOverlap { address: 14 })
        );
    }
}
