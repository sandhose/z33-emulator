use std::collections::HashMap;

use parse_display::Display;
use thiserror::Error;
use tracing::{debug, trace};

use crate::parser::{
    expression::{
        Context as ExpressionContext, EmptyContext as EmptyExpressionContext,
        EvaluationError as ExpressionEvaluationError,
    },
    line::{Line, LineContent},
    value::{DirectiveArgument, DirectiveKind},
};
use crate::{
    constants::{Address, PROGRAM_START},
    parser::location::Located,
};

pub(crate) type Labels = HashMap<String, Address>;

impl ExpressionContext for Labels {
    fn resolve_variable(&self, variable: &str) -> Option<i128> {
        self.get(variable).map(|v| i128::from(*v))
    }
}

#[derive(Display)]
pub(crate) enum Placement<L> {
    /// A memory cell filled by .space
    #[display("SPACE")]
    Reserved,

    /// A memory cell filled by .string
    #[display("{0:?}")]
    Char(char),

    /// A instruction or a .word directive
    #[display("{0}")]
    Line(LineContent<L>),
}

#[derive(Default)]
pub struct Layout<L> {
    pub labels: Labels,
    pub(crate) memory: HashMap<Address, Placement<L>>,
}

impl<L> Layout<L> {
    fn insert_placement(
        &mut self,
        address: Address,
        placement: Placement<L>,
    ) -> Result<(), MemoryLayoutError<L>> {
        if self.memory.contains_key(&address) {
            return Err(MemoryLayoutError::MemoryOverlap { address });
        }

        self.memory.insert(address, placement);
        Ok(())
    }

    fn insert_label(
        &mut self,
        label: Located<String, L>,
        address: Address,
    ) -> Result<(), MemoryLayoutError<L>> {
        if self.labels.contains_key(&label.inner) {
            return Err(MemoryLayoutError::DuplicateLabel {
                label: label.inner,
                location: label.location,
            });
        }

        self.labels.insert(label.inner, address);
        Ok(())
    }

    pub fn memory_report(&self) -> Vec<(Address, String)> {
        let mut v: Vec<_> = self
            .memory
            .iter()
            .map(|(k, v)| (*k, format!("{}", v)))
            .collect();
        v.sort_by_key(|&(k, _)| k);
        v
    }
}

#[derive(Debug, Error, PartialEq)]
pub enum MemoryLayoutError<L> {
    #[error("duplicate label {label}")]
    DuplicateLabel { label: String, location: L },

    #[error("invalid argument for directive .{kind}")]
    InvalidDirectiveArgument { kind: DirectiveKind, location: L },

    #[error("failed to evaluate argument for directive .{kind}")]
    DirectiveArgumentEvaluation {
        kind: DirectiveKind,
        source: ExpressionEvaluationError<L>,
    },

    #[error("address {address} is already filled")]
    MemoryOverlap { address: Address },
}

impl<L> MemoryLayoutError<L> {
    pub fn location(&self) -> Option<&L> {
        match self {
            MemoryLayoutError::DuplicateLabel { location, .. }
            | MemoryLayoutError::InvalidDirectiveArgument { location, .. } => Some(location),
            MemoryLayoutError::DirectiveArgumentEvaluation { .. }
            | MemoryLayoutError::MemoryOverlap { .. } => None,
        }
    }
}

/// Lays out the memory
///
/// It places the labels & prepare a hashmap of cells to be filled.
#[tracing::instrument(skip(program))]
pub(crate) fn layout_memory<L: Clone + Default>(
    program: &[Line<L>],
) -> Result<Layout<L>, MemoryLayoutError<L>> {
    use DirectiveKind::{Addr, Space, String, Word};
    use MemoryLayoutError::{DirectiveArgumentEvaluation, InvalidDirectiveArgument};

    debug!(lines = program.len(), "Laying out memory");
    let mut layout: Layout<L> = Layout::default();
    let mut position = PROGRAM_START;

    for line in program {
        for key in line.symbols.clone() {
            trace!(key = %key.inner, position, "Inserting label");
            layout.insert_label(key, position)?;
        }

        if let Some(ref content) = line.content {
            match &content.inner {
                LineContent::Directive {
                    kind: Located { inner: Word, .. },
                    ..
                }
                | LineContent::Instruction { .. } => {
                    layout.insert_placement(position, Placement::Line(content.inner.clone()))?;
                    trace!(position, content = %content.inner, "Inserting line");
                    position += 1; // Instructions and word directives take one memory cell
                }

                LineContent::Directive {
                    kind: Located { inner: Space, .. },
                    argument:
                        Located {
                            inner: DirectiveArgument::Expression(e),
                            ..
                        },
                } => {
                    let size = e.evaluate(&EmptyExpressionContext).map_err(|source| {
                        DirectiveArgumentEvaluation {
                            kind: Space,
                            source,
                        }
                    })?;

                    trace!(size, position, "Reserving space");

                    for _ in 0..size {
                        layout.insert_placement(position, Placement::Reserved)?;
                        position += 1;
                    }
                }

                LineContent::Directive {
                    kind: Located { inner: Addr, .. },
                    argument:
                        Located {
                            inner: DirectiveArgument::Expression(e),
                            ..
                        },
                } => {
                    let addr = e
                        .evaluate(&EmptyExpressionContext)
                        .map_err(|source| DirectiveArgumentEvaluation { kind: Addr, source })?;

                    debug!(addr, "Changing address");

                    // The ".addr N" directive changes the current address to N
                    position = addr;
                }

                LineContent::Directive {
                    kind: Located { inner: String, .. },
                    argument:
                        Located {
                            inner: DirectiveArgument::StringLiteral(string),
                            ..
                        },
                } => {
                    trace!(position, string = string.as_str(), "Inserting string");
                    // Fill the memory with the chars of the string
                    for c in string.chars() {
                        layout.insert_placement(position, Placement::Char(c))?;
                        position += 1;
                    }
                }

                LineContent::Directive { kind, .. } => {
                    return Err(InvalidDirectiveArgument {
                        kind: kind.inner,
                        location: kind.location.clone(),
                    });
                }
            }
        }
    }

    Ok(layout)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{
        expression::Node,
        line::Line,
        location::RelativeLocation,
        value::{InstructionArgument, InstructionKind},
    };
    use crate::runtime::Reg;

    use InstructionKind::{Add, Jmp};

    #[test]
    fn place_labels_simple_test() {
        let program: Vec<Line<RelativeLocation>> = vec![
            Line::default().symbol("main").instruction(
                Add,
                vec![
                    InstructionArgument::Register(Reg::A),
                    InstructionArgument::Register(Reg::B),
                ],
            ),
            Line::default().symbol("loop").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main".into()))],
            ),
        ];

        let labels = layout_memory(&program).unwrap().labels;
        let expected = {
            let mut h = HashMap::new();
            h.insert(String::from("main"), PROGRAM_START);
            h.insert(String::from("loop"), PROGRAM_START + 1);
            h
        };
        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_addr_test() {
        let program: Vec<Line<RelativeLocation>> = vec![
            Line::default().directive(DirectiveKind::Addr, 10),
            Line::default().symbol("main").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main".into()))],
            ),
        ];

        let labels = layout_memory(&program).unwrap().labels;
        let expected = {
            let mut h = HashMap::new();
            h.insert(String::from("main"), 10);
            h
        };
        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_space_test() {
        let program: Vec<Line<RelativeLocation>> = vec![
            Line::default()
                .symbol("first")
                .directive(DirectiveKind::Space, 10),
            Line::default()
                .symbol("second")
                .directive(DirectiveKind::Space, 5),
            Line::default().symbol("main").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main".into()))],
            ),
        ];

        let labels = layout_memory(&program).unwrap().labels;
        let expected = {
            let mut h = HashMap::new();
            h.insert(String::from("first"), PROGRAM_START);
            h.insert(String::from("second"), PROGRAM_START + 10);
            h.insert(String::from("main"), PROGRAM_START + 15);
            h
        };

        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_word_test() {
        let program: Vec<Line<RelativeLocation>> = vec![
            Line::default()
                .symbol("first")
                .directive(DirectiveKind::Word, 123),
            Line::default()
                .symbol("second")
                .directive(DirectiveKind::Word, 456),
            Line::default().symbol("main").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main".into()))],
            ),
        ];

        let labels = layout_memory(&program).unwrap().labels;
        let expected = {
            let mut h = HashMap::new();
            h.insert(String::from("first"), PROGRAM_START);
            h.insert(String::from("second"), PROGRAM_START + 1);
            h.insert(String::from("main"), PROGRAM_START + 2);
            h
        };

        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_string_test() {
        let program: Vec<Line<RelativeLocation>> = vec![
            Line::default()
                .symbol("first")
                .directive(DirectiveKind::String, "hello"),
            Line::default()
                .symbol("second")
                .directive(DirectiveKind::String, "Émoticône: 🚙"), // length: 12 chars
            Line::default().symbol("main").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main".into()))],
            ),
        ];

        let labels = layout_memory(&program).unwrap().labels;
        let expected = {
            let mut h = HashMap::new();
            h.insert(String::from("first"), PROGRAM_START);
            h.insert(String::from("second"), PROGRAM_START + 5);
            h.insert(String::from("main"), PROGRAM_START + 5 + 12);
            h
        };

        assert_eq!(labels, expected);
    }

    #[test]
    fn duplicate_label_test() {
        let program: Vec<Line<RelativeLocation>> = vec![
            Line::default().symbol("hello"),
            Line::default().symbol("hello"),
        ];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::DuplicateLabel {
                label: "hello".into(),
                location: RelativeLocation::default(),
            })
        );
    }

    #[test]
    fn invalid_directive_argument_test() {
        let program: Vec<Line<RelativeLocation>> =
            vec![Line::default().directive(DirectiveKind::String, 3)];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::InvalidDirectiveArgument {
                kind: DirectiveKind::String,
                location: RelativeLocation::default(),
                // argument: 3.into(),
            })
        );

        let program: Vec<Line<RelativeLocation>> =
            vec![Line::default().directive(DirectiveKind::Space, "hello")];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::InvalidDirectiveArgument {
                kind: DirectiveKind::Space,
                location: RelativeLocation::default(),
                // argument: "hello".into(),
            })
        );

        let program: Vec<Line<RelativeLocation>> =
            vec![Line::default().directive(DirectiveKind::Addr, "hello")];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::InvalidDirectiveArgument {
                kind: DirectiveKind::Addr,
                location: RelativeLocation::default(),
                // argument: "hello".into(),
            })
        );
    }

    #[test]
    fn memory_overlap_test() {
        let program: Vec<Line<RelativeLocation>> = vec![
            Line::default().directive(DirectiveKind::Addr, 10),
            Line::default().directive(DirectiveKind::String, "hello"), // This takes 5 chars, so fills cells 10 to 15
            Line::default().directive(DirectiveKind::Addr, 14),
            Line::default().directive(DirectiveKind::Word, 0), // This overlaps with the second "l"
        ];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::MemoryOverlap { address: 14 })
        );
    }
}
