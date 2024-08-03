use std::collections::{BTreeMap, HashMap};
use std::ops::Range;

use parse_display::Display;
use thiserror::Error;
use tracing::{debug, trace};

use crate::constants::{Address, PROGRAM_START};
use crate::parser::expression::{
    Context as ExpressionContext, EmptyContext as EmptyExpressionContext,
    EvaluationError as ExpressionEvaluationError,
};
use crate::parser::line::{Line, LineContent};
use crate::parser::location::Located;
use crate::parser::value::{DirectiveArgument, DirectiveKind};

pub(crate) type Labels = BTreeMap<String, Address>;

impl ExpressionContext for Labels {
    fn resolve_variable(&self, variable: &str) -> Option<i128> {
        self.get(variable).map(|v| i128::from(*v))
    }
}

#[derive(Display)]
pub(crate) enum Placement {
    /// A memory cell filled by .space
    #[display("SPACE")]
    Reserved,

    /// A memory cell filled by .string
    #[display("{0:?}")]
    Char(char),

    /// An empty memory cell, at the end of a .string
    #[display("NUL")]
    Nul,

    /// A instruction or a .word directive
    #[display("{0}")]
    Line(LineContent),
}

#[derive(Default)]
pub struct Layout {
    pub labels: Labels,
    pub(crate) memory: HashMap<Address, Placement>,
}

impl Layout {
    fn insert_placement(
        &mut self,
        address: Address,
        placement: Placement,
    ) -> Result<(), MemoryLayoutError> {
        if self.memory.contains_key(&address) {
            return Err(MemoryLayoutError::MemoryOverlap { address });
        }

        self.memory.insert(address, placement);
        Ok(())
    }

    fn insert_label(
        &mut self,
        label: Located<String>,
        address: Address,
    ) -> Result<(), MemoryLayoutError> {
        if self.labels.contains_key(&label.inner) {
            return Err(MemoryLayoutError::DuplicateLabel {
                label: label.inner,
                location: label.location,
            });
        }

        self.labels.insert(label.inner, address);
        Ok(())
    }
}

#[derive(Debug, Error, PartialEq)]
pub enum MemoryLayoutError {
    #[error("duplicate label {label}")]
    DuplicateLabel {
        label: String,
        location: Range<usize>,
    },

    #[error("invalid argument for directive .{kind}")]
    InvalidDirectiveArgument {
        kind: DirectiveKind,
        location: Range<usize>,
    },

    #[error("failed to evaluate argument for directive .{kind}")]
    DirectiveArgumentEvaluation {
        kind: DirectiveKind,
        source: ExpressionEvaluationError,
    },

    #[error("address {address} is already filled")]
    MemoryOverlap { address: Address },
}

impl MemoryLayoutError {
    #[must_use]
    pub fn location(&self) -> Option<&Range<usize>> {
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
pub(crate) fn layout_memory(program: &[Line]) -> Result<Layout, MemoryLayoutError> {
    use DirectiveKind::{Addr, Space, String, Word};
    use MemoryLayoutError::{DirectiveArgumentEvaluation, InvalidDirectiveArgument};

    debug!(lines = program.len(), "Laying out memory");
    let mut layout: Layout = Layout::default();
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
                    position += 1; // Instructions and word directives take one
                                   // memory cell
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

                    layout.insert_placement(position, Placement::Nul)?;
                    position += 1;
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
    use InstructionKind::{Add, Jmp};

    use super::*;
    use crate::parser::expression::Node;
    use crate::parser::line::Line;
    use crate::parser::value::{InstructionArgument, InstructionKind};
    use crate::runtime::Reg;

    #[test]
    fn place_labels_simple_test() {
        let program: Vec<Line> = vec![
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
        let expected = BTreeMap::from_iter([
            ("main".into(), PROGRAM_START),
            ("loop".into(), PROGRAM_START + 1),
        ]);
        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_addr_test() {
        let program: Vec<Line> = vec![
            Line::default().directive(DirectiveKind::Addr, 10),
            Line::default().symbol("main").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main".into()))],
            ),
        ];

        let labels = layout_memory(&program).unwrap().labels;
        let expected = BTreeMap::from_iter(vec![("main".into(), 10)]);
        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_space_test() {
        let program: Vec<Line> = vec![
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
        let expected = BTreeMap::from_iter([
            ("first".into(), PROGRAM_START),
            ("second".into(), PROGRAM_START + 10),
            ("main".into(), PROGRAM_START + 15),
        ]);

        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_word_test() {
        let program: Vec<Line> = vec![
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
        let expected = BTreeMap::from_iter(vec![
            ("first".into(), PROGRAM_START),
            ("second".into(), PROGRAM_START + 1),
            ("main".into(), PROGRAM_START + 2),
        ]);

        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_string_test() {
        let program: Vec<Line> = vec![
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
        let expected = BTreeMap::from_iter([
            ("first".into(), PROGRAM_START),
            ("second".into(), PROGRAM_START + 6),
            ("main".into(), PROGRAM_START + 6 + 13),
        ]);

        assert_eq!(labels, expected);
    }

    #[test]
    fn duplicate_label_test() {
        let program: Vec<Line> = vec![
            Line::default().symbol("hello"),
            Line::default().symbol("hello"),
        ];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::DuplicateLabel {
                label: "hello".into(),
                location: 0..0
            })
        );
    }

    #[test]
    fn invalid_directive_argument_test() {
        let program: Vec<Line> = vec![Line::default().directive(DirectiveKind::String, 3)];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::InvalidDirectiveArgument {
                kind: DirectiveKind::String,
                location: 0..0 // argument: 3.into(),
            })
        );

        let program: Vec<Line> = vec![Line::default().directive(DirectiveKind::Space, "hello")];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::InvalidDirectiveArgument {
                kind: DirectiveKind::Space,
                location: 0..0,
                // argument: "hello".into(),
            })
        );

        let program: Vec<Line> = vec![Line::default().directive(DirectiveKind::Addr, "hello")];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::InvalidDirectiveArgument {
                kind: DirectiveKind::Addr,
                location: 0..0,
                // argument: "hello".into(),
            })
        );
    }

    #[test]
    fn memory_overlap_test() {
        let program: Vec<Line> = vec![
            Line::default().directive(DirectiveKind::Addr, 10),
            Line::default().directive(DirectiveKind::String, "hello"), /* This takes 5 chars, so fills cells 10 to 15 */
            Line::default().directive(DirectiveKind::Addr, 14),
            Line::default().directive(DirectiveKind::Word, 0), // This overlaps with the second "l"
        ];

        assert_eq!(
            layout_memory(&program).err(),
            Some(MemoryLayoutError::MemoryOverlap { address: 14 })
        );
    }
}
