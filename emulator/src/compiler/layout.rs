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
    Line(Located<LineContent>),
}

#[derive(Default)]
pub struct Layout {
    pub labels: Labels,
    /// Maps address → (placement, source location of the directive that placed
    /// it)
    pub(crate) memory: HashMap<Address, (Placement, Range<usize>)>,
}

impl Layout {
    /// Extract a source map mapping addresses to byte ranges in the
    /// preprocessor output.
    ///
    /// Every placement now carries its source location, so the source map
    /// covers all memory cells (instructions, `.word`, `.space`, `.string`).
    #[must_use]
    pub fn source_map(&self) -> BTreeMap<Address, Range<usize>> {
        self.memory
            .iter()
            .map(|(address, (_, location))| (*address, location.clone()))
            .collect()
    }

    fn insert_placement(
        &mut self,
        address: Address,
        placement: Placement,
        location: Range<usize>,
    ) -> Result<(), MemoryLayoutError> {
        if let Some((_, original_location)) = self.memory.get(&address) {
            return Err(MemoryLayoutError::MemoryOverlap {
                address,
                original_location: original_location.clone(),
                new_location: location,
            });
        }

        self.memory.insert(address, (placement, location));
        Ok(())
    }

    // r[impl asm.labels]
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
    #[error("duplicate label {label:?}")]
    DuplicateLabel {
        label: String,
        location: Range<usize>,
    },

    #[error("invalid argument for directive \".{kind}\"")]
    InvalidDirectiveArgument {
        kind: DirectiveKind,
        location: Range<usize>,
    },

    #[error("failed to evaluate argument for directive \".{kind}\"")]
    DirectiveArgumentEvaluation {
        kind: DirectiveKind,
        source: ExpressionEvaluationError,
    },

    #[error("address {address} is already filled")]
    MemoryOverlap {
        address: Address,
        /// Location of the directive that originally filled this address.
        original_location: Range<usize>,
        /// Location of the directive that is trying to overwrite it.
        new_location: Range<usize>,
    },
}

impl MemoryLayoutError {
    #[must_use]
    pub fn location(&self) -> Option<&Range<usize>> {
        match self {
            MemoryLayoutError::DuplicateLabel { location, .. }
            | MemoryLayoutError::InvalidDirectiveArgument { location, .. }
            | MemoryLayoutError::MemoryOverlap {
                new_location: location,
                ..
            } => Some(location),
            MemoryLayoutError::DirectiveArgumentEvaluation { .. } => None,
        }
    }
}

/// Lays out the memory, collecting all errors instead of stopping at the first.
///
/// Always produces a `Layout` (possibly partial) plus a vector of errors.
/// Labels are always collected even when other errors occur. Error lines
/// (`LineContent::Error`) are silently skipped.
#[tracing::instrument(skip(program))]
#[allow(clippy::too_many_lines)]
pub(crate) fn layout_memory(program: &[Located<Line>]) -> (Layout, Vec<MemoryLayoutError>) {
    use DirectiveKind::{Addr, Space, String, Word};

    debug!(lines = program.len(), "Laying out memory");
    let mut layout: Layout = Layout::default();
    let mut errors: Vec<MemoryLayoutError> = Vec::new();
    let mut position = PROGRAM_START;

    for line in program {
        let line_offset = line.location.start;
        let line = &line.inner;
        for key in line.symbols.clone() {
            let key = key.offset(line_offset);
            trace!(key = %key.inner, position, "Inserting label");
            if let Err(e) = layout.insert_label(key, position) {
                errors.push(e);
            }
        }

        if let Some(ref content) = line.content {
            let content_offset = line_offset + content.location.start;
            match &content.inner {
                // r[impl asm.directive.word]
                LineContent::Directive {
                    kind: Located { inner: Word, .. },
                    ..
                } => {
                    let abs_location = Range {
                        start: content.location.start + line_offset,
                        end: content.location.end + line_offset,
                    };
                    if let Err(e) = layout.insert_placement(
                        position,
                        Placement::Line(content.clone().offset(line_offset)),
                        abs_location,
                    ) {
                        errors.push(e);
                    }
                    trace!(position, content = %content.inner, "Inserting line");
                    position += 1;
                }

                // Valid instructions (skip error placeholder instructions)
                LineContent::Instruction { kind, .. } => {
                    if kind.inner == crate::parser::value::InstructionKind::Error {
                        // Skip — the diagnostic was already emitted by the
                        // parser
                    } else {
                        let abs_location = Range {
                            start: content.location.start + line_offset,
                            end: content.location.end + line_offset,
                        };
                        if let Err(e) = layout.insert_placement(
                            position,
                            Placement::Line(content.clone().offset(line_offset)),
                            abs_location,
                        ) {
                            errors.push(e);
                        }
                        trace!(position, content = %content.inner, "Inserting line");
                        position += 1;
                    }
                }

                // r[impl asm.directive.space]
                LineContent::Directive {
                    kind: Located { inner: Space, .. },
                    argument:
                        Located {
                            inner: DirectiveArgument::Expression(e),
                            ..
                        },
                } => match e.evaluate(&EmptyExpressionContext) {
                    Ok(size) => {
                        trace!(size, position, "Reserving space");
                        let abs_location = Range {
                            start: content.location.start + line_offset,
                            end: content.location.end + line_offset,
                        };
                        for _ in 0..size {
                            if let Err(e) = layout.insert_placement(
                                position,
                                Placement::Reserved,
                                abs_location.clone(),
                            ) {
                                errors.push(e);
                            }
                            position += 1;
                        }
                    }
                    Err(source) => {
                        errors.push(MemoryLayoutError::DirectiveArgumentEvaluation {
                            kind: Space,
                            source,
                        });
                    }
                },

                // r[impl asm.directive.addr]
                LineContent::Directive {
                    kind: Located { inner: Addr, .. },
                    argument:
                        Located {
                            inner: DirectiveArgument::Expression(e),
                            ..
                        },
                } => match e.evaluate(&EmptyExpressionContext) {
                    Ok(addr) => {
                        debug!(addr, "Changing address");
                        position = addr;
                    }
                    Err(source) => {
                        errors.push(MemoryLayoutError::DirectiveArgumentEvaluation {
                            kind: Addr,
                            source,
                        });
                    }
                },

                // r[impl asm.directive.string]
                LineContent::Directive {
                    kind: Located { inner: String, .. },
                    argument:
                        Located {
                            inner: DirectiveArgument::StringLiteral(string),
                            ..
                        },
                } => {
                    trace!(position, string = string.as_str(), "Inserting string");
                    let abs_location = Range {
                        start: content.location.start + line_offset,
                        end: content.location.end + line_offset,
                    };
                    for c in string.chars() {
                        if let Err(e) = layout.insert_placement(
                            position,
                            Placement::Char(c),
                            abs_location.clone(),
                        ) {
                            errors.push(e);
                        }
                        position += 1;
                    }

                    if let Err(e) = layout.insert_placement(position, Placement::Nul, abs_location)
                    {
                        errors.push(e);
                    }
                    position += 1;
                }

                LineContent::Directive { kind, .. } => {
                    errors.push(MemoryLayoutError::InvalidDirectiveArgument {
                        kind: kind.inner,
                        location: Range {
                            start: kind.location.start + content_offset,
                            end: kind.location.end + content_offset,
                        },
                    });
                }

                // Skip error recovery placeholders
                LineContent::Error => {}
            }
        }
    }

    (layout, errors)
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
        let program: Vec<Located<Line>> = vec![
            Line::empty().symbol("main").instruction(
                Add,
                vec![
                    InstructionArgument::Register(Reg::A),
                    InstructionArgument::Register(Reg::B),
                ],
            ),
            Line::empty().symbol("loop").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main".into()))],
            ),
        ];

        let labels = layout_memory(&program).0.labels;
        let expected = BTreeMap::from_iter([
            ("main".into(), PROGRAM_START),
            ("loop".into(), PROGRAM_START + 1),
        ]);
        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_addr_test() {
        let program: Vec<Located<Line>> = vec![
            Line::empty().directive(DirectiveKind::Addr, 10),
            Line::empty().symbol("main").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main".into()))],
            ),
        ];

        let labels = layout_memory(&program).0.labels;
        let expected = BTreeMap::from_iter(vec![("main".into(), 10)]);
        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_space_test() {
        let program: Vec<Located<Line>> = vec![
            Line::empty()
                .symbol("first")
                .directive(DirectiveKind::Space, 10),
            Line::empty()
                .symbol("second")
                .directive(DirectiveKind::Space, 5),
            Line::empty().symbol("main").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main".into()))],
            ),
        ];

        let labels = layout_memory(&program).0.labels;
        let expected = BTreeMap::from_iter([
            ("first".into(), PROGRAM_START),
            ("second".into(), PROGRAM_START + 10),
            ("main".into(), PROGRAM_START + 15),
        ]);

        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_word_test() {
        let program: Vec<Located<Line>> = vec![
            Line::empty()
                .symbol("first")
                .directive(DirectiveKind::Word, 123),
            Line::empty()
                .symbol("second")
                .directive(DirectiveKind::Word, 456),
            Line::empty().symbol("main").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main".into()))],
            ),
        ];

        let labels = layout_memory(&program).0.labels;
        let expected = BTreeMap::from_iter(vec![
            ("first".into(), PROGRAM_START),
            ("second".into(), PROGRAM_START + 1),
            ("main".into(), PROGRAM_START + 2),
        ]);

        assert_eq!(labels, expected);
    }

    #[test]
    fn place_labels_string_test() {
        let program: Vec<Located<Line>> = vec![
            Line::empty()
                .symbol("first")
                .directive(DirectiveKind::String, "hello"),
            Line::empty()
                .symbol("second")
                .directive(DirectiveKind::String, "Émoticône: 🚙"), // length: 12 chars
            Line::empty().symbol("main").instruction(
                Jmp,
                vec![InstructionArgument::Value(Node::Variable("main".into()))],
            ),
        ];

        let labels = layout_memory(&program).0.labels;
        let expected = BTreeMap::from_iter([
            ("first".into(), PROGRAM_START),
            ("second".into(), PROGRAM_START + 6),
            ("main".into(), PROGRAM_START + 6 + 13),
        ]);

        assert_eq!(labels, expected);
    }

    #[test]
    fn duplicate_label_test() {
        let program: Vec<Located<Line>> =
            vec![Line::empty().symbol("hello"), Line::empty().symbol("hello")];

        let (_, errors) = layout_memory(&program);
        assert_eq!(errors.len(), 1);
        assert_eq!(
            errors[0],
            MemoryLayoutError::DuplicateLabel {
                label: "hello".into(),
                location: 0..0
            }
        );
    }

    #[test]
    fn invalid_directive_argument_test() {
        let program: Vec<Located<Line>> = vec![Line::empty().directive(DirectiveKind::String, 3)];
        let (_, errors) = layout_memory(&program);
        assert_eq!(errors.len(), 1);
        assert_eq!(
            errors[0],
            MemoryLayoutError::InvalidDirectiveArgument {
                kind: DirectiveKind::String,
                location: 0..0,
            }
        );

        let program: Vec<Located<Line>> =
            vec![Line::empty().directive(DirectiveKind::Space, "hello")];
        let (_, errors) = layout_memory(&program);
        assert_eq!(errors.len(), 1);
        assert_eq!(
            errors[0],
            MemoryLayoutError::InvalidDirectiveArgument {
                kind: DirectiveKind::Space,
                location: 0..0,
            }
        );

        let program: Vec<Located<Line>> =
            vec![Line::empty().directive(DirectiveKind::Addr, "hello")];
        let (_, errors) = layout_memory(&program);
        assert_eq!(errors.len(), 1);
        assert_eq!(
            errors[0],
            MemoryLayoutError::InvalidDirectiveArgument {
                kind: DirectiveKind::Addr,
                location: 0..0,
            }
        );
    }

    #[test]
    fn memory_overlap_test() {
        let program: Vec<Located<Line>> = vec![
            Line::empty().directive(DirectiveKind::Addr, 10),
            // This takes 5 chars, so fills cells 10 to 15
            Line::empty().directive(DirectiveKind::String, "hello"),
            Line::empty().directive(DirectiveKind::Addr, 14),
            Line::empty().directive(DirectiveKind::Word, 0), // This overlaps with the second "l"
        ];

        let (_, errors) = layout_memory(&program);
        assert!(!errors.is_empty());
        match &errors[0] {
            MemoryLayoutError::MemoryOverlap { address, .. } => assert_eq!(*address, 14),
            other => panic!("expected MemoryOverlap, got {other:?}"),
        }
    }
}
