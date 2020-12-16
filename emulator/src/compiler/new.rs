use std::collections::HashMap;

use nom::{combinator::all_consuming, Finish};
use thiserror::Error;

use crate::parser::{parse_const_expression, Line, LineContent};
use crate::{constants::*, parser::parse_string_literal};

#[derive(Debug, Error)]
enum PlaceLabelError<'a> {
    #[error("duplicate label {label}")]
    DuplicateLabel { label: &'a str },

    #[error("unsupported directive {directive}")]
    UnsupportedDirective { directive: &'a str },

    #[error("could not parse directive argument")]
    ArgumentParseError(nom::error::Error<&'a str>),
}

fn place_labels<'a>(program: &[Line<'a>]) -> Result<HashMap<&'a str, u64>, PlaceLabelError<'a>> {
    use PlaceLabelError::*;
    let mut labels = HashMap::new();
    let mut position = PROGRAM_START;

    for line in program {
        for label in line.symbols.iter() {
            if labels.contains_key(label) {
                return Err(DuplicateLabel { label });
            }

            labels.insert(*label, position);
        }

        if let Some(ref content) = line.content {
            match content {
                LineContent::Instruction { .. } => {
                    position += 1; // Any instruction takes one memory space
                }
                LineContent::Directive {
                    directive: "word", ..
                } => {
                    position += 1; // A word takes one memory space
                }
                LineContent::Directive {
                    directive: "space",
                    argument,
                } => {
                    let (_, size): (_, u64) = all_consuming(parse_const_expression)(argument)
                        .finish()
                        .map_err(ArgumentParseError)?;
                    // The ".space N" directive takes N memory spaces
                    position += size;
                }
                LineContent::Directive {
                    directive: "addr",
                    argument,
                } => {
                    let (_, addr): (_, u64) = all_consuming(parse_const_expression)(argument)
                        .finish()
                        .map_err(ArgumentParseError)?;
                    // The ".addr N" directive changes the current address to N
                    position = addr;
                }
                LineContent::Directive {
                    directive: "string",
                    argument,
                } => {
                    let (_, literal) = all_consuming(parse_string_literal)(argument)
                        .finish()
                        .map_err(ArgumentParseError)?;
                    // The ".string X" takes N memory cells, where N is the length of X
                    position += literal.chars().count() as u64;
                }
                LineContent::Directive { directive, .. } => {
                    return Err(UnsupportedDirective { directive });
                }
            }
        }
    }

    Ok(labels)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Line;

    #[test]
    fn place_labels_simple_test() {
        let program = vec![
            Line::default()
                .symbol("main")
                .instruction("add", vec!["%a", "%b"]),
            Line::default()
                .symbol("loop")
                .instruction("jmp", vec!["loop"]),
        ];

        let labels = place_labels(&program).unwrap();
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
            Line::default().directive("addr", "10"),
            Line::default()
                .symbol("main")
                .instruction("jmp", vec!["main"]),
        ];

        let labels = place_labels(&program).unwrap();
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
            Line::default().symbol("first").directive("space", "10"),
            Line::default().symbol("second").directive("space", "5"),
            Line::default()
                .symbol("main")
                .instruction("jmp", vec!["main"]),
        ];

        let labels = place_labels(&program).unwrap();
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
            Line::default().symbol("first").directive("word", "123"),
            Line::default().symbol("second").directive("word", "456"),
            Line::default()
                .symbol("main")
                .instruction("jmp", vec!["main"]),
        ];

        let labels = place_labels(&program).unwrap();
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
            Line::default()
                .symbol("first")
                .directive("string", r#""hello""#),
            Line::default()
                .symbol("second")
                .directive("string", r#""Émoticône: 🚙""#), // length: 12 chars
            Line::default()
                .symbol("main")
                .instruction("jmp", vec!["main"]),
        ];

        let labels = place_labels(&program).unwrap();
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
