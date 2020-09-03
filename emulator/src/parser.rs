#![allow(dead_code)]

use std::str::FromStr;

use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_while},
    character::complete::{char, newline, one_of, space0},
    combinator::{map, map_res, value},
    IResult,
};

use crate::processor::{Address, Arg, Instruction, Labelable, Reg, Value};

fn parse_reg(input: &str) -> IResult<&str, Reg> {
    let (input, _) = tag("%")(input)?;
    alt((
        value(Reg::A, tag_no_case("a")),
        value(Reg::B, tag_no_case("b")),
        value(Reg::PC, tag_no_case("pc")),
        value(Reg::SP, tag_no_case("sp")),
        value(Reg::SR, tag_no_case("sr")),
    ))(input)
}

fn from_decimal(input: &str) -> Result<u16, std::num::ParseIntError> {
    u16::from_str(input)
}

fn is_digit(c: char) -> bool {
    c.is_digit(10)
}

fn parse_literal(input: &str) -> IResult<&str, u16> {
    map_res(take_while(is_digit), from_decimal)(input)
}

fn parse_value(input: &str) -> IResult<&str, Value> {
    alt((map(parse_reg, Value::Reg), map(parse_literal, Value::Imm)))(input)
}

fn parse_indexed(input: &str) -> IResult<&str, (Reg, i16)> {
    let (input, reg) = parse_reg(input)?;
    let (input, sign) = one_of("+-")(input)?;
    let (input, val) = parse_literal(input)?;

    let value = if sign == '-' {
        -(val as i16)
    } else {
        val as i16
    };

    Ok((input, (reg, value)))
}

fn parse_inner_address(input: &str) -> IResult<&str, Address> {
    alt((
        map(parse_literal, Address::Dir),
        map(parse_indexed, |(reg, off)| Address::Idx(reg, off)),
        map(parse_reg, Address::Ind),
    ))(input)
}

fn parse_address(input: &str) -> IResult<&str, Address> {
    let (input, _) = char('[')(input)?;
    let (input, addr) = parse_inner_address(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, addr))
}

fn parse_arg(input: &str) -> IResult<&str, Arg> {
    alt((
        map(parse_value, Arg::Value),
        map(parse_address, Arg::Address),
    ))(input)
}

fn is_identifier_char(c: char) -> bool {
    c == '_' || ('0'..'9').contains(&c) || ('a'..'z').contains(&c) || ('A'..'Z').contains(&c)
}

fn parse_label(input: &str) -> IResult<&str, &str> {
    take_while(is_identifier_char)(input)
}

fn parse_label_def(input: &str) -> IResult<&str, &str> {
    let (input, label) = parse_label(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = char(':')(input)?;
    Ok((input, label))
}

pub trait Parsable: Sized {
    fn parse(input: &str) -> IResult<&str, Self>;
    fn parse_labelable(input: &str) -> IResult<&str, (Option<&str>, Self)>
    where
        Self: Labelable,
    {
        alt((
            map(Self::parse, |v| (None, v)),
            map(parse_label, |label| (Some(label), Self::label())),
        ))(input)
    }
}

impl Parsable for Reg {
    fn parse(input: &str) -> IResult<&str, Self> {
        parse_reg(input)
    }
}

impl Parsable for Arg {
    fn parse(input: &str) -> IResult<&str, Self> {
        parse_arg(input)
    }

    fn parse_labelable(input: &str) -> IResult<&str, (Option<&str>, Self)> {
        alt((
            map(parse_arg, |v| (None, v)),
            map(parse_label, |label| (Some(label), Self::label())),
        ))(input)
    }
}

impl Parsable for Address {
    fn parse(input: &str) -> IResult<&str, Self> {
        parse_address(input)
    }

    fn parse_labelable(input: &str) -> IResult<&str, (Option<&str>, Self)> {
        let (input, _) = char('[')(input)?;
        let (input, ret) = alt((
            map(parse_inner_address, |v| (None, v)),
            map(parse_label, |label| (Some(label), Self::label())),
        ))(input)?;
        let (input, _) = char(']')(input)?;
        Ok((input, ret))
    }
}

impl Parsable for Value {
    fn parse(input: &str) -> IResult<&str, Self> {
        parse_value(input)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ProgramLine {
    Instruction(Instruction),
    LabeledInstruction(String, Instruction),
    Label(String),
    Empty,
}

fn is_not_lf(c: char) -> bool {
    c != '\n'
}

fn parse_comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("//")(input)?;
    let (input, _) = space0(input)?; // Trim leading space in comment
    take_while(is_not_lf)(input)
}

fn parse_newline(input: &str) -> IResult<&str, char> {
    newline(input)
}

fn parse_program_line(input: &str) -> IResult<&str, ProgramLine> {
    let (input, _) = space0(input)?; // Remove leading spaces
    let (input, line) = alt((
        map(parse_label_def, |label| ProgramLine::Label(label.into())),
        map(Instruction::parse, |(label, instruction)| {
            if let Some(label) = label {
                ProgramLine::LabeledInstruction(label.into(), instruction)
            } else {
                ProgramLine::Instruction(instruction)
            }
        }),
        value(ProgramLine::Empty, space0),
    ))(input)?;
    let (input, _) = space0(input)?; // Remove trailing spaces

    // Try to parse and consume comment
    let input = match parse_comment(input) {
        Ok((input, _)) => input,
        Err(_) => input,
    };

    Ok((input, line))
}

struct Parser<'a> {
    program: &'a str,
    first: bool,
    errored: bool,
}

impl<'a> Parser<'a> {
    fn new(program: &'a str) -> Self {
        Parser {
            program,
            first: true,
            errored: false,
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<ProgramLine, nom::Err<nom::error::ErrorKind>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.program == "" {
            return None;
        }

        if self.errored {
            return None;
        }

        if !self.first {
            self.program = match parse_newline(self.program) {
                Ok((program, _)) => program,
                Err(e) => {
                    self.errored = true;
                    return Some(Err(e.map(|(_, e)| e)));
                }
            };
        } else {
            self.first = false;
        }

        let (program, line) = match parse_program_line(self.program) {
            Ok(f) => f,
            Err(e) => {
                self.errored = true;
                return Some(Err(e.map(|(_, e)| e)));
            }
        };
        self.program = program;

        Some(Ok(line))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_reg_test() {
        assert_eq!(parse_reg("%a"), Ok(("", Reg::A)));
        assert_eq!(parse_reg("%b"), Ok(("", Reg::B)));
        assert_eq!(parse_reg("%pc"), Ok(("", Reg::PC)));
        assert_eq!(parse_reg("%PC"), Ok(("", Reg::PC)));
        assert_eq!(parse_reg("%sp"), Ok(("", Reg::SP)));
        assert_eq!(parse_reg("%sr"), Ok(("", Reg::SR)));
        assert!(parse_reg("%c").is_err());
    }

    #[test]
    fn parse_literal_test() {
        assert_eq!(parse_literal("100"), Ok(("", 100)));
        assert_eq!(parse_literal("42"), Ok(("", 42)));
        assert!(parse_literal("65536").is_err()); // Value too big
    }

    #[test]
    fn parse_value_test() {
        assert_eq!(parse_value("100"), Ok(("", Value::Imm(100))));
        assert_eq!(parse_value("42"), Ok(("", Value::Imm(42))));
        assert!(parse_value("65536").is_err()); // Value too big

        assert_eq!(parse_value("%a"), Ok(("", Value::Reg(Reg::A))));
        assert_eq!(parse_value("%b"), Ok(("", Value::Reg(Reg::B))));
        assert_eq!(parse_value("%pc"), Ok(("", Value::Reg(Reg::PC))));
        assert_eq!(parse_value("%sp"), Ok(("", Value::Reg(Reg::SP))));
        assert_eq!(parse_value("%sr"), Ok(("", Value::Reg(Reg::SR))));
        assert!(parse_value("%c").is_err());
    }

    #[test]
    fn parse_indexed_test() {
        assert_eq!(parse_indexed("%a+2"), Ok(("", (Reg::A, 2))));
        assert_eq!(parse_indexed("%b-5"), Ok(("", (Reg::B, -5))));
    }

    #[test]
    fn parse_address_test() {
        assert_eq!(parse_address("[100]"), Ok(("", Address::Dir(100))));
        assert_eq!(parse_address("[%a]"), Ok(("", Address::Ind(Reg::A))));
        assert_eq!(parse_address("[%a+2]"), Ok(("", Address::Idx(Reg::A, 2))));
        assert_eq!(parse_address("[%b-5]"), Ok(("", Address::Idx(Reg::B, -5))));
    }

    #[test]
    fn parse_arg_test() {
        assert_eq!(parse_arg("100"), Ok(("", Arg::Value(Value::Imm(100)))));
        assert_eq!(parse_arg("%a"), Ok(("", Arg::Value(Value::Reg(Reg::A)))));
        assert_eq!(
            parse_arg("[100]"),
            Ok(("", Arg::Address(Address::Dir(100))))
        );
        assert_eq!(
            parse_arg("[%a]"),
            Ok(("", Arg::Address(Address::Ind(Reg::A))))
        );
        assert_eq!(
            parse_arg("[%a+2]"),
            Ok(("", Arg::Address(Address::Idx(Reg::A, 2))))
        );
        assert_eq!(
            parse_arg("[%b-5]"),
            Ok(("", Arg::Address(Address::Idx(Reg::B, -5))))
        );
    }

    #[test]
    fn parse_instruction_test() {
        let (_, arg_reg) = parse_arg("%a").unwrap();
        let (_, arg_dir) = parse_arg("[24]").unwrap();
        let (_, addr_dir) = parse_address("[10]").unwrap();
        let (_, arg_ind) = parse_arg("[%a]").unwrap();
        let (_, arg_idx) = parse_arg("[%pc-5]").unwrap();

        assert_eq!(
            Instruction::parse("add %a, %b"),
            Ok(("", (None, Instruction::Add(arg_reg.clone(), Reg::B))))
        );

        assert_eq!(
            Instruction::parse("and   [%a] , %b"),
            Ok(("", (None, Instruction::And(arg_ind, Reg::B))))
        );

        assert_eq!(
            Instruction::parse("call [%pc-5]"),
            Ok(("", (None, Instruction::Call(arg_idx))))
        );

        assert_eq!(
            Instruction::parse("div [24], %A"),
            Ok(("", (None, Instruction::Div(arg_dir, Reg::A))))
        );

        // This should parse
        assert_eq!(
            Instruction::parse("fas [10],%a"),
            Ok(("", (None, Instruction::Fas(addr_dir, Reg::A))))
        );
        // This should not
        assert!(Instruction::parse("fas 10, %A").is_err());

        assert_eq!(
            Instruction::parse("jmp %a"),
            Ok(("", (None, Instruction::Jmp(arg_reg))))
        );
        // Parse label
        assert_eq!(
            Instruction::parse("jmp label"),
            Ok(("", (Some("label"), Instruction::Jmp(Arg::label()))))
        );
    }

    #[test]
    fn parse_comment_test() {
        assert_eq!(parse_comment("// foo"), Ok(("", "foo")));
    }

    #[test]
    fn parse_program_line_test() {
        let program = r#"
            // calcul de n!
            factorielle:
                ld   [%sp+1],%a
                cmp  1,%a
                jge  casparticulier

                // cas général
                sub  1,%a         // a ← n-1
                push %a
                call factorielle  // a ← (n-1) !
                add  1,%sp        // dépile l’argument n-1
                push %b           // sauvegarder b
                ld   [%sp+1],%b   // b ← n (argument original)
                mul  %b,%a        // a ← n * (n-1)!
                pop  %b           // restaurer b
                rtn
            casparticulier:
                ld  1, %a
                rtn
        "#;

        let expected = vec![
            ProgramLine::Empty,
            ProgramLine::Label("factorielle".into()),
            ProgramLine::Instruction(Instruction::Ld(
                Arg::Address(Address::Idx(Reg::SP, 1)),
                Reg::A,
            )),
            ProgramLine::Instruction(Instruction::Cmp(Arg::Value(Value::Imm(1)), Reg::A)),
            ProgramLine::LabeledInstruction(
                "casparticulier".into(),
                Instruction::Jge(Arg::label()),
            ),
            ProgramLine::Empty,
            ProgramLine::Empty,
            ProgramLine::Instruction(Instruction::Sub(Arg::Value(Value::Imm(1)), Reg::A)),
            ProgramLine::Instruction(Instruction::Push(Value::Reg(Reg::A))),
            ProgramLine::LabeledInstruction("factorielle".into(), Instruction::Call(Arg::label())),
            ProgramLine::Instruction(Instruction::Add(Arg::Value(Value::Imm(1)), Reg::SP)),
            ProgramLine::Instruction(Instruction::Push(Value::Reg(Reg::B))),
            ProgramLine::Instruction(Instruction::Ld(
                Arg::Address(Address::Idx(Reg::SP, 1)),
                Reg::B,
            )),
            ProgramLine::Instruction(Instruction::Mul(Arg::Value(Value::Reg(Reg::B)), Reg::A)),
            ProgramLine::Instruction(Instruction::Pop(Reg::B)),
            ProgramLine::Instruction(Instruction::Rtn),
            ProgramLine::Label("casparticulier".into()),
            ProgramLine::Instruction(Instruction::Ld(Arg::Value(Value::Imm(1)), Reg::A)),
            ProgramLine::Instruction(Instruction::Rtn),
            ProgramLine::Empty,
        ];

        let rest = expected.into_iter().fold(program, |program, expected| {
            let (program, _) = parse_newline(program).unwrap();
            let (program, line) = parse_program_line(program).unwrap();
            assert_eq!(line, expected);
            program
        });

        assert_eq!(rest, "");
    }

    #[test]
    fn parser_iterator_test() {
        let program = r#"
            // calcul de n!
            factorielle:
                ld   [%sp+1],%a
                cmp  1,%a
                jge  casparticulier

                // cas général
                sub  1,%a         // a ← n-1
                push %a
                call factorielle  // a ← (n-1) !
                add  1,%sp        // dépile l’argument n-1
                push %b           // sauvegarder b
                ld   [%sp+1],%b   // b ← n (argument original)
                mul  %b,%a        // a ← n * (n-1)!
                pop  %b           // restaurer b
                rtn
            casparticulier:
                ld  1, %a
                rtn
        "#;

        let expected = vec![
            ProgramLine::Empty,
            ProgramLine::Empty,
            ProgramLine::Label("factorielle".into()),
            ProgramLine::Instruction(Instruction::Ld(
                Arg::Address(Address::Idx(Reg::SP, 1)),
                Reg::A,
            )),
            ProgramLine::Instruction(Instruction::Cmp(Arg::Value(Value::Imm(1)), Reg::A)),
            ProgramLine::LabeledInstruction(
                "casparticulier".into(),
                Instruction::Jge(Arg::label()),
            ),
            ProgramLine::Empty,
            ProgramLine::Empty,
            ProgramLine::Instruction(Instruction::Sub(Arg::Value(Value::Imm(1)), Reg::A)),
            ProgramLine::Instruction(Instruction::Push(Value::Reg(Reg::A))),
            ProgramLine::LabeledInstruction("factorielle".into(), Instruction::Call(Arg::label())),
            ProgramLine::Instruction(Instruction::Add(Arg::Value(Value::Imm(1)), Reg::SP)),
            ProgramLine::Instruction(Instruction::Push(Value::Reg(Reg::B))),
            ProgramLine::Instruction(Instruction::Ld(
                Arg::Address(Address::Idx(Reg::SP, 1)),
                Reg::B,
            )),
            ProgramLine::Instruction(Instruction::Mul(Arg::Value(Value::Reg(Reg::B)), Reg::A)),
            ProgramLine::Instruction(Instruction::Pop(Reg::B)),
            ProgramLine::Instruction(Instruction::Rtn),
            ProgramLine::Label("casparticulier".into()),
            ProgramLine::Instruction(Instruction::Ld(Arg::Value(Value::Imm(1)), Reg::A)),
            ProgramLine::Instruction(Instruction::Rtn),
            ProgramLine::Empty,
        ];

        let parser = Parser::new(program);
        let program: Result<Vec<_>, _> = parser.collect();
        let program = program.unwrap();
        assert_eq!(program, expected);
    }
}
