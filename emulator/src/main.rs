use std::path::PathBuf;
use structopt::StructOpt;
use tracing::info;

mod compiler;
mod parser;
mod preprocessor;
mod processor;
mod util;

use crate::compiler::{Compiler, CompilerState};
use crate::parser::{Directive, Parser, ProgramLine};
use crate::preprocessor::preprocess;

#[derive(StructOpt)]
enum Opt {
    /// Preprocess, compile and run a program
    Run {
        /// Input file
        #[structopt(parse(from_os_str))]
        input: PathBuf,

        /// Start label
        entrypoint: String,
    },
    /// Run the preprocessor
    Preprocess {
        /// Input file
        #[structopt(parse(from_os_str))]
        input: PathBuf,
    },
}

impl Opt {
    fn exec(self) -> Result<(), Box<dyn std::error::Error>> {
        match self {
            Opt::Run { input, entrypoint } => run(input, entrypoint),
            Opt::Preprocess { input } => run_preprocessor(input),
        }
    }
}

fn run(input: PathBuf, entrypoint: String) -> Result<(), Box<dyn std::error::Error>> {
    info!("Reading program from file {:?}", input);
    let source = preprocess(input)?;

    info!("Parsing program");
    let parser = Parser::new(source.as_str());

    let mut compiler = CompilerState::default();
    for line in parser {
        let line = line?;
        match line {
            ProgramLine::Instruction(inst) => {
                compiler.ingest(inst)?;
            }
            ProgramLine::LabeledInstruction(label, inst) => {
                compiler.ingest_labeled_instruction(inst, label)?;
            }
            ProgramLine::Directive(Directive::LabelDefinition(label)) => {
                compiler.ingest_label(label)?;
            }
            ProgramLine::Directive(Directive::AddressChange(addr)) => {
                compiler.change_address(addr)?;
            }
            ProgramLine::Directive(Directive::Space(offset)) => {
                compiler.memory_skip(offset)?;
            }
            ProgramLine::Directive(Directive::Word(word)) => {
                compiler.ingest(word)?;
            }
            ProgramLine::Directive(Directive::StringLiteral(literal)) => {
                compiler.ingest(literal)?;
            }
            ProgramLine::Empty => {}
        }
    }

    info!("Buiding computer (entrypoint: {})", entrypoint);
    let mut computer = compiler.build(entrypoint)?;
    info!("Running program");
    computer.run()?;

    println!("Registers:");
    util::display_registers(computer.registers);
    println!("-------");
    println!("Memory:");
    util::display_memory(computer.memory);
    Ok(())
}

fn run_preprocessor(input: PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    info!("Reading program from file {:?}", input);
    let source = preprocess(input)?;
    println!("{}", source);
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .compact()
        .init();

    let opt = Opt::from_args();
    opt.exec()
}
