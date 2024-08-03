use std::path::PathBuf;

use clap::{Parser, ValueHint};
use tracing::{debug, info};
use z33_emulator::parse;
use z33_emulator::preprocessor::{NativeFilesystem, Preprocessor};

#[derive(Parser, Debug)]
pub struct PrintOpt {
    /// Input file
    #[clap(value_parser, value_hint = ValueHint::FilePath)]
    input: PathBuf,
}

impl PrintOpt {
    pub fn exec(&self) -> anyhow::Result<()> {
        let fs = NativeFilesystem::from_env()?;
        info!(path = ?self.input, "Reading program");
        let preprocessor = Preprocessor::new(fs).and_load(&self.input);

        let source = preprocessor.preprocess(&self.input)?;
        let source = source.as_str();

        debug!("Parsing program");
        let program = parse(source).unwrap(); // TODO: the error is tied to the input
        println!("{program}");

        Ok(())
    }
}
