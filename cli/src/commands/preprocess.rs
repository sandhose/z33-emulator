use std::path::PathBuf;

use clap::{Parser, ValueHint};
use tracing::info;

use z33_emulator::preprocessor::{NativeFilesystem, Preprocessor};

#[derive(Parser, Debug)]
pub struct PreprocessOpt {
    /// Input file
    #[clap(value_parser, value_hint = ValueHint::FilePath)]
    input: PathBuf,
}

impl PreprocessOpt {
    pub fn exec(&self) -> anyhow::Result<()> {
        let fs = NativeFilesystem::from_env()?;
        info!(path = ?self.input, "Reading program");
        let preprocessor = Preprocessor::new(fs).and_load(&self.input);
        let source = preprocessor.preprocess(&self.input)?;
        println!("{}", source);
        Ok(())
    }
}
