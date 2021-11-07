use std::path::PathBuf;

use clap::{Parser, ValueHint};
use tracing::info;

use z33_emulator::preprocessor::{preprocess, NativeFilesystem};

#[derive(Parser, Debug)]
pub struct PreprocessOpt {
    /// Input file
    #[clap(parse(from_os_str), value_hint = ValueHint::FilePath)]
    input: PathBuf,
}

impl PreprocessOpt {
    pub fn exec(&self) -> Result<(), Box<dyn std::error::Error>> {
        let fs = NativeFilesystem::from_env()?;
        info!(path = ?self.input, "Reading program");
        let source = preprocess(&fs, &self.input)?;
        println!("{}", source);
        Ok(())
    }
}
