use camino::Utf8PathBuf;
use clap::{Parser, ValueHint};
use tracing::info;
use z33_emulator::preprocessor::{NativeFilesystem, Workspace};

#[derive(Parser, Debug)]
pub struct PreprocessOpt {
    /// Input file
    #[clap(value_parser, value_hint = ValueHint::FilePath)]
    input: Utf8PathBuf,
}

impl PreprocessOpt {
    pub fn exec(self) -> anyhow::Result<()> {
        let fs = NativeFilesystem::from_env()?;
        info!(path = ?self.input, "Reading program");
        let preprocessor = Workspace::new(&fs, &self.input);
        let source = preprocessor.preprocess()?;
        println!("{source}");
        Ok(())
    }
}
