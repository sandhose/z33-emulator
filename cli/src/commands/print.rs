use camino::Utf8PathBuf;
use clap::{Parser, ValueHint};
use tracing::{debug, info};
use z33_emulator::parse;
use z33_emulator::preprocessor::{NativeFilesystem, Workspace};

#[derive(Parser, Debug)]
pub struct PrintOpt {
    /// Input file
    #[clap(value_parser, value_hint = ValueHint::FilePath)]
    input: Utf8PathBuf,
}

impl PrintOpt {
    pub fn exec(self) -> anyhow::Result<()> {
        let fs = NativeFilesystem::from_env()?;
        info!(path = ?self.input, "Reading program");
        let preprocessor = Workspace::new(&fs, &self.input);

        let (_source_map, source) = preprocessor.preprocess()?;
        let source = source.as_str();

        debug!("Parsing program");
        let program = parse(source).unwrap(); // TODO: the error is tied to the input
        println!("{program}");

        Ok(())
    }
}
