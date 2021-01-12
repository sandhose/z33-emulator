use std::path::PathBuf;

use clap::Clap;
use tracing::{debug, info};
use z33_emulator::{
    parse,
    preprocessor::{preprocess, NativeFilesystem},
};

#[derive(Clap, Debug)]
pub struct PrintOpt {
    /// Input file
    #[clap(parse(from_os_str))]
    input: PathBuf,
}

impl PrintOpt {
    pub fn exec(&self) -> Result<(), Box<dyn std::error::Error>> {
        let fs = NativeFilesystem::from_env()?;
        info!(path = ?self.input, "Reading program");
        let source = preprocess(&fs, &self.input)?;
        let source = source.as_str();

        debug!("Parsing program");
        let program = parse(source).unwrap(); // TODO: the error is tied to the input
        println!("{}", program);

        Ok(())
    }
}
