use std::path::PathBuf;

use clap::Clap;
use tracing::info;

use z33_emulator::preprocess;

#[derive(Clap, Debug)]
pub struct PreprocessOpt {
    /// Input file
    #[clap(parse(from_os_str))]
    input: PathBuf,
}

impl PreprocessOpt {
    pub fn exec(&self) -> Result<(), Box<dyn std::error::Error>> {
        info!(path = ?self.input, "Reading program");
        let source = preprocess(&self.input)?;
        println!("{}", source);
        Ok(())
    }
}
