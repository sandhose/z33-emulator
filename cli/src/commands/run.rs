use std::{path::PathBuf, process::exit};

use clap::{Clap, ValueHint};
use tracing::{debug, error, info};
use z33_emulator::{
    compile, parse,
    preprocessor::{preprocess, NativeFilesystem},
};

use crate::interactive::run_interactive;

#[derive(Clap, Debug)]
pub struct RunOpt {
    /// Input file
    #[clap(parse(from_os_str), value_hint = ValueHint::FilePath)]
    input: PathBuf,

    /// Start label
    entrypoint: String,

    /// Run the program in interactive mode
    #[clap(short, long)]
    interactive: bool,
}

impl RunOpt {
    pub fn exec(&self) -> Result<(), Box<dyn std::error::Error>> {
        let fs = NativeFilesystem::from_env()?;
        info!(path = ?self.input, "Reading program");
        let source = preprocess(&fs, &self.input)?;
        let source = source.as_str();

        debug!("Parsing program");
        let program = match parse(source) {
            Ok(p) => p,
            Err(e) => {
                error!("{}", e);
                let offset = crate::util::char_offset(source, e.input);
                crate::util::display_error_offset(source, offset, &e.to_string());
                exit(1);
            }
        };

        debug!(entrypoint = %self.entrypoint, "Building computer");
        let (mut computer, debug_info) = compile(program.inner, &self.entrypoint)?;

        info!("Running program");
        if self.interactive {
            run_interactive(&mut computer, debug_info)?;
        } else {
            computer.run()?;
        }

        info!(registers = %computer.registers, "End of program");

        Ok(())
    }
}
