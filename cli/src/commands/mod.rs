use clap::Clap;

mod dump;
mod preprocess;
mod print;
mod run;

#[derive(Clap)]
#[clap(version, author, about)]
pub enum Opt {
    /// Preprocess, compile and run a program
    Run(self::run::RunOpt),

    /// Run the preprocessor
    Preprocess(self::preprocess::PreprocessOpt),

    /// Print the program as parsed
    Print(self::print::PrintOpt),

    /// Dump the AST of the program
    Dump(self::dump::DumpOpt),
}

impl Opt {
    /// Run a subcommand
    pub fn exec(self) -> Result<(), Box<dyn std::error::Error>> {
        match self {
            Opt::Run(opt) => opt.exec(),
            Opt::Preprocess(opt) => opt.exec(),
            Opt::Print(opt) => opt.exec(),
            Opt::Dump(opt) => opt.exec(),
        }
    }
}
