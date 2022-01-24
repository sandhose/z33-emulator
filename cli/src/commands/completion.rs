use clap::{App, ArgEnum, IntoApp, Parser};
use clap_complete::{
    generate,
    shells::{Bash, Elvish, Fish, PowerShell, Zsh},
    Generator,
};

use crate::Opt;

#[derive(Parser, Debug)]
pub struct CompletionOpt {
    #[clap(arg_enum)]
    shell: ShellKind,
}

#[derive(ArgEnum, Clone, Debug)]
enum ShellKind {
    Bash,
    Elvish,
    Fish,
    #[clap(name = "powershell")]
    PowerShell,
    Zsh,
}

fn print_completions<G: Generator>(generator: G, app: &mut App) {
    let name = app.get_name().to_string();
    generate(generator, app, name, &mut std::io::stdout());
}

impl CompletionOpt {
    pub fn exec(&self) -> Result<(), Box<dyn std::error::Error>> {
        let mut app = Opt::into_app();
        match self.shell {
            ShellKind::Bash => print_completions(Bash, &mut app),
            ShellKind::Elvish => print_completions(Elvish, &mut app),
            ShellKind::Fish => print_completions(Fish, &mut app),
            ShellKind::PowerShell => print_completions(PowerShell, &mut app),
            ShellKind::Zsh => print_completions(Zsh, &mut app),
        }

        Ok(())
    }
}
