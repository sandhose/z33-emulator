use clap::{ArgEnum, Command, IntoApp, Parser};
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

fn print_completions<G: Generator>(generator: G, command: &mut Command) {
    let name = command.get_name().to_string();
    generate(generator, command, name, &mut std::io::stdout());
}

impl CompletionOpt {
    pub fn exec(&self) -> anyhow::Result<()> {
        let mut command = Opt::command();
        match self.shell {
            ShellKind::Bash => print_completions(Bash, &mut command),
            ShellKind::Elvish => print_completions(Elvish, &mut command),
            ShellKind::Fish => print_completions(Fish, &mut command),
            ShellKind::PowerShell => print_completions(PowerShell, &mut command),
            ShellKind::Zsh => print_completions(Zsh, &mut command),
        }

        Ok(())
    }
}
