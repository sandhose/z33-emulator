use clap::{ArgAction, Command, CommandFactory, Parser, ValueEnum};
use clap_complete::shells::{Bash, Elvish, Fish, PowerShell, Zsh};
use clap_complete::{generate, Generator};

use crate::Opt;

#[derive(Parser, Debug)]
pub struct CompletionOpt {
    #[clap(value_enum, action = ArgAction::Set)]
    shell: ShellKind,
}

#[derive(ValueEnum, Clone, Debug)]
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
    pub fn exec(&self) {
        let mut command = Opt::command();
        match self.shell {
            ShellKind::Bash => print_completions(Bash, &mut command),
            ShellKind::Elvish => print_completions(Elvish, &mut command),
            ShellKind::Fish => print_completions(Fish, &mut command),
            ShellKind::PowerShell => print_completions(PowerShell, &mut command),
            ShellKind::Zsh => print_completions(Zsh, &mut command),
        }
    }
}
