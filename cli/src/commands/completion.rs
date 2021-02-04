use clap::{Clap, IntoApp};
use clap_generate::{
    generate,
    generators::{Bash, Elvish, Fish, PowerShell, Zsh},
    Generator,
};

use crate::Opt;

#[derive(Clap, Debug)]
pub struct CompletionOpt {
    #[clap(arg_enum)]
    shell: Shell,
}

#[derive(Clap, Debug)]
enum Shell {
    Bash,
    Elvish,
    Fish,
    #[clap(name = "powershell")]
    PowerShell,
    Zsh,
}

fn print_completions<G: Generator, A: IntoApp>() {
    let mut app = A::into_app();
    let name = app.get_name().to_string();
    generate::<G, _>(&mut app, name, &mut std::io::stdout());
}

impl CompletionOpt {
    pub fn exec(&self) -> Result<(), Box<dyn std::error::Error>> {
        match self.shell {
            Shell::Bash => print_completions::<Bash, Opt>(),
            Shell::Elvish => print_completions::<Elvish, Opt>(),
            Shell::Fish => print_completions::<Fish, Opt>(),
            Shell::PowerShell => print_completions::<PowerShell, Opt>(),
            Shell::Zsh => print_completions::<Zsh, Opt>(),
        }

        Ok(())
    }
}
