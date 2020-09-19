use std::borrow::Cow;
use std::collections::HashSet;

use ansi_term::Style;
use clap::{derive::IntoApp, AppSettings, Clap};
use nom::character::is_space;
use rustyline::{
    completion::Completer,
    config::OutputStreamType,
    highlight::Highlighter,
    hint::Hinter,
    validate::{ValidationContext, ValidationResult, Validator},
    CompletionType, Config, Context, EditMode, Editor,
};
use rustyline_derive::Helper;
use tracing::{debug, info};

use crate::processor::{Address, Computer, Reg};

static HELP: &'static str = r#"
Run "help [command]" for command-specific help.
An empty line re-runs the last valid command."#;

#[derive(Clap, Clone, Debug)]
#[clap(
    help_template = "{about}\n\nCOMMANDS:\n{subcommands}\n{after-help}",
    after_help = HELP,
    global_setting = AppSettings::ColoredHelp,
    global_setting = AppSettings::DisableVersion,
    global_setting = AppSettings::InferSubcommands,
    global_setting = AppSettings::NoBinaryName,
    global_setting = AppSettings::VersionlessSubcommands,
    global_setting = AppSettings::AllowNegativeNumbers,
)]
/// Interactive mode commands
enum Command {
    /// Execute the next instructions
    Step {
        /// Number of steps to execute
        #[clap(default_value = "1")]
        number: u64,
    },

    /// Exit the emulator
    Exit,

    /// Show the state of registers
    Registers { register: Option<Reg> },

    /// Show the content of a block in memory
    Memory {
        /// The address to show. Can be a direct address (number literal) or an indirect one
        /// (register with an optional offset).
        address: Address,

        /// Number of memory cells to show.
        #[clap(default_value = "1")]
        number: i64,
    },
}

/// Rustyline helper, that handles interactive completion, highlighting and hinting.
#[derive(Helper, Debug)]
struct RunHelper {}

impl Completer for RunHelper {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let start = line
            .chars()
            .position(|c| !c.is_ascii() || !is_space(c as u8))
            .unwrap_or_default();

        let word: String = line
            .chars()
            .take(pos)
            .skip_while(|c| c.is_ascii() && is_space(*c as u8))
            .map(|c| c.to_ascii_lowercase())
            .collect();
        let word = word.as_str();

        let app = Command::into_app();
        let candidates: HashSet<_> = app
            .get_subcommands()
            .into_iter()
            // Extract subcommand name & aliases
            .flat_map(|cmd| std::iter::once(cmd.get_name()).chain(cmd.get_all_aliases()))
            .chain(std::iter::once("help"))
            .filter(|alias| alias.starts_with(word))
            .map(|alias| alias.to_string())
            .collect();

        Ok((start, candidates.into_iter().collect()))
    }
}

impl Highlighter for RunHelper {
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        let style = Style::new().dimmed();
        let hint = style.paint(hint).to_string();
        Cow::Owned(hint)
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        let style = Style::new().bold();
        let prompt = style.paint(prompt).to_string();
        Cow::Owned(prompt)
    }
}

impl Hinter for RunHelper {
    fn hint(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Option<String> {
        let word: String = line
            .chars()
            .take(pos)
            .skip_while(|c| c.is_ascii() && is_space(*c as u8))
            .map(|c| c.to_ascii_lowercase())
            .collect();
        let word = word.as_str();

        let app = Command::into_app();
        let candidates: Vec<_> = app
            .get_subcommands()
            .into_iter()
            .map(|cmd| cmd.get_name())
            .chain(std::iter::once("help"))
            .filter_map(|name| {
                if name.starts_with(word) {
                    Some(&name[word.len()..])
                } else {
                    None
                }
            })
            .collect();

        if candidates.len() == 1 {
            Some(candidates[0].to_string())
        } else {
            None
        }
    }
}

impl Validator for RunHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        let input = ctx.input();
        let res = shell_words::split(input);
        if res.is_err() {
            Ok(ValidationResult::Incomplete)
        } else {
            Ok(ValidationResult::Valid(None))
        }
    }
}

#[tracing::instrument(level = "debug", err)]
pub fn run_interactive(computer: &mut Computer) -> Result<(), Box<dyn std::error::Error>> {
    info!("Running in interactive mode. Type \"help\" to list available commands.");
    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .output_stream(OutputStreamType::Stderr)
        .auto_add_history(true)
        .build();

    let h = RunHelper {};
    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(h));

    let mut last_command = None;

    loop {
        let readline = rl.readline(">> ")?;

        let command = if readline == "" {
            if let Some(command) = last_command {
                command
            } else {
                info!("Type \"help\" to get the list of available commands");
                continue;
            }
        } else {
            let words = shell_words::split(readline.as_str())?;
            match Command::try_parse_from(words) {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("{}", e);
                    continue;
                }
            }
        };

        debug!("Executing command: {:?}", command);

        match command.clone() {
            Command::Exit => break,
            Command::Step { number } => {
                // TODO: recover from errors
                for _ in 0..number {
                    computer.step()?;
                }
            }
            Command::Registers { register } => {
                if let Some(reg) = register {
                    match reg {
                        Reg::SR => {
                            info!("Register %sr = {:?}", computer.registers.sr);
                        }
                        reg => {
                            let cell = computer.registers.get(reg);
                            info!("Register {} = {}", reg, cell);
                        }
                    }
                } else {
                    info!("Registers: {}", computer.registers);
                }
            }
            Command::Memory { address, number } => {
                // TODO: recover from error
                let address = computer.resolve_address(address)?;
                if number.is_positive() {
                    for i in 0..(number as u64) {
                        let address = address + i;
                        let cell = computer.memory.get(address as u64)?;
                        info!(address, value = %cell);
                    }
                } else {
                    for i in 0..(number.abs() as u64) {
                        let address = address - i;
                        let cell = computer.memory.get(address as u64)?;
                        info!(address, value = %cell);
                    }
                }
            }
        };

        last_command = Some(command);
    }

    Ok(())
}
