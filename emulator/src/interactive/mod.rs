//! This module implements the TTY interactive interface.
//!
//! It is mainly based on two crates:
//!   - rustyline, to handle the line-editting logic
//!   - clap, to handle the parsing of those interactive commands
//!
//! Using Clap to do this is a bit of a hack, and requires some weird options to have it working
//! but works nonetheless.

use std::collections::{HashMap, HashSet};

use clap::{AppSettings, Clap};
use rustyline::{config::OutputStreamType, CompletionType, Config, EditMode, Editor};
use tracing::{debug, info, warn};

use crate::compiler::DebugInfo;
use crate::processor::{Address, Computer, Exception, Reg};

mod helper;
use self::helper::RunHelper;

static HELP: &str = r#"
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

    /// Trigger a hardware interrupt
    Interrupt,

    /// Show the next few instructions
    List {
        /// Number of instructions to show.
        #[clap(default_value = "10")]
        number: u64,
    },

    /// Set a breakpoint
    Break {
        /// The address where to set the breakpoint
        address: Address,
    },

    /// Remove a breakpoint
    Unbreak {
        /// The address of the breakpoint to remove
        address: Address,
    },

    /// Continue the program until the next breakpoint or reset
    Continue,

    /// Show informations about the current debugging session
    Info {
        #[clap(subcommand)]
        sub: InfoCommand,
    },
}

#[derive(Clap, Clone, Debug)]
enum InfoCommand {
    /// List active breakpoints
    Breakpoints,

    /// List program labels
    Labels,
}

fn labels_to_address_map(labels: &HashMap<String, u64>) -> HashMap<u64, Vec<String>> {
    labels
        .iter()
        .fold(HashMap::new(), |mut acc, (label, address)| {
            acc.entry(*address).or_default().push(label.clone());
            acc
        })
}

pub fn run_interactive(
    computer: &mut Computer,
    debug_info: DebugInfo,
) -> Result<(), Box<dyn std::error::Error>> {
    info!("Running in interactive mode. Type \"help\" to list available commands.");
    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .output_stream(OutputStreamType::Stderr)
        .auto_add_history(true)
        .build();

    let h: RunHelper<Command> = RunHelper::new();
    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(h));

    let mut last_command = None;
    let mut list_address = computer.registers.pc;
    let mut breakpoints = HashSet::new();
    let labels = labels_to_address_map(&debug_info.labels);

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

                list_address = computer.registers.pc;
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

            Command::Interrupt => {
                computer.recover_from_exception(Exception::HardwareInterrupt)?;
                list_address = computer.registers.pc;
            }

            Command::List { number } => {
                for i in 0..number {
                    let addr = list_address + i;
                    let instruction = computer
                        .memory
                        .get(addr)
                        .ok()
                        .and_then(|c| c.extract_instruction().ok());

                    for label in labels.get(&addr).cloned().unwrap_or_default() {
                        info!("          {}:", label);
                    }

                    let gutter = match (breakpoints.contains(&addr), addr == computer.registers.pc)
                    {
                        (true, true) => "B>",
                        (true, false) => "B ",
                        (false, true) => " >",
                        (false, false) => "  ",
                    };

                    if let Some(instruction) = instruction {
                        info!("{:<2} {:>5}    {}", gutter, addr, instruction);
                    } else {
                        info!("{:<2} {:>5}    –", gutter, addr);
                    }
                }

                list_address += number;
            }

            Command::Break { address } => {
                // TODO: recover from error
                let address = computer.resolve_address(address)?;
                if !breakpoints.insert(address) {
                    warn!(address, "A breakpoint was already set");
                } else {
                    info!(address, "Setting a breakpoint");
                }
            }

            Command::Unbreak { address } => {
                // TODO: recover from error
                let address = computer.resolve_address(address)?;
                if !breakpoints.remove(&address) {
                    warn!(address, "No breakpoint was set here");
                } else {
                    info!(address, "Removing breakpoint");
                }
            }

            Command::Continue => {
                loop {
                    // TODO: recover from error
                    computer.step()?;
                    if breakpoints.contains(&computer.registers.pc) {
                        break;
                    }
                }
                info!(address = computer.registers.pc, "Stopped at a breakpoint");
            }

            Command::Info { sub } => match sub {
                InfoCommand::Breakpoints => {
                    match breakpoints.len() {
                        0 => info!("No breakpoints"),
                        1 => info!("1 breakpoint:"),
                        x => info!("{} breakpoints:", x),
                    }
                    for addr in breakpoints.iter() {
                        let instruction = computer
                            .memory
                            .get(*addr)
                            .ok()
                            .and_then(|c| c.extract_instruction().ok());

                        if let Some(instruction) = instruction {
                            info!("{:>5}: {}", addr, instruction);
                        } else {
                            info!("{:>5}: –", addr);
                        }
                    }
                }
                InfoCommand::Labels => {
                    for (label, &addr) in debug_info.labels.iter() {
                        info!("{}: {}", label, addr);
                    }
                }
            },
        };

        last_command = Some(command);
    }

    Ok(())
}
