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
use crate::runtime::{Address, Computer, Exception, Reg};

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
        sub: Option<InfoCommand>,
    },
}

#[derive(Clap, Clone, Debug)]
enum InfoCommand {
    /// List active breakpoints
    Breakpoints,

    /// List program labels
    Labels,

    /// Show the number of CPU cycles used since the beginning of the program
    Cycles,
}

/// Holds informations about a interactive session
#[derive(Debug, Default)]
struct Session {
    /// List of active breakpoints
    breakpoints: HashSet<u64>,

    /// Map of labels in program
    labels: HashMap<String, u64>,

    /// Current address for the `list` command
    list_address: Option<u64>,
}

impl Session {
    fn from_debug_info(info: DebugInfo) -> Session {
        Session {
            labels: info.labels,
            ..Default::default()
        }
    }

    /// Add a breakpoint
    fn add_breakpoint(&mut self, address: u64) {
        if !self.breakpoints.insert(address) {
            warn!(address, "A breakpoint was already set");
        } else {
            info!(address, "Setting a breakpoint");
        }
    }

    /// Remove a breakpoint
    fn remove_breakpoint(&mut self, address: u64) {
        if !self.breakpoints.remove(&address) {
            warn!(address, "No breakpoint was set here");
        } else {
            info!(address, "Removing breakpoint");
        }
    }

    /// Checks if the given address has a breakpoint
    fn has_breakpoint(&self, address: u64) -> bool {
        self.breakpoints.contains(&address)
    }

    /// Reset the `list` command (after running an instruction)
    fn reset_list(&mut self) {
        self.list_address = None;
    }

    /// Offset the `list` command, returns the address to show
    fn offset_list(&mut self, computer: &Computer, offset: u64) -> u64 {
        let addr = self.list_address.clone().unwrap_or(computer.registers.pc);
        self.list_address = Some(addr + offset);
        addr
    }

    /// Display the list of breakpoints
    fn display_breakpoints(&self, computer: &Computer) {
        match self.breakpoints.len() {
            0 => info!("No breakpoints"),
            1 => info!("1 breakpoint:"),
            x => info!("{} breakpoints:", x),
        }

        // This might be an unnecessary copy, but we want them to be sorted by address for
        // readability
        let mut bp: Vec<_> = self.breakpoints.iter().cloned().collect();
        bp.sort_unstable();
        for addr in bp.into_iter() {
            self.display_instruction(computer, addr);
        }
    }

    /// Display an instruction at specified address
    fn display_instruction(&self, computer: &Computer, address: u64) {
        // First, display the labels on the line if any
        self.labels
            .iter()
            .filter(|(_, &a)| a == address)
            .for_each(|(label, _)| info!("          {}:", label));

        // Then compute what is supposed to show in the gutter
        let is_current_line = computer.registers.pc == address;
        let has_breakpoint = self.has_breakpoint(address);

        let gutter = match (has_breakpoint, is_current_line) {
            (true, true) => "B>",
            (true, false) => "B ",
            (false, true) => " >",
            (false, false) => "  ",
        };

        // Find the instruction in memory. This will be `None` if the address is to high or if the
        // cell is not an instruction.
        let instruction = computer
            .memory
            .get(address)
            .ok()
            .and_then(|c| c.extract_instruction().ok());

        if let Some(instruction) = instruction {
            info!("{:<2} {:>5}    {}", gutter, address, instruction);
        } else {
            info!("{:<2} {:>5}    –", gutter, address);
        }
    }

    /// Display the list of labels
    fn display_labels(&self) {
        match self.labels.len() {
            0 => info!("No labels"),
            1 => info!("1 label:"),
            x => info!("{} labels:", x),
        }

        for (label, &addr) in self.labels.iter() {
            info!("  {} => {}", label, addr);
        }
    }

    /// Display the number of CPU cycles used
    fn display_cycles(&self, computer: &Computer) {
        info!("Cycles: {}", computer.cycles);
    }
}

pub(crate) fn run_interactive(
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

    let mut session = Session::from_debug_info(debug_info);

    let h: RunHelper<Command> = RunHelper::new();
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

        match &command {
            Command::Exit => break,
            Command::Step { number } => {
                // TODO: recover from errors
                for _ in 0..*number {
                    computer.step()?;
                }

                session.reset_list();
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
                    for i in 0..(*number as u64) {
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
                session.reset_list();
            }

            Command::List { number } => {
                let addr = session.offset_list(computer, *number);
                for i in 0..*number {
                    let addr = addr + i;
                    session.display_instruction(computer, addr);
                }
            }

            Command::Break { address } => {
                // TODO: recover from error
                let address = computer.resolve_address(address)?;
                session.add_breakpoint(address);
            }

            Command::Unbreak { address } => {
                // TODO: recover from error
                let address = computer.resolve_address(address)?;
                session.remove_breakpoint(address);
            }

            Command::Continue => {
                loop {
                    // TODO: recover from error
                    computer.step()?;
                    if session.has_breakpoint(computer.registers.pc) {
                        break;
                    }
                }
                info!(address = computer.registers.pc, "Stopped at a breakpoint");
            }

            Command::Info { sub } => match sub {
                Some(InfoCommand::Breakpoints) => {
                    session.display_breakpoints(computer);
                }
                Some(InfoCommand::Labels) => {
                    session.display_labels();
                }
                Some(InfoCommand::Cycles) => {
                    session.display_cycles(computer);
                }
                None => {
                    session.display_breakpoints(computer);
                    info!("–");
                    session.display_labels();
                    info!("–");
                    session.display_cycles(computer);
                }
            },
        };

        last_command = Some(command);
    }

    Ok(())
}
