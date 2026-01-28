//! This module implements the TTY interactive interface.
//!
//! It is mainly based on two crates:
//!   - rustyline, to handle the line-editting logic
//!   - clap, to handle the parsing of those interactive commands
//!
//! Using Parser to do this is a bit of a hack, and requires some weird options
//! to have it working but works nonetheless.

use std::collections::{BTreeMap, HashSet};

use clap::Parser;
use rustyline::{Behavior, CompletionType, Config, EditMode, Editor};
use tracing::{debug, info, warn};
use z33_emulator::compiler::DebugInfo;
use z33_emulator::constants as C;
use z33_emulator::runtime::{Cell, Computer, Exception, Reg};

mod helper;
mod parse;
use self::helper::RunHelper;

static HELP: &str = r#"
Run "help [command]" for command-specific help.
An empty line re-runs the last valid command."#;

#[derive(Parser, Clone, Debug)]
#[clap(
    help_template = "{about}\n\nCOMMANDS:\n{subcommands}\n{after-help}",
    after_help = HELP,
    disable_version_flag = true,
    infer_subcommands = true,
    no_binary_name = true,
    allow_negative_numbers = true,
)]
/// Interactive mode commands
enum Command {
    /// Execute the next instructions
    #[command(alias = "s")]
    Step {
        /// Number of steps to execute
        #[clap(value_parser, default_value = "1")]
        number: u64,
    },

    /// Exit the emulator
    Exit,

    /// Show the state of registers
    Registers {
        #[clap(value_parser)]
        register: Option<Reg>,
    },

    /// Show the content of a block in memory
    Memory {
        /// The address to show. Can be a direct address (number literal) or an
        /// indirect one (register with an optional offset).
        #[clap(value_parser)]
        address: parse::Argument,

        /// Number of memory cells to show.
        #[clap(value_parser, default_value = "1")]
        number: i32,
    },

    /// Set a value in memory
    Set {
        /// The address or register to set.
        #[clap(value_parser)]
        target: parse::AssignmentTarget,

        /// The value to set
        #[clap(value_parser)]
        value: parse::Argument,
    },

    /// Trigger a hardware interrupt
    Interrupt,

    /// Show the next few instructions
    List {
        /// Number of instructions to show.
        #[clap(value_parser, default_value = "10")]
        number: u32,
    },

    /// Set a breakpoint
    Break {
        /// The address where to set the breakpoint
        #[clap(value_parser)]
        address: parse::Argument,
    },

    /// Remove a breakpoint
    Unbreak {
        /// The address of the breakpoint to remove
        #[clap(value_parser)]
        address: parse::Argument,
    },

    /// Continue the program until the next breakpoint or reset
    Continue,

    /// Show informations about the current debugging session
    Info {
        #[clap(subcommand)]
        sub: Option<InfoCommand>,
    },
}

#[derive(Parser, Clone, Debug)]
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
    breakpoints: HashSet<C::Address>,

    /// Map of labels in program
    labels: BTreeMap<String, C::Address>,

    /// Current address for the `list` command
    list_address: Option<C::Address>,
}

impl Session {
    fn from_debug_info(info: DebugInfo) -> Session {
        Session {
            labels: info.labels,
            ..Default::default()
        }
    }

    /// Add a breakpoint
    fn add_breakpoint(&mut self, address: C::Address) {
        if self.breakpoints.insert(address) {
            info!(address, "Setting a breakpoint");
        } else {
            warn!(address, "A breakpoint was already set");
        }
    }

    /// Remove a breakpoint
    fn remove_breakpoint(&mut self, address: C::Address) {
        if self.breakpoints.remove(&address) {
            info!(address, "Removing breakpoint");
        } else {
            warn!(address, "No breakpoint was set here");
        }
    }

    /// Checks if the given address has a breakpoint
    fn has_breakpoint(&self, address: C::Address) -> bool {
        self.breakpoints.contains(&address)
    }

    /// Reset the `list` command (after running an instruction)
    fn reset_list(&mut self) {
        self.list_address = None;
    }

    /// Offset the `list` command, returns the address to show
    fn offset_list(&mut self, computer: &Computer, offset: C::Address) -> C::Address {
        let addr = self.list_address.unwrap_or(computer.registers.pc);
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

        // This might be an unnecessary copy, but we want them to be sorted by address
        // for readability
        let mut bp: Vec<_> = self.breakpoints.iter().copied().collect();
        bp.sort_unstable();
        for addr in bp {
            self.display_instruction(computer, addr);
        }
    }

    /// Display an instruction at specified address
    fn display_instruction(&self, computer: &Computer, address: C::Address) {
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

        // Find the instruction in memory. This will be `None` if the address is to high
        // or if the cell is not an instruction.
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

        for (label, &addr) in &self.labels {
            info!("  {} => {}", label, addr);
        }
    }

    /// Display the number of CPU cycles used
    fn display_cycles(computer: &Computer) {
        info!("Cycles: {}", computer.cycles);
    }
}

#[allow(clippy::too_many_lines)]
pub(crate) fn run_interactive(computer: &mut Computer, debug_info: DebugInfo) {
    info!("Running in interactive mode. Type \"help\" to list available commands.");
    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Emacs)
        .behavior(Behavior::PreferTerm)
        .auto_add_history(true)
        .build();

    let mut session = Session::from_debug_info(debug_info);

    let h: RunHelper<Command> = RunHelper::new();
    let mut rl = Editor::with_config(config).expect("Initialize terminal input");
    rl.set_helper(Some(h));

    let mut last_command: Option<Command> = None;
    let mut halted = false;

    'read: loop {
        // A macro to unwrap an error, log it and continue the loop
        macro_rules! warn_and_continue {
            ($e:expr) => {
                match $e {
                    Ok(o) => o,
                    Err(e) => {
                        tracing::warn!(error = %e);
                        continue 'read;
                    }
                }
            };
        }

        let Ok(readline) = rl.readline(">> ") else {
            info!("EOF, exitting");
            return;
        };

        let command = if readline.is_empty() {
            if let Some(command) = &last_command {
                command.clone()
            } else {
                info!("Type \"help\" to get the list of available commands");
                continue 'read;
            }
        } else {
            let Ok(words) = shell_words::split(readline.as_str()) else {
                warn!("Invalid input");
                continue 'read;
            };

            let command = warn_and_continue!(Command::try_parse_from(words));
            last_command = Some(command.clone());
            command
        };

        debug!("Executing command: {:?}", command);

        match (command, halted) {
            (Command::Exit, _) => break,
            (Command::Step { number }, false) => {
                session.reset_list();

                for _ in 0..number {
                    if let Err(e) = computer.step() {
                        warn!(error = &e as &dyn std::error::Error, "Halted");
                        halted = true;
                        continue 'read;
                    }
                }
            }

            (Command::Registers { register }, _) => {
                if let Some(reg) = register {
                    match reg {
                        Reg::SR => {
                            info!("Register %sr = {:?}", computer.registers.sr);
                        }
                        reg => {
                            let cell = computer.registers.get(&reg);
                            info!("Register {} = {}", reg, cell);
                        }
                    }
                } else {
                    info!("Registers: {}", computer.registers);
                }
            }

            (Command::Memory { address, number }, _) => {
                let address: C::Address =
                    warn_and_continue!(address.evaluate(computer, &session.labels));

                if number.is_positive() {
                    for i in 0..(number.unsigned_abs() as C::Address) {
                        let address = address + i;
                        let cell = warn_and_continue!(computer.memory.get(address));
                        info!(address, value = %cell);
                    }
                } else {
                    for i in 0..(number.unsigned_abs() as C::Address) {
                        let address = address - i;
                        let cell = warn_and_continue!(computer.memory.get(address));
                        info!(address, value = %cell);
                    }
                }
            }

            (Command::Set { target, value }, false) => match &target {
                parse::AssignmentTarget::Address(node) => {
                    let address = warn_and_continue!(node.evaluate(&session.labels));
                    let value = warn_and_continue!(value.evaluate(computer, &session.labels));
                    info!("Setting memory at address {address} to {value}");
                    let cell = warn_and_continue!(computer.memory.get_mut(address));
                    *cell = Cell::Word(value);
                }

                parse::AssignmentTarget::Register(reg) => {
                    let value = warn_and_continue!(value.evaluate(computer, &session.labels));
                    info!("Setting register {reg} to {value}");
                    warn_and_continue!(computer.registers.set(*reg, Cell::Word(value)));
                }
            },

            (Command::Interrupt, false) => {
                if let Err(e) = computer.recover_from_exception(&Exception::HardwareInterrupt) {
                    warn!(error = &e as &dyn std::error::Error, "Halted");
                    halted = true;
                    continue 'read;
                }

                session.reset_list();
            }

            (Command::List { number }, _) => {
                let addr = session.offset_list(computer, number);
                for i in 0..number {
                    let addr = addr + i;
                    session.display_instruction(computer, addr);
                }
            }

            (Command::Break { address }, false) => {
                let address = warn_and_continue!(address.evaluate(computer, &session.labels));
                session.add_breakpoint(address);
            }

            (Command::Unbreak { address }, false) => {
                let address = warn_and_continue!(address.evaluate(computer, &session.labels));
                session.remove_breakpoint(address);
            }

            (Command::Continue, false) => loop {
                if let Err(e) = computer.step() {
                    warn!(error = &e as &dyn std::error::Error, "Halted");
                    halted = true;
                    continue 'read;
                }

                if session.has_breakpoint(computer.registers.pc) {
                    info!(address = computer.registers.pc, "Stopped at a breakpoint");
                    break;
                }
            },

            (Command::Info { sub }, _) => match sub {
                Some(InfoCommand::Breakpoints) => {
                    session.display_breakpoints(computer);
                }
                Some(InfoCommand::Labels) => {
                    session.display_labels();
                }
                Some(InfoCommand::Cycles) => {
                    Session::display_cycles(computer);
                }
                None => {
                    session.display_breakpoints(computer);
                    info!("–");
                    session.display_labels();
                    info!("–");
                    Session::display_cycles(computer);
                }
            },

            (_, true) => {
                // Computer is halted but the user asked to continue, we just warn
                warn!("Computer is halted. Use \"exit\" to quit");
            }
        }
    }
}
