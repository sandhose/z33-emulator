//! `z33-cli dap` — a stdio Debug Adapter Protocol server.
//!
//! Uses the same `Content-Length` framing as LSP. The emulator crate provides
//! the transport-agnostic [`DebugSession`]; this command owns the I/O: a
//! background thread reads and de-frames stdin into JSON messages, and the main
//! loop interleaves message handling with the chunked run loop so `continue`
//! never blocks incoming `pause` requests.

use std::io::{BufReader, Read};
use std::sync::mpsc::{self, Receiver, TryRecvError};
use std::thread;

use clap::Parser;
use serde_json::Value;
use z33_emulator::dap::DebugSession;

use crate::framing::{read_content_length, write_message};

/// Start the Zorglub33 Debug Adapter Protocol server (DAP)
#[derive(Parser, Debug)]
pub struct DapOpt {}

impl DapOpt {
    #[allow(clippy::unused_self)]
    pub fn exec(self) -> anyhow::Result<()> {
        let rx = spawn_reader();
        let mut session = DebugSession::new();
        let stdout = std::io::stdout();

        loop {
            let messages = if session.is_running() {
                // While running, poll without blocking so we can advance the
                // program between incoming messages (e.g. `pause`).
                match rx.try_recv() {
                    Ok(msg) => session.handle_message(&msg),
                    Err(TryRecvError::Empty) => session.run_chunk(),
                    Err(TryRecvError::Disconnected) => break,
                }
            } else {
                // Idle: block until the next message arrives.
                match rx.recv() {
                    Ok(msg) => session.handle_message(&msg),
                    Err(_) => break,
                }
            };

            for message in messages {
                write_message(&stdout, &message)?;
            }

            if session.exit_requested() {
                break;
            }
        }

        Ok(())
    }
}

/// Spawn a thread that reads `Content-Length`-framed JSON messages from stdin
/// and forwards each decoded [`Value`] over a channel.
fn spawn_reader() -> Receiver<Value> {
    let (tx, rx) = mpsc::channel();
    thread::spawn(move || {
        let mut reader = BufReader::new(std::io::stdin());
        loop {
            let Some(length) = read_content_length(&mut reader) else {
                break;
            };
            let mut body = vec![0u8; length];
            if reader.read_exact(&mut body).is_err() {
                break;
            }
            let Ok(value) = serde_json::from_slice::<Value>(&body) else {
                continue;
            };
            if tx.send(value).is_err() {
                break;
            }
        }
    });
    rx
}
