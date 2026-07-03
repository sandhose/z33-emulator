//! `z33-cli dap` — a stdio Debug Adapter Protocol server.
//!
//! Uses the same `Content-Length` framing as LSP. The emulator crate provides
//! the transport-agnostic [`DebugSession`]; this command owns the I/O: a
//! background thread reads and de-frames stdin into JSON messages, and the main
//! loop interleaves message handling with the chunked run loop so `continue`
//! never blocks incoming `pause` requests.

use std::io::{BufRead, BufReader, Read, Write};
use std::sync::mpsc::{self, Receiver, TryRecvError};
use std::thread;

use clap::Parser;
use serde_json::Value;
use z33_emulator::dap::DebugSession;

/// Start the Z33 Debug Adapter Protocol server (DAP)
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

/// Read the message headers, returning the `Content-Length` in bytes. Returns
/// `None` on EOF.
fn read_content_length<R: BufRead>(reader: &mut R) -> Option<usize> {
    let mut content_length = None;
    loop {
        let mut line = String::new();
        let read = reader.read_line(&mut line).ok()?;
        if read == 0 {
            return None; // EOF
        }
        let trimmed = line.trim_end_matches(['\r', '\n']);
        if trimmed.is_empty() {
            // End of headers.
            return content_length;
        }
        // DAP (like HTTP/LSP) header names are case-insensitive.
        if let Some((name, value)) = trimmed.split_once(':') {
            if name.trim().eq_ignore_ascii_case("Content-Length") {
                content_length = value.trim().parse::<usize>().ok();
            }
        }
    }
}

/// Serialize and frame a single DAP message to stdout.
fn write_message(stdout: &std::io::Stdout, message: &Value) -> anyhow::Result<()> {
    let body = serde_json::to_vec(message)?;
    let mut handle = stdout.lock();
    write!(handle, "Content-Length: {}\r\n\r\n", body.len())?;
    handle.write_all(&body)?;
    handle.flush()?;
    Ok(())
}
