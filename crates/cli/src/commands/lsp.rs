//! `z33-cli lsp` — a stdio Language Server Protocol server.
//!
//! Uses the same `Content-Length` framing as DAP (see [`crate::framing`]). The
//! emulator crate provides the transport-agnostic [`LspSession`]; this command
//! owns the I/O. LSP is strictly request/response (the session never produces
//! output on its own), so a plain blocking read loop on the main thread is
//! enough — no reader thread is needed, unlike the DAP host's chunked run
//! loop.

use std::io::{BufReader, Read};

use clap::Parser;
use serde_json::Value;
use z33_emulator::lsp::LspSession;

use crate::framing::{read_content_length, write_message};

/// Start the Zorglub33 Language Server (LSP)
#[derive(Parser, Debug)]
pub struct LspOpt {}

impl LspOpt {
    #[allow(clippy::unused_self)]
    pub fn exec(self) -> anyhow::Result<()> {
        let mut session = LspSession::new();
        let mut reader = BufReader::new(std::io::stdin());
        let stdout = std::io::stdout();

        loop {
            let Some(length) = read_content_length(&mut reader) else {
                break; // EOF
            };
            let mut body = vec![0u8; length];
            if reader.read_exact(&mut body).is_err() {
                break;
            }
            let Ok(message) = serde_json::from_slice::<Value>(&body) else {
                continue;
            };

            for message in session.handle_message(&message) {
                write_message(&stdout, &message)?;
            }

            if session.exit_requested() {
                break;
            }
        }

        Ok(())
    }
}
