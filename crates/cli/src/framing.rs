//! `Content-Length` framing shared by the stdio LSP and DAP servers.
//!
//! Both protocols use the same HTTP-style framing: a set of `\r\n`-terminated
//! headers (of which only `Content-Length` matters), a blank line, then a JSON
//! body of exactly that many bytes.

use std::io::{BufRead, Write};

use serde_json::Value;

/// Read the message headers, returning the `Content-Length` in bytes. Returns
/// `None` on EOF.
pub fn read_content_length<R: BufRead>(reader: &mut R) -> Option<usize> {
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
        // LSP/DAP (like HTTP) header names are case-insensitive.
        if let Some((name, value)) = trimmed.split_once(':') {
            if name.trim().eq_ignore_ascii_case("Content-Length") {
                content_length = value.trim().parse::<usize>().ok();
            }
        }
    }
}

/// Serialize and frame a single message to stdout.
pub fn write_message(stdout: &std::io::Stdout, message: &Value) -> anyhow::Result<()> {
    let body = serde_json::to_vec(message)?;
    let mut handle = stdout.lock();
    write!(handle, "Content-Length: {}\r\n\r\n", body.len())?;
    handle.write_all(&body)?;
    handle.flush()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::read_content_length;

    fn read(input: &[u8]) -> Option<usize> {
        let mut reader: &[u8] = input;
        read_content_length(&mut reader)
    }

    #[test]
    fn parses_content_length() {
        assert_eq!(read(b"Content-Length: 42\r\n\r\n"), Some(42));
    }

    #[test]
    fn header_name_is_case_insensitive() {
        assert_eq!(read(b"content-length: 7\r\n\r\n"), Some(7));
    }

    #[test]
    fn ignores_unrelated_headers() {
        assert_eq!(
            read(b"Content-Type: application/json\r\nContent-Length: 3\r\n\r\n"),
            Some(3)
        );
    }

    #[test]
    fn accepts_bare_lf_line_endings() {
        assert_eq!(read(b"Content-Length: 9\n\n"), Some(9));
    }

    #[test]
    fn eof_returns_none() {
        assert_eq!(read(b""), None);
    }

    #[test]
    fn missing_blank_terminator_returns_none() {
        // A header line but no terminating blank line before EOF.
        assert_eq!(read(b"Content-Length: 5\r\n"), None);
    }

    #[test]
    fn invalid_length_yields_none() {
        // A non-numeric value fails to parse and the headers end with no length.
        assert_eq!(read(b"Content-Length: abc\r\n\r\n"), None);
    }

    #[test]
    fn no_content_length_header_returns_none() {
        assert_eq!(read(b"Content-Type: text/plain\r\n\r\n"), None);
    }
}
