// Serial console terminal for Zorglub33 debug sessions.
//
// Each debug session gets a pseudo-terminal ("Zorglub33 Serial") that mirrors
// the program's serial I/O: keystrokes go to the emulator via the custom
// `zorglub33/serialInput` DAP request, and program output (the DAP `stdout`
// output events) is written back into the terminal instead of the Debug
// Console. There is deliberately NO local echo — the running program echoes
// what it reads, matching real serial hardware and the web IDE's console.
//
// The extension owns the lifecycle (create on session start, mark terminated on
// session end); the debug adapter owns the outgoing message stream, so it is
// the one that funnels stdout into the pty (see `Z33DebugAdapter.emit`). The
// two are connected through the session-id-keyed `SerialTerminalManager`.

import * as vscode from "vscode";

const TERMINAL_NAME = "Zorglub33 Serial";
const SERIAL_INPUT_REQUEST = "zorglub33/serialInput";

/**
 * A pseudo-terminal mirroring one debug session's serial console.
 *
 * Output arriving before VS Code calls `open()` (i.e. before the terminal is
 * actually shown on screen) MUST be buffered and flushed on open — otherwise
 * early program output is silently dropped. This is the classic pseudo-terminal
 * gotcha: `onDidWrite` fires into the void until the terminal is live.
 */
class SerialPty implements vscode.Pseudoterminal {
  private readonly writeEmitter = new vscode.EventEmitter<string>();
  readonly onDidWrite: vscode.Event<string> = this.writeEmitter.event;
  private readonly closeEmitter = new vscode.EventEmitter<void>();
  readonly onDidClose: vscode.Event<void> = this.closeEmitter.event;

  private opened = false;
  private pending = "";
  private accepting = true;
  private closed = false;

  constructor(private readonly session: vscode.DebugSession) {}

  /** True once the user (or a session-replacing dispose) closed the terminal. */
  get isClosed(): boolean {
    return this.closed;
  }

  open(): void {
    this.opened = true;
    if (this.pending.length > 0) {
      this.writeEmitter.fire(this.pending);
      this.pending = "";
    }
  }

  close(): void {
    // The user closed the terminal panel (or we disposed it for a new session).
    // Stop mirroring; the manager treats a closed pty as "not attached" so
    // output falls back to the Debug Console rather than vanishing.
    this.closed = true;
    this.accepting = false;
  }

  /** Emit an already-CRLF-correct string, buffering until `open()`. */
  private emit(text: string): void {
    if (this.opened) {
      this.writeEmitter.fire(text);
    } else {
      this.pending += text;
    }
  }

  /** Mirror program stdout. Translates LF → CRLF for terminal display. */
  write(text: string): void {
    if (this.closed) {
      return;
    }
    this.emit(text.replace(/\r?\n/g, "\r\n"));
  }

  /** Note that the session ended: dim notice, keep scrollback, refuse input. */
  markTerminated(): void {
    this.accepting = false;
    if (!this.closed) {
      // Dim (SGR 2) so it reads as chrome, not program output.
      this.emit("\r\n\x1b[2m[program terminated]\x1b[0m\r\n");
    }
  }

  handleInput(data: string): void {
    if (!this.accepting) {
      return;
    }
    // Translate terminal conventions to the host's serial convention:
    //   * Enter arrives as CR (\r) → send LF (\n), the host line convention.
    //   * Backspace arrives as DEL (\x7f) → send BS (\x08), matching the web
    //     console.
    // Everything else (including control chars like Ctrl-D = EOT \x04) passes
    // through untouched — that is the whole point: the program decides what to
    // do with them (echo.s exits on EOT).
    const translated = data.replace(/\r/g, "\n").replace(/\x7f/g, "\x08");
    // Forward the whole chunk as ONE request so a paste is a single IRQ edge,
    // matching the emulator's one-edge-per-`push_input` contract.
    this.session
      .customRequest(SERIAL_INPUT_REQUEST, { data: translated })
      .then(undefined, (error: unknown) => {
        console.error("[zorglub33] serial input failed", error);
      });
  }
}

/**
 * Owns one serial terminal per live debug session and routes stdout to it.
 *
 * Keyed two ways: by session id (so the adapter can find the pty when an
 * `output` event flows) and by workspace (so starting a new session in the same
 * workspace disposes the previous session's terminal — a fresh terminal per
 * session, no unbounded accumulation, while a terminated session's scrollback
 * survives until then).
 */
export class SerialTerminalManager {
  private readonly bySession = new Map<string, SerialPty>();
  private readonly byWorkspace = new Map<string, vscode.Terminal>();

  /** Create (and reveal) the serial terminal for a starting session. */
  start(session: vscode.DebugSession): void {
    const workspaceKey = session.workspaceFolder?.uri.toString() ?? "";
    // Replace any leftover terminal from a previous run in this workspace.
    this.byWorkspace.get(workspaceKey)?.dispose();

    const pty = new SerialPty(session);
    const terminal = vscode.window.createTerminal({ name: TERMINAL_NAME, pty });
    this.bySession.set(session.id, pty);
    this.byWorkspace.set(workspaceKey, terminal);
    // Reveal without stealing focus from the editor.
    terminal.show(true);
  }

  /** Note that a session ended: dim notice in its terminal, stop accepting. */
  terminate(session: vscode.DebugSession): void {
    const pty = this.bySession.get(session.id);
    pty?.markTerminated();
    this.bySession.delete(session.id);
  }

  /**
   * Write program stdout to the session's terminal. Returns `true` if a live
   * terminal consumed it (the caller then suppresses the Debug Console echo),
   * `false` if none is attached (caller forwards the event so output is never
   * lost).
   */
  writeStdout(sessionId: string, text: string): boolean {
    const pty = this.bySession.get(sessionId);
    if (pty === undefined || pty.isClosed) {
      return false;
    }
    pty.write(text);
    return true;
  }

  dispose(): void {
    for (const terminal of this.byWorkspace.values()) {
      terminal.dispose();
    }
    this.bySession.clear();
    this.byWorkspace.clear();
  }
}
