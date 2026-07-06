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
  // Cap the pre-open buffer so a chatty program that outputs long before the
  // terminal is shown can't grow `pending` without bound. 256 KiB is far more
  // than any reasonable pre-open burst; older bytes are dropped from the front.
  private static readonly MAX_PENDING = 256 * 1024;

  private readonly writeEmitter = new vscode.EventEmitter<string>();
  readonly onDidWrite: vscode.Event<string> = this.writeEmitter.event;
  private readonly closeEmitter = new vscode.EventEmitter<void>();
  readonly onDidClose: vscode.Event<void> = this.closeEmitter.event;

  private opened = false;
  private pending = "";
  private pendingTruncated = false;
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
      const notice = this.pendingTruncated ? "\x1b[2m[... output truncated]\x1b[0m\r\n" : "";
      this.writeEmitter.fire(notice + this.pending);
      this.pending = "";
      this.pendingTruncated = false;
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
      if (this.pending.length > SerialPty.MAX_PENDING) {
        this.pending = this.pending.slice(this.pending.length - SerialPty.MAX_PENDING);
        this.pendingTruncated = true;
      }
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
    //   * Enter arrives as CR (\r), possibly followed by LF (\r\n on a
    //     Windows-style paste or other CRLF-delivering path) → send a single
    //     LF (\n), the host line convention. Matching `\r\n?` (not just `\r`)
    //     avoids turning one CRLF into two host newlines.
    //   * Backspace arrives as DEL (\x7f) → send BS (\x08), matching the web
    //     console.
    // Everything else (including control chars like Ctrl-D = EOT \x04) passes
    // through untouched — that is the whole point: the program decides what to
    // do with them (echo.s exits on EOT).
    const translated = data.replace(/\r\n?/g, "\n").replace(/\x7f/g, "\x08");
    // Forward the whole chunk as ONE request so a paste is a single IRQ edge,
    // matching the emulator's one-edge-per-`push_input` contract.
    this.session
      .customRequest(SERIAL_INPUT_REQUEST, { data: translated })
      .then(undefined, (error: unknown) => {
        console.error("[zorglub33] serial input failed", error);
      });
  }
}

/** One workspace-scoped terminal, tracked so a live one is never clobbered. */
interface WorkspaceEntry {
  sessionId: string;
  terminal: vscode.Terminal;
  /** Set once the owning session's debug adapter has terminated. */
  ended: boolean;
}

/**
 * Owns one serial terminal per live debug session and routes stdout to it.
 *
 * Keyed two ways: by session id (so the adapter can find the pty when an
 * `output` event flows) and by workspace (so starting a new session in the same
 * workspace disposes any *already-ended* previous session's terminal — a fresh
 * terminal per sequential session, no unbounded accumulation, while a
 * terminated session's scrollback survives until then). Terminals belonging to
 * still-running sessions in the same workspace are deliberately left alone:
 * disposing them unconditionally would kill a live sibling session's terminal
 * out from under it whenever a second concurrent session starts.
 */
export class SerialTerminalManager {
  private readonly bySession = new Map<string, SerialPty>();
  private readonly byWorkspace = new Map<string, WorkspaceEntry[]>();

  /** Create (and reveal) the serial terminal for a starting session. */
  start(session: vscode.DebugSession): void {
    const workspaceKey = session.workspaceFolder?.uri.toString() ?? "";
    const entries = this.byWorkspace.get(workspaceKey) ?? [];

    // Dispose only the terminals whose owning session has already ended;
    // keep the rest so concurrent sessions in the same workspace coexist.
    const live: WorkspaceEntry[] = [];
    for (const entry of entries) {
      if (entry.ended) {
        entry.terminal.dispose();
      } else {
        live.push(entry);
      }
    }

    const pty = new SerialPty(session);
    // VS Code disambiguates duplicate terminal names (e.g. "Zorglub33 Serial
    // #2") automatically, so concurrent sessions don't need distinct names.
    const terminal = vscode.window.createTerminal({ name: TERMINAL_NAME, pty });
    this.bySession.set(session.id, pty);
    live.push({ sessionId: session.id, terminal, ended: false });
    this.byWorkspace.set(workspaceKey, live);
    // Reveal without stealing focus from the editor.
    terminal.show(true);
  }

  /** Note that a session ended: dim notice in its terminal, stop accepting. */
  terminate(session: vscode.DebugSession): void {
    const pty = this.bySession.get(session.id);
    pty?.markTerminated();
    this.bySession.delete(session.id);

    const workspaceKey = session.workspaceFolder?.uri.toString() ?? "";
    const entry = this.byWorkspace
      .get(workspaceKey)
      ?.find((candidate) => candidate.sessionId === session.id);
    if (entry !== undefined) {
      entry.ended = true;
    }
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
    for (const entries of this.byWorkspace.values()) {
      for (const entry of entries) {
        entry.terminal.dispose();
      }
    }
    this.bySession.clear();
    this.byWorkspace.clear();
  }
}
