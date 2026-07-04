// Inline Z33 debug adapter, driven directly over the wasm `WasmDapServer`.
//
// Runs on the extension host (itself a worker in the web). The wasm module must
// already be instantiated (see `initDap` in extension.ts) before constructing
// an adapter.
//
// Driving loop: `WasmDapServer.run_chunk()` advances a bounded number of
// instructions and yields; an empty result means "still running, call again".
// We schedule each chunk via `setTimeout(0)` so that incoming `handleMessage`
// calls (pause / disconnect / etc.) can interleave with execution.

import * as vscode from "vscode";
import type { WasmDapServer } from "../dist/pkg/z33_web.js";
import { collectWorkspaceFiles } from "./workspace-paths.js";

/** A launch request as sent by VS Code, before we inject the file map. */
interface LaunchRequest {
  seq: number;
  type: "request";
  command: "launch";
  arguments?: {
    program?: string;
    entrypoint?: string;
    stopOnEntry?: boolean;
    files?: Record<string, string>;
  };
}

function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

function isLaunchRequest(message: unknown): message is LaunchRequest {
  return (
    typeof message === "object" &&
    message !== null &&
    (message as { type?: unknown }).type === "request" &&
    (message as { command?: unknown }).command === "launch"
  );
}

export class Z33DebugAdapter implements vscode.DebugAdapter {
  private readonly sendEmitter = new vscode.EventEmitter<vscode.DebugProtocolMessage>();
  readonly onDidSendMessage: vscode.Event<vscode.DebugProtocolMessage> = this.sendEmitter.event;

  private pumping = false;

  // Path translation between the server's workspace-relative keys (used by the
  // in-memory filesystem / `LineIndex`) and the paths VS Code speaks. Built in
  // `handleLaunch` from the collected workspace URIs. Because that build awaits
  // async file I/O — and the server emits `initialized` during `initialize`, so
  // configuration-phase `setBreakpoints` can arrive before the launch dispatch
  // finishes — we queue every non-launch message that comes in while the launch
  // is in flight (see `launching`/`queued`) and flush it afterwards. That
  // guarantees both these maps and the server's loaded program exist before any
  // queued request reaches the server.
  //
  // `relToClient`: relative key → real client path (`uri.fsPath` for `file:`
  // URIs, otherwise `uri.toString()` — VS Code opens both). `clientToRel`:
  // client representation → relative key (`fsPath` and URI string for `file:`,
  // URI string only for virtual schemes — keying `fsPath` there would collide
  // across authorities).
  private readonly relToClient = new Map<string, string>();
  private readonly clientToRel = new Map<string, string>();

  // While `handleLaunch` is in flight, incoming non-launch messages are queued
  // here instead of dispatched, then flushed in order once the launch dispatch
  // completes (success or failure).
  private launching = false;
  private readonly queued: vscode.DebugProtocolMessage[] = [];

  constructor(private readonly server: WasmDapServer) {}

  handleMessage(message: vscode.DebugProtocolMessage): void {
    if (isLaunchRequest(message)) {
      this.launching = true;
      void this.handleLaunch(message);
      return;
    }
    if (this.launching) {
      // Maps (and the loaded program) aren't ready yet; defer until the launch
      // dispatch completes so message order is preserved.
      this.queued.push(message);
      return;
    }
    this.dispatch(message);
  }

  private async handleLaunch(message: LaunchRequest): Promise<void> {
    try {
      const args = message.arguments ?? {};
      const { files, program, uris } = await collectWorkspaceFiles(args.program);
      this.buildPathMaps(uris);

      const augmented: LaunchRequest = {
        ...message,
        arguments: { ...args, program, files },
      };
      this.dispatch(augmented);
    } catch (error) {
      // A failed launch (e.g. file read error) must fail the request visibly,
      // otherwise VS Code shows a spinner forever.
      this.emitErrorResponse(message, "launch", errorMessage(error));
    } finally {
      // Flush anything that arrived during the launch, in order. Clear the flag
      // first so `dispatch` runs normally (and any message that somehow arrives
      // mid-flush is handled directly rather than re-queued).
      this.launching = false;
      while (this.queued.length > 0) {
        const next = this.queued.shift();
        if (next !== undefined) {
          this.dispatch(next);
        }
      }
    }
  }

  /** (Re)build the client↔relative-key path maps from the collected URIs. */
  private buildPathMaps(uris: Map<string, vscode.Uri>): void {
    this.relToClient.clear();
    this.clientToRel.clear();
    for (const [relative, uri] of uris) {
      // Prefer a filesystem path for `file:` URIs; fall back to the URI string
      // for virtual schemes (e.g. `vscode-vfs://` on vscode.dev), which VS Code
      // also accepts in `Source.path`.
      const clientPath = uri.scheme === "file" ? uri.fsPath : uri.toString();
      this.relToClient.set(relative, clientPath);
      // Map the client representation(s) back to the relative key. For `file:`
      // URIs VS Code may send either `fsPath` or the URI string, so key on
      // both. For virtual schemes VS Code sends `uri.toString()`; we must NOT
      // key on `fsPath` there because `Uri.fsPath` drops the URI authority, so
      // two roots on different authorities with the same tail path would
      // collide (last write wins → wrong-file breakpoints).
      if (uri.scheme === "file") {
        this.clientToRel.set(uri.fsPath, relative);
      }
      this.clientToRel.set(uri.toString(), relative);
    }
  }

  /** Feed one message to the server, emit its output, then drive the run loop. */
  private dispatch(message: unknown): void {
    const rewritten = this.rewriteIncomingSource(message);
    try {
      const responses = this.server.handle_message(rewritten);
      for (const response of responses) {
        this.emit(response);
      }
    } catch (error) {
      // Surface the failure as a DAP error response for this request so the
      // client isn't left awaiting a reply that never comes.
      const req = message as { seq?: number; command?: unknown };
      this.emitErrorResponse(
        message,
        typeof req.command === "string" ? req.command : "",
        errorMessage(error),
      );
      return;
    }
    this.maybePump();
  }

  private maybePump(): void {
    if (this.pumping || !this.server.is_running()) {
      return;
    }
    this.pumping = true;
    this.scheduleChunk();
  }

  private scheduleChunk(): void {
    setTimeout(() => {
      try {
        if (!this.server.is_running()) {
          this.pumping = false;
          return;
        }
        const events = this.server.run_chunk();
        for (const event of events) {
          this.emit(event);
        }
        if (this.server.is_running()) {
          this.scheduleChunk();
        } else {
          this.pumping = false;
        }
      } catch (error) {
        // The run loop crashed; report it and end the session cleanly instead
        // of leaving `pumping` stuck true (which would wedge future runs).
        this.pumping = false;
        this.emitOutput(`Execution error: ${errorMessage(error)}`);
        this.emitTerminated();
      }
    }, 0);
  }

  private emit(message: unknown): void {
    this.rewriteOutgoingSources(message);
    this.sendEmitter.fire(message as vscode.DebugProtocolMessage);
  }

  /**
   * Reverse-map a request's `arguments.source.path` from the client path VS
   * Code sends back to the server's relative key, so `setBreakpoints`,
   * `source` and friends resolve to the right in-memory file (rather than
   * relying on `LineIndex`'s ambiguous base-name fallback). Unknown paths pass
   * through unchanged.
   *
   * Non-mutating: VS Code retains and reuses the request object it hands to
   * `handleMessage` (persistent `Source.raw` state in its debug model), so we
   * must not write into it. When a rewrite applies we return a shallow-cloned
   * chain with the swapped path; otherwise we return the original message.
   */
  private rewriteIncomingSource(message: unknown): unknown {
    if (typeof message !== "object" || message === null) {
      return message;
    }
    const args = (message as { arguments?: unknown }).arguments;
    if (typeof args !== "object" || args === null) {
      return message;
    }
    const source = (args as { source?: unknown }).source;
    if (typeof source !== "object" || source === null) {
      return message;
    }
    const src = source as { path?: unknown };
    if (typeof src.path !== "string") {
      return message;
    }
    const relative = this.clientToRel.get(src.path);
    if (relative === undefined) {
      return message;
    }
    return {
      ...(message as object),
      arguments: {
        ...(args as object),
        source: { ...(source as object), path: relative },
      },
    };
  }

  /**
   * Rewrite server-side relative `Source.path` values to real client paths so
   * VS Code opens the actual workspace file instead of a read-only `debug:`
   * virtual document. Walks the whole message and rewrites any object under a
   * `source` key carrying a string `path` — stack frames (`stackTrace`),
   * `Breakpoint.source` (`setBreakpoints` responses / `breakpoint` events) and
   * `output` events. Paths with no known key are left untouched so the DAP
   * `source`-request fallback still applies (e.g. `#include`d files).
   */
  private rewriteOutgoingSources(value: unknown): void {
    if (Array.isArray(value)) {
      for (const item of value) {
        this.rewriteOutgoingSources(item);
      }
      return;
    }
    if (typeof value !== "object" || value === null) {
      return;
    }
    for (const [key, child] of Object.entries(value as Record<string, unknown>)) {
      if (key === "source" && typeof child === "object" && child !== null) {
        const source = child as { path?: unknown };
        if (typeof source.path === "string") {
          const mapped = this.relToClient.get(source.path);
          if (mapped !== undefined) {
            source.path = mapped;
          }
        }
      }
      this.rewriteOutgoingSources(child);
    }
  }

  /** Emit a failed DAP response for a request (matched by `request_seq`). */
  private emitErrorResponse(request: unknown, command: string, message: string): void {
    const seq = (request as { seq?: number }).seq ?? 0;
    this.emit({
      seq: 0,
      type: "response",
      request_seq: seq,
      success: false,
      command,
      message,
      body: { error: { id: 0, format: message, showUser: true } },
    });
  }

  private emitOutput(text: string): void {
    this.emit({
      seq: 0,
      type: "event",
      event: "output",
      body: { category: "stderr", output: `${text}\n` },
    });
  }

  private emitTerminated(): void {
    this.emit({ seq: 0, type: "event", event: "terminated" });
  }

  dispose(): void {
    this.sendEmitter.dispose();
    this.server.free();
  }
}
