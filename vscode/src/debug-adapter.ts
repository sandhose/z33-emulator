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
import { includeWorkspaceFolderInPaths } from "./workspace-paths.js";

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

/**
 * Gather every workspace `.s`/`.S` file as a workspace-relative path → content
 * map, and resolve `program` to the matching relative path.
 */
async function collectWorkspaceFiles(
  program: string | undefined,
): Promise<{ files: Record<string, string>; program: string }> {
  const files: Record<string, string> = {};
  const uris = await vscode.workspace.findFiles("**/*.{s,S}");

  const decoder = new TextDecoder();
  // Must match the LSP push in extension.ts so include resolution and program
  // lookup agree on keys (folder-prefixed only when there are multiple roots).
  const includeFolder = includeWorkspaceFolderInPaths();
  let resolvedProgram = program ?? "";

  for (const uri of uris) {
    const relative = vscode.workspace.asRelativePath(uri, includeFolder);
    const bytes = await vscode.workspace.fs.readFile(uri);
    files[relative] = decoder.decode(bytes);

    // Match the configured program (an absolute/`${file}` path) to its
    // workspace-relative key so the in-memory FS lookup succeeds.
    if (program !== undefined && program.length > 0) {
      if (uri.fsPath === program || uri.toString() === program) {
        resolvedProgram = relative;
      }
    }
  }

  return { files, program: resolvedProgram };
}

export class Z33DebugAdapter implements vscode.DebugAdapter {
  private readonly sendEmitter = new vscode.EventEmitter<vscode.DebugProtocolMessage>();
  readonly onDidSendMessage: vscode.Event<vscode.DebugProtocolMessage> = this.sendEmitter.event;

  private pumping = false;

  constructor(private readonly server: WasmDapServer) {}

  handleMessage(message: vscode.DebugProtocolMessage): void {
    if (isLaunchRequest(message)) {
      void this.handleLaunch(message);
      return;
    }
    this.dispatch(message);
  }

  private async handleLaunch(message: LaunchRequest): Promise<void> {
    try {
      const args = message.arguments ?? {};
      const { files, program } = await collectWorkspaceFiles(args.program);

      const augmented: LaunchRequest = {
        ...message,
        arguments: { ...args, program, files },
      };
      this.dispatch(augmented);
    } catch (error) {
      // A failed launch (e.g. file read error) must fail the request visibly,
      // otherwise VS Code shows a spinner forever.
      this.emitErrorResponse(message, "launch", errorMessage(error));
    }
  }

  /** Feed one message to the server, emit its output, then drive the run loop. */
  private dispatch(message: unknown): void {
    try {
      const responses = this.server.handle_message(message);
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
    this.sendEmitter.fire(message as vscode.DebugProtocolMessage);
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
