// Singleton client for the WASM language server running in `lsp.worker.ts`.
//
// Owns the worker and a `vscode-jsonrpc` `MessageConnection`, performs the
// LSP `initialize`/`initialized` handshake, and exposes thin request/notify
// helpers plus the server-provided semantic-tokens legend. Diagnostics are
// delivered through `onDiagnostics`.
import {
  BrowserMessageReader,
  BrowserMessageWriter,
  createMessageConnection,
  type MessageConnection,
} from "vscode-jsonrpc/browser";
import type {
  Diagnostic,
  InitializeResult,
  PublishDiagnosticsParams,
  SemanticTokensLegend,
} from "vscode-languageserver-protocol";
import { isWorkerErrorFrame } from "../workers/worker-protocol";

/** How long to wait for the `initialize`/`initialized` handshake to complete. */
const HANDSHAKE_TIMEOUT_MS = 30_000;

type DiagnosticsListener = (uri: string, diagnostics: Diagnostic[]) => void;

interface LspClient {
  readonly connection: MessageConnection;
  readonly legend: SemanticTokensLegend | null;
  request<T>(method: string, params: unknown): Promise<T>;
  notify(method: string, params: unknown): void;
  onDiagnostics(listener: DiagnosticsListener): () => void;
}

const CLIENT_CAPABILITIES = {
  textDocument: {
    synchronization: { dynamicRegistration: false },
    publishDiagnostics: { relatedInformation: true },
    hover: { contentFormat: ["markdown", "plaintext"] },
    completion: {
      completionItem: {
        snippetSupport: true,
        documentationFormat: ["markdown", "plaintext"],
      },
    },
    signatureHelp: {
      signatureInformation: { documentationFormat: ["markdown", "plaintext"] },
    },
    definition: { linkSupport: true },
    references: {},
    documentHighlight: {},
    documentSymbol: { hierarchicalDocumentSymbolSupport: true },
    rename: { prepareSupport: true },
    codeLens: {},
    semanticTokens: {
      requests: { full: true },
      tokenTypes: [],
      tokenModifiers: [],
      formats: ["relative"],
    },
  },
  workspace: {},
};

class LspClientImpl implements LspClient {
  readonly connection: MessageConnection;
  legend: SemanticTokensLegend | null = null;

  #worker: Worker;
  #diagnosticsListeners = new Set<DiagnosticsListener>();
  #ready: Promise<void>;
  #resolveReady!: () => void;
  #rejectReady!: (error: Error) => void;
  #settled = false;
  #handshakeTimer: ReturnType<typeof setTimeout> | undefined;

  constructor() {
    this.#ready = new Promise<void>((resolve, reject) => {
      this.#resolveReady = resolve;
      this.#rejectReady = reject;
    });
    // Never let the readiness promise be an unhandled rejection: consumers that
    // care (`request`/`ready`) await it and handle the throw; this keeps the
    // others quiet.
    this.#ready.catch(() => {});

    this.#worker = new Worker(
      new URL("../workers/lsp.worker.ts", import.meta.url),
      { type: "module", name: "z33-lsp" },
    );

    // The worker posts a sentinel frame if its wasm init fails, and fires an
    // `error` event on a hard script failure. Either means the server will
    // never come up.
    this.#worker.addEventListener("message", (event: MessageEvent) => {
      if (isWorkerErrorFrame(event.data)) {
        this.#fail(
          new Error(`LSP worker failed to start: ${event.data.message}`),
        );
      }
    });
    this.#worker.addEventListener("error", (event: ErrorEvent) => {
      this.#fail(new Error(`LSP worker error: ${event.message}`));
    });

    this.connection = createMessageConnection(
      new BrowserMessageReader(this.#worker),
      new BrowserMessageWriter(this.#worker),
    );

    this.connection.onNotification(
      "textDocument/publishDiagnostics",
      (params: PublishDiagnosticsParams) => {
        for (const listener of this.#diagnosticsListeners) {
          listener(params.uri, params.diagnostics);
        }
      },
    );

    this.connection.onError((error) => {
      this.#fail(new Error(`LSP connection error: ${String(error[0])}`));
    });
    this.connection.onClose(() => {
      this.#fail(new Error("LSP worker connection closed"));
    });

    this.#handshakeTimer = setTimeout(() => {
      this.#fail(
        new Error(`LSP handshake timed out after ${HANDSHAKE_TIMEOUT_MS}ms`),
      );
    }, HANDSHAKE_TIMEOUT_MS);

    this.connection.listen();
    this.#handshake().then(
      () => {
        this.#succeed();
      },
      (error: unknown) => {
        this.#fail(error instanceof Error ? error : new Error(String(error)));
      },
    );
  }

  #succeed(): void {
    if (this.#settled) return;
    this.#settled = true;
    clearTimeout(this.#handshakeTimer);
    this.#resolveReady();
  }

  #fail(error: Error): void {
    if (this.#settled) return;
    this.#settled = true;
    clearTimeout(this.#handshakeTimer);
    console.error("[lsp-client]", error);
    this.#rejectReady(error);
  }

  async #handshake(): Promise<void> {
    const result = await this.connection.sendRequest<InitializeResult>(
      "initialize",
      {
        processId: null,
        rootUri: null,
        capabilities: CLIENT_CAPABILITIES,
        clientInfo: { name: "z33-web" },
      },
    );

    const provider = result.capabilities.semanticTokensProvider;
    if (provider && "legend" in provider) {
      this.legend = provider.legend;
    }

    await this.connection.sendNotification("initialized", {});
  }

  async ready(): Promise<void> {
    await this.#ready;
  }

  async request<T>(method: string, params: unknown): Promise<T> {
    await this.#ready;
    return this.connection.sendRequest<T>(method, params);
  }

  notify(method: string, params: unknown): void {
    this.#ready.then(
      () => this.connection.sendNotification(method, params),
      // Readiness rejected (worker/init failure); nothing to notify.
      () => {},
    );
  }

  onDiagnostics(listener: DiagnosticsListener): () => void {
    this.#diagnosticsListeners.add(listener);
    return () => {
      this.#diagnosticsListeners.delete(listener);
    };
  }
}

let instance: LspClientImpl | null = null;

/** Get (or lazily create) the singleton LSP client. */
export function getLspClient(): LspClientImpl {
  instance ??= new LspClientImpl();
  return instance;
}
