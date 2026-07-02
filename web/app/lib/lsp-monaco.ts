// Wires the singleton LSP client into Monaco: language feature providers,
// diagnostics, and document synchronization.
//
// URI scheme: every file in the store is a flat name (no leading slash). A
// Monaco model created with `monaco.Uri.file(name)` stringifies to
// `file:///${name}`, which is exactly the URI we use on the wire. The server
// (with `rootUri: null`) relativizes URIs to their last path segment, so
// `file:///fact.s` maps to the workspace-relative path `fact.s` and
// `#include` directives resolve between open documents and the pushed
// `z33/workspaceFiles` base map.
import type * as monaco from "monaco-editor";
import {
  toCodeLens,
  toCompletionList,
  toDefinition,
  toDocumentHighlight,
  toDocumentSymbol,
  toHover,
  toLocation,
  toMarkerData,
  toRange,
  toSemanticTokens,
  toSignatureHelp,
  toWorkspaceEdit,
} from "monaco-languageserver-types";
import type {
  CodeLens,
  CompletionItem,
  CompletionList,
  Definition,
  Diagnostic,
  DocumentHighlight,
  DocumentSymbol,
  Hover,
  Location,
  Range as LspRange,
  SemanticTokens,
  SignatureHelp,
  WorkspaceEdit,
} from "vscode-languageserver-protocol";
import { useFileStore } from "../stores/file-store";
import { getLspClient } from "./lsp-client";

type Monaco = typeof monaco;
type Disposable = monaco.IDisposable;
type CancellationToken = monaco.CancellationToken;

/** Flush any pending debounced `didChange` for a model before a pull request. */
type FlushFn = (model: monaco.editor.ITextModel) => void;

const LANGUAGE_ID = "z33";
const MARKER_OWNER = "z33-lsp";

const uriOf = (model: monaco.editor.ITextModel): string => model.uri.toString();

const lspPosition = (position: monaco.IPosition) => ({
  line: position.lineNumber - 1,
  character: position.column - 1,
});

/** All files currently in the store, as an LSP `z33/workspaceFiles` payload. */
function workspaceFilesParams(): { files: Record<string, string> } {
  return { files: { ...useFileStore.getState().files } };
}

const sortedKeys = (files: Record<string, string>): string =>
  // oxlint-disable-next-line unicorn/no-array-sort -- sorting a fresh Object.keys array
  Object.keys(files).sort().join("\n");

function setupDiagnostics(m: Monaco, disposables: Disposable[]): void {
  const client = getLspClient();
  const off = client.onDiagnostics((uri, diagnostics: Diagnostic[]) => {
    const model = m.editor.getModel(m.Uri.parse(uri));
    if (!model) return;
    m.editor.setModelMarkers(
      model,
      MARKER_OWNER,
      diagnostics.map((d) => toMarkerData(d)),
    );
  });
  disposables.push({ dispose: off });
}

function setupDocumentSync(m: Monaco, disposables: Disposable[]): FlushFn {
  const client = getLspClient();
  const versions = new Map<string, number>();
  const changeTimers = new Map<string, ReturnType<typeof setTimeout>>();

  const nextVersion = (uri: string): number => {
    const v = (versions.get(uri) ?? 0) + 1;
    versions.set(uri, v);
    return v;
  };

  const didOpen = (model: monaco.editor.ITextModel): void => {
    const uri = uriOf(model);
    client.notify("textDocument/didOpen", {
      textDocument: {
        uri,
        languageId: LANGUAGE_ID,
        version: nextVersion(uri),
        text: model.getValue(),
      },
    });
  };

  const didChange = (model: monaco.editor.ITextModel): void => {
    const uri = uriOf(model);
    client.notify("textDocument/didChange", {
      textDocument: { uri, version: nextVersion(uri) },
      contentChanges: [{ text: model.getValue() }],
    });
  };

  // Send the pending debounced change (if any) immediately and cancel the
  // timer. Monaco pull requests (semantic tokens, hover, completion, rename…)
  // can otherwise race ahead of the 300ms-debounced `didChange`, so the server
  // would answer against stale text and stale semantic tokens would linger
  // until the next edit. Providers call this before every request.
  const flush: FlushFn = (model) => {
    const uri = uriOf(model);
    const timer = changeTimers.get(uri);
    if (timer === undefined) return;
    clearTimeout(timer);
    changeTimers.delete(uri);
    didChange(model);
  };

  const attach = (model: monaco.editor.ITextModel): void => {
    if (model.getLanguageId() !== LANGUAGE_ID) return;
    didOpen(model);
    disposables.push(
      model.onDidChangeContent(() => {
        const uri = uriOf(model);
        const existing = changeTimers.get(uri);
        if (existing) clearTimeout(existing);
        changeTimers.set(
          uri,
          setTimeout(() => {
            changeTimers.delete(uri);
            didChange(model);
          }, 300),
        );
      }),
    );
  };

  // Push the base file map first so includes resolve immediately, then open
  // every existing model.
  client.notify("z33/workspaceFiles", workspaceFilesParams());
  for (const model of m.editor.getModels()) attach(model);

  const onCreate = m.editor.onDidCreateModel(attach);
  const onDispose = m.editor.onWillDisposeModel((model) => {
    if (model.getLanguageId() !== LANGUAGE_ID) return;
    const uri = uriOf(model);
    const timer = changeTimers.get(uri);
    if (timer) clearTimeout(timer);
    changeTimers.delete(uri);
    versions.delete(uri);
    client.notify("textDocument/didClose", { textDocument: { uri } });
  });

  // Re-push the base map whenever the *set* of files changes (create / delete /
  // rename); content edits of open documents flow through didChange.
  let knownKeys = sortedKeys(useFileStore.getState().files);
  const unsubscribe = useFileStore.subscribe((state) => {
    const keys = sortedKeys(state.files);
    if (keys === knownKeys) return;
    knownKeys = keys;
    client.notify("z33/workspaceFiles", workspaceFilesParams());
  });

  disposables.push(onCreate, onDispose, { dispose: unsubscribe });

  return flush;
}

/** Whether a token asked for cancellation; if so the provider should bail. */
const cancelled = (token: CancellationToken): boolean =>
  token.isCancellationRequested;

function registerProviders(
  m: Monaco,
  flush: FlushFn,
  disposables: Disposable[],
): void {
  const client = getLspClient();

  // The converters build URIs with their own bundled `vscode-uri`, which Monaco
  // does not recognize as its own `Uri` class (navigation/edits silently fail).
  // Re-parse every URI through the live Monaco instance.
  const monacoUri = (uri: unknown): monaco.Uri => m.Uri.parse(String(uri));

  disposables.push(
    m.languages.registerCompletionItemProvider(LANGUAGE_ID, {
      triggerCharacters: ["%", "."],
      async provideCompletionItems(model, position, _context, token) {
        flush(model);
        const result = await client.request<
          CompletionList | CompletionItem[] | null
        >("textDocument/completion", {
          textDocument: { uri: uriOf(model) },
          position: lspPosition(position),
        });
        if (cancelled(token)) return null;
        if (!result) return { suggestions: [] };
        const list: CompletionList = Array.isArray(result)
          ? { isIncomplete: false, items: result }
          : result;
        const word = model.getWordUntilPosition(position);
        const range = new m.Range(
          position.lineNumber,
          word.startColumn,
          position.lineNumber,
          word.endColumn,
        );
        return toCompletionList(list, { range });
      },
    }),
    m.languages.registerHoverProvider(LANGUAGE_ID, {
      async provideHover(model, position, token) {
        flush(model);
        const result = await client.request<Hover | null>(
          "textDocument/hover",
          {
            textDocument: { uri: uriOf(model) },
            position: lspPosition(position),
          },
        );
        if (cancelled(token)) return null;
        return result ? toHover(result) : null;
      },
    }),
    m.languages.registerDefinitionProvider(LANGUAGE_ID, {
      async provideDefinition(model, position, token) {
        flush(model);
        const result = await client.request<Definition | null>(
          "textDocument/definition",
          {
            textDocument: { uri: uriOf(model) },
            position: lspPosition(position),
          },
        );
        if (cancelled(token) || !result) return null;
        const def = toDefinition(result);
        const list = Array.isArray(def) ? def : [def];
        return list.map((loc) => ({ ...loc, uri: monacoUri(loc.uri) }));
      },
    }),
    m.languages.registerReferenceProvider(LANGUAGE_ID, {
      async provideReferences(model, position, context, token) {
        flush(model);
        const result = await client.request<Location[] | null>(
          "textDocument/references",
          {
            textDocument: { uri: uriOf(model) },
            position: lspPosition(position),
            context: { includeDeclaration: context.includeDeclaration },
          },
        );
        if (cancelled(token)) return null;
        return result
          ? result.map((location) => {
              const ml = toLocation(location);
              return { ...ml, uri: monacoUri(ml.uri) };
            })
          : [];
      },
    }),
    m.languages.registerDocumentHighlightProvider(LANGUAGE_ID, {
      async provideDocumentHighlights(model, position, token) {
        flush(model);
        const result = await client.request<DocumentHighlight[] | null>(
          "textDocument/documentHighlight",
          {
            textDocument: { uri: uriOf(model) },
            position: lspPosition(position),
          },
        );
        if (cancelled(token)) return null;
        return result ? result.map((h) => toDocumentHighlight(h)) : [];
      },
    }),
    m.languages.registerDocumentSymbolProvider(LANGUAGE_ID, {
      async provideDocumentSymbols(model, token) {
        flush(model);
        // The server returns hierarchical DocumentSymbols.
        const result = await client.request<DocumentSymbol[] | null>(
          "textDocument/documentSymbol",
          { textDocument: { uri: uriOf(model) } },
        );
        if (cancelled(token)) return null;
        return result ? result.map((s) => toDocumentSymbol(s)) : [];
      },
    }),
    m.languages.registerRenameProvider(LANGUAGE_ID, {
      async provideRenameEdits(model, position, newName, token) {
        flush(model);
        const result = await client.request<WorkspaceEdit | null>(
          "textDocument/rename",
          {
            textDocument: { uri: uriOf(model) },
            position: lspPosition(position),
            newName,
          },
        );
        if (cancelled(token)) return null;
        if (!result) return { edits: [] };
        const edit = toWorkspaceEdit(result);
        return {
          edits: edit.edits.map((e) =>
            "resource" in e ? { ...e, resource: monacoUri(e.resource) } : e,
          ),
        };
      },
      async resolveRenameLocation(model, position, token) {
        flush(model);
        const result = await client.request<
          LspRange | { range: LspRange; placeholder: string } | null
        >("textDocument/prepareRename", {
          textDocument: { uri: uriOf(model) },
          position: lspPosition(position),
        });
        if (cancelled(token)) return null;
        if (!result) {
          const empty = new m.Range(
            position.lineNumber,
            position.column,
            position.lineNumber,
            position.column,
          );
          return {
            range: empty,
            text: "",
            rejectReason: "You cannot rename this element.",
          };
        }
        if ("placeholder" in result) {
          return { range: toRange(result.range), text: result.placeholder };
        }
        const range = toRange(result);
        return { range, text: model.getValueInRange(range) };
      },
    }),
    m.languages.registerSignatureHelpProvider(LANGUAGE_ID, {
      signatureHelpTriggerCharacters: [","],
      async provideSignatureHelp(model, position, token) {
        flush(model);
        const result = await client.request<SignatureHelp | null>(
          "textDocument/signatureHelp",
          {
            textDocument: { uri: uriOf(model) },
            position: lspPosition(position),
          },
        );
        if (cancelled(token) || !result) return null;
        return { value: toSignatureHelp(result), dispose() {} };
      },
    }),
    m.languages.registerCodeLensProvider(LANGUAGE_ID, {
      async provideCodeLenses(model, token) {
        flush(model);
        const result = await client.request<CodeLens[] | null>(
          "textDocument/codeLens",
          { textDocument: { uri: uriOf(model) } },
        );
        if (cancelled(token)) return null;
        return {
          lenses: result ? result.map((lens) => toCodeLens(lens)) : [],
          dispose() {},
        };
      },
    }),
  );
}

const encoder = new TextEncoder();
const decoder = new TextDecoder();

/**
 * The server emits semantic-token *lengths* in UTF-8 bytes but token *offsets*
 * in UTF-16 code units. On lines with multi-byte characters (e.g. comments with
 * accents) that makes a token overrun the line, and Monaco rejects the whole
 * set. Rewrite each length from bytes to UTF-16 code units using the model text.
 */
function fixSemanticTokenLengths(
  model: monaco.editor.ITextModel,
  tokens: SemanticTokens,
): SemanticTokens {
  const data = Array.from(tokens.data);
  let line = 0;
  let char = 0;
  for (let i = 0; i < data.length; i += 5) {
    const deltaLine = data[i] ?? 0;
    const deltaStart = data[i + 1] ?? 0;
    line += deltaLine;
    char = deltaLine === 0 ? char + deltaStart : deltaStart;

    const byteLength = data[i + 2] ?? 0;
    const lineText = model.getLineContent(line + 1);
    const rest = lineText.slice(char);
    const restBytes = encoder.encode(rest);
    const utf16Length =
      byteLength >= restBytes.length
        ? rest.length
        : decoder.decode(restBytes.slice(0, byteLength)).length;
    data[i + 2] = utf16Length;
  }
  return tokens.resultId === undefined
    ? { data }
    : { resultId: tokens.resultId, data };
}

async function registerSemanticTokens(
  m: Monaco,
  flush: FlushFn,
  disposables: Disposable[],
): Promise<void> {
  const client = getLspClient();
  await client.ready();
  const legend = client.legend;
  if (!legend) return;

  disposables.push(
    m.languages.registerDocumentSemanticTokensProvider(LANGUAGE_ID, {
      getLegend() {
        return {
          tokenTypes: legend.tokenTypes,
          tokenModifiers: legend.tokenModifiers,
        };
      },
      async provideDocumentSemanticTokens(model, _lastResultId, token) {
        flush(model);
        const result = await client.request<SemanticTokens | null>(
          "textDocument/semanticTokens/full",
          { textDocument: { uri: uriOf(model) } },
        );
        if (cancelled(token) || !result) return null;
        return toSemanticTokens(fixSemanticTokenLengths(model, result));
      },
      releaseDocumentSemanticTokens() {},
    }),
  );
}

let initialized = false;

/** Register all Z33 language features against the given Monaco instance. */
export function setupLsp(m: Monaco): void {
  if (initialized) return;
  initialized = true;

  const disposables: Disposable[] = [];

  setupDiagnostics(m, disposables);
  const flush = setupDocumentSync(m, disposables);
  registerProviders(m, flush, disposables);
  void registerSemanticTokens(m, flush, disposables);

  // Dev HMR re-imports this module without a full reload; without cleanup we
  // would stack a fresh set of providers/listeners on top of the old ones.
  import.meta.hot?.dispose(() => {
    for (const d of disposables) d.dispose();
    initialized = false;
  });
}
