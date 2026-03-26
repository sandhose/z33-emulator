import type { Monaco } from "@monaco-editor/react";
import { useDebouncer } from "@tanstack/react-pacer";
import type { editor as MonacoEditor } from "monaco-editor";
import { useCallback, useEffect, useRef, useState } from "react";
import type { Computer } from "z33-web-bindings";
import { InMemoryPreprocessor } from "z33-web-bindings";
import { toMonacoPath } from "../lib/file-paths";
import { getMonacoFiles, initMonacoSync } from "../lib/monaco-sync";
import { reportSchema, toMonacoDiagnostics } from "../report";
import { useFileStore } from "../stores/file-store";

type CompilationResult =
  | { type: "idle" }
  | {
      type: "success";
      labels: string[];
      compileFn: (ep: string) => Computer;
    }
  | { type: "error"; message: string };

type UICompilationStatus = "idle" | "pending" | "success" | "error";

/**
 * Apply parsed diagnostics (markers + line decorations) to all Monaco models.
 * Returns the error message from the report.
 */
function applyReportDiagnostics(
  monaco: Monaco,
  reportJson: string,
  decorationIds: Map<string, string[]>,
): string | null {
  try {
    const parsed = JSON.parse(reportJson);
    // Support both a single report object and an array of reports
    const reports: unknown[] = Array.isArray(parsed) ? parsed : [parsed];
    let firstMessage: string | null = null;

    for (const raw of reports) {
      const reportObject = reportSchema.parse(raw);
      if (!firstMessage) firstMessage = reportObject.message;
      for (const model of monaco.editor.getModels()) {
        const { markers, decorations } = toMonacoDiagnostics(
          model,
          reportObject,
        );
        monaco.editor.setModelMarkers(model, "z33", [
          ...monaco.editor.getModelMarkers({ resource: model.uri }),
          ...markers,
        ]);
        if (decorations.length > 0) {
          const existing = decorationIds.get(model.uri.toString()) ?? [];
          const ids = model.deltaDecorations([], decorations);
          decorationIds.set(model.uri.toString(), [...existing, ...ids]);
        }
      }
    }

    return firstMessage;
  } catch (e) {
    console.warn(e);
    return null;
  }
}

export function useCompilation(activeFile: string, monacoInstance: Monaco) {
  const [compilationResult, setCompilationResult] = useState<CompilationResult>(
    { type: "idle" },
  );

  // Decoration IDs for line highlights (squiggly markers are tracked by Monaco itself)
  const decorationIds = useRef(new Map<string, string[]>());

  // Initialize Monaco sync once after Monaco loads
  useEffect(() => {
    if (!monacoInstance) return;
    return initMonacoSync(monacoInstance, {
      onEdit: (name, content) => {
        useFileStore.getState()._onMonacoEdit(name, content);
      },
      getFiles: () => useFileStore.getState().files,
      subscribe: (listener) =>
        useFileStore.subscribe((state, prev) => {
          listener(state.files, prev.files);
        }),
    });
  }, [monacoInstance]);

  const performCompile = useCallback(() => {
    if (!monacoInstance) return;
    const files = new Map(Object.entries(getMonacoFiles()));
    const preprocessor = new InMemoryPreprocessor(
      files,
      toMonacoPath(activeFile),
    );
    const result = preprocessor.compile();
    const { program: prog, report } = result;

    // Clear all z33 markers and line-highlight decorations from every model
    for (const model of monacoInstance.editor.getModels()) {
      monacoInstance.editor.setModelMarkers(model, "z33", []);
    }
    for (const [uriStr, ids] of decorationIds.current) {
      monacoInstance.editor
        .getModel(monacoInstance.Uri.parse(uriStr))
        ?.deltaDecorations(ids, []);
    }
    decorationIds.current.clear();

    if (prog) {
      // Check for fill/layout errors (unresolved labels, etc.)
      const checkReport = prog.check();
      if (checkReport !== undefined) {
        const message =
          applyReportDiagnostics(
            monacoInstance,
            checkReport,
            decorationIds.current,
          ) ?? "Compilation failed";
        setCompilationResult({ type: "error", message });
        return;
      }

      setCompilationResult({
        type: "success",
        labels: prog.labels,
        compileFn: (ep) => prog.compile(ep),
      });
    } else if (report) {
      const message =
        applyReportDiagnostics(monacoInstance, report, decorationIds.current) ??
        "Failed to parse";
      setCompilationResult({ type: "error", message });
    }
  }, [monacoInstance, activeFile]);

  const compileDebouncer = useDebouncer(
    performCompile,
    { wait: 300 },
    (state) => ({ isPending: state.isPending }),
  );

  // Attach Monaco content-change listeners and trigger initial compile
  useEffect(() => {
    if (!monacoInstance) return;

    compileDebouncer.maybeExecute();

    type Disposable = { dispose(): void };
    const disposables: Disposable[] = [];

    for (const model of monacoInstance.editor.getModels()) {
      disposables.push(
        model.onDidChangeContent(() => {
          compileDebouncer.maybeExecute();
        }),
      );
    }

    disposables.push(
      monacoInstance.editor.onDidCreateModel(
        (model: MonacoEditor.ITextModel) => {
          disposables.push(
            model.onDidChangeContent(() => {
              compileDebouncer.maybeExecute();
            }),
          );
        },
      ),
    );

    return () => {
      for (const d of disposables) d.dispose();
    };
  }, [monacoInstance, compileDebouncer]);

  // Re-trigger on activeFile change (new preprocessor entrypoint)
  useEffect(() => {
    compileDebouncer.maybeExecute();
    compileDebouncer.flush();
  }, [activeFile, compileDebouncer]);

  const compilationStatus: UICompilationStatus = compileDebouncer.state
    .isPending
    ? "pending"
    : compilationResult.type;

  return { compilationResult, compilationStatus };
}
