import type { Monaco } from "@monaco-editor/react";
import { useDebouncer } from "@tanstack/react-pacer";
import type { editor as MonacoEditor } from "monaco-editor";
import { useCallback, useEffect, useState } from "react";
import { InMemoryPreprocessor } from "../lib/wasm";
import { toMonacoPath } from "../lib/file-paths";
import { getMonacoFiles, initMonacoSync } from "../lib/monaco-sync";
import { useFileStore } from "../stores/file-store";

type CompilationResult =
  | { type: "idle" }
  | { type: "success"; labels: string[] }
  | { type: "error"; message: string; labels: string[] };

type UICompilationStatus = "idle" | "pending" | "success" | "error";

/**
 * Compiles the active file to drive the edit-mode toolbar: the entrypoint
 * selector (labels) and the Run button (success/error status).
 *
 * Diagnostics (squiggles, markers) are owned by the LSP, not this hook. The
 * actual runnable program is (re)built in the emulator worker on Run; here we
 * only surface whether it *can* be built and its labels.
 */
export function useCompilation(activeFile: string, monacoInstance: Monaco) {
  const [compilationResult, setCompilationResult] = useState<CompilationResult>(
    { type: "idle" },
  );

  // Keep the Zustand file store and Monaco models in sync.
  useEffect(() => {
    if (!monacoInstance) return;
    initMonacoSync(monacoInstance, {
      onEdit: (name, content) => {
        useFileStore.getState().onMonacoEdit(name, content);
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
    const { program } = preprocessor.compile();

    if (!program) {
      setCompilationResult({
        type: "error",
        message: "Failed to preprocess program",
        labels: [],
      });
      return;
    }

    const labels = program.labels;
    const checkReport = program.check();
    if (checkReport === undefined) {
      setCompilationResult({ type: "success", labels });
    } else {
      setCompilationResult({
        type: "error",
        message: "Compilation failed",
        labels,
      });
    }
  }, [monacoInstance, activeFile]);

  const compileDebouncer = useDebouncer(
    performCompile,
    { wait: 300 },
    (state) => ({ isPending: state.isPending }),
  );

  // Attach Monaco content-change listeners and trigger the initial compile.
  useEffect(() => {
    if (!monacoInstance) return () => {};

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

  // Re-trigger on activeFile change (new preprocessor entrypoint).
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
