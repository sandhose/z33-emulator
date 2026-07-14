import { useMonaco } from "@monaco-editor/react";
import type * as monaco from "monaco-editor";
import { useCallback, useEffect, useRef } from "react";
import { DebugLayout } from "./debug-layout";
import { EditToolbar } from "./edit-toolbar";
import { FileSidebar } from "./file-sidebar";
import { useCompilation } from "./hooks/use-compilation";
import { stripLeadingSlash } from "./lib/file-paths";
import { setRunCommandHandler } from "./lib/lsp-monaco";
import { getMonacoFiles } from "./lib/monaco-sync";
import { MultiFileEditor } from "./multi-file-editor";
import { useAppStore } from "./stores/app-store";
import { useFileStore } from "./stores/file-store";

const App = () => {
  const monacoInstance = useMonaco();
  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);

  const activeFile = useFileStore((s) => s.activeFile);
  const startDebug = useAppStore((s) => s.startDebug);
  const mode = useAppStore((s) => s.mode);
  const setEntrypoint = useFileStore((s) => s.setEntrypoint);
  const entrypoints = useFileStore((s) => s.entrypoints);

  const { compilationResult, compilationStatus } = useCompilation(
    activeFile,
    monacoInstance,
  );

  const runFile = useCallback(
    (file: string, entrypoint: string) => {
      setEntrypoint(file, entrypoint);
      // Use the freshest editor contents, keyed by file-store name (no leading
      // slash) to match the emulator worker / LSP URI conventions.
      const files = Object.fromEntries(
        Object.entries(getMonacoFiles()).map(([path, content]) => [
          stripLeadingSlash(path),
          content,
        ]),
      );
      void startDebug(files, file, entrypoint).catch((e: unknown) => {
        // The compile check should have caught errors before Run; this is a
        // safety net.
        console.error("Failed to start debug session:", e);
      });
    },
    [setEntrypoint, startDebug],
  );

  const handleRun = useCallback(
    (entrypoint: string) => {
      if (compilationResult.type !== "success") return;
      runFile(activeFile, entrypoint);
    },
    [compilationResult, activeFile, runFile],
  );

  // The LSP run code lens: start (or restart) a debug session with the lens's
  // file as the program and its label as the entrypoint. The lens path is the
  // server's workspace-relative path — the same no-leading-slash convention as
  // the file store.
  useEffect(() => {
    setRunCommandHandler(({ path, label }) => {
      const { mode, stopDebug } = useAppStore.getState();
      if (mode.type === "debug") stopDebug();
      runFile(path, label);
    });
    return () => setRunCommandHandler(null);
  }, [runFile]);

  const handleEditorMount = useCallback(
    (editor: monaco.editor.IStandaloneCodeEditor) => {
      editorRef.current = editor;
    },
    [],
  );

  const isDebugging = mode.type === "debug";

  return (
    <div className="flex flex-col h-screen bg-background">
      {!isDebugging && (
        <header>
          <EditToolbar
            onRun={handleRun}
            compilationStatus={compilationStatus}
            compilationError={
              compilationResult.type === "error"
                ? compilationResult.message
                : undefined
            }
            labels={
              compilationResult.type === "idle" ? [] : compilationResult.labels
            }
            defaultEntrypoint={entrypoints[activeFile]}
          />
        </header>
      )}
      <div className="flex flex-1 min-h-0">
        <FileSidebar />
        <main
          className="flex-1 min-w-0 flex flex-col min-h-0"
          aria-label="Program editor"
        >
          {isDebugging ? (
            <DebugLayout onEditorMount={handleEditorMount} />
          ) : (
            <div className="flex-1 min-h-0">
              <MultiFileEditor
                filePath={activeFile}
                onEditorMount={handleEditorMount}
              />
            </div>
          )}
        </main>
      </div>
    </div>
  );
};

export default App;
