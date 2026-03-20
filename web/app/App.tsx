import { useMonaco } from "@monaco-editor/react";
import type * as monaco from "monaco-editor";
import { useCallback, useRef } from "react";
import { DebugLayout } from "./debug-layout";
import { EditToolbar } from "./edit-toolbar";
import { FileSidebar } from "./file-sidebar";
import { useCompilation } from "./hooks/use-compilation";
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

  const handleRun = useCallback(
    (entrypoint: string) => {
      if (compilationResult.type !== "success") return;
      try {
        setEntrypoint(activeFile, entrypoint);
        startDebug(compilationResult.compileFn, entrypoint);
      } catch (e) {
        // check() should have caught errors before Run; this is a safety net
        console.error("Compilation failed at run time:", e);
      }
    },
    [compilationResult, activeFile, setEntrypoint, startDebug],
  );

  const handleEditorMount = useCallback(
    (editor: monaco.editor.IStandaloneCodeEditor) => {
      editorRef.current = editor;
    },
    [],
  );

  const isDebugging = mode.type === "debug";

  return (
    <main className="flex flex-col h-screen bg-background">
      {!isDebugging && (
        <EditToolbar
          onRun={handleRun}
          compilationStatus={compilationStatus}
          compilationError={
            compilationResult.type === "error"
              ? compilationResult.message
              : undefined
          }
          labels={
            compilationResult.type === "success" ? compilationResult.labels : []
          }
          defaultEntrypoint={entrypoints[activeFile]}
        />
      )}
      <div className="flex flex-1 min-h-0">
        <FileSidebar />
        <div className="flex-1 min-w-0 flex flex-col min-h-0">
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
        </div>
      </div>
    </main>
  );
};

export default App;
