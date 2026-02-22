import { useMonaco } from "@monaco-editor/react";
import { useDebouncer } from "@tanstack/react-pacer";
import { CheckCircle2Icon, XCircleIcon } from "lucide-react";
import type * as monaco from "monaco-editor";
import { useCallback, useEffect, useRef, useState } from "react";
import { Group, Panel } from "react-resizable-panels";
import type { Computer, SourceMap } from "z33-web-bindings";
import { InMemoryPreprocessor } from "z33-web-bindings";
import type { ComputerInterface, Labels } from "./computer";
import { DebugSidebar } from "./debug-sidebar";
import { DebugToolbar } from "./debug-toolbar";
import { FileSidebar } from "./file-sidebar";
import { getMonacoFiles, initMonacoSync } from "./lib/monaco-sync";
import { MemoryPanel } from "./memory-panel";
import { MultiFileEditor } from "./multi-file-editor";
import { ResizeHandle } from "./panel-resize-handle";
import { reportSchema, toMonacoDiagnostics } from "./report";
import { useAppStore } from "./stores/app-store";
import { useFileStore } from "./stores/file-store";
import { useSourceHighlight } from "./use-source-highlight";

type CompilationResult =
  | { type: "idle" }
  | {
      type: "success";
      labels: string[];
      compileFn: (ep: string) => Computer;
    }
  | { type: "error"; message: string };

type UICompilationStatus = "idle" | "pending" | "success" | "error";

const App = () => {
  const monacoInstance = useMonaco();
  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);

  const activeFile = useFileStore((s) => s.activeFile);
  const startDebug = useAppStore((s) => s.startDebug);
  const mode = useAppStore((s) => s.mode);
  const setEntrypoint = useFileStore((s) => s.setEntrypoint);
  const entrypoints = useFileStore((s) => s.entrypoints);

  const [compilationResult, setCompilationResult] = useState<CompilationResult>(
    { type: "idle" },
  );

  // Decoration IDs for line highlights (squiggly markers are tracked by Monaco itself)
  const decorationIds = useRef(new Map<string, string[]>());

  // Initialize Monaco sync once after Monaco loads
  useEffect(() => {
    if (!monacoInstance) return;
    return initMonacoSync(monacoInstance);
  }, [monacoInstance]);

  const performCompile = useCallback(() => {
    if (!monacoInstance) return;
    const files = new Map(Object.entries(getMonacoFiles()));
    const preprocessor = new InMemoryPreprocessor(files, `/${activeFile}`);
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
        let errorMessage = "Compilation failed";
        try {
          const reportObject = reportSchema.parse(JSON.parse(checkReport));
          errorMessage = reportObject.message;
          for (const model of monacoInstance.editor.getModels()) {
            const { markers, decorations } = toMonacoDiagnostics(
              model,
              reportObject,
            );
            monacoInstance.editor.setModelMarkers(model, "z33", markers);
            if (decorations.length > 0) {
              const ids = model.deltaDecorations([], decorations);
              decorationIds.current.set(model.uri.toString(), ids);
            }
          }
        } catch (e) {
          console.warn(e);
        }
        setCompilationResult({ type: "error", message: errorMessage });
        return;
      }

      setCompilationResult({
        type: "success",
        labels: prog.labels,
        compileFn: (ep) => prog.compile(ep),
      });
    } else if (report) {
      let errorMessage = "Failed to parse";
      try {
        const reportObject = reportSchema.parse(JSON.parse(report));
        errorMessage = reportObject.message;
        for (const model of monacoInstance.editor.getModels()) {
          const { markers, decorations } = toMonacoDiagnostics(
            model,
            reportObject,
          );
          monacoInstance.editor.setModelMarkers(model, "z33", markers);
          if (decorations.length > 0) {
            const ids = model.deltaDecorations([], decorations);
            decorationIds.current.set(model.uri.toString(), ids);
          }
        }
      } catch (e) {
        console.warn(e);
      }
      setCompilationResult({ type: "error", message: errorMessage });
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
      monacoInstance.editor.onDidCreateModel((model) => {
        disposables.push(
          model.onDidChangeContent(() => {
            compileDebouncer.maybeExecute();
          }),
        );
      }),
    );

    return () => {
      for (const d of disposables) d.dispose();
    };
  }, [monacoInstance, compileDebouncer.maybeExecute]);

  // Re-trigger on activeFile change (new preprocessor entrypoint)
  // biome-ignore lint/correctness/useExhaustiveDependencies: activeFile is an intentional trigger dep
  useEffect(() => {
    compileDebouncer.maybeExecute();
    compileDebouncer.flush();
  }, [activeFile, compileDebouncer.maybeExecute, compileDebouncer.flush]);

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

  const compilationStatus: UICompilationStatus = compileDebouncer.state
    .isPending
    ? "pending"
    : compilationResult.type;

  const isDebugging = mode.type === "debug";

  return (
    <main className="flex h-screen bg-background">
      <FileSidebar
        onRun={handleRun}
        compilationStatus={compilationStatus}
        labels={
          compilationResult.type === "success" ? compilationResult.labels : []
        }
        defaultEntrypoint={entrypoints[activeFile]}
      />

      <div className="flex-1 min-w-0 flex flex-col">
        {isDebugging ? (
          <DebugLayout onEditorMount={handleEditorMount} />
        ) : (
          <>
            <div className="flex-1 min-h-0">
              <MultiFileEditor
                filePath={activeFile}
                onEditorMount={handleEditorMount}
              />
            </div>
            {compilationStatus !== "idle" &&
              compilationStatus !== "pending" && (
                <div className="px-6 py-4 border-t border-border text-xs flex items-center gap-2 shrink-0">
                  {compilationStatus === "error" ? (
                    <>
                      <XCircleIcon className="shrink-0 size-3.5 text-destructive" />
                      <span className="text-destructive">
                        {compilationResult.type === "error"
                          ? compilationResult.message
                          : "Compilation failed"}
                      </span>
                    </>
                  ) : (
                    <>
                      <CheckCircle2Icon className="shrink-0 size-3.5 text-green-600 dark:text-green-400" />
                      <span className="text-green-600 dark:text-green-400">
                        No errors
                      </span>
                    </>
                  )}
                </div>
              )}
          </>
        )}
      </div>
    </main>
  );
};

type DebugLayoutProps = {
  onEditorMount: (editor: monaco.editor.IStandaloneCodeEditor) => void;
};

/** Outer shell: reads mode from store and passes concrete values to DebugLayoutInner */
const DebugLayout: React.FC<DebugLayoutProps> = ({ onEditorMount }) => {
  const mode = useAppStore((s) => s.mode);
  const stopDebug = useAppStore((s) => s.stopDebug);

  if (mode.type !== "debug") return null;

  return (
    <DebugLayoutInner
      onEditorMount={onEditorMount}
      computer={mode.computer}
      sourceMap={mode.sourceMap}
      labels={mode.labels}
      onStopDebug={stopDebug}
    />
  );
};

type DebugLayoutInnerProps = {
  onEditorMount: (editor: monaco.editor.IStandaloneCodeEditor) => void;
  computer: ComputerInterface;
  sourceMap: SourceMap;
  labels: Labels;
  onStopDebug: () => void;
};

/** Inner component: all hooks called unconditionally, no early returns */
const DebugLayoutInner: React.FC<DebugLayoutInnerProps> = ({
  onEditorMount,
  computer,
  sourceMap,
  labels,
  onStopDebug,
}) => {
  const activeFile = useFileStore((s) => s.activeFile);
  const setActiveFile = useFileStore((s) => s.setActiveFile);

  const [editor, setEditor] =
    useState<monaco.editor.IStandaloneCodeEditor | null>(null);

  const handleDebugEditorMount = useCallback(
    (ed: monaco.editor.IStandaloneCodeEditor) => {
      setEditor(ed);
      onEditorMount(ed);
    },
    [onEditorMount],
  );

  const handleSourceSwitch = useCallback(
    (filePath: string) => {
      setActiveFile(filePath.replace(/^\//, ""));
    },
    [setActiveFile],
  );

  useSourceHighlight({
    computer,
    sourceMap,
    editor,
    onSwitchFile: handleSourceSwitch,
  });

  return (
    <Group orientation="horizontal" id="z33-h">
      <Panel defaultSize="70%" minSize="40%" id="z33-main">
        <Group orientation="vertical" id="z33-v">
          <Panel defaultSize="70%" minSize="30%" id="z33-editor">
            <div className="flex flex-col h-full">
              <DebugToolbar />
              <div className="flex-1 min-h-0">
                <MultiFileEditor
                  filePath={activeFile}
                  readOnly
                  onEditorMount={handleDebugEditorMount}
                />
              </div>
            </div>
          </Panel>
          <ResizeHandle direction="vertical" />
          <Panel defaultSize="30%" minSize="10%" collapsible id="z33-memory">
            <MemoryPanel computer={computer} labels={labels} />
          </Panel>
        </Group>
      </Panel>
      <ResizeHandle />
      <Panel defaultSize="30%" minSize="15%" maxSize="50%" id="z33-debug">
        <DebugSidebar onClose={onStopDebug} />
      </Panel>
    </Group>
  );
};

export default App;
