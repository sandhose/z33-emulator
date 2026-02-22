import { useMonaco } from "@monaco-editor/react";
import { useDebouncer } from "@tanstack/react-pacer";
import type * as monaco from "monaco-editor";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { Group, Panel } from "react-resizable-panels";
import type { Computer, SourceMap } from "z33-web-bindings";
import { InMemoryPreprocessor } from "z33-web-bindings";
import type { ComputerInterface, Labels } from "./computer";
import { RegisterPanel } from "./debug-sidebar";
import { DebugToolbar } from "./debug-toolbar";
import { EditToolbar } from "./edit-toolbar";
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

  const touchedFiles = useMemo(() => {
    const names = new Set<string>();
    for (const loc of sourceMap.values()) {
      names.add(loc.file.replace(/^\//, ""));
    }
    return [...names];
  }, [sourceMap]);

  return (
    <div className="flex flex-col h-full">
      <DebugToolbar
        touchedFiles={touchedFiles}
        activeFile={activeFile}
        onFileChange={setActiveFile}
        onStop={onStopDebug}
      />
      <Group orientation="horizontal" id="z33-h" className="flex-1 min-h-0">
        <Panel defaultSize="65%" minSize="40%" id="z33-editor">
          <MultiFileEditor
            filePath={activeFile}
            readOnly
            onEditorMount={handleDebugEditorMount}
          />
        </Panel>
        <ResizeHandle />
        <Panel defaultSize="35%" minSize="20%" maxSize="50%" id="z33-right">
          <Group orientation="vertical" id="z33-right-v">
            <Panel defaultSize="30%" minSize="15%" id="z33-registers">
              <RegisterPanel computer={computer} labels={labels} />
            </Panel>
            <ResizeHandle direction="vertical" />
            <Panel defaultSize="70%" minSize="30%" id="z33-memory">
              <MemoryPanel computer={computer} labels={labels} />
            </Panel>
          </Group>
        </Panel>
      </Group>
    </div>
  );
};

export default App;
