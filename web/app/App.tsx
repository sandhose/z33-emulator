import { useMonaco } from "@monaco-editor/react";
import { useDebouncer } from "@tanstack/react-pacer";
import type * as monaco from "monaco-editor";
import { useCallback, useEffect, useRef, useState } from "react";
import { Group, Panel } from "react-resizable-panels";
import type { Computer, SourceMap } from "z33-web-bindings";
import { InMemoryPreprocessor } from "z33-web-bindings";
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "./components/ui/popover";
import type { ComputerInterface, Labels } from "./computer";
import { DebugSidebar } from "./debug-sidebar";
import { DebugToolbar } from "./debug-toolbar";
import { EntrypointSelector } from "./entrypoint-selector";
import { FileSidebar } from "./file-sidebar";
import { getMonacoFiles, initMonacoSync } from "./lib/monaco-sync";
import { MemoryPanel } from "./memory-panel";
import { MultiFileEditor } from "./multi-file-editor";
import { ResizeHandle } from "./panel-resize-handle";
import { reportSchema, toMonacoDecoration } from "./report";
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
  | { type: "error" };

type UICompilationStatus = "idle" | "pending" | "success" | "error";

const App = () => {
  const monacoInstance = useMonaco();
  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);

  const activeFile = useFileStore((s) => s.activeFile);
  const startCompile = useAppStore((s) => s.startCompile);
  const confirmEntrypoint = useAppStore((s) => s.confirmEntrypoint);
  const startDebug = useAppStore((s) => s.startDebug);
  const stopDebug = useAppStore((s) => s.stopDebug);
  const mode = useAppStore((s) => s.mode);
  const setEntrypoint = useFileStore((s) => s.setEntrypoint);
  const entrypoints = useFileStore((s) => s.entrypoints);

  const [compilationResult, setCompilationResult] = useState<CompilationResult>(
    { type: "idle" },
  );

  // Tracks Monaco decoration IDs per model URI string for proper cleanup
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

    // Clear old decorations from all models
    for (const [uriStr, ids] of decorationIds.current) {
      monacoInstance.editor
        .getModel(monacoInstance.Uri.parse(uriStr))
        ?.deltaDecorations(ids, []);
    }
    decorationIds.current.clear();

    if (prog) {
      setCompilationResult({
        type: "success",
        labels: prog.labels,
        compileFn: (ep) => prog.compile(ep),
      });
    } else if (report) {
      try {
        const reportObject = reportSchema.parse(JSON.parse(report));
        for (const model of monacoInstance.editor.getModels()) {
          const decorations = toMonacoDecoration(model, reportObject);
          if (decorations.length > 0) {
            const newIds = model.deltaDecorations([], decorations);
            decorationIds.current.set(model.uri.toString(), newIds);
          }
        }
      } catch (e) {
        console.warn(e);
      }
      setCompilationResult({ type: "error" });
    }
  }, [monacoInstance, activeFile]);

  const compileDebouncer = useDebouncer(
    performCompile,
    { wait: 600 },
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
  }, [activeFile, compileDebouncer.maybeExecute]);

  const handleRun = useCallback(() => {
    if (compilationResult.type !== "success") return;
    const { labels, compileFn } = compilationResult;
    const rememberedEp = entrypoints[activeFile];
    if (rememberedEp && labels.includes(rememberedEp)) {
      startDebug(compileFn, rememberedEp);
    } else {
      startCompile(labels, compileFn);
    }
  }, [compilationResult, entrypoints, activeFile, startDebug, startCompile]);

  const handleConfirmEntrypoint = useCallback(
    (ep: string) => {
      setEntrypoint(activeFile, ep);
      confirmEntrypoint(ep);
    },
    [activeFile, setEntrypoint, confirmEntrypoint],
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
      <FileSidebar onRun={handleRun} compilationStatus={compilationStatus} />

      <div className="flex-1 min-w-0">
        {isDebugging ? (
          <DebugLayout onEditorMount={handleEditorMount} />
        ) : (
          <MultiFileEditor
            filePath={activeFile}
            onEditorMount={handleEditorMount}
          />
        )}
      </div>

      <Popover
        open={mode.type === "pending-entrypoint"}
        onOpenChange={(open) => {
          if (!open) stopDebug();
        }}
      >
        <PopoverTrigger
          render={<span className="fixed top-4 left-1/2 w-0 h-0" />}
        />
        <PopoverContent>
          {mode.type === "pending-entrypoint" && (
            <EntrypointSelector
              entrypoints={mode.labels}
              onRun={handleConfirmEntrypoint}
              {...(entrypoints[activeFile] !== undefined && {
                defaultEntrypoint: entrypoints[activeFile],
              })}
            />
          )}
        </PopoverContent>
      </Popover>
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
