import { useMonaco } from "@monaco-editor/react";
import type * as monaco from "monaco-editor";
import { useCallback, useEffect, useRef, useState } from "react";
import { Group, Panel } from "react-resizable-panels";
import type { SourceMap } from "z33-web-bindings";
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

const App = () => {
  const monacoInstance = useMonaco();
  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);

  const activeFile = useFileStore((s) => s.activeFile);
  const startCompile = useAppStore((s) => s.startCompile);
  const confirmEntrypoint = useAppStore((s) => s.confirmEntrypoint);
  const stopDebug = useAppStore((s) => s.stopDebug);
  const mode = useAppStore((s) => s.mode);

  // Initialize Monaco sync once after Monaco loads
  useEffect(() => {
    if (!monacoInstance) return;
    return initMonacoSync(monacoInstance);
  }, [monacoInstance]);

  const handleCompileAndRun = useCallback(() => {
    if (!monacoInstance) return;

    const filesRecord = getMonacoFiles();
    const files = new Map(Object.entries(filesRecord));
    const entrypoint = `/${activeFile}`;

    const preprocessor = new InMemoryPreprocessor(files, entrypoint);
    const result = preprocessor.compile();
    const prog = result.program;
    const report = result.report;

    if ((prog && report) || (!prog && !report)) {
      throw Error("Invalid return value");
    }

    if (prog) {
      startCompile(prog.labels, (ep: string) => prog.compile(ep));
    }

    if (report) {
      try {
        const reportObject = reportSchema.parse(JSON.parse(report));
        for (const model of monacoInstance.editor.getModels()) {
          const decorations = toMonacoDecoration(model, reportObject);
          model.deltaDecorations([], decorations);
        }
      } catch (e: unknown) {
        console.log(String(e));
      }
    }
  }, [monacoInstance, activeFile, startCompile]);

  const handleEditorMount = useCallback(
    (editor: monaco.editor.IStandaloneCodeEditor) => {
      editorRef.current = editor;
    },
    [],
  );

  const isDebugging = mode.type === "debug";

  return (
    <main className="flex h-screen bg-background">
      <FileSidebar onCompileAndRun={handleCompileAndRun} />

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
              onRun={confirmEntrypoint}
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
