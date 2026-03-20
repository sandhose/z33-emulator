import { useMonaco } from "@monaco-editor/react";
import type * as monaco from "monaco-editor";
import { useCallback, useMemo, useRef, useState } from "react";
import { Group, Panel } from "react-resizable-panels";
import type { SourceMap } from "z33-web-bindings";
import type { ComputerInterface, Following, Labels } from "./computer";
import { RegisterPanel } from "./debug-sidebar";
import { DebugToolbar } from "./debug-toolbar";
import { EditToolbar } from "./edit-toolbar";
import { FileSidebar } from "./file-sidebar";
import { useCompilation } from "./hooks/use-compilation";
import { stripLeadingSlash } from "./lib/file-paths";
import { MemoryPanel } from "./memory-panel";
import { MultiFileEditor } from "./multi-file-editor";
import { ResizeHandle } from "./panel-resize-handle";
import { useAppStore } from "./stores/app-store";
import { useFileStore } from "./stores/file-store";
import { useSourceHighlight } from "./use-source-highlight";

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

  const [following, setFollowing] = useState<Following | null>("%sp");

  const handleDebugEditorMount = useCallback(
    (ed: monaco.editor.IStandaloneCodeEditor) => {
      setEditor(ed);
      onEditorMount(ed);
    },
    [onEditorMount],
  );

  const handleSourceSwitch = useCallback(
    (filePath: string) => {
      setActiveFile(stripLeadingSlash(filePath));
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
      names.add(stripLeadingSlash(loc.file));
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
          <div className="flex flex-col h-full border-l">
            <div className="shrink-0 border-b">
              <RegisterPanel
                computer={computer}
                labels={labels}
                following={following}
                onFollow={setFollowing}
              />
            </div>
            <div className="flex-1 min-h-0">
              <MemoryPanel
                computer={computer}
                labels={labels}
                following={following}
                onFollow={setFollowing}
              />
            </div>
          </div>
        </Panel>
      </Group>
    </div>
  );
};

export default App;
