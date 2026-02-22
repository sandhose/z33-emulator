import { useMonaco } from "@monaco-editor/react";
import type * as monaco from "monaco-editor";
import { Uri } from "monaco-editor/esm/vs/editor/editor.api.js";
import { useCallback, useEffect, useRef, useState } from "react";
import { Group, Panel } from "react-resizable-panels";
import type { Computer, Program, SourceMap } from "z33-web-bindings";
import { InMemoryPreprocessor } from "z33-web-bindings";
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "./components/ui/popover";
import type { Labels } from "./computer";
import { DebugSidebar } from "./debug-sidebar";
import { DebugToolbar } from "./debug-toolbar";
import { EntrypointSelector } from "./entrypoint-selector";
import { FileSidebar } from "./file-sidebar";
import {
  loadActiveFile,
  loadWorkspace,
  saveActiveFile,
  saveWorkspace,
} from "./file-store";
import { MemoryPanel } from "./memory-panel";
import { MultiFileEditor } from "./multi-file-editor";
import { ResizeHandle } from "./panel-resize-handle";
import { reportSchema, toMonacoDecoration } from "./report";
import { useSourceHighlight } from "./use-source-highlight";

const sampleFiles = import.meta.glob<string>("../../samples/*.S", {
  query: "?raw",
  import: "default",
  eager: true,
});

function getSampleFiles(): Map<string, string> {
  return new Map(
    Object.entries(sampleFiles).map(([path, content]) => [
      path.replace(/^.*[\\/]/, ""),
      content,
    ]),
  );
}

function getInitialFiles(): { files: Map<string, string>; selected: string } {
  const stored = loadWorkspace();
  if (stored && stored.size > 0) {
    const active = loadActiveFile() ?? stored.keys().next().value ?? "";
    return { files: stored, selected: active };
  }

  const files = getSampleFiles();
  saveWorkspace(files);
  return { files, selected: "fact.S" };
}

const { files: initialFiles, selected: initialSelected } = getInitialFiles();

type DebugSession = {
  computer: Computer;
  sourceMap: SourceMap;
  labels: Labels;
};

const App = () => {
  const monacoInstance = useMonaco();
  const [debugSession, setDebugSession] = useState<DebugSession | null>(null);
  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);
  const [compilePopoverOpen, setCompilePopoverOpen] = useState(false);
  const [compiledProgram, setCompiledProgram] = useState<{
    labels: string[];
    compile: (entrypoint: string) => Computer;
  } | null>(null);

  const [fileName, setFileName] = useState(Uri.file(initialSelected));
  const [fileNames, setFileNames] = useState<Uri[]>([]);

  const syncFileNames = useCallback(() => {
    if (!monacoInstance) return;
    setFileNames(monacoInstance.editor.getModels().map((m) => m.uri));
  }, [monacoInstance]);

  // Auto-save workspace on content changes
  const saveTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const persistWorkspace = useCallback(() => {
    if (!monacoInstance) return;
    const files = new Map<string, string>();
    for (const model of monacoInstance.editor.getModels()) {
      files.set(model.uri.path, model.getValue());
    }
    saveWorkspace(files);
  }, [monacoInstance]);

  useEffect(() => {
    if (!monacoInstance || debugSession) return;

    const disposables: { dispose(): void }[] = [];
    const debouncedSave = () => {
      if (saveTimerRef.current) clearTimeout(saveTimerRef.current);
      saveTimerRef.current = setTimeout(() => {
        persistWorkspace();
        saveTimerRef.current = null;
      }, 500);
    };

    for (const model of monacoInstance.editor.getModels()) {
      disposables.push(model.onDidChangeContent(debouncedSave));
    }
    const sub = monacoInstance.editor.onDidCreateModel((model) => {
      disposables.push(model.onDidChangeContent(debouncedSave));
    });
    disposables.push(sub);

    return () => {
      if (saveTimerRef.current) clearTimeout(saveTimerRef.current);
      for (const d of disposables) d.dispose();
    };
  }, [monacoInstance, debugSession, persistWorkspace]);

  const handleSwitchFile = useCallback((uri: Uri) => {
    setFileName(uri);
    saveActiveFile(uri.path);
  }, []);

  const compile = useCallback((): Program | null => {
    if (!monacoInstance) return null;

    const models = monacoInstance.editor.getModels();
    const files = new Map(
      models.map((model) => [model.uri.path, model.getValue()]),
    );

    const preprocessor = new InMemoryPreprocessor(files, fileName.path);
    const result = preprocessor.compile();
    const prog = result.program;
    const report = result.report;
    if ((prog && report) || (!prog && !report)) {
      throw Error("Invalid return value");
    }

    if (prog) {
      return prog;
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

    return null;
  }, [monacoInstance, fileName.path]);

  const handleCompileAndRun = useCallback(() => {
    const program = compile();
    if (program) {
      setCompiledProgram({
        labels: program.labels,
        compile: (entrypoint: string) => program.compile(entrypoint),
      });
      setCompilePopoverOpen(true);
    }
  }, [compile]);

  const handleRunEntrypoint = useCallback(
    (entrypoint: string) => {
      if (!compiledProgram) return;
      const computer = compiledProgram.compile(entrypoint);
      const sourceMap = computer.source_map;

      const labels: Labels = new Map();
      for (const [label, address] of computer.labels) {
        const values = labels.get(address) || [];
        labels.set(address, [...values, label]);
      }

      setDebugSession({ computer, sourceMap, labels });
      setCompilePopoverOpen(false);
      setCompiledProgram(null);
    },
    [compiledProgram],
  );

  const handleStopDebug = useCallback(() => {
    setDebugSession(null);
  }, []);

  const handleEditorMount = useCallback(
    (editor: monaco.editor.IStandaloneCodeEditor) => {
      editorRef.current = editor;
      syncFileNames();
    },
    [syncFileNames],
  );

  return (
    <main className="flex h-screen bg-background">
      <FileSidebar
        monaco={monacoInstance}
        fileName={fileName}
        fileNames={fileNames}
        onSwitchFile={handleSwitchFile}
        onCompileAndRun={handleCompileAndRun}
        isDebugging={debugSession !== null}
        onStopDebug={handleStopDebug}
        sampleFiles={getSampleFiles}
        onSync={syncFileNames}
      />

      <div className="flex-1 min-w-0">
        {debugSession ? (
          <DebugLayout
            debugSession={debugSession}
            initialFiles={initialFiles}
            fileName={fileName}
            onSwitchFile={handleSwitchFile}
            onEditorMount={handleEditorMount}
            onStopDebug={handleStopDebug}
          />
        ) : (
          <MultiFileEditor
            initialFiles={initialFiles}
            fileName={fileName}
            onEditorMount={handleEditorMount}
          />
        )}
      </div>

      <Popover
        open={compilePopoverOpen}
        onOpenChange={(open) => {
          if (!open) {
            setCompilePopoverOpen(false);
            setCompiledProgram(null);
          }
        }}
      >
        <PopoverTrigger
          render={<span className="fixed top-4 left-1/2 w-0 h-0" />}
        />
        <PopoverContent>
          {compiledProgram && (
            <EntrypointSelector
              entrypoints={compiledProgram.labels}
              onRun={handleRunEntrypoint}
            />
          )}
        </PopoverContent>
      </Popover>
    </main>
  );
};

type DebugLayoutProps = {
  debugSession: DebugSession;
  initialFiles: Map<string, string>;
  fileName: Uri;
  onSwitchFile: (uri: Uri) => void;
  onEditorMount: (editor: monaco.editor.IStandaloneCodeEditor) => void;
  onStopDebug: () => void;
};

const DebugLayout: React.FC<DebugLayoutProps> = ({
  debugSession,
  initialFiles,
  fileName,
  onSwitchFile,
  onEditorMount,
  onStopDebug,
}) => {
  const { computer, sourceMap, labels } = debugSession;
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
      const uri = Uri.file(filePath.replace(/^\//, ""));
      onSwitchFile(uri);
    },
    [onSwitchFile],
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
              <DebugToolbar computer={computer} />
              <div className="flex-1 min-h-0">
                <MultiFileEditor
                  initialFiles={initialFiles}
                  fileName={fileName}
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
        <DebugSidebar
          computer={computer}
          sourceMap={sourceMap}
          labels={labels}
          onClose={onStopDebug}
        />
      </Panel>
    </Group>
  );
};

export default App;
