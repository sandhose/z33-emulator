import type * as monaco from "monaco-editor";
import { useCallback, useMemo, useState } from "react";
import type { SourceMap } from "z33-web-bindings";
import type { ComputerInterface, Following, Labels } from "./computer-types";
import { RegisterPanel } from "./debug-sidebar";
import { DebugToolbar } from "./debug-toolbar";
import { useSourceHighlight } from "./hooks/use-source-highlight";
import { stripLeadingSlash } from "./lib/file-paths";
import { MemoryPanel } from "./memory-panel";
import { MultiFileEditor } from "./multi-file-editor";
import { ResizeHandle } from "./panel-resize-handle";
import { useAppStore } from "./stores/app-store";
import { useFileStore } from "./stores/file-store";
import { Group, Panel } from "react-resizable-panels";

type DebugLayoutProps = {
  onEditorMount: (editor: monaco.editor.IStandaloneCodeEditor) => void;
};

/** Outer shell: reads mode from store and passes concrete values to DebugLayoutInner */
export const DebugLayout: React.FC<DebugLayoutProps> = ({ onEditorMount }) => {
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
