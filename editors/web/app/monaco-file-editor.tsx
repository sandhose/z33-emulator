// The Monaco-backed editor. Monaco is reached from this module and from the
// `./monaco` setup it imports, and nowhere else, so all of it lands in the
// chunk `./multi-file-editor` loads on demand.
import { Editor } from "@monaco-editor/react";
import type * as monaco from "monaco-editor";
import { useEffect, useRef } from "react";
import {
  breakpointDecorations,
  requestedLineForClick,
} from "./lib/breakpoint-gutter";
import { toMonacoPath } from "./lib/file-paths";
import { initMonacoSync } from "./lib/monaco-sync";
import { monacoApi } from "./monaco";
import { useBreakpointStore } from "./stores/breakpoint-store";
import { useFileStore } from "./stores/file-store";
import { useThemeStore } from "./stores/theme-store";

export type EditorProps = {
  filePath: string;
  readOnly?: boolean;
  onEditorMount?: (editor: monaco.editor.IStandaloneCodeEditor) => void;
};

const MonacoFileEditor: React.FC<EditorProps> = ({
  filePath,
  readOnly = false,
  onEditorMount,
}: EditorProps) => {
  const effective = useThemeStore((s) => s.effective);

  // Breakpoint state for the currently shown file.
  const breakpoints = useBreakpointStore((s) => s.breakpoints);
  const resolved = useBreakpointStore((s) => s.resolved);
  const toggle = useBreakpointStore((s) => s.toggle);

  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);
  const decorationsRef =
    useRef<monaco.editor.IEditorDecorationsCollection | null>(null);
  const filePathRef = useRef(filePath);
  useEffect(() => {
    filePathRef.current = filePath;
  }, [filePath]);

  // Mirror the file store into Monaco's models and every edit back out. This
  // lives with the editor because the models are what it needs: the compile
  // check reads the store, and the language server takes its workspace map
  // from the store and each document's content from Monaco's didChange.
  useEffect(
    () =>
      initMonacoSync(monacoApi, {
        onEdit: (name, content) => {
          useFileStore.getState().onMonacoEdit(name, content);
        },
        getFiles: () => useFileStore.getState().files,
        subscribe: (listener) =>
          useFileStore.subscribe((state, prev) => {
            listener(state.files, prev.files);
          }),
      }),
    [],
  );

  const lines = breakpoints[filePath];
  const resolvedForFile = resolved[filePath];

  // (Re)apply breakpoint glyphs. Re-runs on file switch and breakpoint changes;
  // the decorations collection re-targets the active model.
  useEffect(() => {
    const editor = editorRef.current;
    const decorations = decorationsRef.current;
    if (!editor || !decorations) return;
    decorations.set(breakpointDecorations(lines, resolvedForFile));
  }, [lines, resolvedForFile, filePath]);

  return (
    <Editor
      className="editor h-full"
      theme={effective === "dark" ? "vs-dark" : "light"}
      path={toMonacoPath(filePath)}
      keepCurrentModel
      onMount={(editor, monacoInstance) => {
        editorRef.current = editor;
        decorationsRef.current = editor.createDecorationsCollection();

        const glyphMargin =
          monacoInstance.editor.MouseTargetType.GUTTER_GLYPH_MARGIN;
        editor.onMouseDown((e) => {
          // Left click only: middle/right clicks in the gutter must not toggle.
          if (!e.event.leftButton) return;
          if (e.target.type !== glyphMargin || !e.target.position) return;
          const file = filePathRef.current;
          const fileResolved = useBreakpointStore.getState().resolved[file];
          toggle(
            file,
            requestedLineForClick(e.target.position.lineNumber, fileResolved),
          );
        });
        // Glyphs for the model currently shown: needed once at mount, because
        // the effect above ran before the refs existed, and again after every
        // model (file) switch.
        const applyGlyphs = () => {
          const decorations = decorationsRef.current;
          if (!decorations) return;
          const { breakpoints: bp, resolved: res } =
            useBreakpointStore.getState();
          decorations.set(
            breakpointDecorations(
              bp[filePathRef.current],
              res[filePathRef.current],
            ),
          );
        };
        applyGlyphs();
        editor.onDidChangeModel(applyGlyphs);

        onEditorMount?.(editor);
      }}
      options={{
        readOnly,
        glyphMargin: true,
        "semanticHighlighting.enabled": true,
      }}
    />
  );
};

export default MonacoFileEditor;
