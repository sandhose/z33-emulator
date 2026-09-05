// The Monaco-backed editor. Monaco is reached from this module and from the
// `./monaco` setup it imports, and nowhere else, so all of it lands in the
// chunk `./multi-file-editor` loads on demand.
import { Editor } from "@monaco-editor/react";
import type * as monaco from "monaco-editor";
import { useEffect, useRef } from "react";
import { toMonacoPath } from "./lib/file-paths";
import { initMonacoSync } from "./lib/monaco-sync";
import { monacoApi } from "./monaco";
import {
  type ResolvedBreakpoint,
  useBreakpointStore,
} from "./stores/breakpoint-store";
import { useFileStore } from "./stores/file-store";
import { useThemeStore } from "./stores/theme-store";

export type EditorProps = {
  filePath: string;
  readOnly?: boolean;
  onEditorMount?: (editor: monaco.editor.IStandaloneCodeEditor) => void;
};

/**
 * Build the gutter glyph decorations for a file's breakpoints. During a debug
 * session breakpoints snap to their resolved line; unresolvable ones render
 * greyed out. Outside a session (no resolution info) all render as-is.
 */
function breakpointDecorations(
  lines: number[] | undefined,
  resolvedForFile: Record<number, ResolvedBreakpoint | null> | undefined,
): monaco.editor.IModelDeltaDecoration[] {
  return (lines ?? []).map((line) => {
    const entry = resolvedForFile?.[line];
    const targetLine = entry ? entry.line : line;
    const verified = resolvedForFile === undefined || Boolean(entry);
    return {
      range: {
        startLineNumber: targetLine,
        startColumn: 1,
        endLineNumber: targetLine,
        endColumn: 1,
      },
      options: {
        glyphMarginClassName: verified ? "bp-glyph" : "bp-glyph-unverified",
        stickiness: 1, // NeverGrowsWhenTypingAtEdges
      },
    };
  });
}

/**
 * Map a clicked gutter line back to the *requested* breakpoint line that owns
 * it. During a session a breakpoint requested on line N renders its glyph at the
 * resolved line M, so a click on M must toggle the N entry (removing it) rather
 * than adding a fresh, never-resolving breakpoint at M. Falls back to the
 * clicked line when it isn't a resolved target (i.e. a brand-new breakpoint).
 */
function requestedLineForClick(
  clickedLine: number,
  resolvedForFile: Record<number, ResolvedBreakpoint | null> | undefined,
): number {
  if (resolvedForFile) {
    for (const [requested, entry] of Object.entries(resolvedForFile)) {
      if (entry && entry.line === clickedLine) return Number(requested);
    }
  }
  return clickedLine;
}

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

        if (import.meta.env.DEV) {
          // e2e hook: lets tests drive Monaco through its API instead of
          // querying the rendered token-span DOM, whose slicing differs
          // across platforms/builds.
          (window as unknown as { __z33e2e?: unknown }).__z33e2e = {
            showHoverAt(lineNumber: number, column: number) {
              editor.setPosition({ lineNumber, column });
              editor.focus();
              editor.trigger("e2e", "editor.action.showHover", {});
            },
          };
        }

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
