import { Editor } from "@monaco-editor/react";
import type * as monaco from "monaco-editor";
import { useEffect, useRef } from "react";
import { toMonacoPath } from "./lib/file-paths";
import {
  type ResolvedBreakpoint,
  useBreakpointStore,
} from "./stores/breakpoint-store";
import { useThemeStore } from "./stores/theme-store";

type Props = {
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

export const MultiFileEditor: React.FC<Props> = ({
  filePath,
  readOnly = false,
  onEditorMount,
}: Props) => {
  const effective = useThemeStore((s) => s.effective);

  // Breakpoint state for the currently shown file.
  const breakpoints = useBreakpointStore((s) => s.breakpoints);
  const resolved = useBreakpointStore((s) => s.resolved);
  const toggle = useBreakpointStore((s) => s.toggle);

  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);
  const decorationsRef =
    useRef<monaco.editor.IEditorDecorationsCollection | null>(null);
  const filePathRef = useRef(filePath);
  filePathRef.current = filePath;

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
        // Re-apply glyphs after a model (file) switch.
        editor.onDidChangeModel(() => {
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
        });

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
