import type * as monaco from "monaco-editor";
import {
  useCallback,
  useEffect,
  useRef,
  useState,
  useSyncExternalStore,
} from "react";
import type { ComputerProxy } from "../lib/computer-proxy";
import { stripLeadingSlash } from "../lib/file-paths";

type UseSourceHighlightOptions = {
  computer: ComputerProxy;
  editor: monaco.editor.IStandaloneCodeEditor | null;
  onSwitchFile: (filePath: string) => void;
};

/**
 * Highlights the current instruction line, driven by the worker-resolved source
 * location of `%pc` (carried in each snapshot). Auto-switches the active file to
 * follow execution across `#include`d files.
 */
export function useSourceHighlight({
  computer,
  editor,
  onSwitchFile,
}: UseSourceHighlightOptions): void {
  const location = useSyncExternalStore(
    useCallback((cb) => computer.subscribePc(cb), [computer]),
    () => computer.getPcLocation(),
  );

  const status = useSyncExternalStore(
    useCallback((cb) => computer.subscribeStatus(cb), [computer]),
    () => computer.getStatus(),
  );

  const decorationsRef =
    useRef<monaco.editor.IEditorDecorationsCollection | null>(null);
  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);
  /** Timestamp of the last auto-reveal, to throttle scrolling while running. */
  const lastRevealRef = useRef(0);

  // Track the active model URI so the highlight re-applies after a file switch.
  const [currentModelUri, setCurrentModelUri] = useState<string | null>(null);
  useEffect(() => {
    if (!editor) return () => {};
    setCurrentModelUri(editor.getModel()?.uri.toString() ?? null);
    const disposable = editor.onDidChangeModel((e) => {
      setCurrentModelUri(e.newModelUrl?.toString() ?? null);
    });
    return () => {
      disposable.dispose();
    };
  }, [editor]);

  // Follow execution into other files when the PC's file differs from the one
  // currently shown.
  useEffect(() => {
    if (!editor || !location) return;
    const model = editor.getModel();
    if (!model) return;
    if (stripLeadingSlash(model.uri.path) !== location.file) {
      onSwitchFile(location.file);
    }
  }, [editor, location, onSwitchFile]);

  useEffect(() => {
    if (!editor) return;

    if (editorRef.current !== editor) {
      decorationsRef.current?.clear();
      decorationsRef.current = editor.createDecorationsCollection();
      editorRef.current = editor;
    }
    const decorations = decorationsRef.current;
    const model = editor.getModel();
    if (!decorations || !model) return;

    if (!location || stripLeadingSlash(model.uri.path) !== location.file) {
      decorations.clear();
      return;
    }

    const line = location.line;
    decorations.set([
      {
        range: {
          startLineNumber: line,
          startColumn: 1,
          endLineNumber: line,
          endColumn: model.getLineMaxColumn(line),
        },
        options: {
          isWholeLine: true,
          className: "debug-line-highlight",
          glyphMarginClassName: "debug-line-glyph",
        },
      },
    ]);

    // Revealing (scrolling) on every ~40ms snapshot during a run is a scroll
    // storm. While running, throttle reveals to at most one per 300ms; on any
    // stop (paused/halted/panicked) always reveal so the final line is centered.
    const now = performance.now();
    if (status !== "running" || now - lastRevealRef.current >= 300) {
      editor.revealLineInCenter(line);
      lastRevealRef.current = now;
    }
  }, [editor, location, currentModelUri, status]);

  useEffect(() => {
    return () => {
      decorationsRef.current?.clear();
      decorationsRef.current = null;
    };
  }, []);
}
