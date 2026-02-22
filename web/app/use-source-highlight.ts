import type * as monaco from "monaco-editor";
import { useEffect, useRef } from "react";
import type { Computer, SourceMap } from "z33-web-bindings";
import { useRegisters } from "./computer";

type UseSourceHighlightOptions = {
  computer: Computer;
  sourceMap: SourceMap;
  editor: monaco.editor.IStandaloneCodeEditor | null;
  onSwitchFile: (filePath: string) => void;
};

export function useSourceHighlight({
  computer,
  sourceMap,
  editor,
  onSwitchFile,
}: UseSourceHighlightOptions): void {
  const registers = useRegisters(computer);
  const decorationsRef =
    useRef<monaco.editor.IEditorDecorationsCollection | null>(null);
  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);

  useEffect(() => {
    if (!editor) return;

    // Recreate decorations collection when editor instance changes
    if (editorRef.current !== editor) {
      decorationsRef.current?.clear();
      decorationsRef.current = editor.createDecorationsCollection();
      editorRef.current = editor;
    }

    const decorations = decorationsRef.current;
    if (!decorations) return;

    const location = sourceMap.get(registers.pc);
    if (!location) {
      decorations.clear();
      return;
    }

    const model = editor.getModel();
    if (!model) {
      decorations.clear();
      return;
    }

    // Switch file if the source location is in a different file
    if (model.uri.path !== location.file) {
      onSwitchFile(location.file);
      // The editor model will change, decorations will be applied on next render
      decorations.clear();
      return;
    }

    const startPos = model.getPositionAt(location.span[0]);
    const endPos = model.getPositionAt(location.span[1]);

    decorations.set([
      // Whole-line background on the start line only
      {
        range: {
          startLineNumber: startPos.lineNumber,
          startColumn: 1,
          endLineNumber: startPos.lineNumber,
          endColumn: model.getLineMaxColumn(startPos.lineNumber),
        },
        options: {
          isWholeLine: true,
          className: "debug-line-highlight",
          glyphMarginClassName: "debug-line-glyph",
        },
      },
      // Inline marker for the exact source span
      {
        range: {
          startLineNumber: startPos.lineNumber,
          startColumn: startPos.column,
          endLineNumber: endPos.lineNumber,
          endColumn: endPos.column,
        },
        options: {
          className: "debug-span-highlight",
        },
      },
    ]);

    editor.revealLineInCenter(startPos.lineNumber);
  }, [editor, sourceMap, registers.pc, onSwitchFile]);

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      decorationsRef.current?.clear();
      decorationsRef.current = null;
    };
  }, []);
}
