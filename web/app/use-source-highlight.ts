import type * as monaco from "monaco-editor";
import { useEffect, useRef, useState } from "react";
import type { Computer, SourceMap } from "z33-web-bindings";
import { useRegisters } from "./computer";

/**
 * Build a lookup table from UTF-8 byte offset → JS string character offset.
 * The Rust source map produces byte offsets, but Monaco's getPositionAt()
 * expects character (UTF-16 code unit) offsets.
 */
function buildByteToCharMap(text: string): Uint32Array {
  const encoded = new TextEncoder().encode(text);
  // Map from byte index → char index. +1 so we can look up the end-of-string offset.
  const map = new Uint32Array(encoded.length + 1);
  let charIdx = 0;
  let byteIdx = 0;
  while (charIdx < text.length) {
    const cp = text.codePointAt(charIdx);
    if (cp === undefined) break;
    const byteLen = cp <= 0x7f ? 1 : cp <= 0x7ff ? 2 : cp <= 0xffff ? 3 : 4;
    const charLen = cp > 0xffff ? 2 : 1; // surrogate pair
    for (let i = 0; i < byteLen; i++) {
      map[byteIdx + i] = charIdx;
    }
    byteIdx += byteLen;
    charIdx += charLen;
  }
  map[byteIdx] = charIdx;
  return map;
}

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
  const byteToCharRef = useRef<Uint32Array | null>(null);
  const modelUriRef = useRef<string | null>(null);

  // Re-run the highlight effect whenever the editor switches to a different model.
  // This is needed because onSwitchFile triggers a React state update that causes
  // @monaco-editor/react to call editor.setModel(), but the highlight effect's
  // deps don't change — so we track the active model URI as state to force a re-run.
  const [currentModelUri, setCurrentModelUri] = useState<string | null>(null);
  useEffect(() => {
    if (!editor) return;
    setCurrentModelUri(editor.getModel()?.uri.toString() ?? null);
    const disposable = editor.onDidChangeModel((e) => {
      setCurrentModelUri(e.newModelUrl?.toString() ?? null);
    });
    return () => disposable.dispose();
  }, [editor]);

  useEffect(() => {
    if (!editor) return;

    // Recreate decorations collection when editor instance changes
    if (editorRef.current !== editor) {
      decorationsRef.current?.clear();
      decorationsRef.current = editor.createDecorationsCollection();
      editorRef.current = editor;
    }

    // Rebuild byte-to-char map when the model (file) changes
    const model = editor.getModel();
    const modelUri = model?.uri.toString() ?? null;
    if (modelUri !== modelUriRef.current) {
      modelUriRef.current = modelUri;
      byteToCharRef.current = model
        ? buildByteToCharMap(model.getValue())
        : null;
    }

    const decorations = decorationsRef.current;
    if (!decorations) return;

    const location = sourceMap.get(registers.pc);
    if (!location) {
      decorations.clear();
      return;
    }

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

    // Convert UTF-8 byte offsets from the Rust source map to character offsets
    const byteToChar = byteToCharRef.current;
    const charStart = byteToChar
      ? (byteToChar[location.span[0]] ?? location.span[0])
      : location.span[0];
    const charEnd = byteToChar
      ? (byteToChar[location.span[1]] ?? location.span[1])
      : location.span[1];

    const startPos = model.getPositionAt(charStart);
    const endPos = model.getPositionAt(charEnd);

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
  }, [editor, sourceMap, registers.pc, onSwitchFile, currentModelUri]);

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      decorationsRef.current?.clear();
      decorationsRef.current = null;
    };
  }, []);
}
