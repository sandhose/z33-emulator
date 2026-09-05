// How the breakpoint store's per-file state becomes gutter glyphs, and how a
// click on one maps back to the store entry that owns it.
import type * as monaco from "monaco-editor";
import type { ResolvedForFile } from "../stores/breakpoint-store";

/**
 * Build the gutter glyph decorations for a file's breakpoints. During a debug
 * session breakpoints snap to their resolved line; unresolvable ones render
 * greyed out. Outside a session (no resolution info) all render as-is.
 */
export function breakpointDecorations(
  lines: number[] | undefined,
  resolvedForFile: ResolvedForFile | undefined,
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
export function requestedLineForClick(
  clickedLine: number,
  resolvedForFile: ResolvedForFile | undefined,
): number {
  if (resolvedForFile) {
    for (const [requested, entry] of Object.entries(resolvedForFile)) {
      if (entry && entry.line === clickedLine) return Number(requested);
    }
  }
  return clickedLine;
}
