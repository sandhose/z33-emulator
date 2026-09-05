// oxlint-disable unicorn/no-useless-undefined -- `undefined` is the "no debug session" case under test
import { describe, expect, it } from "vitest";
import {
  breakpointDecorations,
  requestedLineForClick,
} from "./breakpoint-gutter";
import type { ResolvedForFile } from "../stores/breakpoint-store";

const resolvedAt = (line: number) => ({ line, address: 1000 + line });

describe("breakpointDecorations", () => {
  it("returns nothing for a file without breakpoints", () => {
    expect(breakpointDecorations(undefined, undefined)).toEqual([]);
    expect(breakpointDecorations([], {})).toEqual([]);
  });

  it("marks every breakpoint verified outside a debug session", () => {
    const decorations = breakpointDecorations([3, 7], undefined);
    expect(decorations).toHaveLength(2);
    expect(decorations.map((d) => d.range.startLineNumber)).toEqual([3, 7]);
    expect(decorations.map((d) => d.options.glyphMarginClassName)).toEqual([
      "bp-glyph",
      "bp-glyph",
    ]);
  });

  it("draws a resolved breakpoint on the line it snapped to", () => {
    const resolved: ResolvedForFile = { 3: resolvedAt(5) };
    const [decoration] = breakpointDecorations([3], resolved);
    expect(decoration?.range).toEqual({
      startLineNumber: 5,
      startColumn: 1,
      endLineNumber: 5,
      endColumn: 1,
    });
    expect(decoration?.options.glyphMarginClassName).toBe("bp-glyph");
  });

  it("greys out a breakpoint the program could not resolve", () => {
    const resolved: ResolvedForFile = { 3: null };
    const [decoration] = breakpointDecorations([3], resolved);
    expect(decoration?.range.startLineNumber).toBe(3);
    expect(decoration?.options.glyphMarginClassName).toBe(
      "bp-glyph-unverified",
    );
  });

  // A breakpoint added mid-session has no entry until the worker answers.
  it("greys out a breakpoint missing from the resolution map", () => {
    const [decoration] = breakpointDecorations([9], { 3: resolvedAt(5) });
    expect(decoration?.range.startLineNumber).toBe(9);
    expect(decoration?.options.glyphMarginClassName).toBe(
      "bp-glyph-unverified",
    );
  });
});

describe("requestedLineForClick", () => {
  it("returns the clicked line when there is no session", () => {
    expect(requestedLineForClick(4, undefined)).toBe(4);
  });

  it("maps a click on a snapped glyph back to the requested line", () => {
    expect(requestedLineForClick(5, { 3: resolvedAt(5) })).toBe(3);
  });

  it("returns the clicked line when it is not a resolved target", () => {
    expect(requestedLineForClick(9, { 3: resolvedAt(5) })).toBe(9);
  });

  it("ignores unresolved entries", () => {
    expect(requestedLineForClick(3, { 3: null })).toBe(3);
  });

  it("keeps a self-resolving breakpoint on its own line", () => {
    expect(requestedLineForClick(3, { 3: resolvedAt(3) })).toBe(3);
  });

  // Two requested lines can snap to the same instruction, stacking their
  // glyphs; the click clears the lowest of them, so repeated clicks peel them
  // off one at a time. Integer keys iterate in ascending order.
  it("clears the lowest requested line when several snapped together", () => {
    expect(
      requestedLineForClick(5, { 3: resolvedAt(5), 4: resolvedAt(5) }),
    ).toBe(3);
  });
});
