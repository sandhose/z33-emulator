import { loader } from "@monaco-editor/react";
import * as monaco from "monaco-editor/esm/vs/editor/editor.api.js";

// Import editor contributions that aren't included in the minimal editor.api.js
// bundle. Each registers the UI/controller for a language feature; the actual
// providers are wired to the WASM language server in ./lib/lsp-monaco.
import "monaco-editor/esm/vs/editor/contrib/hover/browser/hoverContribution.js";
import "monaco-editor/esm/vs/editor/contrib/suggest/browser/suggestController.js";
import "monaco-editor/esm/vs/editor/contrib/parameterHints/browser/parameterHints.js";
import "monaco-editor/esm/vs/editor/contrib/gotoSymbol/browser/goToCommands.js";
import "monaco-editor/esm/vs/editor/contrib/gotoSymbol/browser/link/goToDefinitionAtPosition.js";
import "monaco-editor/esm/vs/editor/contrib/gotoSymbol/browser/peek/referencesController.js";
import "monaco-editor/esm/vs/editor/contrib/gotoError/browser/gotoError.js";
import "monaco-editor/esm/vs/editor/contrib/rename/browser/rename.js";
import "monaco-editor/esm/vs/editor/contrib/wordHighlighter/browser/wordHighlighter.js";
import "monaco-editor/esm/vs/editor/contrib/documentSymbols/browser/documentSymbols.js";
import "monaco-editor/esm/vs/editor/contrib/codelens/browser/codelensController.js";
import "monaco-editor/esm/vs/editor/contrib/semanticTokens/browser/documentSemanticTokens.js";

import editorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";
import { setupLsp } from "./lib/lsp-monaco";

self.MonacoEnvironment = {
  getWorker(_workerId, _label) {
    return new editorWorker();
  },
};

loader.config({ monaco });

const Instructions = [
  /// Add a value to a register
  "add",
  /// Bitwise `and` with a given value
  "and",
  /// Push `%pc` and go to the given address
  "call",
  /// Compare a value with a register
  "cmp",
  /// Divide a register by a value
  "div",
  /// Load a memory cell to a register and set this cell to 1
  "fas",
  /// Read a value from an I/O controller
  "in",
  /// Unconditional jump
  "jmp",
  /// Jump if equal
  "jeq",
  /// Jump if not equal
  "jne",
  /// Jump if less or equal
  "jle",
  /// Jump if strictly less
  "jlt",
  /// Jump if greater of equal
  "jge",
  /// Jump if strictly greater
  "jgt",
  /// Load a register with a value
  "ld",
  /// Multiply a value to a register
  "mul",
  "neg",
  /// No-op
  "nop",
  /// Bitwise negation of a register
  "not",
  /// Bitwise `or` with a given value
  "or",
  /// Write a value to an I/O controller
  "out",
  /// Pop a value from the stack
  "pop",
  /// Push a value into the stack
  "push",
  /// Reset the computer
  "reset",
  /// Return from an interrupt or an exception
  "rti",
  /// Return from a `call`
  "rtn",
  /// Bitshift to the left
  "shl",
  /// Bitshift to the right
  "shr",
  /// Store a register value in memory
  "st",
  /// Substract a value from a register
  "sub",
  /// Swap a value and a register
  "swap",
  /// Start a `trap` exception
  "trap",
  /// Bitwise `xor` with a given value
  "xor",
];

monaco.languages.register({ id: "z33" });

// Register a tokens provider for the language
monaco.languages.setMonarchTokensProvider("z33", {
  includeLF: true,
  tokenizer: {
    root: [
      [
        new RegExp(`\\s(${Instructions.join("|")})\\b`, "u"),
        "keyword",
        "@instruction",
      ],
      [
        new RegExp(`^(${Instructions.join("|")})\\b`, "u"),
        "keyword",
        "@instruction",
      ],
      [/[\w_]+:/u, "type"],
      [/#\w+/u, "strong", "@preprocessor"],
      [/\.\w+/u, "keyword", "@directive"],
      { include: "@constants" },
      { include: "@comments" },
    ],
    preprocessor: [
      [/\n$/u, "endofline", "@pop"],
      { include: "@constants" },
      { include: "@string" },
    ],
    directive: [
      [/\n$/u, "endofline", "@pop"],
      { include: "@constants" },
      { include: "@string" },
      { include: "@comments" },
    ],
    instruction: [
      [/\n$/u, "endofline", "@pop"],
      [/(\w|%)+[^a-zA-Z0-9_,/]+(\w|%)+/u, "invalid", "@pop"],
      [/\[[^\]]*\][^a-zA-Z0-9_,/]+(\w|%)+/u, "invalid", "@pop"],
      [/(\w|%)+[^a-zA-Z0-9_,/]+\[[^\]]*\]/u, "invalid", "@pop"],
      [/\[/u, "operator", "@idx"],
      [/,/u, "operator"],
      { include: "@constants" },
      { include: "@comments" },
      { include: "@registers" },
    ],
    string: [[/"[^"]*"/u, "string"]],
    constants: [[/[0-9]+/u, "constant"]],
    comments: [[/\/\/.*/u, "comment"]],
    registers: [
      [/%(a|b|sp|pc|sr)\b/u, "tag"],
      [/%[^ab]\b/u, "invalid"],
      [/%s[^pr]/u, "invalid"],
      [/%p[^c]/u, "invalid"],
      [/%[^absp]/u, "invalid"],
    ],
    idx: [
      [/\]/u, "operator", "@pop"],
      [/[^a-zA-Z0-9\-+% \t\]][^\n]*/u, { token: "error", next: "@pop" }],
      { include: "@constants" },
      { include: "@registers" },
    ],
  },
});

// Wire the WASM language server: diagnostics, completion, hover, definition,
// references, rename, symbols, semantic tokens, signature help, code lens.
// The Monarch tokenizer above stays as the base highlighting layer; semantic
// tokens overlay it (editor option `semanticHighlighting.enabled`).
// The minimal `editor.api` module is a structural subset of the full
// `monaco-editor` type; it exposes everything the LSP integration touches.
// oxlint-disable-next-line typescript/no-unsafe-type-assertion
setupLsp(monaco as unknown as typeof import("monaco-editor"));
