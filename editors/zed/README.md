# Zorglub33 for Zed

Language support and debugging for Zorglub-33 (Z33) assembly, the teaching
architecture of the *Architecture des Systèmes d'Exploitation* course at the
University of Strasbourg.

- Syntax highlighting, outline and brackets through the `tree-sitter-z33`
  grammar.
- Diagnostics, completion, hover documentation, go to definition, references
  and rename through the `z33-cli lsp` language server.
- Debugging (breakpoints, stepping, registers, stack, memory) through the
  `z33-cli dap` debug adapter.

The extension uses `z33-cli` from your `PATH` when present and otherwise
downloads the prebuilt binary for your platform from the GitHub releases of
<https://github.com/sandhose/z33-emulator>.

## Debugging

Add a `zorglub33` entry to the project's `.zed/debug.json`, or fill the
`debugger: new process` modal, with the program to run:

```json
{
  "label": "Run fact.s",
  "adapter": "zorglub33",
  "request": "launch",
  "program": "$ZED_WORKTREE_ROOT/fact.s",
  "entrypoint": "main"
}
```

`entrypoint` defaults to whichever of `main`, `start`, `run` or `entry` the
program defines.

## Development

The extension lives in `editors/zed/` of the main repository. Install it as a
dev extension from Zed's command palette (`zed: install dev extension`) and
point it at that directory; a Rust toolchain with the `wasm32-wasip1` target
is required.
