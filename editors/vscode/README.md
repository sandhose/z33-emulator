# Zorglub33 for Visual Studio Code

Language support and debugging for the Zorglub-33 (Z33) educational
computer architecture, used in the *Architecture des Systèmes
d'Exploitation* course at the University of Strasbourg.

This is a **pure web extension**: everything (assembler, language
server, debugger) runs as WebAssembly inside the editor. It works
identically on desktop VS Code and in the browser on
[vscode.dev](https://vscode.dev) / [github.dev](https://github.dev) —
no compiler or runtime to install.

## Features

- **Syntax highlighting** for Z33 assembly (`.s` / `.S` files)
- **Diagnostics** as you type: preprocessor, parser and layout errors
  with precise spans, across `#include`d files
- **Completion** for instructions, directives, registers, labels and
  `#define`d macros — argument-aware
- **Hover documentation** for every instruction and directive
- **Go to definition / find references / rename** for labels and
  macros, across files
- **Document symbols** and **code lens** (resolved address and
  reference count on each label)
- **Debugging**: set breakpoints, step/continue, inspect registers,
  the stack and memory, and evaluate expressions like `[%sp+2]`, via a
  built-in Debug Adapter with no external process and no configuration
  needed
- **Debug hover**: hovering a register or a memory operand like
  `[%sp+2]` while stopped shows its current value
- **Inline register values**: while stopped, every register named on a
  visible line up to the stopped instruction is shown inline in the
  editor

## Debugging quick start

Open a `.s` file and press F5 to start debugging immediately, stopped on
entry. With no `entrypoint` configured, execution starts at the first
label the program defines among `main`, `start`, `run` and `entry`. No
`launch.json` is needed.

Clicking the play button in the editor title bar, or running "Run
Current File" from the command palette (category Zorglub33), asks for
the entrypoint label first, defaulting to `main`.

Debugging an unsaved (untitled) file works once `debug.saveBeforeStart`
is set to `nonUntitledEditorsInActiveGroup` or `none`; otherwise VS Code
asks to save it first. With the default setting, F5 also saves the
current file before running it, so edits are always what runs.

"Debug: Select and Start Debugging" lists a `Run fact.s` entry for the
active file under "Zorglub33 Debug".

### launch.json (optional)

Add `.vscode/launch.json` if you want a fixed entrypoint, or several
configurations:

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type": "zorglub33",
      "request": "launch",
      "name": "Run fact.s",
      "program": "fact.s",
      "entrypoint": "main"
    }
  ]
}
```

`entrypoint` is the label execution starts from. `program` also
accepts `${workspaceFolder}/fact.s` and `${file}`.

## Related tools

- **Web IDE**: the same emulator and language tooling in the browser —
  <https://sandhose.github.io/z33-emulator/>
- **CLI** (`z33-cli`): batch runs and an interactive debugger —
  grab a binary from the
  [GitHub releases](https://github.com/sandhose/z33-emulator/releases)
- **Source & issues**: <https://github.com/sandhose/z33-emulator>
