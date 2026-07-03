# Z33 Assembly for Visual Studio Code

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
  the stack and memory, and evaluate expressions like `[%sp+2]` — via a
  built-in Debug Adapter, no external process

## Debugging quick start

Open a folder containing your `.s` files and create
`.vscode/launch.json`:

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type": "z33",
      "request": "launch",
      "name": "Run fact.s",
      "program": "${workspaceFolder}/fact.s",
      "entrypoint": "main"
    }
  ]
}
```

Then press F5. `entrypoint` is the label execution starts from.

## Related tools

- **Web IDE**: the same emulator and language tooling in the browser —
  <https://sandhose.github.io/z33-emulator/>
- **CLI** (`z33-cli`): batch runs and an interactive debugger —
  grab a binary from the
  [GitHub releases](https://github.com/sandhose/z33-emulator/releases)
- **Source & issues**: <https://github.com/sandhose/z33-emulator>
