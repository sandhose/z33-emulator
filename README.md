# Zorglub-33 emulator

This is an emulator for the z33 architecture used in the _"Architecture des Systèmes d'Exploitation"_ course at the University of Strasbourg.

## Web editor

The easiest way to use the emulator is the web IDE at **<https://sandhose.github.io/z33-emulator/>** — an editor with completion, hover documentation and live diagnostics, plus a step debugger with registers, breakpoints and a full memory view. Everything runs in the browser; there is nothing to install.

## Editor extensions

### Visual Studio Code

Install **Zorglub33** from the [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=quentin-gliech.z33-assembly), or from [Open VSX](https://open-vsx.org/extension/quentin-gliech/z33-assembly) for VSCodium and other compatible editors.

It is a pure web extension: syntax highlighting, diagnostics, completion, go-to-definition/rename and the debugger (`F5` with a `zorglub33` launch configuration) all run as WebAssembly inside the editor, so it works the same on desktop VS Code and on [vscode.dev](https://vscode.dev) / [github.dev](https://github.dev).

### Zed

The Zed extension is not published to the extension registry yet, but it can be installed as a dev extension (this compiles it locally, so a [Rust toolchain](https://www.rust-lang.org/tools/install) is required):

1. Clone this repository.
2. In Zed, open the command palette and run `zed: install dev extension`, then select the `editors/zed/` directory.

The extension provides highlighting (tree-sitter), the language server and the debugger. It uses `z33-cli` from your `PATH` if available, and otherwise downloads a prebuilt binary from the GitHub releases automatically.

### Vim / Neovim

A single universal plugin for both editors lives in
[`editors/vim/`](editors/vim). In **classic Vim** it provides filetype
detection for `.s`/`.S` (a content heuristic so it won't hijack your GNU asm
files), regex syntax highlighting, comment/indent defaults, and optional
zero-config [vim-lsp](https://github.com/prabirshrestha/vim-lsp) integration
against `z33-cli lsp`. In **Neovim** it adds tree-sitter highlighting (with the
regex syntax as a fallback until you `:TSInstall z33`), native LSP (0.11+),
debugging via [`nvim-dap`](https://github.com/mfussenegger/nvim-dap), and
automatic download of the `z33-cli` binary from GitHub releases.

Install with [vim-plug](https://github.com/junegunn/vim-plug) (Vim or Neovim):

```vim
Plug 'sandhose/z33-emulator', { 'rtp': 'editors/vim' }
```

lazy.nvim can't load a monorepo subdirectory from a plain
`"sandhose/z33-emulator"` spec, so its users need the `dir =` recipe against a
local clone. See [`editors/vim/README.md`](editors/vim/README.md) for that,
plus all install methods, config variables, the tree-sitter-vs-regex story,
`:checkhealth z33`, `:Z33Download` and `:TSInstall z33`.

## Command-line emulator

### Binaries

Binaries for [the latest release](https://github.com/sandhose/z33-emulator/releases/latest) are available here:

- [Linux (x86 64bit)](https://github.com/sandhose/z33-emulator/releases/latest/download/z33-cli-x86_64-linux.tar.gz)
- [Linux (ARM 64bit)](https://github.com/sandhose/z33-emulator/releases/latest/download/z33-cli-aarch64-linux.tar.gz)
- [macOS (x86 64bit)](https://github.com/sandhose/z33-emulator/releases/latest/download/z33-cli-x86_64-macos.tar.gz)
- [macOS (ARM 64bit)](https://github.com/sandhose/z33-emulator/releases/latest/download/z33-cli-aarch64-macos.tar.gz)
- [Windows (x86 64bit)](https://github.com/sandhose/z33-emulator/releases/latest/download/z33-cli-x86_64-windows.exe)
- [Windows (ARM 64bit)](https://github.com/sandhose/z33-emulator/releases/latest/download/z33-cli-aarch64-windows.exe)

#### Using the binaries on macOS

The macOS binaries are not signed, and macOS will quarantine them by default.
This can be fixed by removing the quarantine flag on the binary:

```sh
xattr -r -d com.apple.quarantine [path to]/z33-cli
```

### Compiling

Alternatively, it can be compiled from source with the Rust toolchain pinned in `rust-toolchain.toml` (rustup installs it automatically).
Check the official documentation on [how to install Rust](https://www.rust-lang.org/tools/install).

```sh
git clone https://github.com/sandhose/z33-emulator
cd z33-emulator
cargo build --release
# Binary is available under `target/release/z33-cli'
```

## Running a program

```sh
[path to]/z33-cli run samples/fact.s main
```

## Interactive mode

```sh
[path to]/z33-cli run -i samples/fact.s main
```

### Available commands:

- `help [command]`: Print the help message of a command
- `memory <address> [n]`: Show a block of memory. The address can be either a register with or without offset (e.g. `%sp-5`) or a literal (e.g. `100`). The second argument is the number of cells to show (one by default).
- `registers [register]`: Show the value of a register. If no register is specified, shows the value of all five of them.
- `list`: Show the code that will be run next.
- `step [n]`: Run `n` step of the program (one by default).
- `break <address>`: Set a breakpoint at given address.
- `unbreak <address>`: Remove a breakpoint at given address.
- `info breakpoints`: Show the list of breakpoints
- `continue`: Run the code until the next breakpoint
- `interrupt`: Trigger a hardware interrupt
- `exit`: Exit the emulator

## Releasing

Releases are cut from the GitHub Actions UI:

 1. Run the **Release** workflow on `main` with the new version number
    (e.g. `0.8.0`). It bumps every manifest (workspace crates, VS Code
    extension, Zed extension, tree-sitter grammar) through
    `scripts/set-version.sh` and opens a `Release vX.Y.Z` pull request
    with CI running on it.
 2. Merge the pull request. The merge commit is tagged automatically,
    the binaries and extensions are built, and the GitHub release is
    published with generated notes and a `SHA256SUMS` file.

# License

[MIT](./LICENSE) © [Quentin Gliech](https://sandhose.fr)
