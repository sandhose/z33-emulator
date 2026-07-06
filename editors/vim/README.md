# Z33 for Vim

A classic Vim plugin for [Zorglub-33](https://github.com/sandhose/z33-emulator)
assembly (`.s` / `.S`). It provides:

- **Filetype detection** — a content heuristic that recognizes Z33 sources
  without breaking real GNU/other `asm` files that share the `.s`/`.S`
  extension.
- **Syntax highlighting** — mnemonics, registers, directives, the `#…`
  preprocessor, labels, numbers, strings, operators and `//` comments.
- **Filetype defaults** — `//` comments, 4-space soft tabs, `%`-aware
  keywords.
- **Minimal indentation** — labels flush-left, everything else keeps its
  indent.
- **Optional LSP** — zero-config [vim-lsp](https://github.com/prabirshrestha/vim-lsp)
  registration, active only when both vim-lsp and the `z33-cli` binary are
  present.

> **Neovim users:** this plugin works in Neovim too (it is pure Vimscript), but
> the dedicated `editors/nvim/` plugin adds tree-sitter highlighting, native
> LSP, nvim-dap debugging and CLI auto-download. Prefer that one on Neovim.

## Requirements

- Vim 8+ (or Neovim).
- For LSP: [vim-lsp](https://github.com/prabirshrestha/vim-lsp) and the
  `z33-cli` binary on your `PATH`. Grab a prebuilt binary from the
  [releases page](https://github.com/sandhose/z33-emulator/releases) or build it
  with `cargo build --release -p z33-cli`. Debugging (DAP) is not offered for
  classic Vim — use the Neovim plugin (nvim-dap) or another supported editor.

## Install

The plugin lives in the `editors/vim/` subdirectory of the monorepo, so point
your plugin manager at that subpath.

**[vim-plug](https://github.com/junegunn/vim-plug)** (supports `rtp` subpaths):

```vim
Plug 'sandhose/z33-emulator', { 'rtp': 'editors/vim' }
```

**Native packages** (`:h packages`) — clone the repo and symlink the
subdirectory into a package:

```sh
git clone https://github.com/sandhose/z33-emulator ~/src/z33-emulator
mkdir -p ~/.vim/pack/plugins/start
ln -s ~/src/z33-emulator/editors/vim ~/.vim/pack/plugins/start/z33
```

**Manual** — copy the five subdirectories (`ftdetect/`, `syntax/`,
`ftplugin/`, `indent/`, `plugin/`) into `~/.vim`.

## Configuration

All settings are optional.

| Variable | Effect |
|---|---|
| `g:z33_filetypes` | Force **every** `.s`/`.S` file to filetype `z33` (skip the heuristic). Set it if you only ever write Z33. |
| `g:z33_no_ftdetect` | Disable the filetype-detection autocommand entirely. |
| `g:z33_no_indent` | Disable the indent script. |
| `g:z33_no_lsp` | Disable the vim-lsp registration. |

Per-file override — because Z33 comments are `//`, a modeline works and always
wins over the heuristic:

```
// vim: ft=z33
```

## How detection avoids the `.s` collision

`.s`/`.S` default to `asm`. To reclaim only genuine Z33 files, the autocommand
scans the first 64 lines for signals that are specific to Z33 and essentially
never appear in GNU asm: the `%pc`/`%sr`/`%a`/`%b` registers (GNU asm uses
`%eax`, `%rdi`, … and never these — `%sp` is deliberately excluded since it's
a real x86 AT&T register), the `.addr` directive (`.word`/`.space`/`.string`
are excluded since they're standard GNU `as` directives too), and the
`#include`/`#define`/`#undefine`/`#if`/`#elif`/`#endif`/`#error` preprocessor
(note `#undefine`, not `#undef`). It only ever acts when the buffer's filetype
is empty, `asm`, or `r`, so it never overrides a more confident detector.

**vim-polyglot users:** polyglot's r-lang detector also claims `.s`/`.S`
(legacy S-PLUS used that extension) and can win the race if its autocmd runs
before ours, leaving the buffer as filetype `r`. Our heuristic overrides that
guess too, but if you'd rather disable polyglot's r-lang detector outright,
add `let g:polyglot_disabled = ['r-lang']` before polyglot loads.
