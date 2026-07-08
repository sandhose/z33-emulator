# Z33 for Vim & Neovim

A single, universal plugin for [Zorglub-33 (Z33)](https://github.com/sandhose/z33-emulator)
assembly (`.s` / `.S`) that works in **both classic Vim and Neovim**. It lives in
the `editors/vim/` subdirectory of the monorepo.

The one directory serves both editors because of how they load runtime files:
classic Vim only sources `.vim` scripts (it ignores `lua/`, `lsp/`, `queries/`
and any `plugin/*.lua`), while Neovim sources both the `.vim` files and the Lua
ones. So the `.vim` files provide the classic-Vim experience and act as the
Neovim fallback, and the Lua files layer Neovim-only features on top. The two
`.vim` entry points that would otherwise conflict with the Lua path
(`ftdetect/z33.vim`, `plugin/z33.vim`) early-`finish` under Neovim.

## What you get

| Feature | Classic Vim | Neovim |
|---|---|---|
| Filetype detection (`.s`/`.S` content heuristic) | ✅ Vimscript autocmd | ✅ `vim.filetype.add` + force-override autocmd |
| Highlighting | ✅ regex syntax (`syntax/z33.vim`) | ✅ **tree-sitter** when the parser is installed, otherwise the same regex syntax as a fallback |
| Comment / indent / `%`-keyword defaults | ✅ | ✅ (same `ftplugin`/`indent`) |
| Language server | ✅ optional [vim-lsp](https://github.com/prabirshrestha/vim-lsp) | ✅ native LSP (Neovim 0.11+) |
| Debugging (DAP) | — | ✅ [nvim-dap](https://github.com/mfussenegger/nvim-dap) |
| Automatic `z33-cli` download | — (put `z33-cli` on `PATH`) | ✅ from GitHub releases, with consent |

### Highlighting: tree-sitter vs. the regex fallback (Neovim)

In Neovim, highlighting depends on whether the `z33` tree-sitter parser is
installed:

- **Parser installed** — a `FileType z33` autocmd runs `vim.treesitter.start`,
  giving you tree-sitter highlighting. Neovim core disables the legacy regex
  syntax for that buffer automatically (`vim.treesitter.start` sets
  `b:ts_highlight` and blanks `'syntax'`), so there is no double highlighting.
- **Parser absent** — the buffer falls back to the bundled regex syntax
  (`syntax/z33.vim`, the same file classic Vim uses); `b:current_syntax` is
  `z33`. Install the parser with `:TSInstall z33` to switch to tree-sitter.

Nothing needs to be configured either way. Disable the tree-sitter auto-start
(and stay on the regex fallback) with `vim.g.z33_no_treesitter_start = true`.

## Requirements

- Vim 8+ or Neovim (Neovim **0.11+** for the native LSP; everything else works
  on older Neovim too).
- For LSP/DAP: the `z33-cli` binary. Classic Vim needs it on your `PATH`
  (grab a prebuilt binary from the
  [releases page](https://github.com/sandhose/z33-emulator/releases) or build it
  with `cargo build --release -p z33-cli`). Neovim can auto-download it (see
  below). Debugging (DAP) is Neovim-only; classic Vim gets LSP but not DAP.

## Install

The plugin is the `editors/vim/` **subdirectory** of the monorepo — that
subdirectory, not the repository root, must be on your `runtimepath`.

### vim-plug / packer (Vim or Neovim)

These managers support an `rtp` subpath pointing straight at `editors/vim`:

```vim
" vim-plug
Plug 'sandhose/z33-emulator', { 'rtp': 'editors/vim' }
```

```lua
-- packer.nvim
use { 'sandhose/z33-emulator', rtp = 'editors/vim' }
```

### lazy.nvim (Neovim)

> **Why `"sandhose/z33-emulator"` alone does not work:** lazy.nvim puts the
> *repository root* on the runtimepath and has no option to point at a
> subdirectory (unlike vim-plug/packer's `rtp`). The plugin files live under
> `editors/vim/`, which the root spec never exposes. Use the `dir =` recipe
> against a local clone instead:

```lua
-- One time, clone the repo somewhere stable:
--   git clone https://github.com/sandhose/z33-emulator \
--     ~/.local/share/nvim/z33-emulator
{
  dir = vim.fn.stdpath("data") .. "/z33-emulator/editors/vim",
  name = "z33",
  -- plugin/z33.lua self-initializes; `opts`/`config` are only for tweaks.
}
```

If you already have a checkout, point `dir =` at `.../editors/vim` inside it
(or symlink it).

### Native packages (`:h packages`)

Clone the repo and symlink `editors/vim` into a package. For Vim:

```sh
git clone https://github.com/sandhose/z33-emulator ~/src/z33-emulator
mkdir -p ~/.vim/pack/plugins/start
ln -s ~/src/z33-emulator/editors/vim ~/.vim/pack/plugins/start/z33
```

For Neovim:

```sh
git clone https://github.com/sandhose/z33-emulator ~/src/z33-emulator
mkdir -p ~/.local/share/nvim/site/pack/z33/start
ln -s ~/src/z33-emulator/editors/vim \
      ~/.local/share/nvim/site/pack/z33/start/z33
```

## Configuration

All settings are optional. Set them **before** the plugin loads (e.g. at the top
of your `vimrc` / `init.lua`). Use `g:` in Vimscript, `vim.g.` in Lua — they are
the same variables.

| Variable | Effect |
|---|---|
| `z33_filetypes` | Force **every** `.s`/`.S` file to filetype `z33` (skip the content heuristic). Set it if you only ever write Z33. |
| `z33_no_ftdetect` | Disable the filetype-detection autocommand(s) entirely. |
| `z33_no_indent` | Disable the indent script (Vim / regex path). |
| `z33_no_lsp` | Disable the vim-lsp registration (classic Vim only). |
| `z33_no_treesitter_start` | Neovim: don't auto-start tree-sitter highlighting (stay on the regex fallback). |
| `z33_auto_download` | Neovim: `z33-cli` download consent — unset = prompt once, `true` = download silently, `false` = never download (PATH/cache only). |

### lazy.nvim `opts`

If you configure via lazy.nvim's `opts = { ... }`, the keys `filetypes`,
`no_ftdetect` and `auto_download` are mapped onto the matching `vim.g.z33_*`
variables for you (an explicit `vim.g.z33_*` always wins):

```lua
{
  dir = vim.fn.stdpath("data") .. "/z33-emulator/editors/vim",
  name = "z33",
  opts = { auto_download = true, filetypes = false },
}
```

### Per-file override (modeline)

Because Z33 comments are `//`, a modeline works and always wins over the
heuristic (modelines are applied after ftdetect):

```
// vim: ft=z33
```

## Language server

The server (`z33-cli lsp`) provides diagnostics, completion, hover,
go-to-definition, references, rename, document symbols and code lens.

- **Neovim 0.11+** — enabled automatically for `z33` buffers via the native
  `vim.lsp.enable` mechanism (config in `lsp/z33.lua`). Nothing to wire. The
  server offers an informational `▶ Run <label>` code lens; this plugin does not
  advertise the `experimental.commands` capability, so use nvim-dap (below) to
  actually run/debug.
- **Classic Vim** — zero-config [vim-lsp](https://github.com/prabirshrestha/vim-lsp)
  registration, active only when both vim-lsp and the `z33-cli` binary are
  present. Opt out with `g:z33_no_lsp = 1`.

### Pre-0.11 / nvim-lspconfig fallback

On Neovim < 0.11, or if you prefer `nvim-lspconfig`, register the server manually
(the plugin ships nothing for this path, and it needs `z33-cli` on `PATH` —
auto-download only backs the native LSP/DAP integrations):

```lua
require("lspconfig.configs").z33 = {
  default_config = {
    cmd = { "z33-cli", "lsp" },
    filetypes = { "z33" },
    root_dir = function(fname)
      return vim.fs.dirname(vim.fs.find(".git", { path = fname, upward = true })[1])
        or vim.fs.dirname(fname)
    end,
  },
}
require("lspconfig").z33.setup({})
```

## The `z33-cli` binary (Neovim auto-download)

The LSP and debugger both need `z33-cli`. Resolution order in Neovim:

1. `z33-cli` on your `PATH` (always preferred).
2. A previously downloaded copy under `stdpath('data')/z33/z33-cli-<tag>/`.
3. An on-demand download from
   [GitHub releases](https://github.com/sandhose/z33-emulator/releases) (asks for
   consent the first time, unless `vim.g.z33_auto_download` forces the decision).

Run **`:Z33Download`** to pre-fetch the binary at any time. Downloads never block
startup and only happen on first actual LSP/DAP use. On macOS the quarantine
attribute is stripped best-effort; the binaries are unsigned.

**Classic Vim has no auto-download** — install `z33-cli` on your `PATH` yourself.

## Tree-sitter (Neovim)

Install the parser with **`:TSInstall z33`** — this works on **both** the
nvim-treesitter *master* and *main* (rewrite) branches. The plugin registers the
`z33` parser's install info automatically for whichever branch you run, so
`:TSInstall z33` (or, on `main`, `require("nvim-treesitter").install("z33")`)
downloads this repo and builds the grammar from the committed `src/parser.c`
under `tree-sitter-z33/` (no `tree-sitter generate` step). The highlight, indent
and fold queries under `queries/z33/` on this plugin's runtimepath work
regardless of how the parser was installed.

> On `main`, registration is done via a `User TSUpdate` autocmd (the documented
> hook), because `main` reloads its parser table on every install.

## Debugging (nvim-dap)

With `nvim-dap` installed, a `z33` launch configuration is registered
automatically. Open a `.s` file and run `require("dap").continue()`, then pick
**“Launch Z33 program”**; you will be prompted for an entrypoint label (default
`main`). The current file is used as the program and execution stops on entry.
Standard nvim-dap keymaps work as usual.

## Health check (Neovim)

Run **`:checkhealth z33`** to verify your setup: Neovim version, whether
`z33-cli` is on `PATH`/cached, platform download support, nvim-treesitter (+ the
`z33` parser) and nvim-dap availability, and which highlighting mode
(tree-sitter vs. regex fallback) is in effect. If highlighting looks basic when
you wanted tree-sitter, run `:TSInstall z33`.

## Troubleshooting

### Filetype fights over `.s` / `.S` (e.g. vim-polyglot → `r`)

`.s`/`.S` is a contested extension: Vim's builtin maps it to `asm`, and
[vim-polyglot](https://github.com/sheerun/vim-polyglot) bundles an `r-lang`
package that maps it to `r` (legacy S-PLUS used `.s`). Both use `:setf`, which
silently no-ops once a filetype has been set during detection, so depending on
load order they can win over this plugin.

To reclaim only genuine Z33 files without stealing real GNU-asm ones, the plugin
scans each `.s`/`.S` buffer for Z33-specific markers (registers, the `.addr`
directive, the preprocessor) that effectively never appear in GNU asm, acting
only when no confident filetype is already set — so a GNU-asm `.s` keeps its
filetype (the exact signal list and its rationale live in one place, the header
comment of `ftdetect/z33.vim`). If you'd rather stop polyglot from claiming
`.s`/`.S` at the source, set `let g:polyglot_disabled = ['r-lang']` before it
loads.
