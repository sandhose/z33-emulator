# Z33 for Neovim

A Neovim-first plugin for the [Zorglub-33 (Z33)](https://github.com/sandhose/z33-emulator)
educational assembly language. It provides:

- **Filetype detection** for `.s` / `.S` files (with a content heuristic so real
  GNU-asm `.s` files keep their `asm` filetype).
- **Tree-sitter** highlighting, indentation and folding (via
  `nvim-treesitter`), with queries shipped for the `z33` grammar.
- **Native LSP** (Neovim 0.11+) wired to `z33-cli lsp` — diagnostics,
  completion, hover, go-to-definition, references, rename, symbols and code lens.
- **Debugging** via [`nvim-dap`](https://github.com/mfussenegger/nvim-dap),
  driving `z33-cli dap`.
- **Automatic `z33-cli` download** from GitHub releases (with consent), so you
  do not have to install the binary manually.

Everything is optional and guarded: with neither `nvim-treesitter` nor
`nvim-dap` installed, the plugin still loads cleanly and gives you filetype
detection + native LSP.

> Requires **Neovim 0.11+** for the native LSP. On older Neovim the plugin still
> works for everything else; see the [nvim-lspconfig fallback](#pre-011--nvim-lspconfig-fallback).

## Installation

The plugin lives in the `editors/nvim/` **subdirectory** of the monorepo. That
subdirectory — not the repository root — must be on your `runtimepath`.

### lazy.nvim

> **Why `"sandhose/z33-emulator"` alone does not work:** lazy.nvim puts the
> *repository root* on the runtimepath and has no option to point at a
> subdirectory (unlike vim-plug/packer's `rtp`). The plugin files live under
> `editors/nvim/`, which the root spec never exposes. Use the `dir =` recipe
> against a local clone instead:

```lua
-- One time, clone the repo somewhere stable:
--   git clone https://github.com/sandhose/z33-emulator \
--     ~/.local/share/nvim/z33-emulator
{
  dir = vim.fn.stdpath("data") .. "/z33-emulator/editors/nvim",
  name = "z33",
  -- plugin/z33.lua self-initializes; `opts`/`config` are only for tweaks.
}
```

If you already have a checkout, point `dir =` at `.../editors/nvim` inside it
(or symlink it).

### vim-plug

```vim
Plug 'sandhose/z33-emulator', { 'rtp': 'editors/nvim' }
```

### packer.nvim

```lua
use { 'sandhose/z33-emulator', rtp = 'editors/nvim' }
```

### Native packages (`:h packages`)

Clone the repo and symlink the `editors/nvim` directory into a package:

```sh
git clone https://github.com/sandhose/z33-emulator ~/src/z33-emulator
mkdir -p ~/.local/share/nvim/site/pack/z33/start
ln -s ~/src/z33-emulator/editors/nvim \
      ~/.local/share/nvim/site/pack/z33/start/z33
```

## The `z33-cli` binary

The LSP and debugger both need the `z33-cli` binary. Resolution order:

1. `z33-cli` on your `PATH` (always preferred).
2. A previously downloaded copy under `stdpath('data')/z33/z33-cli-<tag>/`.
3. An on-demand download from
   [GitHub releases](https://github.com/sandhose/z33-emulator/releases)
   (asked for consent the first time it is needed).

Control the download behaviour with `vim.g.z33_auto_download`:

| Value | Behaviour |
|-------|-----------|
| _unset_ (default) | Prompt once before downloading. |
| `true` | Download without prompting. |
| `false` | Never download; use `PATH`/cache only. |

Run **`:Z33Download`** to pre-fetch the binary at any time. Downloads never
block startup and never happen at plugin load — only on first actual LSP/DAP
use. On macOS the quarantine attribute is stripped best-effort; the binaries are
unsigned.

## Configuration

All configuration is via global variables — set them **before** the plugin
loads (e.g. at the top of your `init.lua`):

```lua
vim.g.z33_filetypes = true    -- force every .s/.S to filetype=z33 (skip the heuristic)
vim.g.z33_auto_download = true -- download z33-cli without prompting
```

| Variable | Effect |
|----------|--------|
| `vim.g.z33_filetypes` | Force **all** `.s`/`.S` files to `z33` (no content heuristic). |
| `vim.g.z33_no_ftdetect` | Disable the `.s`/`.S` filetype heuristic entirely (including the force-override autocmd). |
| `vim.g.z33_auto_download` | Download consent tri-state (see above). |
| `vim.g.z33_no_treesitter_start` | Disable auto-starting tree-sitter highlighting on `z33` buffers. |

### lazy.nvim `opts`

If you configure via lazy.nvim's `opts = { ... }`, the keys `filetypes`,
`no_ftdetect` and `auto_download` are mapped onto the matching `vim.g.z33_*`
variables for you:

```lua
{
  dir = vim.fn.stdpath("data") .. "/z33-emulator/editors/nvim",
  name = "z33",
  opts = { auto_download = true, filetypes = false },
}
```

An explicit `vim.g.z33_*` always wins over the corresponding `opts` key (opts
only fill variables you did not set yourself).

You can also force a single file with a modeline (Z33 uses `//` comments):

```
// vim: ft=z33
```

### Filetype fights over `.s` / `.S` (e.g. vim-polyglot → `r`)

`.s`/`.S` is a contested extension. [vim-polyglot](https://github.com/sheerun/vim-polyglot)
bundles an `r-lang` package that maps `.s`/`.S` to R (`filetype=r` — legacy
S-PLUS used `.s`), and Vim's builtin maps them to `asm`. Both use `:setf`, which
silently no-ops once a filetype is set during detection, so depending on load
order they can win over this plugin's `vim.filetype.add` matcher (symptom:
opening a Z33 file gives `filetype=r`).

To fix it, the plugin ships an `ftdetect/z33.lua` that **force-overrides** to
`z33` — but only when the buffer content looks like Z33 *and* the current
filetype is one of the known `.s`/`.S` claimants (`""`, `asm`, `r`). A GNU-asm
`.s` file keeps its `asm`/`r` filetype, and an explicit modeline or your own
autocmd still wins (both run after ftdetect).

If you would rather stop vim-polyglot from claiming `.s`/`.S` at the source, set
`let g:polyglot_disabled = ['r-lang']` before it loads.

## Tree-sitter

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

Highlighting starts automatically for `z33` buffers once the parser is present —
on `main` too, where nvim-treesitter does not otherwise auto-start it (the plugin
calls `vim.treesitter.start()` for you via a `FileType z33` autocmd; it is a
no-op until the parser is installed). Disable this with
`vim.g.z33_no_treesitter_start = true`.

## LSP

On Neovim 0.11+ the server is enabled automatically for `z33` buffers via the
native `vim.lsp.enable` mechanism (config in `lsp/z33.lua`). Nothing to wire.

> The server offers an informational `▶ Run <label>` code lens; the plugin does
> not advertise the `experimental.commands` capability in v1, so use **nvim-dap**
> (below) to actually run/debug.

### Pre-0.11 / nvim-lspconfig fallback

If you are on Neovim < 0.11, or prefer `nvim-lspconfig`, register the server
manually (the plugin ships nothing for this path):

```lua
require("lspconfig.configs").z33 = {
  default_config = {
    cmd = { "z33-cli", "lsp" },
    filetypes = { "z33" },
    -- `lspconfig.util.find_git_ancestor` was removed from recent
    -- nvim-lspconfig; use the version-robust vim.fs form instead.
    root_dir = function(fname)
      return vim.fs.dirname(vim.fs.find(".git", { path = fname, upward = true })[1])
        or vim.fs.dirname(fname)
    end,
  },
}
require("lspconfig").z33.setup({})
```

(This path needs `z33-cli` on your `PATH`; auto-download only backs the native
LSP/DAP integrations.)

## Debugging (nvim-dap)

With `nvim-dap` installed, a `z33` launch configuration is registered
automatically. Open a `.s` file and:

```lua
require("dap").continue()
```

Pick **“Launch Z33 program”**; you will be prompted for an entrypoint label
(default `main`). The current file is used as the program and execution stops on
entry. Standard nvim-dap keymaps (`toggle_breakpoint`, `step_over`, `step_into`,
…) work as usual.

## Health check

Run **`:checkhealth z33`** to verify your setup: Neovim version, whether
`z33-cli` is on `PATH`/cached, platform download support, and whether
nvim-treesitter (+ the `z33` parser) and nvim-dap are installed.

## Using the classic Vim plugin instead

The sibling `editors/vim/` plugin is pure Vimscript and also loads in Neovim,
giving syntax highlighting + filetype settings without any of the above. Prefer
this `editors/nvim/` plugin on Neovim for tree-sitter, LSP and DAP.
