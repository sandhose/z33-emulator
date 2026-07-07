-- Tree-sitter parser registration for the Z33 grammar.
--
-- The grammar lives at the `tree-sitter-z33/` subpath of this repo with a
-- committed `src/parser.c` (no `tree-sitter generate` step needed). We register
-- an install spec with BOTH nvim-treesitter branches:
--
--   * master (stable): `parsers.get_parser_configs()` returns a mutable table;
--     we add a `z33` entry to it.
--   * main (rewrite): `require("nvim-treesitter.parsers")` IS the config table
--     (there is no `get_parser_configs`). The module is *reloaded* on every
--     `:TSInstall`/`:TSUpdate` (`reload_parsers()` clears `package.loaded` and
--     re-requires it, then fires `User TSUpdate`), which wipes a direct
--     mutation — so registration must be (re)done inside a `User TSUpdate`
--     autocmd, the documented hook. We also seed it once for the current
--     session in case highlighting/`get_available` is queried before any
--     reload.
--
-- Either way `:TSInstall z33` / `require("nvim-treesitter").install("z33")`
-- downloads this repo, `cd`s into `tree-sitter-z33`, and compiles the committed
-- `src/parser.c`. The highlight/indent/fold queries under `queries/z33/` on
-- this plugin's runtimepath make highlighting work once the parser is present.

local M = {}

-- Shared install spec. Same repo/subdir for both branches. No `revision` (track
-- the default branch) and no `generate` (the repo ships a committed
-- `src/parser.c`, so the build step is a plain compile). `files` is used by the
-- master-branch installer and harmlessly ignored on main.
local function install_info()
  return {
    url = "https://github.com/sandhose/z33-emulator",
    -- Grammar lives in a subdirectory of the monorepo.
    location = "tree-sitter-z33",
    files = { "src/parser.c" },
  }
end

-- master branch: `get_parser_configs()` returns a mutable table we add to.
local function register_master(parsers)
  local configs = parsers.get_parser_configs()
  if not configs.z33 then
    configs.z33 = {
      install_info = install_info(),
      filetype = "z33",
    }
  end
end

-- main branch: (re)register on every reload via the `User TSUpdate` hook, plus
-- once now for the current session.
local function register_main()
  local function add()
    local ok, parsers = pcall(require, "nvim-treesitter.parsers")
    -- Guard against the master shape in case both somehow coexist.
    if ok and type(parsers) == "table" and parsers.get_parser_configs == nil then
      if not parsers.z33 then
        parsers.z33 = { install_info = install_info() }
      end
    end
  end

  vim.api.nvim_create_autocmd("User", {
    pattern = "TSUpdate",
    desc = "z33: register the z33 tree-sitter parser (nvim-treesitter main)",
    callback = add,
  })
  -- Seed the current session; `reload_parsers()` will call `add` again via the
  -- autocmd on the next :TSInstall/:TSUpdate.
  add()
end

-- Auto-start tree-sitter highlighting for `z33` buffers once the parser exists.
-- nvim-treesitter's `main` branch does NOT auto-start highlighting (users call
-- `vim.treesitter.start()` themselves); `master`'s module attaches on FileType.
-- Starting an already-active highlighter is a no-op, so this is safe on both.
-- Opt out with `vim.g.z33_no_treesitter_start`. No-ops (no error) when the
-- parser is absent, so a z33 buffer before `:TSInstall z33` is harmless.
--
-- Interaction with the bundled regex syntax (syntax/z33.vim, shared with the
-- classic-Vim side of this plugin): this needs NO special handling here, and it
-- is deliberately left to Neovim core. On the `FileType z33` event two autocmds
-- fire — core's `syntaxset` (`au FileType * if !exists('b:ts_highlight') | set
-- syntax=<ft>`, which loads syntax/z33.vim) and this one. `vim.treesitter.start`
-- BOTH sets `b:ts_highlight = true` AND blanks `syntax` to `''` (clearing any
-- regex syntax already loaded). So regardless of which autocmd runs first, the
-- end state with a parser is TS-only (no double highlight); without a parser the
-- probe below returns early, `b:ts_highlight` is never set, and core's syntaxset
-- autocmd loads the regex fallback (`b:current_syntax == 'z33'`). Both branches
-- verified empirically on Neovim 0.12.
local function register_autostart()
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "z33",
    desc = "z33: start tree-sitter highlighting when the parser is available",
    callback = function(args)
      if vim.g.z33_no_treesitter_start then
        return
      end
      -- Availability probe: `language.add` loads the parser if installed and
      -- returns true, or errors / returns false otherwise (caught either way).
      local ok, added = pcall(vim.treesitter.language.add, "z33")
      if not ok or added == false then
        return
      end
      pcall(vim.treesitter.start, args.buf, "z33")
    end,
  })
end

function M.register()
  -- Map the `z33` filetype to the `z33` tree-sitter language (identity here,
  -- but this is what `vim.treesitter.start()` and query lookups consult).
  pcall(vim.treesitter.language.register, "z33", "z33")

  -- Convenience: highlighting Just Works on both branches once the parser is
  -- installed (independent of whether nvim-treesitter is present at all).
  pcall(register_autostart)

  local ok, parsers = pcall(require, "nvim-treesitter.parsers")
  if not ok then
    -- nvim-treesitter not installed; queries on rtp still work if the parser is
    -- installed by other means.
    return
  end

  if type(parsers.get_parser_configs) == "function" then
    pcall(register_master, parsers)
  else
    pcall(register_main)
  end
end

return M
