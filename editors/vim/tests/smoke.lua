-- CI smoke test for the Neovim side of the Z33 plugin
-- (.github/workflows/check.yaml, job `editors-vim-nvim`). Not a real test suite
-- (no plugin-manager/CI Lua-test framework is set up for this small plugin) —
-- just enough to catch "plugin doesn't even load" regressions: `setup()` must
-- be idempotent and must not error, the `.s`/`.S` filetype heuristic must
-- actually detect a real Z33 sample, and — with no tree-sitter parser present
-- (the CI case) — a z33 buffer must fall back to the bundled Vimscript syntax
-- (syntax/z33.vim, shared with classic Vim).
--
-- Run from the repository root with:
--   nvim --headless -u NONE -N -l editors/vim/tests/smoke.lua

vim.opt.rtp:prepend(vim.fn.getcwd() .. "/editors/vim")

local z33 = require("z33")

-- setup() is documented as idempotent and safe to call more than once (see
-- plugin/z33.lua) — calling it twice here guards that contract.
z33.setup()
z33.setup()

local bufnr = vim.fn.bufadd("samples/fact.s")
vim.fn.bufload(bufnr)

local ft = vim.filetype.match({ buf = bufnr, filename = "fact.s" })
assert(ft == "z33", "expected filetype z33 for samples/fact.s, got: " .. tostring(ft))

-- Highlighting: with no tree-sitter parser installed (the CI case), a z33
-- buffer must fall back to the bundled Vimscript syntax (syntax/z33.vim, shared
-- with classic Vim), i.e. b:current_syntax == "z33". With a parser present,
-- vim.treesitter.start takes over instead (it sets b:ts_highlight and blanks
-- 'syntax', so the legacy syntax is disabled per-buffer — no double highlight).
-- Branch on the parser so this is honest in both environments; CI has none, so
-- it exercises the regex fallback there.
-- `syntax on` can raise a benign E495 ("<afile>") while running its
-- `doautoall` over the initial unnamed buffer in headless mode; the syntax
-- machinery (the `syntaxset` FileType autocmd) is installed before that point,
-- so swallow it. On a normal CI checkout it does not even fire.
pcall(vim.cmd, "syntax on")
local sbuf = vim.api.nvim_create_buf(true, false)
-- Give it a name: some `*` autocmds installed by `syntax on`/`filetype on`
-- expand `<afile>` and error on a nameless buffer in headless mode.
vim.api.nvim_buf_set_name(sbuf, vim.fn.tempname() .. ".s")
vim.api.nvim_set_current_buf(sbuf)
vim.bo[sbuf].filetype = "z33"

local parser_ok, added = pcall(vim.treesitter.language.add, "z33")
local have_parser = parser_ok and added == true
local mode
if have_parser then
  -- Parser present: the FileType autostart must have started the TS highlighter.
  assert(
    vim.treesitter.highlighter.active[sbuf] ~= nil,
    "parser is installed but tree-sitter highlighter did not start on the z33 buffer"
  )
  mode = "tree-sitter"
else
  -- No parser (CI): the bundled regex syntax must be loaded.
  local cur = vim.b[sbuf].current_syntax
  assert(cur == "z33", "expected regex-fallback syntax z33, got: " .. tostring(cur))
  mode = "regex fallback (b:current_syntax=z33)"
end

print("OK: nvim smoke test passed (filetype=" .. tostring(ft) .. ", highlighting=" .. mode .. ")")
