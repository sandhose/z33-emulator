-- CI smoke test for the Z33 Neovim plugin (.github/workflows/check.yaml,
-- job `editors-vim-nvim`). Not a real test suite (no plugin-manager/CI
-- Lua-test framework is set up for this small plugin) — just enough to catch
-- "plugin doesn't even load" regressions: `setup()` must be idempotent and
-- must not error, and the `.s`/`.S` filetype heuristic must actually detect a
-- real Z33 sample.
--
-- Run from the repository root with:
--   nvim --headless -u NONE -N -l editors/nvim/tests/smoke.lua

vim.opt.rtp:prepend(vim.fn.getcwd() .. "/editors/nvim")

local z33 = require("z33")

-- setup() is documented as idempotent and safe to call more than once (see
-- plugin/z33.lua) — calling it twice here guards that contract.
z33.setup()
z33.setup()

local bufnr = vim.fn.bufadd("samples/fact.s")
vim.fn.bufload(bufnr)

local ft = vim.filetype.match({ buf = bufnr, filename = "fact.s" })
assert(ft == "z33", "expected filetype z33 for samples/fact.s, got: " .. tostring(ft))

print("OK: nvim smoke test passed, filetype=" .. tostring(ft))
