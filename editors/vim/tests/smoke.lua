-- CI smoke test for the Neovim side of the Z33 plugin
-- (.github/workflows/check.yaml, job `editors-vim-nvim`). Not a real test suite
-- (no plugin-manager/CI Lua-test framework is set up for this small plugin) —
-- just enough to catch "plugin doesn't even load" regressions: `setup()` must
-- be idempotent and must not error, the `.s`/`.S` content heuristic must agree
-- with the fixture corpus in `tests/fixtures/` (`z33/` claimed, `asm/` left to
-- the builtin asm detection — the classic-Vim copy of the heuristic is run
-- over the same corpus in CI), and — with no tree-sitter parser present (the
-- CI case) — a z33 buffer must fall back to the bundled Vimscript syntax
-- (syntax/z33.vim, shared with classic Vim).
--
-- Run from the repository root with:
--   nvim --headless -u NONE -N -l editors/vim/tests/smoke.lua

vim.opt.rtp:prepend(vim.fn.getcwd() .. "/editors/vim")

-- The first z33 buffer triggers the z33-cli download consent prompt when no
-- binary is on PATH. Headless with stdin at EOF, that prompt makes Neovim exit
-- with status 0 before any assertion below runs, so forbid downloads up front.
vim.g.z33_auto_download = false

local z33 = require("z33")

-- setup() is documented as idempotent and safe to call more than once (see
-- plugin/z33.lua) — calling it twice here guards that contract.
z33.setup()
z33.setup()

-- Line-level cases the fixture corpus does not spell out: the Z33 vs GNU
-- spellings that are one character apart, and the GNU mnemonics/registers that
-- must not count as positives.
local ftdetect = require("z33.ftdetect")
for _, case in ipairs({
  { "#undefine FOO", "z33" },
  { "#undef FOO", "gnu" },
  { "#  ifdef X", "gnu" },
  { '#include "lib.s"', "z33" },
  { "#include <stdio.h>", "gnu" },
  { "  .text", "gnu" },
  { ".addr 100", "z33" },
  { ".word 1", nil },
  { "    push %a", "z33" },
  { "    movl %ax, %bx", nil },
  { "    pushq %rbp", nil },
  { "main: reset", "z33" },
  { "  RESET", "z33" },
  { "  resetting", nil },
  { "    jle done", nil },
  { "    jeq done", "z33" },
  { "    call foo", nil },
}) do
  local got = ftdetect.line_signal(case[1])
  assert(
    got == case[2],
    ("line_signal(%q) = %s, expected %s"):format(case[1], tostring(got), tostring(case[2]))
  )
end

-- End to end through a real `:edit`, where the `vim.filetype.add` matcher and
-- the force-override autocmd run on a buffer load instead of being called
-- directly. (`filetype on` is a no-op under `nvim -l`, which has already
-- loaded the filetype machinery; it is here for other harnesses.) The negative
-- case only asserts "not z33" — a third-party `.s` claimant installed on the
-- host (vim-polyglot's r-lang) can legitimately win there; the exact builtin
-- answer is asserted below.
pcall(vim.cmd, "filetype on")
for _, case in ipairs({
  { "editors/vim/tests/fixtures/z33/no_registers.s", true },
  { "editors/vim/tests/fixtures/asm/kernel.S", false },
}) do
  local path, want_z33 = case[1], case[2]
  vim.cmd.edit(path)
  local ft = vim.bo.filetype
  assert(
    (ft == "z33") == want_z33,
    ("%s: filetype %s after :edit, expected %sz33"):format(path, ft, want_z33 and "" or "not ")
  )
end

-- Filetype heuristic. Every file under `tests/fixtures/<ft>/` must come out as
-- `<ft>`, and every sample must be z33. Checked twice: through the module (what
-- the force-override autocmd calls) and end to end through `vim.filetype.match`
-- (Neovim's own detection, which must hand non-Z33 files back to the builtin
-- asm detection rather than leaving them filetype-less).
local function check(path, expected)
  local buf = vim.fn.bufadd(path)
  vim.fn.bufload(buf)
  local direct = ftdetect.detect(buf)
  local want_direct = expected == "z33" and "z33" or nil
  assert(
    direct == want_direct,
    ("%s: ftdetect.detect returned %s, expected %s"):format(path, tostring(direct), tostring(want_direct))
  )
  local ft = vim.filetype.match({ buf = buf, filename = path })
  assert(ft == expected, ("%s: filetype %s, expected %s"):format(path, tostring(ft), expected))
end

local files = {}
for _, expected in ipairs({ "z33", "asm" }) do
  for _, path in ipairs(vim.fn.glob("editors/vim/tests/fixtures/" .. expected .. "/*", false, true)) do
    files[path] = expected
  end
end
for _, path in ipairs(vim.fn.glob("samples/*.s", false, true)) do
  files[path] = "z33"
end

local checked = 0
for path, expected in pairs(files) do
  check(path, expected)
  checked = checked + 1
end
assert(checked >= 10, "expected the fixture corpus to be found, only checked " .. checked .. " files")

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

print("OK: nvim smoke test passed (" .. checked .. " filetype cases, highlighting=" .. mode .. ")")
