-- Shared `.s` / `.S` filetype heuristic for the Z33 vs GNU-asm collision.
--
-- This is the ONE copy of the content heuristic. It backs two mechanisms:
--   1. `vim.filetype.add` (wired in `z33/init.lua`) — the idiomatic matcher,
--      which also feeds `vim.filetype.match` consumers and works under
--      `nvim --clean` where no third-party rule fights over `.s`/`.S`.
--   2. `ftdetect/z33.lua` — a force-override autocmd for the real world, where
--      vim-polyglot's `r-lang` package (`au ... setf r`, legacy S-PLUS used
--      `.s`) or Vim's builtin (`setf asm`) claim `.s`/`.S` first. `setf` no-ops
--      once a filetype is set in the detection sequence, so those rules can
--      shadow our matcher; a forced `setlocal`-equivalent wins regardless of
--      ordering.
--
-- Signals are deliberately narrowed to ones that effectively never appear in
-- GNU/other asm sharing the `.s`/`.S` extension, so a real GNU file is not
-- misdetected as Z33.

local M = {}

-- Preprocessor keywords whose presence strongly implies Z33 (note `#undefine`,
-- not the C-style `#undef`; `#` may be followed by spaces).
local PREPROC = { "include", "define", "undefine", "if", "elif", "endif", "error" }
-- Z33-specific assembler directives. Only `.addr` — `.word`/`.space`/`.string`
-- are standard GNU `as` directives too, so they were dropped to avoid
-- misdetecting GNU sources (a bare `.word 5` file is not Z33).
local DIRECTIVES = { "addr" }
-- Z33 registers — a strong discriminator: GNU asm uses `%eax`/`%rdi`/… and
-- never `%pc`/`%sr`. `%sp` is excluded because it is a real x86 AT&T register.
-- The greedy `%a+` capture below word-bounds these, so `%a`/`%b` cannot fire
-- inside `%ax`/`%bp`.
local REGISTERS = { "a", "b", "pc", "sr" }

--- Returns true when a single source line looks distinctively like Z33.
function M.line_is_z33(line)
  -- `#<spaces>keyword` preprocessor directive.
  local kw = line:match("^%s*#%s*(%a+)")
  if kw then
    for _, k in ipairs(PREPROC) do
      if kw == k then
        return true
      end
    end
  end
  -- `.directive` at line start.
  local dir = line:match("^%s*%.(%a+)")
  if dir then
    for _, d in ipairs(DIRECTIVES) do
      if dir == d then
        return true
      end
    end
  end
  -- A `%reg` register anywhere (word-bounded so `%pc` matches but `%pcx`
  -- does not). Matched case-insensitively.
  for reg in line:gmatch("%%(%a+)") do
    local lreg = reg:lower()
    for _, r in ipairs(REGISTERS) do
      if lreg == r then
        return true
      end
    end
  end
  return false
end

--- Scans the head of a buffer and returns true if it looks like Z33.
function M.buf_is_z33(bufnr)
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, 64, false)
  for _, line in ipairs(lines) do
    if M.line_is_z33(line) then
      return true
    end
  end
  return false
end

--- Decides the filetype for a `.s` / `.S` buffer. Returns "z33" or nil (fall
--- through to whatever else claims the buffer). Honors the config flags:
---   - `vim.g.z33_no_ftdetect` — disable the heuristic entirely.
---   - `vim.g.z33_filetypes`   — force every `.s`/`.S` buffer to z33.
--- @param bufnr integer
--- @return string|nil
function M.detect(bufnr)
  if vim.g.z33_no_ftdetect then
    return nil
  end
  if vim.g.z33_filetypes then
    return "z33"
  end
  if M.buf_is_z33(bufnr) then
    return "z33"
  end
  return nil
end

-- Only these filetypes are candidates for override: a fresh buffer, or the two
-- known `.s`/`.S` claimants (Vim builtin → `asm`, vim-polyglot `r-lang` → `r`).
-- Anything else means a more specific/confident detector already spoke — leave
-- it alone.
local OVERRIDABLE = { [""] = true, asm = true, r = true }

local armed = false

--- Installs the `BufRead`/`BufNewFile` force-override autocmd (idempotent).
---
--- Called from BOTH `ftdetect/z33.lua` (the idiomatic vehicle, sourced under a
--- standard package layout / `--clean`) and `setup()` — because some configs
--- (e.g. home-manager-managed Neovim) source a package's `plugin/` scripts but
--- NOT its `ftdetect/` scripts, so the ftdetect file alone is not enough to be
--- robust. The `armed` guard means running both is harmless (one autocmd).
function M.arm_force_override()
  if armed then
    return
  end
  armed = true
  vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
    pattern = { "*.s", "*.S" },
    desc = "z33: force filetype=z33 when a .s/.S buffer looks like Z33",
    callback = function(args)
      -- `vim.g.z33_no_ftdetect` is re-checked here (not just at arm time) so a
      -- late-set flag still disables the override.
      if vim.g.z33_no_ftdetect then
        return
      end
      if not OVERRIDABLE[vim.bo[args.buf].filetype] then
        return
      end
      if M.detect(args.buf) == "z33" then
        -- Buffer-local, forced: wins over any competing `:setf` regardless of
        -- ordering (`:setf` no-ops once a filetype is set; a direct assign does
        -- not). A trailing modeline still wins, as modelines run after ftdetect.
        vim.bo[args.buf].filetype = "z33"
      end
    end,
  })
end

return M
