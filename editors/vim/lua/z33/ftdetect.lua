-- The `.s` / `.S` filetype heuristic for the Z33 vs GNU-asm collision.
--
-- The Neovim copy of the heuristic (classic Vim has its own in
-- `ftdetect/z33.vim`; CI runs both over `tests/fixtures/`). It backs two
-- mechanisms:
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
-- A GNU or Plan 9 marker anywhere in the scanned window vetoes detection, so
-- the positives below need only be things those assemblers do not routinely
-- write.

local M = {}

-- Anti-signals. Indentation and case are tolerated on all of them: they
-- describe foreign files, not what the Z33 parser accepts.
--
-- C-preprocessor directives GNU uses and Z33 does not: Z33 has `#if` and
-- `#undefine`, no `#ifdef`/`#ifndef`/`#undef`/`#pragma`, and its `#include`
-- takes a quoted string (parser/preprocessor.rs), never `<...>`.
local ANTI_PREPROC = { ifdef = true, ifndef = true, undef = true, pragma = true }
-- GNU `as` directives; Z33 only has `.addr`, `.space`, `.string` and `.word`.
local ANTI_DIRECTIVES = {
  globl = true,
  global = true,
  section = true,
  type = true,
  size = true,
  text = true,
  data = true,
  macro = true,
  endm = true,
}
-- Plan 9 / Go assembly openers, matched case-sensitively because that dialect
-- spells them in upper case. Go's `.s` files are otherwise mistaken for Z33:
-- `#include "textflag.h"` is a Z33 positive and `JEQ`/`JLT`/`JGT` are Plan 9
-- mnemonics.
local ANTI_PLAN9 = { TEXT = true, DATA = true, GLOBL = true, FUNCDATA = true, PCDATA = true }

-- Preprocessor keywords Z33 shares with C, hence only trusted because any
-- anti-signal vetoes the whole file. `#include` counts only with a quoted
-- argument. Matched at column 0 and in lower case, the only spelling
-- parser/preprocessor.rs accepts.
local WEAK_PREPROC = { define = true, ["if"] = true, elif = true, endif = true, error = true }
-- Z33-only spellings and directives.
local STRONG_PREPROC = { undefine = true }
local DIRECTIVES = { addr = true }
-- Z33 registers: GNU asm uses `%eax`/`%rdi`/… and never `%pc`/`%sr`. `%sp` is
-- excluded — it is a real x86 AT&T register. `%a0`…`%a7` (m68k) are not
-- registers to the parser either, so the captures below take a whole
-- identifier and look it up, rather than matching a prefix.
local REGISTERS = { a = true, b = true, pc = true, sr = true }
-- Z33 mnemonics rare enough in GNU asm to carry weight. m68k spells
-- `reset`/`rti`/`swap`/`trap` the same way, but a real m68k file also carries
-- a `.text`/`.globl`/`.section` veto.
local MNEMONICS = {
  fas = true,
  rti = true,
  rtn = true,
  swap = true,
  reset = true,
  trap = true,
  jeq = true,
  jlt = true,
  jgt = true,
}

--- Classifies one source line: "z33" for a Z33 signal, "gnu" for a marker that
--- rules Z33 out, nil for neither. Every capture takes a whole `[0-9A-Za-z_]`
--- word, so the Vimscript copy's `\>` word boundaries mean the same thing here.
--- @param line string
--- @return string|nil
function M.line_signal(line)
  -- Anti-signals first: one of them on a line outweighs anything else on it.
  local anti_kw, anti_rest = line:match("^%s*#%s*([%w_]+)(.*)$")
  if anti_kw then
    anti_kw = anti_kw:lower()
    if ANTI_PREPROC[anti_kw] then
      return "gnu"
    end
    if anti_kw == "include" and anti_rest:match("^%s*<") then
      return "gnu"
    end
  end
  local anti_dir = line:match("^%s*%.([%w_]+)")
  if anti_dir and ANTI_DIRECTIVES[anti_dir:lower()] then
    return "gnu"
  end
  -- Plan 9: `TEXT ·name(SB)`, `GLOBL sym(SB)`, `PCDATA $0, $1`. `(SB)` is the
  -- dialect's static-base pseudo-register and appears on every symbol
  -- reference.
  local plan9 = line:match("^(%u+)[ \t]") or line:match("^(%u+)·")
  if (plan9 and ANTI_PLAN9[plan9]) or line:find("(SB)", 1, true) then
    return "gnu"
  end

  -- Z33 preprocessor: `#` in column 0, lower-case keyword.
  local kw, rest = line:match("^#%s*([%w_]+)(.*)$")
  if kw then
    if kw == "include" then
      return rest:match('^%s*"') and "z33" or nil
    end
    if STRONG_PREPROC[kw] or WEAK_PREPROC[kw] then
      return "z33"
    end
  end
  -- `.directive` at line start. Directives, mnemonics and registers are all
  -- case-insensitive to the parser.
  local dir = line:match("^%s*%.([%w_]+)")
  if dir and DIRECTIVES[dir:lower()] then
    return "z33"
  end
  -- A mnemonic opening an instruction, with or without a leading `label:`.
  local labelled = line:match("^%s*[%w_]+%s*:%s*([%w_]+)")
  local bare = line:match("^%s*([%w_]+)")
  if (labelled and MNEMONICS[labelled:lower()]) or (bare and MNEMONICS[bare:lower()]) then
    return "z33"
  end
  -- A `%reg` register anywhere.
  for reg in line:gmatch("%%([%w_]+)") do
    if REGISTERS[reg:lower()] then
      return "z33"
    end
  end
  return nil
end

--- Scans the head of a buffer and returns true if it looks like Z33. One
--- anti-signal vetoes the whole buffer, whatever was found before it.
function M.buf_is_z33(bufnr)
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, 64, false)
  local found = false
  for _, line in ipairs(lines) do
    local signal = M.line_signal(line)
    if signal == "gnu" then
      return false
    elseif signal == "z33" then
      found = true
    end
  end
  return found
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
