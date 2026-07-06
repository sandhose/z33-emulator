-- ftdetect/z33.lua — force filetype=z33 when a third-party `.s`/`.S` rule wins.
--
-- Neovim sources every `ftdetect/*.lua` on the runtimepath during filetype
-- detection setup. Our `vim.filetype.add` matcher (in `z33/init.lua`) is the
-- idiomatic path, but it can be beaten in a real config: vim-polyglot's
-- `r-lang` package registers `au BufNewFile,BufRead *.s,*.S setf r` (legacy
-- S-PLUS used `.s`), and Vim's builtin registers `setf asm`. Because `:setf`
-- no-ops once a filetype has been set during the detection sequence, whichever
-- of those runs relative to our matcher can shadow it (observed: polyglot wins,
-- yielding filetype=r for Z33 files).
--
-- The actual override autocmd lives in `z33/ftdetect.lua` (`arm_force_override`)
-- and is idempotent, because `setup()` also arms it: some hosts (e.g.
-- home-manager-managed Neovim) source a package's `plugin/` scripts but not its
-- `ftdetect/` scripts, so this file alone is not enough to be robust.
--
-- Controls (documented in the README): `vim.g.z33_no_ftdetect` disables the
-- override; `vim.g.z33_filetypes` forces every `.s`/`.S` to z33.

if vim.g.z33_no_ftdetect then
  return
end

require("z33.ftdetect").arm_force_override()
