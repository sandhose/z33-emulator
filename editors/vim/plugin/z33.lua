-- Entry point for the Z33 (Zorglub-33) Neovim plugin.
--
-- This file is sourced once at startup and must stay cheap: it only calls the
-- idempotent `setup()`, which registers filetype detection, the tree-sitter
-- parser config, native LSP, and (if present) nvim-dap. Nothing here downloads
-- anything or blocks the event loop — CLI resolution is fully lazy.

if vim.g.loaded_z33 then
  return
end
vim.g.loaded_z33 = true

-- setup() is idempotent; running it again from a user's config is harmless.
require("z33").setup()
