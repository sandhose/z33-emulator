-- Neovim 0.11+ native LSP configuration for the Z33 language server.
--
-- This file lives on the runtimepath at `lsp/z33.lua`; it is picked up by
-- `vim.lsp.config`/`vim.lsp.enable("z33")`. Crucially, `require('z33').setup()`
-- only ever calls `vim.lsp.enable("z33")` once a `z33-cli` binary is actually
-- resolvable (PATH or cache), or after a successful download re-enables it.
-- That invariant means `cmd` below can never legitimately run without a binary,
-- so it asserts rather than returning nil — a nil `cmd` return crashes nvim's
-- native LSP core (`attempt to index local 'rpc'`) and leaves a zombie client
-- in the registry that poisons every later retry.

return {
  cmd = function(dispatchers)
    local path = require("z33").cli_path()
    -- Unreachable: setup() gates `vim.lsp.enable("z33")` on a resolvable
    -- binary. Assert with a clear message instead of returning nil (which
    -- would crash the LSP core) if the invariant is ever violated.
    assert(
      path,
      "z33: LSP was started without a resolvable z33-cli binary; run :Z33Download or add z33-cli to PATH"
    )
    return vim.lsp.rpc.start({ path, "lsp" }, dispatchers)
  end,
  filetypes = { "z33" },
  -- Prefer a `.git` root; with no marker, nvim core attaches to the buffer's
  -- own directory (single-file mode), so a lone scratch file still works.
  root_markers = { ".git" },
}
