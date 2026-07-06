-- Public API + one-time wiring for the Z33 Neovim plugin.
--
-- `setup()` is idempotent and cheap: it registers filetype detection, the
-- tree-sitter parser config, native LSP, and (when present) nvim-dap. The
-- `z33-cli` binary is resolved lazily through `cli_path` / `cli_cmd` /
-- `cli_path_async` — nothing downloads or blocks at startup.

local M = {}

-- ---------------------------------------------------------------------------
-- CLI path resolution
-- ---------------------------------------------------------------------------

--- Sync resolution: a binary on PATH always wins, else the newest cached
--- download, else nil. Never triggers a download.
--- @return string|nil
function M.cli_path()
  local on_path = vim.fn.exepath("z33-cli")
  if on_path ~= "" then
    return on_path
  end
  return require("z33.download").cached_binary()
end

--- Builds the `{ path, subcmd }` argv for the given subcommand, or nil if no
--- binary is available. Purely synchronous: it never triggers a download.
--- `setup()` guarantees the native LSP is only enabled once this resolves, so
--- the `lsp/z33.lua` `cmd` function can treat a nil return as a hard bug.
--- @param subcmd string
--- @return string[]|nil
function M.cli_cmd(subcmd)
  local path = M.cli_path()
  if path then
    return { path, subcmd }
  end
  return nil
end

--- Async resolution: resolves a usable binary path (downloading with consent if
--- necessary) and calls `cb(path_or_nil)` on the main loop. Used by the DAP
--- adapter.
--- @param cb fun(path: string|nil)
function M.cli_path_async(cb)
  local path = M.cli_path()
  if path then
    return cb(path)
  end
  require("z33.download").ensure(cb)
end

-- ---------------------------------------------------------------------------
-- Filetype detection (the `.s` / `.S` vs GNU-asm collision)
-- ---------------------------------------------------------------------------

-- The content heuristic lives in `z33/ftdetect.lua` — the single copy shared by
-- this `vim.filetype.add` matcher AND the `ftdetect/z33.lua` force-override
-- autocmd, so the two never drift. `vim.filetype.add` extension callbacks are
-- passed `(path, bufnr)`.
local function register_ftdetect()
  local ftdetect = require("z33.ftdetect")
  local function matcher(_, bufnr)
    return ftdetect.detect(bufnr)
  end
  vim.filetype.add({
    extension = {
      s = matcher,
      S = matcher,
    },
  })
end

-- ---------------------------------------------------------------------------
-- setup()
-- ---------------------------------------------------------------------------

local did_setup = false

-- Maps lazy.nvim-style `opts = { ... }` keys onto the `vim.g.*` config
-- variables (the plugin's real config surface). Runs even on a repeated
-- setup() call so lazy's `opts` are never silently dropped, but only fills a
-- `vim.g` that the user did not set themselves — an explicit `vim.g` always
-- wins. Documented in the README.
local OPTS_TO_G = {
  filetypes = "z33_filetypes",
  auto_download = "z33_auto_download",
  no_ftdetect = "z33_no_ftdetect",
}

local function apply_opts(opts)
  if type(opts) ~= "table" then
    return
  end
  for k, g in pairs(OPTS_TO_G) do
    if opts[k] ~= nil and vim.g[g] == nil then
      vim.g[g] = opts[k]
    end
  end
end

--- Enables the native LSP for `z33` buffers. `vim.lsp.enable` reattaches to
--- already-open matching buffers, so calling it after a download makes the
--- server attach without restarting nvim. Safe to call more than once.
local function enable_lsp()
  if vim.lsp and vim.lsp.enable then
    pcall(vim.lsp.enable, "z33")
  end
end

M.enable_lsp = enable_lsp

-- Guard so we only ever arm the lazy download-then-enable flow once.
local lsp_download_armed = false

--- Called when no binary is resolvable at setup() time. Instead of enabling the
--- LSP (whose `cmd` would then have no binary and crash the core), it arms a
--- one-shot FileType=z33 autocmd: the first z33 buffer runs the async
--- ensure/download flow (consent prompt included) and, on success, enables the
--- LSP so that buffer (and any others already open) attaches. On decline or
--- failure the download layer notifies once and we do not retry until
--- `:Z33Download` succeeds (which re-enables the LSP itself).
local function arm_lazy_lsp()
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "z33",
    once = true,
    desc = "z33: fetch z33-cli then enable the LSP on the first z33 buffer",
    callback = function()
      if lsp_download_armed then
        return
      end
      lsp_download_armed = true
      require("z33.download").ensure(function(bin)
        if bin then
          enable_lsp()
        end
        -- On decline/failure the download layer already notified the user; do
        -- not retry until :Z33Download succeeds.
      end)
    end,
  })
end

--- Idempotent. Safe to call from both `plugin/z33.lua` and a user's config
--- (e.g. lazy.nvim `opts = { ... }`, whose keys are mapped onto `vim.g.*`).
function M.setup(opts)
  -- Map opts→vim.g before the idempotency guard: with lazy.nvim, plugin/z33.lua
  -- may have already run setup() with no args, so the opts-bearing call would
  -- otherwise be a no-op. vim.g.* are read lazily (at detect/download time),
  -- so applying them here still takes effect.
  apply_opts(opts)

  if did_setup then
    return
  end
  did_setup = true

  -- 1. Filetype detection: the `vim.filetype.add` matcher (works under
  --    `--clean`) plus the force-override autocmd. The latter is normally
  --    armed by `ftdetect/z33.lua`, but we also arm it here (idempotently)
  --    because some hosts source `plugin/` but not `ftdetect/` — without this
  --    a third-party `.s`/`.S` claimant like vim-polyglot's `r-lang` wins.
  pcall(register_ftdetect)
  pcall(function()
    require("z33.ftdetect").arm_force_override()
  end)

  -- 2. Tree-sitter parser config (pure config; no compilation).
  pcall(function()
    require("z33.treesitter").register()
  end)

  -- 3. Native LSP (0.11+). Only enable it when a binary is resolvable right
  --    now; otherwise arm a lazy fetch-then-enable so we never hand the LSP
  --    core a `cmd` that has no binary (which crashes it and poisons retries).
  if vim.lsp and vim.lsp.enable then
    if M.cli_path() then
      enable_lsp()
    else
      pcall(arm_lazy_lsp)
    end
  else
    -- One-time hint for pre-0.11 users; the README documents the
    -- nvim-lspconfig fallback.
    vim.schedule(function()
      vim.notify(
        "[z33] native LSP needs Neovim 0.11+; see editors/nvim/README.md for the nvim-lspconfig fallback",
        vim.log.levels.WARN
      )
    end)
  end

  -- 4. nvim-dap (only if installed).
  pcall(function()
    require("z33.dap").setup()
  end)

  -- 5. :Z33Download command.
  pcall(function()
    require("z33.download").setup_command()
  end)
end

return M
