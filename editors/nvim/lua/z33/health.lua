-- `:checkhealth z33` diagnostics.

local M = {}

local health = vim.health or require("health")

function M.check()
  health.start("z33")

  -- Neovim version (native LSP needs 0.11+).
  if vim.fn.has("nvim-0.11") == 1 then
    health.ok("Neovim " .. tostring(vim.version()) .. " (native vim.lsp.enable available)")
  else
    health.warn("Neovim < 0.11: native LSP unavailable; use the nvim-lspconfig fallback (see README)")
  end

  -- z33-cli availability.
  local z33 = require("z33")
  local on_path = vim.fn.exepath("z33-cli")
  local download = require("z33.download")
  if on_path ~= "" then
    health.ok("z33-cli found on PATH: " .. on_path)
  else
    local cached = download.cached_binary()
    if cached then
      health.ok("z33-cli found in cache: " .. cached)
    else
      health.warn("z33-cli not found on PATH or in cache; run :Z33Download or add it to PATH")
    end
  end

  -- Platform support for auto-download.
  local platform, perr = download.platform()
  if platform then
    health.ok("auto-download supported for this platform (asset: " .. platform.asset .. ")")
  else
    health.warn("auto-download not supported: " .. (perr or "unknown platform"))
  end

  if vim.uv.os_uname().sysname == "Darwin" then
    health.info("macOS: downloaded binaries have the quarantine xattr stripped best-effort")
  end

  -- nvim-treesitter + z33 parser.
  local ts_ok = pcall(require, "nvim-treesitter")
  if ts_ok then
    health.ok("nvim-treesitter installed")

    -- Which branch? master exposes `parsers.get_parser_configs()`; main's
    -- `parsers` module IS the config table (no such function). We register
    -- `z33` on both, so `:TSInstall z33` works either way.
    local parsers_ok, parsers = pcall(require, "nvim-treesitter.parsers")
    if parsers_ok and type(parsers.get_parser_configs) == "function" then
      local registered = parsers.get_parser_configs().z33 ~= nil
      health.ok(
        "nvim-treesitter branch: master (z33 "
          .. (registered and "registered" or "NOT registered")
          .. "); :TSInstall z33 to build the parser"
      )
    elseif parsers_ok then
      -- On main, direct registration is wiped by reload; we re-add via a
      -- `User TSUpdate` autocmd, so a nil here at check time is expected.
      health.ok(
        "nvim-treesitter branch: main (z33 registered via a User TSUpdate autocmd); "
          .. ":TSInstall z33 to build the parser"
      )
    end

    local has_parser = false
    local lang_ok, has = pcall(function()
      -- Works regardless of how the parser was installed.
      return vim.treesitter.language.add and vim.treesitter.language.add("z33")
    end)
    if lang_ok and has ~= false then
      -- language.add returns true (or nil on older nvim) when the parser loads.
      has_parser = pcall(vim.treesitter.get_string_parser, "", "z33")
    end
    if has_parser then
      health.ok("z33 tree-sitter parser is installed")
    else
      health.warn("z33 tree-sitter parser not installed; run :TSInstall z33")
    end
    if vim.g.z33_no_treesitter_start then
      health.info("vim.g.z33_no_treesitter_start set (auto-start of tree-sitter highlighting disabled)")
    end
  else
    health.info("nvim-treesitter not installed (optional; enables syntax highlighting)")
  end

  -- nvim-dap.
  if pcall(require, "dap") then
    health.ok("nvim-dap installed (debugging available)")
  else
    health.info("nvim-dap not installed (optional; enables debugging)")
  end

  -- Config flags.
  local auto = vim.g.z33_auto_download
  if auto == true then
    health.info("vim.g.z33_auto_download = true (downloads without prompting)")
  elseif auto == false then
    health.info("vim.g.z33_auto_download = false (downloads disabled; PATH only)")
  else
    health.info("vim.g.z33_auto_download unset (will prompt before downloading)")
  end
  if vim.g.z33_filetypes then
    health.info("vim.g.z33_filetypes set (all .s/.S files forced to z33)")
  end

  -- Silence an unused-variable lint while keeping the require for side effects.
  local _ = z33
end

return M
