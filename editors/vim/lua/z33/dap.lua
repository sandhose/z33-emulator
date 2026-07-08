-- nvim-dap integration for the Z33 debug adapter (`z33-cli dap`).
--
-- Only registers when nvim-dap is actually installed. The adapter is in
-- function form so the `z33-cli` path resolves lazily/async (downloading on
-- first use if needed) without blocking startup.

local M = {}

function M.setup()
  local ok, dap = pcall(require, "dap")
  if not ok then
    return
  end

  -- Function-form adapter: resolve the CLI path asynchronously, then hand
  -- nvim-dap an executable adapter. If resolution fails, report an error via
  -- the callback's error string so nvim-dap surfaces it instead of hanging.
  dap.adapters.z33 = function(callback, _config)
    require("z33.download").ensure(function(bin)
      if not bin then
        return callback({ type = "executable", command = "z33-cli", args = { "dap" } })
        -- Note: passing a bogus command lets nvim-dap fail visibly; the
        -- download layer has already notified the user why.
      end
      callback({ type = "executable", command = bin, args = { "dap" } })
    end)
  end

  -- The launch request body sent to the server is this table minus the
  -- nvim-dap envelope fields (type/request/name). It must contain exactly
  -- `program` / `entrypoint` / `stopOnEntry` (see crates/emulator DAP
  -- LaunchArguments). Do NOT send `files` — this is on-disk debugging.
  dap.configurations.z33 = {
    {
      type = "z33",
      request = "launch",
      name = "Launch Z33 program",
      program = "${file}",
      entrypoint = function()
        return vim.fn.input("Entrypoint: ", "main")
      end,
      stopOnEntry = true,
    },
  }
end

return M
