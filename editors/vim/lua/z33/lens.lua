-- Code lens support for the Z33 LSP.
--
-- The server puts an informational lens (address, reference count) on every
-- label and, when the client advertises the `zorglub33.run` command, a
-- "Run <label>" lens on executable labels. Neovim requests lenses only when
-- asked, so `attach` schedules refreshes, and `run` executes the run lens.

local M = {}

M.RUN_COMMAND = "zorglub33.run"

--- Client capabilities merged into the LSP client config: the server emits
--- the run lens only for clients listing the command here.
function M.capabilities()
  return { experimental = { commands = { M.RUN_COMMAND } } }
end

--- Show the lenses of `bufnr` and keep them fresh. Neovim 0.12 tracks the
--- refresh events itself; 0.11 only exposes a one-shot refresh.
--- `vim.g.z33_no_codelens` opts out (documented in the README).
function M.attach(bufnr)
  if vim.g.z33_no_codelens then
    return
  end
  local codelens = vim.lsp.codelens
  if not codelens then
    return
  end
  if codelens.enable then
    codelens.enable(true, { bufnr = bufnr })
    return
  end
  local name = "z33_codelens_" .. bufnr
  local group = vim.api.nvim_create_augroup(name, { clear = true })
  vim.api.nvim_create_autocmd({ "BufEnter", "CursorHold", "InsertLeave", "BufWritePost" }, {
    group = group,
    buffer = bufnr,
    desc = "z33: refresh LSP code lenses",
    callback = function()
      codelens.refresh({ bufnr = bufnr })
    end,
  })
  -- The group name embeds a buffer number, which Neovim reuses for later
  -- buffers, so it has to go when this one does.
  vim.api.nvim_create_autocmd("BufWipeout", {
    group = group,
    buffer = bufnr,
    desc = "z33: drop the code lens refresh group with the buffer",
    callback = function()
      pcall(vim.api.nvim_del_augroup_by_name, name)
    end,
  })
  codelens.refresh({ bufnr = bufnr })
end

local function program_path(args, ctx)
  local name = vim.api.nvim_buf_get_name(ctx.bufnr)
  if name ~= "" then
    return name
  end
  -- The lens path is relative to the server's root; join it back.
  local client = vim.lsp.get_client_by_id(ctx.client_id)
  local root = client and client.root_dir or vim.fn.getcwd()
  return vim.fs.joinpath(root, args.path)
end

--- Executes the run lens: with nvim-dap installed it starts a debug session
--- stopped on the label's first instruction, otherwise it runs the program in
--- a terminal split.
function M.run(command, ctx)
  local args = (command.arguments or {})[1] or {}
  local label = args.label or "main"
  local program = program_path(args, ctx)

  local has_dap, dap = pcall(require, "dap")
  if has_dap then
    dap.run({
      type = "z33",
      request = "launch",
      name = "Run " .. label,
      program = program,
      entrypoint = label,
      stopOnEntry = true,
    })
    return
  end

  require("z33.download").ensure(function(bin)
    if not bin then
      return
    end
    vim.cmd.split()
    vim.cmd.enew()
    -- A list argument, not `:terminal`: that builds a command line, where
    -- spaces in `program` split it into extra arguments and `%`/`#` expand to
    -- file names. `jobstart`'s `term` needs 0.11, the floor for the native LSP
    -- that carries this command.
    vim.fn.jobstart({ bin, "run", program, label }, { term = true })
  end)
end

return M
