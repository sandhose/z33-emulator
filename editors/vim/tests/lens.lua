-- Code lens check for the Neovim plugin. Needs `z33-cli` on PATH.
--
-- Run from the repository root with:
--   nvim --headless -u NONE -N -l editors/vim/tests/lens.lua

vim.opt.rtp:prepend(vim.fn.getcwd() .. "/editors/vim")

local z33 = require("z33")
z33.setup()
assert(z33.cli_path(), "z33-cli must be on PATH for this test")

vim.cmd.edit("samples/fact.s")
local bufnr = vim.api.nvim_get_current_buf()

-- 0.12 returns `{ client_id, lens }` pairs; 0.11 returns the lenses directly.
local function lenses()
  if vim.fn.has("nvim-0.12") == 0 then
    return vim.lsp.codelens.get(bufnr)
  end
  return vim.tbl_map(function(entry)
    return entry.lens
  end, vim.lsp.codelens.get({ bufnr = bufnr }))
end

local attached = vim.wait(10000, function()
  return #vim.lsp.get_clients({ bufnr = bufnr, name = "z33" }) > 0
end, 50)
assert(attached, "the z33 LSP client did not attach")

local ok = vim.wait(10000, function()
  return #lenses() > 0
end, 100)
assert(ok, "no code lenses were received for samples/fact.s")

local titles = {}
local run_lenses = 0
for _, lens in ipairs(lenses()) do
  titles[#titles + 1] = lens.command.title
  if lens.command.command == require("z33.lens").RUN_COMMAND then
    run_lenses = run_lenses + 1
  end
end
assert(run_lenses > 0, "no run lens; the client capability was not advertised: " .. vim.inspect(titles))

local run
for _, lens in ipairs(lenses()) do
  if lens.command.command == require("z33.lens").RUN_COMMAND and lens.command.arguments[1].label == "main" then
    run = lens
  end
end
assert(run, "no run lens for main")
local client = vim.lsp.get_clients({ bufnr = bufnr, name = "z33" })[1]
local ctx = { bufnr = bufnr, client_id = client.id }

-- With nvim-dap present (stubbed here) the lens starts a debug session on the
-- label, stopped on entry.
local launched
package.loaded["dap"] = {
  run = function(config)
    launched = config
  end,
}
vim.lsp.commands[run.command.command](run.command, ctx)
assert(launched, "the run lens did not start a debug session")
assert(launched.type == "z33" and launched.entrypoint == "main" and launched.stopOnEntry == true, vim.inspect(launched))
assert(launched.program == vim.api.nvim_buf_get_name(bufnr), "program should be the buffer path: " .. launched.program)

-- Without nvim-dap the lens runs the program in a terminal split. The copy
-- sits under a directory with a space in its name: the path has to reach the
-- process as one argument, so a run that finishes proves it was not split.
package.loaded["dap"] = nil
package.preload["dap"] = function()
  error("nvim-dap is not installed")
end

local dir = vim.fn.tempname() .. "/my dir"
assert(vim.fn.mkdir(dir, "p") == 1, "could not create " .. dir)
local spaced = dir .. "/fact.s"
assert(vim.uv.fs_copyfile("samples/fact.s", spaced), "could not copy samples/fact.s")
local spaced_buf = vim.fn.bufadd(spaced)
vim.fn.bufload(spaced_buf)
vim.lsp.commands[run.command.command](run.command, { bufnr = spaced_buf, client_id = client.id })

local output = ""
local finished = vim.wait(10000, function()
  if vim.bo.buftype ~= "terminal" then
    return false
  end
  output = table.concat(vim.api.nvim_buf_get_lines(0, 0, -1, false), "\n")
  -- The CLI's end-of-run line differs between the info log ("End of program")
  -- and the quiet summary ("Program ended"); either means the run succeeded.
  return output:find("End of program", 1, true) ~= nil or output:find("Program ended", 1, true) ~= nil
end, 100)
assert(finished, "the terminal run did not finish; buftype " .. vim.bo.buftype .. ", output:\n" .. output)

print("lens.lua: ok (" .. #titles .. " lenses, " .. run_lenses .. " run lenses)")
