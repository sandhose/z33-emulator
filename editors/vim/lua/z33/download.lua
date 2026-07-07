-- Async download & caching of the prebuilt `z33-cli` binary from GitHub
-- releases. Mirrors the Zed extension's `platform_cli()` / `download_cli()`
-- (editors/zed/src/lib.rs) but in pure, dependency-free Lua.
--
-- Design invariants (see design doc §6 / §9):
--   * NEVER blocks startup or the event loop. Every network/filesystem step
--     runs through `vim.system(..., cb)` and chains via callbacks.
--   * Consent is asked exactly once per session via `vim.ui.select`, unless
--     `vim.g.z33_auto_download` forces (true) or forbids (false) it.
--   * A partial/failed download leaves no directory behind, so a corrupt
--     binary can never shadow a retry.
--   * Windows assets are a bare `.exe` (no archive); unix assets are tar.gz.

local M = {}

local REPO = "sandhose/z33-emulator"

--- Base cache directory: `stdpath('data')/z33/`.
local function base_dir()
  return vim.fs.joinpath(vim.fn.stdpath("data"), "z33")
end

--- Detects the release asset layout for the current platform.
--- Returns a table { asset, archive = "tar"|"exe", binary } or nil + message.
--- Exposed for :checkhealth and tests.
function M.platform()
  local uname = vim.uv.os_uname()
  local sysname = uname.sysname
  local machine = uname.machine

  local arch
  if machine == "x86_64" or machine == "amd64" then
    arch = "x86_64"
  elseif machine == "aarch64" or machine == "arm64" then
    arch = "aarch64"
  else
    return nil, ("unsupported architecture: %s"):format(machine)
  end

  if sysname == "Darwin" then
    return { asset = ("z33-cli-%s-macos.tar.gz"):format(arch), archive = "tar", binary = "z33-cli" }
  elseif sysname == "Linux" then
    return { asset = ("z33-cli-%s-linux.tar.gz"):format(arch), archive = "tar", binary = "z33-cli" }
  elseif sysname:match("Windows") or sysname:match("MINGW") or sysname:match("MSYS") then
    -- Windows assets are a *bare* .exe, not a tar.gz.
    return { asset = ("z33-cli-%s-windows.exe"):format(arch), archive = "exe", binary = "z33-cli.exe" }
  end
  return nil, ("unsupported OS: %s"):format(sysname)
end

--- Parses `z33-cli-vX.Y.Z` into a numerically comparable key.
--- (Lexicographic comparison would rank v0.9.0 above v0.10.0.)
local function version_key(dir)
  local tag = dir:match("^z33%-cli%-v(.+)$")
  if not tag then
    return nil
  end
  local major, minor, patch = tag:match("^(%d+)%.(%d+)%.(%d+)$")
  if not major then
    return nil
  end
  return { tonumber(major), tonumber(minor), tonumber(patch) }
end

local function key_less(a, b)
  for i = 1, 3 do
    if a[i] ~= b[i] then
      return a[i] < b[i]
    end
  end
  return false
end

--- Newest previously downloaded binary, if any (used when GitHub is
--- unreachable, and as the PATH-less cached lookup). Returns the absolute path
--- or nil.
function M.cached_binary()
  local platform = M.platform()
  if not platform then
    return nil
  end
  local root = base_dir()
  local entries = vim.fn.readdir and vim.fn.isdirectory(root) == 1 and vim.fn.readdir(root) or {}
  local best, best_key
  for _, name in ipairs(entries) do
    local key = version_key(name)
    if key then
      local bin = vim.fs.joinpath(root, name, platform.binary)
      if vim.uv.fs_stat(bin) then
        if not best_key or key_less(best_key, key) then
          best, best_key = bin, key
        end
      end
    end
  end
  if best then
    return best
  end
  -- Lowest priority: the unversioned `z33-cli-latest/` fallback dir (installed
  -- by the `releases/latest/download/` redirect when the GitHub API is
  -- unreachable). `version_key` rejects its name, so without this it would be
  -- re-downloaded every session and never pruned.
  local latest = vim.fs.joinpath(root, "z33-cli-latest", platform.binary)
  if vim.uv.fs_stat(latest) then
    return latest
  end
  return nil
end

-- Session-level guard so we only prompt for consent once, and never overlap
-- two downloads.
local state = {
  in_progress = false,
  pending = {}, -- callbacks awaiting the in-progress download
}

local function notify(msg, level)
  vim.schedule(function()
    vim.notify("[z33] " .. msg, level or vim.log.levels.INFO)
  end)
end

--- Single completion path for BOTH `M.ensure` and `:Z33Download`. Clears the
--- in-progress flag, notifies, re-enables the native LSP on success (so any
--- already-open z33 buffers attach without a restart), and drains every queued
--- callback exactly once. Using one code path is what stops a `:Z33Download`
--- from stranding callbacks an `ensure()` queued onto it (a permanent DAP hang).
local function finish(path, err)
  state.in_progress = false
  if not path and err then
    notify("download failed: " .. err, vim.log.levels.ERROR)
  elseif path then
    notify("z33-cli ready at " .. path)
    -- Re-enable the LSP now that a binary exists (no-op pre-0.11 / when the
    -- z33 module is unavailable).
    pcall(function()
      require("z33").enable_lsp()
    end)
  end
  local waiting = state.pending
  state.pending = {}
  for _, c in ipairs(waiting) do
    pcall(c, path)
  end
end

--- Removes a directory tree, best effort.
local function rm_rf(path)
  vim.fn.delete(path, "rf")
end

--- Runs the actual download pipeline for a resolved asset URL into
--- `version_dir`, calling `done(binary_path_or_nil, err)`.
local function run_download(url, version_dir, platform, done)
  local binary_path = vim.fs.joinpath(version_dir, platform.binary)

  -- Already present (race with a concurrent install): reuse it.
  if vim.uv.fs_stat(binary_path) then
    return done(binary_path)
  end

  vim.fn.mkdir(version_dir, "p")

  local function fail(err)
    rm_rf(version_dir) -- never leave a partial download behind
    done(nil, err)
  end

  -- Bare .exe: download straight to the final binary path.
  if platform.archive == "exe" then
    vim.system(
      { "curl", "-fSL", "-o", binary_path, url },
      { text = true },
      vim.schedule_wrap(function(out)
        if out.code ~= 0 then
          return fail("curl failed: " .. (out.stderr or ("exit " .. out.code)))
        end
        if not vim.uv.fs_stat(binary_path) then
          return fail("downloaded asset did not contain " .. platform.binary)
        end
        done(binary_path)
      end)
    )
    return
  end

  -- tar.gz: download to a temp file, then extract into version_dir.
  local tmp = vim.fs.joinpath(version_dir, platform.asset)
  vim.system(
    { "curl", "-fSL", "-o", tmp, url },
    { text = true },
    vim.schedule_wrap(function(out)
      if out.code ~= 0 then
        return fail("curl failed: " .. (out.stderr or ("exit " .. out.code)))
      end
      vim.system(
        { "tar", "-xzf", tmp, "-C", version_dir },
        { text = true },
        vim.schedule_wrap(function(tout)
          if tout.code ~= 0 then
            return fail("tar failed: " .. (tout.stderr or ("exit " .. tout.code)))
          end
          if not vim.uv.fs_stat(binary_path) then
            return fail(platform.asset .. " did not contain " .. platform.binary)
          end
          -- Drop the archive now that it is extracted (best effort).
          pcall(vim.fn.delete, tmp)
          -- chmod +x (extraction usually preserves it, but be safe).
          pcall(vim.uv.fs_chmod, binary_path, 493) -- 0755
          -- macOS: strip the quarantine xattr, best effort.
          if vim.uv.os_uname().sysname == "Darwin" then
            vim.system({ "xattr", "-d", "com.apple.quarantine", binary_path }, { text = true }, function() end)
          end
          done(binary_path)
        end)
      )
    end)
  )
end

--- Best-effort cleanup of downloads from older releases. Also removes the
--- unversioned `z33-cli-latest/` fallback dir once a real versioned install
--- lands, so it does not linger uncleaned.
local function prune_old(keep_dir)
  local root = base_dir()
  local entries = vim.fn.isdirectory(root) == 1 and vim.fn.readdir(root) or {}
  for _, name in ipairs(entries) do
    local full = vim.fs.joinpath(root, name)
    if full ~= keep_dir and (version_key(name) or name == "z33-cli-latest") then
      rm_rf(full)
    end
  end
end

--- Resolves the latest release tag + asset URL via the GitHub API, then
--- downloads. Falls back to the `releases/latest/download/` redirect when the
--- API is unreachable but a platform is known. Calls `done(path, err)`.
local function fetch_and_install(platform, done)
  local api = ("https://api.github.com/repos/%s/releases/latest"):format(REPO)
  vim.system(
    { "curl", "-fSL", "-H", "Accept: application/vnd.github+json", api },
    { text = true },
    vim.schedule_wrap(function(out)
      local tag, url
      if out.code == 0 then
        local ok, json = pcall(vim.json.decode, out.stdout)
        if ok and type(json) == "table" and json.tag_name then
          tag = json.tag_name
          for _, asset in ipairs(json.assets or {}) do
            if asset.name == platform.asset then
              url = asset.browser_download_url
              break
            end
          end
        end
      end

      if tag and url then
        local version_dir = vim.fs.joinpath(base_dir(), "z33-cli-" .. tag)
        run_download(url, version_dir, platform, function(path, err)
          if path then
            prune_old(version_dir)
          end
          done(path, err)
        end)
        return
      end

      -- API failed or asset not found. Try a newest cached copy first.
      local cached = M.cached_binary()
      if cached then
        notify("using cached z33-cli (GitHub API unreachable)")
        return done(cached)
      end

      -- Last resort: the stable "latest download" redirect into a stable dir.
      local redirect = ("https://github.com/%s/releases/latest/download/%s"):format(REPO, platform.asset)
      local version_dir = vim.fs.joinpath(base_dir(), "z33-cli-latest")
      rm_rf(version_dir) -- avoid a stale "latest" shadowing a newer real tag
      run_download(redirect, version_dir, platform, done)
    end)
  )
end

--- Resolves the consent decision, then downloads. `cb(path_or_nil)`.
--- `notify_only` skips the eventual error notification (caller handles it).
local function with_consent(cb)
  local auto = vim.g.z33_auto_download
  if auto == false then
    notify("z33-cli not found and downloads are disabled (vim.g.z33_auto_download = false)", vim.log.levels.WARN)
    return cb(nil)
  end
  if auto == true then
    return cb(true)
  end
  vim.ui.select({ "Yes", "No" }, {
    prompt = "z33-cli not found. Download prebuilt binary from GitHub releases?",
  }, function(choice)
    cb(choice == "Yes")
  end)
end

--- Ensures a `z33-cli` binary is available, downloading it if the user
--- consents. Async; `cb(path_or_nil)` on the main loop. Multiple concurrent
--- callers share a single download.
--- @param cb fun(path: string|nil)
function M.ensure(cb)
  -- Already cached from a previous session? Use it without prompting.
  local cached = M.cached_binary()
  if cached then
    return cb(cached)
  end

  local platform, perr = M.platform()
  if not platform then
    notify("cannot auto-download: " .. (perr or "unsupported platform"), vim.log.levels.ERROR)
    return cb(nil)
  end

  if state.in_progress then
    table.insert(state.pending, cb)
    return
  end
  state.in_progress = true
  table.insert(state.pending, cb)

  with_consent(function(ok)
    if not ok then
      -- User declined (or downloads disabled): point them at the README.
      if vim.g.z33_auto_download ~= false then
        notify("z33-cli required. Install it on PATH or run :Z33Download. See editors/vim/README.md", vim.log.levels.WARN)
      end
      return finish(nil)
    end
    notify("downloading z33-cli...")
    fetch_and_install(platform, finish)
  end)
end

--- Registers the `:Z33Download` user command (pre-fetch the CLI).
function M.setup_command()
  vim.api.nvim_create_user_command("Z33Download", function()
    -- Force a download prompt even if already on PATH: this pre-fetches the
    -- release binary into the cache for offline/pinned use.
    local platform, perr = M.platform()
    if not platform then
      return notify("cannot download: " .. (perr or "unsupported platform"), vim.log.levels.ERROR)
    end
    if state.in_progress then
      return notify("a download is already in progress")
    end
    state.in_progress = true
    -- Shares the single `finish` path with M.ensure so any callback an
    -- ensure() queued onto this in-progress download is still drained.
    with_consent(function(ok)
      if not ok then
        return finish(nil)
      end
      notify("downloading z33-cli...")
      fetch_and_install(platform, finish)
    end)
  end, { desc = "Download the prebuilt z33-cli binary from GitHub releases" })
end

return M
