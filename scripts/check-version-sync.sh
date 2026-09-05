#!/usr/bin/env bash
# Asserts that every manifest scripts/set-version.sh writes still agrees on
# one version, so a hand-edit can't silently desync them between releases, and
# that the Zed extension still pins its grammar at a commit SHA.
set -euo pipefail

cd "$(dirname "$0")/.."

workspace="$(perl -ne 'if (/^version = "([^"]+)"/) { print $1; exit }' Cargo.toml)"
zed="$(perl -ne 'if (/^version = "([^"]+)"/) { print $1; exit }' editors/zed/extension.toml)"
vscode="$(jq -r .version editors/vscode/package.json)"
grammar_pkg="$(jq -r .version tree-sitter-z33/package.json)"
grammar_ts="$(jq -r .metadata.version tree-sitter-z33/tree-sitter.json)"

status=0
for pair in \
  "editors/zed/extension.toml=$zed" \
  "editors/vscode/package.json=$vscode" \
  "tree-sitter-z33/package.json=$grammar_pkg" \
  "tree-sitter-z33/tree-sitter.json=$grammar_ts"; do
  file="${pair%%=*}"
  version="${pair#*=}"
  if [[ "$version" != "$workspace" ]]; then
    echo "::error::$file has version $version, but Cargo.toml has $workspace" >&2
    status=1
  fi
done

# Zed resolves the grammar `rev` with `git fetch --depth 1 origin <rev>`
# followed by `git checkout <rev>`, which a tag or branch name does not survive.
rev="$(perl -ne 'if (/^rev = "([^"]+)"/) { print $1; exit }' editors/zed/extension.toml)"
if ! [[ "$rev" =~ ^[0-9a-f]{40}$ ]]; then
  echo "::error::editors/zed/extension.toml pins the grammar at '$rev', not a full commit SHA" >&2
  status=1
fi

if [[ "$status" -eq 0 ]]; then
  echo "All manifests agree on version $workspace"
fi
exit "$status"
