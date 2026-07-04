#!/usr/bin/env bash
# Asserts that every manifest scripts/set-version.sh writes still agrees on
# one version, so a hand-edit can't silently desync them between releases.
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

if [[ "$status" -eq 0 ]]; then
  echo "All manifests agree on version $workspace"
fi
exit "$status"
