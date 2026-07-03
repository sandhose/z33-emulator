#!/usr/bin/env bash
# Sets the project version across every shipped manifest:
#   - Cargo.toml [workspace.package] (inherited by all crates) + Cargo.lock
#   - vscode/package.json (the VS Code extension)
#   - zed/extension.toml (the Zed extension), whose pinned tree-sitter
#     grammar `rev` is also refreshed to the current HEAD
#   - tree-sitter-z33/package.json and tree-sitter.json (the grammar),
#     plus the committed generated parser, which embeds that version
#
# Usage: scripts/set-version.sh <version>    e.g. scripts/set-version.sh 0.8.0
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <version>" >&2
  exit 1
fi

VERSION="$1"
if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "error: version must be MAJOR.MINOR.PATCH without a leading v (got '$VERSION')" >&2
  exit 1
fi

cd "$(dirname "$0")/.."

GRAMMAR_REV="$(git rev-parse HEAD)"
export VERSION GRAMMAR_REV

# The first `version = "…"` line is the [workspace.package] one.
perl -0777 -pi -e 's/^version = "[^"]+"/version = "$ENV{VERSION}"/m' Cargo.toml

perl -0777 -pi -e '
  s/^version = "[^"]+"/version = "$ENV{VERSION}"/m;
  s/^rev = "[0-9a-f]+"/rev = "$ENV{GRAMMAR_REV}"/m;
' zed/extension.toml

(cd vscode && npm pkg set version="$VERSION")
(cd tree-sitter-z33 && npm pkg set version="$VERSION")

TMP="$(mktemp)"
jq --arg version "$VERSION" '.metadata.version = $version' \
  tree-sitter-z33/tree-sitter.json >"$TMP"
mv "$TMP" tree-sitter-z33/tree-sitter.json

# The generated parser embeds the tree-sitter.json version, and CI asserts
# that the committed src/ matches a fresh `tree-sitter generate`.
(
  cd tree-sitter-z33 &&
    npm install --no-audit --no-fund >/dev/null &&
    npx tree-sitter generate
)

cargo update --workspace --quiet

echo "Set version $VERSION (grammar rev $GRAMMAR_REV)"
