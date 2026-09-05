#!/usr/bin/env bash
# Pins the commit the Zed extension compiles the tree-sitter grammar from
# (`rev` in editors/zed/extension.toml).
#
# The release workflow calls this with the SHA of the version-bump commit, so
# the published extension builds the grammar exactly as the release ships it.
# A commit cannot name itself, which is why this runs as a separate step and
# lands in its own commit on the release branch.
#
# Usage: scripts/set-grammar-rev.sh <commit-sha>
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <commit-sha>" >&2
  exit 1
fi

GRAMMAR_REV="$1"
if ! [[ "$GRAMMAR_REV" =~ ^[0-9a-f]{40}$ ]]; then
  echo "error: expected a full 40-character commit SHA (got '$GRAMMAR_REV')" >&2
  exit 1
fi

cd "$(dirname "$0")/.."

export GRAMMAR_REV
perl -0777 -pi -e 's/^rev = "[0-9a-f]+"/rev = "$ENV{GRAMMAR_REV}"/m' \
  editors/zed/extension.toml

if ! grep -q "^rev = \"$GRAMMAR_REV\"\$" editors/zed/extension.toml; then
  echo "error: no grammar rev line was rewritten in editors/zed/extension.toml" >&2
  exit 1
fi

echo "Pinned the Zed grammar rev to $GRAMMAR_REV"
