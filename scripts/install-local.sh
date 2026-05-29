#!/usr/bin/env bash
# Build the alsdiff CLI and install it into ~/.local/bin (no .exe extension).
#
# Replaces only the `alsdiff` binary — it does NOT install alsflow/alsdiff-tui or
# spread libs/docs the way `dune install --prefix ~/.local` would.
#
# Usage:
#   scripts/install-local.sh            # -> ~/.local/bin/alsdiff
#   scripts/install-local.sh /some/dir/alsdiff   # custom destination path
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
dest="${1:-$HOME/.local/bin/alsdiff}"

opam exec -- dune build bin/alsdiff.exe
install -m 755 "$repo_root/_build/default/bin/alsdiff.exe" "$dest"
echo "Installed alsdiff -> $dest"
