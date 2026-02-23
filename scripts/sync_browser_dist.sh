#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

echo "[sync-browser-dist] Building browser bundle..."
dune build lib/browser/alsdiff.js

echo "[sync-browser-dist] Syncing test/browser/index.html -> test/browser/dist/index.html"
cp test/browser/index.html test/browser/dist/index.html

# dist index should load local bundle next to itself
perl -0pi -e 's|<script src="\.\./\.\./\.\./_build/default/lib/browser/alsdiff\.js"></script>|<script src="alsdiff.js"></script>|' test/browser/dist/index.html

echo "[sync-browser-dist] Syncing bundle -> test/browser/dist/alsdiff.js"
cp _build/default/lib/browser/alsdiff.js test/browser/dist/alsdiff.js

echo "[sync-browser-dist] Done"
