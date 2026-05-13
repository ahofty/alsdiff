#!/usr/bin/env bash
set -euo pipefail

retry() {
  local max=$1; shift
  local n=0
  until "$@"; do
    n=$((n + 1))
    if [ $n -ge $max ]; then return 1; fi
    echo "::warning::Attempt $n/$max failed, retrying in 10s..."
    sleep 10
  done
}
