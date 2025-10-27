#!/usr/bin/env bash
set -euo pipefail
VER="${1:?version required}"
OUTDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../dist" && pwd)"
cd "$OUTDIR"
: > SHA256SUMS
for f in mindscript-*-v"$VER".tar.gz; do
  sha256sum "$f" >> SHA256SUMS
done
echo "Wrote $OUTDIR/SHA256SUMS"
