#!/usr/bin/env bash
set -euo pipefail
VER="${1:?version required}"
PLAT="${2:?platform required}"   # e.g., linux-amd64
APP="mindscript"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

OUTDIR="$ROOT/dist"
STAGE="$OUTDIR/$APP-$PLAT"
rm -rf "$STAGE"
mkdir -p "$STAGE"/{bin,lib,examples,docs,data}

# Copy built binaries
cp "$ROOT/build/$PLAT/msg"     "$STAGE/bin/"
cp "$ROOT/build/$PLAT/msg-lsp" "$STAGE/bin/"

# Copy runtime assets (adjust if you want to slim)
cp -r "$ROOT/lib"/*      "$STAGE/lib/"      || true
cp -r "$ROOT/examples"/* "$STAGE/examples/" || true
cp -r "$ROOT/docs"/*     "$STAGE/docs/"     || true
cp -r "$ROOT/data"/*     "$STAGE/data/"     || true

# Metadata
echo "$VER" > "$STAGE/VERSION"
cp "$ROOT/LICENSE" "$STAGE/" 2>/dev/null || true
cp "$ROOT/README.md" "$STAGE/" 2>/dev/null || true

# Tarball
TAR="$OUTDIR/$APP-$PLAT-v$VER.tar.gz"
( cd "$OUTDIR" && tar -czf "$(basename "$TAR")" "$(basename "$STAGE")" )
echo "Packaged $TAR"
