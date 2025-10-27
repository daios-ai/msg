#!/usr/bin/env bash
set -euo pipefail
INSTALL_ROOT="${MS_INSTALL_ROOT:-$HOME/.mindscript}"
echo "This will remove $INSTALL_ROOT and PATH snippets in ~/.profile (if present)."
read -r -p "Continue? [y/N] " ans
[[ "$ans" =~ ^[Yy]$ ]] || exit 0
rm -rf "$INSTALL_ROOT"
sed -i.bak '/# MindScript/,+2 d' "$HOME/.profile" || true
echo "Removed. You may need to open a new shell."
