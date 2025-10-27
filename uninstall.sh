#!/usr/bin/env bash
set -euo pipefail

PREFIX="${MSGPATH:-$HOME/.mindscript}"
echo "This will remove ${PREFIX} and shell snippets (bash/zsh/fish). Continue? [y/N]"
read -r ans
[[ "$ans" == "y" || "$ans" == "Y" ]] || exit 0

rm -rf "$PREFIX"
sed -i.bak '/export MSGPATH=/d;/\$MSGPATH\/bin/d' "$HOME/.profile" 2>/dev/null || true
sed -i.bak '/export MSGPATH=/d;/\$MSGPATH\/bin/d' "$HOME/.zprofile" 2>/dev/null || true
rm -f "$HOME/.config/fish/conf.d/mindscript.fish" 2>/dev/null || true
echo "Uninstalled. Restart your shell."
