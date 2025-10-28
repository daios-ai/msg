#!/usr/bin/env bash
set -euo pipefail

INSTALL_DIR="${HOME}/.mindscript"
YES=0

# Parse args
while [[ $# -gt 0 ]]; do
  case "$1" in
    --yes|-y) YES=1; shift ;;
    *) echo "Unknown option: $1"; echo "Usage: $0 [--yes]"; exit 1 ;;
  esac
done

echo "MindScript uninstaller"
echo "Target install dir: $INSTALL_DIR"
if [[ $YES -ne 1 ]]; then
  read -r -p "This will remove MindScript and update your shell config files. Continue? [y/N] " ans
  case "${ans:-}" in [yY][eE][sS]|[yY]) ;; *) echo "Aborted."; exit 0;; esac
fi

# Helper: clean a shell rc file (bash/zsh)
clean_rc() {
  local rcfile="$1"
  [[ -f "$rcfile" ]] || return 0
  cp -p "$rcfile" "${rcfile}.bak-$(date +%Y%m%d%H%M%S)"
  # Remove lines that set MSGPATH to ~/.mindscript or add MSGPATH/bin to PATH
  # (these match what install.sh wrote)
  sed -i '' -e '/MSGPATH=.*\.mindscript/d' -e '/\$MSGPATH\/bin/d' "$rcfile" 2>/dev/null || \
  sed -i     -e '/MSGPATH=.*\.mindscript/d' -e '/\$MSGPATH\/bin/d' "$rcfile"
}

# 1) Remove install dir
if [[ -d "$INSTALL_DIR" ]]; then
  rm -rf "$INSTALL_DIR"
  echo "✔ Removed $INSTALL_DIR"
else
  echo "WARNING: Install directory not found; skipping: $INSTALL_DIR"
fi

# 2) Clean bash/zsh profiles
clean_rc "$HOME/.bashrc"
clean_rc "$HOME/.zshrc"

# 3) Remove fish snippet
FISH_SNIPPET="$HOME/.config/fish/conf.d/mindscript.fish"
if [[ -f "$FISH_SNIPPET" ]]; then
  rm -f "$FISH_SNIPPET"
  echo "OK: Removed $FISH_SNIPPET"
fi

# 4) Final message
echo "OK: Uninstall complete."
echo "• Backups of modified files (if any) are saved as *.bak-<timestamp>."
echo "• Open a new terminal or reload your shell config (e.g., 'source ~/.bashrc')."
echo "• If 'msg' still resolves, your shell session may still have old PATH values cached."
