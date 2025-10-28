#!/usr/bin/env bash
set -euo pipefail

# -----------------------------
# Config
# -----------------------------
APP_NAME="mindscript"
BIN1="msg"
BIN2="msg-lsp"
INSTALL_DIR="${HOME}/.mindscript"

# Online source (works if repo is public)
# - "latest" uses GitHub's latest/download endpoint (no latest.txt needed)
VERSION="${VERSION:-latest}"
BASE_URL_LATEST="https://github.com/DAIOS-AI/msg/releases/latest/download"
BASE_URL_TAGGED="https://github.com/DAIOS-AI/msg/releases/download"

# -----------------------------
# Args: --local [dir]
# -----------------------------
LOCAL_DIR=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --local)
      # optional argument: if the next token is missing or starts with '-', use '.'
      if [[ $# -ge 2 && "${2:0:1}" != "-" ]]; then
        LOCAL_DIR="$2"; shift 2
      else
        LOCAL_DIR="."; shift 1
      fi
      ;;
    --version)  # optional convenience (ENV VERSION also works)
      VERSION="$2"; shift 2;;
    *)
      echo "Unknown option: $1"
      echo "Usage: $0 [--local [dir]] [--version vX.Y.Z]"
      exit 1;;
  esac
done

# -----------------------------
# Detect OS/arch → tarball name
# -----------------------------
os="$(uname -s)"
arch="$(uname -m)"

case "$os" in
  Darwin) osn="macos" ;;
  Linux)  osn="linux" ;;
  *) echo "Unsupported OS: $os"; exit 1 ;;
esac

case "$arch" in
  x86_64)            archn="x86_64" ;;
  aarch64|arm64)     archn="arm64"  ;;
  *) echo "Unsupported arch: $arch"; exit 1 ;;
esac

TAR="${APP_NAME}-${osn}-${archn}.tar.gz"
SHA="${TAR}.sha256"

# -----------------------------
# Try online first, else local
# -----------------------------
tmp="$(mktemp -d)"
cleanup() { rm -rf "$tmp"; }
trap cleanup EXIT

download_failed=false
if [[ -z "$LOCAL_DIR" ]]; then
  echo "→ Attempting online download: ${TAR} (VERSION=${VERSION})"
  if [[ "$VERSION" == "latest" ]]; then
    CURL_URL_TAR="${BASE_URL_LATEST}/${TAR}"
    CURL_URL_SHA="${BASE_URL_LATEST}/${SHA}"
  else
    CURL_URL_TAR="${BASE_URL_TAGGED}/${VERSION}/${TAR}"
    CURL_URL_SHA="${BASE_URL_TAGGED}/${VERSION}/${SHA}"
  fi
  set +e
  curl -fL --retry 2 -o "${tmp}/${TAR}" "${CURL_URL_TAR}"
  rc1=$?
  curl -fL --retry 2 -o "${tmp}/${SHA}" "${CURL_URL_SHA}"
  rc2=$?
  set -e
  if [[ $rc1 -ne 0 || $rc2 -ne 0 ]]; then
    echo "WARNING: Online download unavailable. Falling back to local artifacts."
    download_failed=true
  else
    SRC_TAR="${tmp}/${TAR}"
    SRC_SHA="${tmp}/${SHA}"
  fi
fi

# Local fallback (explicit --local or after download failure)
if [[ -n "$LOCAL_DIR" || "$download_failed" == "true" ]]; then
  # default dir is '.' if --local had no argument; if no --local provided and failed, also use '.'
  FALLBACK_DIR="${LOCAL_DIR:-.}"
  SRC_TAR="${FALLBACK_DIR}/${TAR}"
  SRC_SHA="${FALLBACK_DIR}/${SHA}"
  [[ -f "$SRC_TAR" ]] || { echo "Missing local tarball: $SRC_TAR"; exit 1; }
  if [[ ! -f "$SRC_SHA" ]]; then
    echo "WARNING: Checksum not found at $SRC_SHA; proceeding without verification."
    SRC_SHA=""
  fi
fi

# -----------------------------
# Verify checksum (if present)
# -----------------------------
if [[ -n "${SRC_SHA:-}" && -f "$SRC_SHA" ]]; then
  echo "→ Verifying checksum"
  ( cd "$(dirname "$SRC_TAR")"
    if command -v shasum >/dev/null 2>&1; then
      shasum -a 256 -c "$(basename "$SRC_SHA")"
    else
      sha256sum -c "$(basename "$SRC_SHA")"
    fi
  )
fi

# -----------------------------
# Install
# -----------------------------
echo "→ Installing to $INSTALL_DIR"
work="${tmp}/unpack"
mkdir -p "$work"
tar -C "$work" -xzf "$SRC_TAR"

# tar should contain a top-level 'mindscript/' directory
if [[ -d "${work}/${APP_NAME}" ]]; then
  rm -rf "$INSTALL_DIR"
  mv "${work}/${APP_NAME}" "$INSTALL_DIR"
else
  echo "Archive format unexpected (no '${APP_NAME}/' dir)."
  exit 1
fi

# Ensure binaries exist
[[ -x "${INSTALL_DIR}/bin/${BIN1}" ]] || { echo "Missing ${BIN1} in bin/"; exit 1; }
[[ -x "${INSTALL_DIR}/bin/${BIN2}" ]] || { echo "Missing ${BIN2} in bin/"; exit 1; }

# -----------------------------
# Shell profile setup (bash/zsh + fish)
# -----------------------------
SNIPPET='export MSGPATH="$HOME/.mindscript"
case ":$PATH:" in *":$MSGPATH/bin:"*) ;; *) export PATH="$MSGPATH/bin:$PATH";; esac'

for rc in "$HOME/.bashrc" "$HOME/.zshrc"; do
  if [[ -f "$rc" ]]; then
    grep -q 'MSGPATH=.*\.mindscript' "$rc" 2>/dev/null || printf '\n%s\n' "$SNIPPET" >> "$rc"
  fi
done

if [[ -d "$HOME/.config/fish/conf.d" ]]; then
  printf 'set -gx MSGPATH "%s"\nset -gx PATH "$MSGPATH/bin" $PATH\n' "$INSTALL_DIR" \
    > "$HOME/.config/fish/conf.d/mindscript.fish"
fi

echo "OK: Installed. Open a new terminal or run:  source ~/.bashrc   (or ~/.zshrc)"
"${INSTALL_DIR}/bin/${BIN1}" --version || true
