#!/usr/bin/env bash
set -euo pipefail

REPO="mindscript-lang/mindscript"   # <-- change to your org/name
INSTALL_DIR_DEFAULT="${HOME}/.mindscript"
INSTALL_DIR_SYSTEM="/opt/mindscript"
PROFILE_SNIPPET_BASH="${HOME}/.profile"
PROFILE_SNIPPET_ZSH="${HOME}/.zprofile"
FISH_CONF_DIR="${HOME}/.config/fish/conf.d"
VERSION="${MSG_VERSION:-latest}"    # allow override: MSG_VERSION=v0.3.0

usage() {
  echo "Usage: $0 [--system] [--prefix DIR] [--version vX.Y.Z]"
  exit 1
}

PREFIX="${INSTALL_DIR_DEFAULT}"
SYSTEM=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --system) SYSTEM=1; PREFIX="${INSTALL_DIR_SYSTEM}"; shift ;;
    --prefix) PREFIX="$2"; shift 2 ;;
    --version) VERSION="$2"; shift 2 ;;
    *) usage ;;
  esac
done

os=$(uname -s | tr '[:upper:]' '[:lower:]')
arch=$(uname -m)
case "$arch" in
  x86_64|amd64) arch="amd64" ;;
  arm64|aarch64) arch="arm64" ;;
  *) echo "Unsupported architecture: $arch"; exit 1 ;;
esac
case "$os" in
  linux)  target="linux-${arch}" ;;
  darwin) target="darwin-${arch}" ;;
  *) echo "Unsupported OS: $os"; exit 1 ;;
esac

# determine version
if [[ "$VERSION" == "latest" ]]; then
  # lightweight latest resolution without jq
  VERSION=$(curl -fsSL "https://api.github.com/repos/${REPO}/releases/latest" | grep -m1 '"tag_name":' | sed -E 's/.*"v?([^"]+)".*/\1/')
fi
echo "Installing MindScript ${VERSION} for ${target} to ${PREFIX}"

TARBALL="mindscript-${target}.tar.gz"
BASE_URL="https://github.com/${REPO}/releases/download/v${VERSION}"
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

curl -fL "${BASE_URL}/${TARBALL}" -o "${TMPDIR}/${TARBALL}"
curl -fL "${BASE_URL}/SHA256SUMS" -o "${TMPDIR}/SHA256SUMS" || true

if command -v sha256sum >/dev/null 2>&1 && [[ -f "${TMPDIR}/SHA256SUMS" ]]; then
  (cd "${TMPDIR}" && sha256sum -c --ignore-missing SHA256SUMS)
fi

# install
if [[ "$SYSTEM" -eq 1 ]]; then
  sudo mkdir -p "${PREFIX}"
  sudo tar -xzf "${TMPDIR}/${TARBALL}" -C "${PREFIX}" --strip-components=1
else
  mkdir -p "${PREFIX}"
  tar -xzf "${TMPDIR}/${TARBALL}" -C "${PREFIX}" --strip-components=1
fi

# write env snippets
write_shell_snippets() {
  local root="$1"
  local line1="export MSGPATH=\"${root}\""
  local line2='export PATH="$MSGPATH/bin:$PATH"'
  grep -qxF "$line1" "$PROFILE_SNIPPET_BASH" 2>/dev/null || echo "$line1" >> "$PROFILE_SNIPPET_BASH"
  grep -qxF "$line2" "$PROFILE_SNIPPET_BASH" 2>/dev/null || echo "$line2" >> "$PROFILE_SNIPPET_BASH"
  grep -qxF "$line1" "$PROFILE_SNIPPET_ZSH" 2>/dev/null || echo "$line1" >> "$PROFILE_SNIPPET_ZSH"
  grep -qxF "$line2" "$PROFILE_SNIPPET_ZSH" 2>/dev/null || echo "$line2" >> "$PROFILE_SNIPPET_ZSH"
  mkdir -p "$FISH_CONF_DIR"
  echo "set -gx MSGPATH \"${root}\"" > "${FISH_CONF_DIR}/mindscript.fish"
  echo 'set -gx PATH "$MSGPATH/bin" $PATH' >> "${FISH_CONF_DIR}/mindscript.fish"
}
if [[ "$SYSTEM" -eq 1 ]]; then
  sudo sh -c "echo 'export MSGPATH=${PREFIX}' >/etc/profile.d/mindscript.sh; echo 'export PATH=\$MSGPATH/bin:\$PATH' >> /etc/profile.d/mindscript.sh"
else
  write_shell_snippets "${PREFIX}"
fi

echo
echo "MindScript installed to ${PREFIX}"
echo "Open a new shell or run:  source ${PROFILE_SNIPPET_BASH}  (or your shell's profile)"
echo "Verify with:  msg --version"
