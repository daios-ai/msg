#!/usr/bin/env bash
set -euo pipefail

REPO="mindscript-lang/mindscript"   # <-- change if needed
DEFAULT_BASE="${HOME}/.mindscript"
VERSION="${VERSION:-$(curl -fsSL https://raw.githubusercontent.com/${REPO}/main/VERSION)}"

# Detect OS/arch
uname_os="$(uname -s | tr '[:upper:]' '[:lower:]')"
case "$uname_os" in
  linux)  OS=linux ;;
  darwin) OS=darwin ;;
  *) echo "Unsupported OS: $uname_os"; exit 1 ;;
esac

uname_arch="$(uname -m)"
case "$uname_arch" in
  x86_64|amd64) ARCH=amd64 ;;
  arm64|aarch64) ARCH=arm64 ;;
  *) echo "Unsupported arch: $uname_arch"; exit 1 ;;
esac

# Where to install
BASE="${MSGPATH:-$DEFAULT_BASE}"
TARGET="${BASE}/versions/${VERSION}"
BINLINK="${BASE}/current"

# URLs
BASE_URL="https://github.com/${REPO}/releases/download/v${VERSION}"
TARBALL="mindscript-${OS}-${ARCH}-${VERSION}.tar.gz"
SHAFILE="checksums.txt"

echo "MindScript ${VERSION} for ${OS}/${ARCH}"
echo "Install base: ${BASE}"

mkdir -p "${BASE}/downloads"
cd "${BASE}/downloads"

# Fetch tarball + checksums
curl -fL -o "${TARBALL}" "${BASE_URL}/${TARBALL}"
curl -fL -o "${SHAFILE}" "${BASE_URL}/${SHAFILE}"

# Verify checksum
if command -v sha256sum >/dev/null 2>&1; then
  sha256sum --check <(grep " ${TARBALL}\$" "${SHAFILE}") --status || { echo "Checksum failed"; exit 1; }
else
  shasum -a 256 -c <(grep " ${TARBALL}\$" "${SHAFILE}") || { echo "Checksum failed"; exit 1; }
fi

# Unpack into versioned dir
mkdir -p "${TARGET}"
tar -C "${TARGET}" -xzf "${TARBALL}"

# Update 'current' symlink
ln -sfn "${TARGET}" "${BINLINK}"

# Write profile snippet(s)
MSGPATH_EXPORT="export MSGPATH=\"${BINLINK}\""
PATH_EXPORT='export PATH="$MSGPATH/bin:$PATH"'

write_snippet() {
  local file="$1"
  local line="$2"
  grep -Fq "$line" "$file" 2>/dev/null || echo "$line" >> "$file"
}

mkdir -p "${HOME}/.config" 2>/dev/null || true

# bash/zsh
PROFILE="${HOME}/.profile"
touch "$PROFILE"
write_snippet "$PROFILE" "$MSGPATH_EXPORT"
write_snippet "$PROFILE" "$PATH_EXPORT"

# fish
FISHDIR="${HOME}/.config/fish/conf.d"
mkdir -p "$FISHDIR"
cat > "${FISHDIR}/mindscript.fish" <<EOF
set -gx MSGPATH "${BINLINK}"
if not contains \$MSGPATH/bin \$fish_user_paths
  set -g fish_user_paths \$MSGPATH/bin \$fish_user_paths
end
EOF

echo
echo "Installed to: ${TARGET}"
echo "MSGPATH -> ${BINLINK}"
echo "Add to current shell: run 'source ${PROFILE}' (or open a new terminal)."
echo
echo "Try: msg --version && msg run examples/hello.ms"
