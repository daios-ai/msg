#!/usr/bin/env bash
#set -euo pipefail

#
# BUILD SCRIPT FOR DEVELOPMENT PURPOSES.
#
# WARNING: This is strictly experimental!
#

# For tests, we need to override the MindScript root directory
# inferred from the path of the binary.
export MSGPATH="$(pwd)"

# Single source of truth for base version
BASE_VERSION="$(cat VERSION)"
VERSION="${BASE_VERSION}-dev"
BUILD_DATE="$(date -u '+%Y-%m-%dT%H:%M:%SZ')"

echo "Building MindScript with tinygo."
echo ""
echo "WARNING: THIS IS STRICLY EXPERIMENTAL."
echo ""

echo "1) Tidying"
go mod tidy

echo "2) MindScript Library: testing (tinygo)"
echo "SKIPPED DUE TO LACK OF SUPPORT"
# tinygo test -v ./mindscript

echo "3) MindScript CLI: building (tinygo)"
tinygo build \
  -ldflags="
    -X github.com/daios-ai/msg/mindscript.Version=${VERSION} \
    -X github.com/daios-ai/msg/mindscript.BuildDate=${BUILD_DATE}
  " \
  -o ./bin/msg ./cmd/msg

echo "4) MindScript CLI: testing (tinygo)"
./bin/msg test lib -v

printf "Done.\n\n"
