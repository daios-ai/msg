#!/usr/bin/env bash
#set -euo pipefail

# For tests, we need to override the MindScript root directory
# inferred from the path of the binary.
export MSGPATH="$(pwd)"

# Single source of truth for base version
BASE_VERSION="$(cat VERSION)"
VERSION="${BASE_VERSION}-dev"
BUILD_DATE="$(date -u '+%Y-%m-%dT%H:%M:%SZ')"

echo "1) Tidying"
go mod tidy

echo "2a) MindScript Library: testing"
go test -v ./mindscript

echo "2b) MindScript Library: building"
go build ./mindscript

echo "3a) MindScript CLI: building and testing (version: ${VERSION})"
go build -trimpath \
  -ldflags="-s -w \
    -X github.com/daios-ai/msg/mindscript.Version=${VERSION} \
    -X github.com/daios-ai/msg/mindscript.BuildDate=${BUILD_DATE}" \
  -o ./bin/msg ./cmd/msg

./bin/msg test lib -v -p

printf "Done.\n\n"
