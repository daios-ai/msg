# MindScript packaging notes (WHY this Makefile is structured this way)
# --------------------------------------------------------------------
# Problem:
# - We must ensure the binaries always use the bundled libffi (in mindscript/lib) rather than
#   whatever is installed on a user’s system.
# - macOS and Linux behave differently:
#   • macOS binaries record a dylib’s “install name”. If that points to a Homebrew path, things
#     can break when Homebrew updates or moves files. Discovering/copying *after* build was brittle.
#   • Linux resolves .so files from filesystem search paths, and distros place libffi.so.N in
#     different locations (/usr/lib/x86_64-linux-gnu, /usr/lib64, etc.), so guessing one path can fail.
#
# Fix (combined strategy):
# - We set RPATH so binaries look in ../lib relative to themselves at runtime.
# - macOS: Always pre-vendor. Copy libffi*.dylib into repo ./lib and set its install name to
#   @loader_path/../lib/<file>. Build against that vendored copy, and ship the same file.
# - Linux: Prefer pre-vendor. Try to find libffi.so.* up front (pkg-config + common multi-arch
#   dirs), copy the real file into repo ./lib and create a SONAME symlink; build against it.
#   If pre-vendoring isn’t possible, fall back *after* build: use ldd on the built binary to
#   discover the actual resolved libffi path, then copy that exact file (plus SONAME symlink)
#   into dist/mindscript/lib.
# Result:
# - macOS gets a stable, bundle-relative install name we control.
# - Linux remains resilient to distro layout differences while still shipping the exact .so that
#   satisfies the binary’s NEEDED entry.
#
# Minimal MindScript Makefile (auto-detects host OS/arch and builds that one)
# Targets:
#   make            → build + package for the current machine
#   make build      → just build binaries into dist/mindscript/bin
#   make package    → tar.gz + checksum
#   make clean      → remove dist/

# --- autodetect host ---
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

# Map to Go’s GOOS/GOARCH and to our tarball naming
ifeq ($(UNAME_S),Darwin)
  GOOS := darwin
  ifeq ($(UNAME_M),arm64)
    GOARCH := arm64
    TARNAME := mindscript-macos-arm64
  else
    $(error macOS x86_64 is not a supported release target)
  endif
else ifeq ($(UNAME_S),Linux)
  GOOS := linux
  ifeq ($(UNAME_M),x86_64)
    GOARCH := amd64
    TARNAME := mindscript-linux-x86_64
  else ifeq ($(UNAME_M),aarch64)
    GOARCH := arm64
    TARNAME := mindscript-linux-arm64
  else ifeq ($(UNAME_M),arm64)
    GOARCH := arm64
    TARNAME := mindscript-linux-arm64
  else
    $(error Unsupported Linux arch: $(UNAME_M). Expected x86_64 or (aarch64/arm64))
  endif
else
  $(error Unsupported OS: $(UNAME_S))
endif

# --- version & paths ---
VERSION ?= $(shell git describe --tags --always 2>/dev/null || echo 0.0.0)
DIST_DIR := dist
ROOT     := $(DIST_DIR)/mindscript
BIN_DIR  := $(ROOT)/bin
LIB_DIR  := $(ROOT)/lib
# where we vendor native libs inside the repo (already copied into $(ROOT)/lib later)
VENDOR_LIB_DIR := lib

# --- CGO + libffi ---
export CGO_ENABLED := 1
# For macOS Homebrew libffi (pkg-config lives off the default path)
ifeq ($(UNAME_S),Darwin)
  export PKG_CONFIG_PATH ?= $(shell brew --prefix 2>/dev/null)/opt/libffi/lib/pkgconfig
endif

# RPATH so binaries search ../lib next to themselves
# NOTE: backslash escapes '$' for the shell so the linker sees literal $ORIGIN
RPATH_LINUX := -Wl,-rpath,\$$ORIGIN/../lib
RPATH_MACOS := -Wl,-rpath,@loader_path/../lib

# --- checksum tool ---
ifeq ($(UNAME_S),Darwin)
  SHASUM := shasum -a 256
else
  SHASUM := sha256sum
endif

# --- default ---
.PHONY: all build package clean print check-deps vendor-libffi test-go test-ms
all: print check-deps vendor-libffi test-go build test-ms package

print:
	@echo "Building for: GOOS=$(GOOS) GOARCH=$(GOARCH)  VERSION=$(VERSION)"

check-deps:
	@command -v pkg-config >/dev/null 2>&1 || { echo "Error: pkg-config not found."; exit 1; }
	@pkg-config --exists libffi || { \
		echo "Error: libffi dev files not found (pkg-config name: libffi)."; \
		echo "  Ubuntu:  sudo apt-get install -y libffi-dev pkg-config"; \
		echo "  macOS:   brew install libffi"; \
		exit 1; }

# Prepare vendored libffi in <repo>/lib so build-time == ship-time
# - macOS: required (fail if not found)
# - Linux: best effort; if not found, we'll fall back post-build using ldd
vendor-libffi:
	@mkdir -p $(VENDOR_LIB_DIR)
ifeq ($(UNAME_S),Linux)
	@set -e; \
	LIBDIR=$$(pkg-config --variable=libdir libffi); \
	FOUND=""; \
	for C in "$$LIBDIR"/libffi.so* /usr/lib/*-linux-gnu/libffi.so* /lib/*-linux-gnu/libffi.so* /usr/lib64/libffi.so* /usr/lib/libffi.so*; do \
	  for F in $$C; do \
	    if [ -e "$$F" ]; then FOUND="$$F"; break 2; fi; \
	  done; \
	done; \
	if [ -n "$$FOUND" ]; then \
	  REAL=$$(readlink -f "$$FOUND"); \
	  BASE=$$(basename "$$REAL"); \
	  cp -f "$$REAL" "$(VENDOR_LIB_DIR)/$$BASE"; \
	  SONAME=$$(readelf -d "$$REAL" | awk '/SONAME/{gsub(/[\[\]]/,"");print $$5; exit}'); \
	  if [ -n "$$SONAME" ]; then ln -sfn "$$BASE" "$(VENDOR_LIB_DIR)/$$SONAME"; fi; \
	  echo "Vendored Linux libffi: $(VENDOR_LIB_DIR)/$$BASE (SONAME=$$SONAME)"; \
	else \
	  echo "Note: could not pre-vendor libffi on Linux; will discover post-build."; \
	fi
else ifeq ($(UNAME_S),Darwin)
	@set -e; \
	LIBDIR=$$(pkg-config --variable=libdir libffi 2>/dev/null || echo "$$(brew --prefix)/opt/libffi/lib"); \
	CAND=$$(ls -1 "$$LIBDIR"/libffi*.dylib 2>/dev/null | head -n1); \
	if [ -z "$$CAND" ]; then echo "Error: libffi dylib not found in $$LIBDIR"; exit 1; fi; \
	BASE=$$(basename "$$CAND"); \
	cp -f "$$CAND" "$(VENDOR_LIB_DIR)/$$BASE"; \
	install_name_tool -id "@loader_path/../lib/$$BASE" "$(VENDOR_LIB_DIR)/$$BASE"; \
	echo "Vendored macOS libffi: $(VENDOR_LIB_DIR)/$$BASE (id set to @loader_path/../lib/$$BASE)"
endif

# --- tests ---
test-go:
	@echo "Running Go tests..."
	@mkdir -p bin
	MSGPATH=$(PWD) go test -v ./internal/mindscript
	MSGPATH=$(PWD) go test -v ./cmd/msg-lsp

test-ms:
	@echo "Running MindScript stdlib tests with staged binary..."
	MSGPATH=$(ROOT) $(BIN_DIR)/msg test lib -v

build:
	@rm -rf $(ROOT)
	@mkdir -p $(BIN_DIR) $(LIB_DIR)
ifeq ($(UNAME_S),Linux)
	GOOS=$(GOOS) GOARCH=$(GOARCH) CGO_LDFLAGS="-L$(PWD)/$(VENDOR_LIB_DIR) -lffi" go build -trimpath \
	 -ldflags='-linkmode external -extldflags "$(RPATH_LINUX)" -s -w' \
	 -o $(BIN_DIR)/msg     ./cmd/msg
	GOOS=$(GOOS) GOARCH=$(GOARCH) CGO_LDFLAGS="-L$(PWD)/$(VENDOR_LIB_DIR) -lffi" go build -trimpath \
	 -ldflags='-linkmode external -extldflags "$(RPATH_LINUX)" -s -w' \
	 -o $(BIN_DIR)/msg-lsp ./cmd/msg-lsp
	@# If we didn't successfully vendor libffi before build, fall back to post-build discovery (old, proven path)
	@if ! ls "$(VENDOR_LIB_DIR)"/libffi.so* >/dev/null 2>&1; then \
	  set -e; \
	  CAND=$$(ldd $(BIN_DIR)/msg | awk '/libffi\.so/{print $$3; exit}'); \
	  test -n "$$CAND"; \
	  REAL=$$(readlink -f "$$CAND"); \
	  BASENAME=$$(basename "$$REAL"); \
	  cp -v "$$REAL" "$(LIB_DIR)/$$BASENAME"; \
	  SONAME=$$(readelf -d "$$REAL" | awk '/SONAME/{gsub(/[\[\]]/,"");print $$5; exit}'); \
	  if [ -n "$$SONAME" ]; then ln -sfn "$$BASENAME" "$(LIB_DIR)/$$SONAME"; fi; \
	  echo "Post-build discovered Linux libffi: $$REAL → $(LIB_DIR)/$$BASENAME (SONAME=$$SONAME)"; \
	fi
else ifeq ($(UNAME_S),Darwin)
	GOOS=$(GOOS) GOARCH=$(GOARCH) CGO_LDFLAGS="-L$(PWD)/$(VENDOR_LIB_DIR) -lffi" go build -trimpath \
	 -ldflags='-linkmode external -extldflags "$(RPATH_MACOS)" -s -w' \
	 -o $(BIN_DIR)/msg     ./cmd/msg
	GOOS=$(GOOS) GOARCH=$(GOARCH) CGO_LDFLAGS="-L$(PWD)/$(VENDOR_LIB_DIR) -lffi" go build -trimpath \
	 -ldflags='-linkmode external -extldflags "$(RPATH_MACOS)" -s -w' \
	 -o $(BIN_DIR)/msg-lsp ./cmd/msg-lsp
	@# On macOS we pre-vendored and set the dylib id; binaries now reference @loader_path/../lib/<BASE>
endif
	@test -d lib      && cp -R lib      $(ROOT)/ || true
	@test -d examples && cp -R examples $(ROOT)/ || true
	@test -d docs     && cp -R docs     $(ROOT)/ || true
	@test -d data     && cp -R data     $(ROOT)/ || true
	@test -f LICENSE  && cp LICENSE     $(ROOT)/ || true
	@test -f README.md && cp README.md  $(ROOT)/ || true
	@echo "$(VERSION)" > $(ROOT)/VERSION

package:
	@mkdir -p $(DIST_DIR)
	tar -C $(DIST_DIR) -czf $(TARNAME).tar.gz mindscript
	@$(SHASUM) $(TARNAME).tar.gz > $(TARNAME).tar.gz.sha256
	@echo "Created: $(TARNAME).tar.gz (+ .sha256)"

clean:
	@rm -rf $(DIST_DIR)
