PROJECT   := mindscript
BIN1      := msg
BIN2      := msg-lsp
VERSION   := $(shell cat VERSION)
DISTDIR   := dist/$(VERSION)
TMPBIN    := dist/.tmp
GOFLAGS   := -trimpath -ldflags "-s -w"

COPY_DIRS := lib examples docs data
COPY_FILES:= VERSION LICENSE README.md

# ----- helpers -----
define stage_layout
	rm -rf $(1)/$(PROJECT)
	mkdir -p $(1)/$(PROJECT)/bin
	[ -f VERSION ] && cp VERSION $(1)/$(PROJECT)/ || true
	for f in $(COPY_FILES); do [ -f $$f ] && cp $$f $(1)/$(PROJECT)/ || true; done
	for d in $(COPY_DIRS); do [ -d $$d ] && cp -R $$d $(1)/$(PROJECT)/ || true; done
endef

# ----- builds (CGO ON) -----
build-linux-amd64:
	@echo ">> build linux/amd64 (CGO ON)"
	mkdir -p $(TMPBIN)
	CGO_ENABLED=1 GOOS=linux GOARCH=amd64 \
	  go build $(GOFLAGS) -o $(TMPBIN)/$(BIN1) ./cmd/$(BIN1)
	CGO_ENABLED=1 GOOS=linux GOARCH=amd64 \
	  go build $(GOFLAGS) -o $(TMPBIN)/$(BIN2) ./cmd/$(BIN2)

build-linux-arm64:
	@echo ">> build linux/arm64 (CGO ON)"
	mkdir -p $(TMPBIN)
	CGO_ENABLED=1 CC?=aarch64-linux-gnu-gcc GOOS=linux GOARCH=arm64 \
	  go build $(GOFLAGS) -o $(TMPBIN)/$(BIN1) ./cmd/$(BIN1)
	CGO_ENABLED=1 CC?=aarch64-linux-gnu-gcc GOOS=linux GOARCH=arm64 \
	  go build $(GOFLAGS) -o $(TMPBIN)/$(BIN2) ./cmd/$(BIN2)

build-darwin-amd64:
	@echo ">> build darwin/amd64 (CGO ON)"
	mkdir -p $(TMPBIN)
	SDKROOT=$$(xcrun --sdk macosx --show-sdk-path) \
	CGO_ENABLED=1 CC=clang GOOS=darwin GOARCH=amd64 \
	  go build $(GOFLAGS) -o $(TMPBIN)/$(BIN1) ./cmd/$(BIN1)
	SDKROOT=$$(xcrun --sdk macosx --show-sdk-path) \
	CGO_ENABLED=1 CC=clang GOOS=darwin GOARCH=amd64 \
	  go build $(GOFLAGS) -o $(TMPBIN)/$(BIN2) ./cmd/$(BIN2)

build-darwin-arm64:
	@echo ">> build darwin/arm64 (CGO ON)"
	mkdir -p $(TMPBIN)
	SDKROOT=$$(xcrun --sdk macosx --show-sdk-path) \
	CGO_ENABLED=1 CC=clang GOOS=darwin GOARCH=arm64 \
	  go build $(GOFLAGS) -o $(TMPBIN)/$(BIN1) ./cmd/$(BIN1)
	SDKROOT=$$(xcrun --sdk macosx --show-sdk-path) \
	CGO_ENABLED=1 CC=clang GOOS=darwin GOARCH=arm64 \
	  go build $(GOFLAGS) -o $(TMPBIN)/$(BIN2) ./cmd/$(BIN2)

# ----- packaging -----
pack-%: build-%
	@target=$*; out="$(DISTDIR)"; tmp="$(TMPBIN)"; \
	mkdir -p "$$out"; \
	$(call stage_layout,$$out); \
	mv "$$tmp/$(BIN1)" "$$out/$(PROJECT)/bin/$(BIN1)"; \
	mv "$$tmp/$(BIN2)" "$$out/$(PROJECT)/bin/$(BIN2)"; \
	( cd "$$out" && tar -czf "$(PROJECT)-$$target.tar.gz" "$(PROJECT)" && rm -rf "$(PROJECT)" ); \
	echo ">> packaged $$out/$(PROJECT)-$$target.tar.gz"

release: clean \
         pack-linux-amd64 \
         pack-linux-arm64 \
         pack-darwin-amd64 \
         pack-darwin-arm64
	@cd $(DISTDIR) && (sha256sum $(PROJECT)-*.tar.gz > SHA256SUMS || shasum -a 256 $(PROJECT)-*.tar.gz > SHA256SUMS)

clean:
	rm -rf dist
.PHONY: build-* pack-* release clean
