// modules.go — MindScript module system (public API + private implementation)
//
// OVERVIEW
// --------
// MindScript modules are ordinary MindScript programs whose *exported bindings*
// are snapshotted into a map-like value and paired with the lexical environment
// where the program executed.
//
// At runtime, a module is represented as a `VTModule` value whose payload is:
//
//	type Module struct {
//	  Name string     // canonical identity (path/URL) or caller-provided name
//	  Map  *MapObject // ordered export surface with per-key annotations
//	  Env  *Env       // lexical environment where the module executed
//	}
//
// Design goal: a module should behave ergonomically like a map. The interpreter
// exposes a small coercion (`AsMapValue`, defined in interpreter.go) that lets
// module values participate in all `VTMap` operations (length, overlay with '+',
// iteration, property/index reads, etc.) without duplicating map logic.
//
// PUBLIC API (this file)
// ----------------------
// The public surface is deliberately minimal and stable:
//
//   - (*Interpreter).ImportAST(name string, ast S) (Value, error)
//     Evaluate a *ready AST* as a module and return a `VTModule`. No cache.
//     Evaluation happens in a fresh env parented to the interpreter's Core.
//
//   - (*Interpreter).ImportCode(name, src string) (Value, error)
//     Parse `src` into an AST (with rich source-wrapped errors) and delegate
//     to ImportAST. No cache.
//
//   - (*Interpreter).ImportFile(spec, importer string) (Value, error)
//     Resolve + fetch + parse + evaluate with cycle detection and caching.
//     Resolution rules support both filesystem and absolute http(s) URLs.
//     Successful loads are cached by their *canonical* identity.
//
//   - (*Module).Get(key string) (Value, bool)
//     Return an exported binding and a presence flag. This mirrors the private
//     `get` method used by the VM for property/index reads.
//
// What ImportFile does, precisely:
//  1. Resolution & Fetching
//     • HTTP(S) — only absolute URLs are accepted. If the path lacks an
//     extension, `.ms` is appended. The canonical cache key is the full URL.
//     • Filesystem — resolve `spec` relative to the *importer’s directory*
//     (when importer is a file path), then the current working directory,
//     then each root in `MindScriptPath`. If `spec` lacks an extension,
//     try `spec + ".ms"` and then `spec`. The canonical key is the cleaned,
//     absolute path of the resolved file.
//  2. Cycle detection
//     A per-import call stack (`ip.loadStack`) plus an in-progress state guard
//     detects cycles and produces a friendly `A -> B -> … -> A` chain.
//  3. Parse + Evaluate
//     Parsing wraps syntax errors with the *original source* for good diagnostics.
//     Evaluation runs in an isolated child env of Core. Both runtime annotated
//     nulls and internal `rtErr` panics are converted into rich errors that
//     include a display name for the module being loaded.
//  4. Snapshot
//     The module’s public surface is captured into a `MapObject`:
//     • Exported keys are sorted lexicographically (deterministic order).
//     • Exported `VTType` values that lack a pinned env are rewritten via
//     `TypeValIn(..., modEnv)` so they resolve under the module’s env.
//     • If an exported value carries `Annot`, it is mirrored to `KeyAnn[key]`.
//
// Error semantics:
//   - Parse errors are reported as `parse error in <display>:\n<wrapped error>`.
//   - Runtime failures during module init are reported as
//     `runtime error in <display>: <message>`.
//   - Cycles are reported as `import cycle detected: A -> B -> … -> A`.
//
// Caching:
//   - Only successful loads are cached under the canonical identity.
//   - Failures are never cached.
//   - This cache is in-memory and persistent for the lifetime of the Interpreter.
//
// Concurrency:
//   - The module cache and import stack are not synchronized; callers should
//     avoid concurrent ImportFile calls on the same Interpreter.
//
// DEPENDENCIES (other files)
// -------------------------
//   - lexer.go / parser.go
//   - ParseSExpr(src string) (S, error) — building ASTs.
//   - interpreter.go
//   - types: Value, Env, MapObject, TypeValue, TypeValIn, Null
//   - methods: EvalASTUncaught, AsMapValue (for module/map coercion)
//   - struct: Interpreter (fields Core, modules, loadStack)
//   - errors.go
//   - WrapErrorWithSource(err, src string) error — enrich parse errors.
//   - vm.go
//   - The VM performs property/index reads via (*Module).get (private here).
//
// The remainder of this file is intentionally split into:
//  1. PUBLIC API — small, heavily documented wrappers that define behavior.
//  2. PRIVATE    — detailed implementation (resolution, fetching, parsing,
//     evaluation, snapshotting, caching, and cycle detection).
package mindscript

import (
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"path"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

////////////////////////////////////////////////////////////////////////////////
//                                   PUBLIC API
////////////////////////////////////////////////////////////////////////////////

// MindScript Library Path
const MindScriptPath = "MSGPATH"

// Module is the payload carried by a VTModule value.
//
// A Module’s public *map-like* surface is stored in Map (ordered exports with
// per-key annotations), while Env retains the lexical environment used during
// evaluation (closures/types capture from here). Name is the canonical identity
// (absolute path or full URL) when loaded from ImportFile, or the caller’s
// chosen label for ImportAST/ImportCode. See `AsMapValue` in interpreter.go for
// VTModule→VTMap coercion when consuming a module as a map.
type Module struct {
	Name string
	Map  *MapObject
	Env  *Env
}

// Get returns the exported binding named key and whether it exists.
// It mirrors the private `get` used by the VM for fast property/index reads.
func (m *Module) Get(key string) (Value, bool) { return m.get(key) }

// ImportAST evaluates a ready AST as a module.
//
// Behavior:
//   - Evaluates `ast` in a fresh environment parented to `ip.Core`.
//   - On success, returns a VTModule whose Module.Name = `name`.
//   - No cache and no cycle detection are involved.
//
// Errors:
//   - Runtime failures during evaluation (including annotated nulls) are
//     converted into rich errors whose messages include `name`.
func (ip *Interpreter) ImportAST(name string, ast S) (Value, error) {
	return ip.importAST(name, ast)
}

// ImportCode parses source and evaluates it as a module.
//
// Behavior:
//   - Parses `src` into an AST with source-wrapped diagnostics.
//   - Delegates to ImportAST using the same `name`.
//   - No cache and no cycle detection are involved.
//
// Errors:
//   - Syntax errors are wrapped with source and labeled with `name`.
//   - Runtime failures during evaluation are reported like ImportAST.
func (ip *Interpreter) ImportCode(name string, src string) (Value, error) {
	return ip.importCode(name, src)
}

// ImportFile resolves, fetches, parses, evaluates, caches, and detects cycles.
//
// Behavior:
//   - Resolution & fetching follow the rules described in this file header.
//   - Cycles are detected using a per-call import stack and an in-progress state.
//   - Successful loads are cached by canonical identity and returned from cache
//     on subsequent calls.
//   - On success, returns a VTModule whose Module.Name is the canonical identity.
//
// Errors:
//   - Parse/runtime errors are enriched with display context.
//   - Cycles are reported using a compact `A -> B -> … -> A` chain.
func (ip *Interpreter) ImportFile(spec string, importer string) (Value, error) {
	return ip.importFile(spec, importer)
}

//// END_OF_PUBLIC

////////////////////////////////////////////////////////////////////////////////
//                             PRIVATE IMPLEMENTATION
////////////////////////////////////////////////////////////////////////////////

// ---- Module runtime structs & VM hook --------------------------------------

type moduleState int

const (
	modUnloaded moduleState = iota
	modLoading
	modLoaded
)

// moduleRec tracks cached module state by canonical identity.
type moduleRec struct {
	spec        string
	displayName string
	src         string
	env         *Env
	mod         *Module
	state       moduleState
	err         error
}

// get returns an exported binding by key. The VM uses this for property/index reads.
func (m *Module) get(key string) (Value, bool) {
	v, ok := m.Map.Entries[key]
	return v, ok
}

// ---- Public-ish entry points (backing the exported wrappers) ---------------

// importAST creates a module from a READY AST, evaluating it in an Env parented to Core.
// The produced module is NOT cached (no cycles involved).
func (ip *Interpreter) importAST(name string, ast S) (Value, error) {
	m, err := ip.buildModuleFromAST(name, ast)
	if err != nil {
		return Null, err
	}
	m.Name = name
	return Value{Tag: VTModule, Data: m}, nil
}

// importCode parses source then delegates to importAST. No cache/cycle handling here.
func (ip *Interpreter) importCode(name string, src string) (Value, error) {
	ast, err := parseSource(name, src)
	if err != nil {
		return Null, err
	}
	return ip.importAST(name, ast)
}

// importFile resolves (FS/HTTP), parses, evaluates, caches successes, and detects cycles.
func (ip *Interpreter) importFile(spec string, importer string) (Value, error) {
	// Resolve + fetch → (src, display, canon)
	src, display, canon, rerr := resolveAndFetch(spec, importer)
	if rerr != nil {
		return Null, rerr
	}

	// Detect cycles against canonical key
	for _, s := range ip.loadStack {
		if s == canon {
			return Null, fmt.Errorf("import cycle detected: %s", joinCyclePath(ip.loadStack, canon))
		}
	}
	ip.loadStack = append(ip.loadStack, canon)
	defer func() { ip.loadStack = ip.loadStack[:len(ip.loadStack)-1] }()

	// Cache lookup
	if ip.modules == nil {
		ip.modules = map[string]*moduleRec{}
	}
	if rec, ok := ip.modules[canon]; ok {
		if rec.state == modLoading {
			return Null, fmt.Errorf("import cycle detected: %s", joinCyclePath(ip.loadStack, canon))
		}
		if rec.state == modLoaded && rec.mod != nil {
			return Value{Tag: VTModule, Data: rec.mod}, nil
		}
	}

	// Mark loading
	ip.modules[canon] = &moduleRec{spec: canon, state: modLoading}

	// Parse (shared helper)
	ast, perr := parseSource(display, src)
	if perr != nil {
		delete(ip.modules, canon) // do not cache failures
		return Null, perr
	}

	// Build module from AST (shared helper)
	m, err := ip.buildModuleFromAST(display, ast)
	if err != nil {
		delete(ip.modules, canon) // do not cache failures
		return Null, err
	}

	// Commit success to cache
	rec := ip.modules[canon]
	rec.displayName = display
	rec.src = src
	rec.env = m.Env
	m.Name = canon
	rec.mod = m
	rec.state = modLoaded
	rec.err = nil

	return Value{Tag: VTModule, Data: rec.mod}, nil
}

// ---- Shared helpers ---------------------------------------------------------

// parseSource parses src into an S-expr AST and wraps errors with source context.
func parseSource(display string, src string) (S, error) {
	ast, perr := ParseSExpr(src)
	if perr != nil {
		perr = WrapErrorWithSource(perr, src)
		return nil, fmt.Errorf("parse error in %s:\n%s", display, perr.Error())
	}
	return ast, nil
}

// buildModuleFromAST evaluates ast in an Env parented to Core and snapshots the public surface.
// Runtime annotated nulls and rtErr panics are converted to rich errors with display context.
func (ip *Interpreter) buildModuleFromAST(display string, ast S) (*Module, error) {
	modEnv := NewEnv(ip.Core)

	// Prefer direct evaluation that returns (Value, error).
	// This respects runtimeErrorsAsGoError=true and doesn't drop errors.
	var v Value
	var err error

	if len(ast) > 0 {
		v, err = ip.runTopWithSource(ast, modEnv, false, nil)
	}

	// Convert failures into the conventional module error wording.
	if err != nil {
		return nil, fmt.Errorf("runtime error in %s: %s", display, err.Error())
	}
	if v.Tag == VTNull && v.Annot != "" {
		return nil, fmt.Errorf("runtime error in %s: %s", display, v.Annot)
	}

	// Snapshot public surface (ordered keys, key annotations, pin VTType envs)
	mo := buildModuleMap(modEnv)

	return &Module{
		Map: mo,
		Env: modEnv,
	}, nil
}

// buildModuleMap snapshots modEnv.table into a MapObject:
// • Keys are sorted for determinism (Env.table is a Go map).
// • VTType exports without a pinned env are rewrapped with TypeValIn(..., modEnv).
// • If a value carries Annot, mirror it into KeyAnn for that key.
func buildModuleMap(modEnv *Env) *MapObject {
	keys := make([]string, 0, len(modEnv.table))
	for k := range modEnv.table {
		keys = append(keys, k)
	}
	sort.Strings(keys) // deterministic order

	mo := &MapObject{
		Entries: make(map[string]Value, len(keys)),
		KeyAnn:  make(map[string]string, len(keys)),
		Keys:    make([]string, 0, len(keys)),
	}
	for _, k := range keys {
		v := modEnv.table[k]

		// Pin exported types to the module env if needed
		if v.Tag == VTType {
			tv := v.Data.(*TypeValue)
			if tv.Env == nil {
				nv := TypeValIn(tv.Ast, modEnv)
				nv.Annot = v.Annot // preserve docs on the value
				v = nv
			}
		}

		mo.Entries[k] = v
		mo.Keys = append(mo.Keys, k)
		if ann := v.Annot; ann != "" {
			mo.KeyAnn[k] = ann
		}
	}
	return mo
}

// ---- Autoloader (resolution & fetching) ------------------------------------

const defaultModuleExt = ".ms" // adjust if you prefer a different extension

// resolveAndFetch returns (src, display, canonicalKey) for the given spec.
//
// Network:
//   - Absolute http(s) URLs are fetched via GET with a timeout.
//   - If the URL path has no extension, defaultModuleExt is appended.
//
// Filesystem:
//   - Resolve relative specs against importer dir → CWD → MindScriptPath.
//   - If spec has no extension, try spec+defaultModuleExt then spec.
//   - Returns canonical ABSOLUTE path (cleaned) as both display and cache key.
func resolveAndFetch(spec string, importer string) (string, string, string, error) {
	// Network?
	if strings.HasPrefix(spec, "http://") || strings.HasPrefix(spec, "https://") {
		u, perr := url.Parse(spec)
		if perr != nil {
			return "", "", "", fmt.Errorf("invalid import url: %w", perr)
		}
		if path.Ext(u.Path) == "" && defaultModuleExt != "" {
			u.Path = strings.TrimSuffix(u.Path, "/") + defaultModuleExt
		}
		canon := u.String()
		src, display, err := httpFetch(canon)
		return src, display, canon, err
	}

	// Filesystem
	canon, ferr := resolveFS(spec, importer)
	if ferr != nil {
		return "", "", "", ferr
	}
	b, rerr := os.ReadFile(canon)
	if rerr != nil {
		return "", "", "", fmt.Errorf("module not found: %s", spec)
	}
	return string(b), canon, canon, nil
}

func resolveFS(spec string, importer string) (string, error) {
	var bases []string
	if importer != "" && !strings.HasPrefix(importer, "http://") && !strings.HasPrefix(importer, "https://") {
		bases = append(bases, filepath.Dir(importer))
	}
	if cwd, err := os.Getwd(); err == nil {
		bases = append(bases, cwd)
	}

	try := func(base, s string) (string, bool) {
		cands := []string{}
		if filepath.Ext(s) != "" {
			cands = append(cands, filepath.Join(base, s))
		} else {
			cands = append(cands, filepath.Join(base, s)+defaultModuleExt, filepath.Join(base, s))
		}
		for _, c := range cands {
			if fi, err := os.Stat(c); err == nil && !fi.IsDir() {
				abs, _ := filepath.Abs(c)
				return filepath.Clean(abs), true
			}
		}
		return "", false
	}

	// Absolute path?
	if filepath.IsAbs(spec) {
		if p, ok := try("", spec); ok {
			return p, nil
		}
		// fallthrough to MindScriptPath for completeness
	} else {
		for _, b := range bases {
			if p, ok := try(b, spec); ok {
				return p, nil
			}
		}
	}

	// MindScriptPath
	if sp := os.Getenv(MindScriptPath); sp != "" {
		for _, root := range filepath.SplitList(sp) {
			if root == "" {
				continue
			}
			if p, ok := try(root, spec); ok {
				return p, nil
			}
		}
	}

	return "", fmt.Errorf("module not found: %s", spec)
}

func httpFetch(canonURL string) (src string, display string, err error) {
	client := &http.Client{Timeout: 15 * time.Second}
	resp, err := client.Get(canonURL)
	if err != nil {
		return "", canonURL, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		return "", canonURL, fmt.Errorf("http %d", resp.StatusCode)
	}
	b, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", canonURL, err
	}
	return string(b), canonURL, nil
}

// prettySpec returns a short display name for a canonical spec:
//   - file path   -> basename without extension
//   - http(s) URL -> last segment without extension
//   - fallback: original string if parsing fails
func prettySpec(s string) string {
	// Try URL first
	if u, err := url.Parse(s); err == nil && u.Scheme != "" {
		base := path.Base(u.Path)
		name := strings.TrimSuffix(base, path.Ext(base))
		if name != "" {
			return name
		}
		return base
	}
	// Filesystem path
	base := filepath.Base(s)
	name := strings.TrimSuffix(base, filepath.Ext(base))
	if name != "" {
		return name
	}
	return base
}

// "A -> B -> C -> A" using pretty names instead of full canonical specs.
func joinCyclePath(stack []string, again string) string {
	i := 0
	for idx, s := range stack {
		if s == again {
			i = idx
			break
		}
	}
	chain := append(stack[i:], again)
	out := make([]string, len(chain))
	for k, s := range chain {
		out[k] = prettySpec(s)
	}
	return strings.Join(out, " -> ")
}

func nativeMakeModule(ip *Interpreter, ctx CallCtx) Value {
	nameV := ctx.MustArg("name")
	bodyV := ctx.MustArg("body")
	baseV := ctx.MustArg("base")

	if nameV.Tag != VTStr {
		fail("module name must be a string")
	}
	if bodyV.Tag != VTType {
		fail("internal error: module body must be a Type")
	}

	tv := bodyV.Data.(*TypeValue)
	bodyAst := tv.Ast

	// decode absolute base path from [Int]
	var base NodePath
	if baseV.Tag == VTArray {
		xs := baseV.Data.([]Value)
		base = make(NodePath, 0, len(xs))
		for _, v := range xs {
			if v.Tag != VTInt {
				fail("internal error: module base path must be [Int]")
			}
			base = append(base, int(v.Data.(int64)))
		}
	}

	// fresh env
	modEnv := NewEnv(ip.Core)

	// SourceRef rooted at the module BODY path (absolute)
	var sr *SourceRef
	if ip.currentSrc != nil {
		sr = &SourceRef{
			Name:     ip.currentSrc.Name,
			Src:      ip.currentSrc.Src,
			Spans:    ip.currentSrc.Spans,
			PathBase: base, // IMPORTANT: use base as-is (no extra prefix)
		}
	}

	// JIT + run (like runTopWithSource, but we handle errors to avoid re-wrap)
	ch := ip.jitTop(bodyAst, sr)

	prev := ip.currentSrc
	ip.currentSrc = ch.Src
	res := ip.runChunk(ch, modEnv, 0)
	ip.currentSrc = prev

	switch res.status {
	case vmOK, vmReturn:
		// ok
	case vmRuntimeError:
		line, col := ip.sourcePosFromChunk(ch, ch.Src, res.pc)
		msg := res.value.Annot
		if msg == "" {
			msg = "runtime error"
		}
		// rethrow as structured inner-source error (single caret at true site)
		panicRt(msg, ch.Src, line, col)
	default:
		line, col := ip.sourcePosFromChunk(ch, ch.Src, res.pc)
		panicRt("unknown VM status", ch.Src, line, col)
	}

	// snapshot exports
	mo := buildModuleMap(modEnv)
	m := &Module{Name: nameV.Data.(string), Map: mo, Env: modEnv}
	return Value{Tag: VTModule, Data: m}
}
