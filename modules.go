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
//	  Name string     // canonical identity (path/URL/memory-name)
//	  Map  *MapObject // ordered export surface with per-key annotations
//	  Env  *Env       // lexical environment where the module executed
//	}
//
// Ergonomics: a module should behave like a map. Use `AsMapValue` (see
// interpreter.go) to coerce VTModule→VTMap for length/overlay/iteration/property
// reads without duplicating map logic.
//
// NEW BEHAVIOR (uniform across all entry points)
// ---------------------------------------------
//
//  1. Cycle detection is **uniform** for *every* entry point (AST/Code/File/inline).
//     It happens inside the central constructor `nativeMakeModule`, so even inline
//     `module "…" do … end` participates. Errors are reported as hard errors:
//     "import cycle detected: A -> B -> … -> A".
//
//  2. Caching is **uniform** for *every* entry point. Successful module builds are
//     cached under their **canonical identity** (see below) by `nativeMakeModule`.
//     Subsequent constructions of the same canonical name return the cached module.
//
//     Canonical identity rules used throughout:
//     • ImportFile: absolute filesystem path (cleaned) or full https? URL.
//     • ImportCode / ImportAST: the exact `name` you pass in (no "mem:" prefix).
//     • Inline `module "Name"`: the string literal "Name".
//
// 3) Source mapping is **precise** for *every* entry point.
//
//   - ImportFile/ImportCode: parse with spans via ParseSExprWithSpans.
//
//   - ImportAST: first render to source via FormatSExpr, then parse with spans.
//
//   - Inline `module …`: VM re-roots spans to the body using an absolute NodePath.
//
//     4. Module names are **not mutated** post-construction. The value passed as the
//     "name" argument to `nativeMakeModule` **is** the Module.Name (the canonical
//     identity). We do not overwrite it afterward.
//
// MIND SCRIPT PATH
// ----------------
// The environment variable MSGPATH remains the library search path for files.
// The resolution order is unchanged (importer dir → CWD → each root in MSGPATH).
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

// MindScript Library Path (preserved)
const MindScriptPath = "MSGPATH"

// Module is the payload carried by a VTModule value.
type Module struct {
	Name string
	Map  *MapObject
	Env  *Env
}

// Get returns the exported binding named key and whether it exists.
func (m *Module) Get(key string) (Value, bool) { return m.get(key) }

// ImportAST evaluates a ready AST as a module with **precise** source mapping,
// **uniform** caching, and **uniform** cycle detection.
//
// Behavior:
//   - Canonical identity: the provided `name` string (unchanged).
//   - Formats the AST to a stable source string (FormatSExpr) and reparses it
//     with spans for caret-precise diagnostics.
//   - Executes in a fresh environment parented to ip.Core.
//   - Caching & cycle detection happen in nativeMakeModule (uniform with others).
//
// Errors:
//   - Parse errors are wrapped with source and returned as hard errors.
//   - Runtime outcomes propagate as produced by the runtime.
func (ip *Interpreter) ImportAST(name string, ast S) (Value, error) {
	// Round-trip AST → source → AST-with-spans for precise caret mapping.
	src := FormatSExpr(ast)
	parsed, spans, perr := parseSourceWithSpans(name, src)
	if perr != nil {
		return Null, perr
	}
	// Canonical identity is the exact name.
	canon := name
	return ip.importWithBody(canon, name, parsed, src, spans)
}

// ImportCode parses source and evaluates it as a module with **precise** source
// mapping, **uniform** caching, and **uniform** cycle detection.
//
// Behavior:
//   - Canonical identity: the provided `name` string (unchanged).
//   - Parses `src` into an AST with spans (precise diagnostics).
//   - Executes in a fresh environment parented to ip.Core.
//
// Errors:
//   - Syntax errors are wrapped with source and returned as hard errors.
//   - Runtime outcomes propagate as produced by the runtime.
func (ip *Interpreter) ImportCode(name string, src string) (Value, error) {
	ast, spans, perr := parseSourceWithSpans(name, src)
	if perr != nil {
		return Null, perr
	}
	canon := name
	return ip.importWithBody(canon, name, ast, src, spans)
}

// ImportFile resolves, fetches, parses, evaluates, *and* participates in the
// same **uniform** caching/cycle-detection as other entry points.
//
// Behavior:
//   - Canonical identity: absolute path (filesystem) or full URL (http/https).
//   - Resolution & fetching follow the preserved rules (see header).
//   - If resolution/fetch fails, returns **annotated null** with nil Go error.
//   - Parses with spans for precise carets and evaluates in a fresh env.
//
// Caching:
//   - Uniform caching is centralized in nativeMakeModule (so inline/AST/code/file
//     all share the same semantics). ImportFile still resolves+reads the source;
//     if the canonical module is already loaded, execution short-circuits inside
//     nativeMakeModule and reuses the cached module.
func (ip *Interpreter) ImportFile(spec string, importer string) (Value, error) {
	src, display, canon, rerr := resolveAndFetch(spec, importer)
	if rerr != nil {
		// Operational/soft: return annotated null; nil Go error.
		return annotNull(fmt.Sprintf("import %q: %v", spec, rerr)), nil
	}
	ast, spans, perr := parseSourceWithSpans(display, src)
	if perr != nil {
		return Null, perr
	}
	// Pass both canonical identity and display name to preserve error labels.
	return ip.importWithBody(canon, display, ast, src, spans)
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

// ---- Unified module import path --------------------------------------------

// importWithBody evaluates a prepared module BODY AST by lowering to
// ("module", ("str", canonName), body) and running it via runTopWithSource under
// a SourceRef that points to the module’s own source text and spans.
//
// Cycle detection and caching are **centralized** in nativeMakeModule so that
// *all* entry points (AST/Code/File/inline) share identical semantics.
func (ip *Interpreter) importWithBody(canonName string, display string, body S, src string, spans *SpanIndex) (Value, error) {
	// Prepare SourceRef for the module source so emitter can pass the absolute
	// body path to __make_module, and the VM can render correct carets.
	var sr *SourceRef
	if src != "" && spans != nil {
		sr = &SourceRef{Name: display, Src: src, Spans: spans}
	}

	// Lower to ("module", <canonical name>, body). Name is NOT overwritten later.
	modAst := S{"module", S{"str", canonName}, body}

	// Evaluate in an env that sees Core + natives, with proper VM entry.
	env := NewEnv(ip.Core)
	env.SealParentWrites()
	v, err := ip.runTopWithSource(modAst, env, false, sr)
	if err != nil {
		return Null, err // hard runtime/parse errors with carets
	}
	if v.Tag != VTModule {
		return Null, fmt.Errorf("internal error: expected module value")
	}
	return v, nil
}

// parseSourceWithSpans parses src into an S-expr AST + spans and wraps errors
// with source context. Prefer this whenever you have the source text so we can
// produce precise caret diagnostics during module execution.
func parseSourceWithSpans(display string, src string) (S, *SpanIndex, error) {
	ast, spans, perr := ParseSExprWithSpans(src)
	if perr != nil {
		return nil, nil, WrapErrorWithName(perr, display, src)
	}
	return ast, spans, nil
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

const defaultModuleExt = ".ms" // preserved

// resolveAndFetch returns (src, display, canonicalKey) for the given spec.
//
// Network:
//   - Absolute http(s) URLs are fetched via GET with a timeout.
//   - If the URL path has no extension, defaultModuleExt is appended.
//
// Filesystem:
//   - Resolve relative specs against importer dir → CWD → MindScriptPath (MSGPATH).
//   - If spec has no extension, try spec+defaultModuleExt then spec.
//   - Returns canonical ABSOLUTE path (cleaned) as both display and cache key.
//
// NOTE: This function returns Go errors; ImportFile is responsible for classifying
// resolution/fetch failures as soft (annotated null) at the API boundary.
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
	// Prefer the importer’s directory, if present and not a URL.
	if importer != "" && !strings.HasPrefix(importer, "http://") && !strings.HasPrefix(importer, "https://") {
		bases = append(bases, filepath.Dir(importer))
	}
	// Then the current working directory.
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

	// MindScriptPath (MSGPATH) — preserved behavior
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
	// Filesystem path (or arbitrary name)
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

// nativeMakeModule is the implementation of the __make_module primitive.
//
// UNIFORM CACHING & CYCLE DETECTION LIVE HERE.
// This ensures AST/Code/File/inline constructions all behave the same.
//
// It receives:
//   - name: Str   — the **canonical identity** for the module (NOT overwritten).
//   - body: Type  — AST for the module body wrapped as a type value.
//   - base: [Int] — absolute NodePath indicating where the body lives in the
//     caller’s SourceRef; used to re-root spans to the body.
//
// Plumbing:
//   - We build a child SourceRef with PathBase=base so VM marks and PC→(line,col)
//     map into the module body text.
//   - Runtime errors are rethrown with exact location using panicRt, so they
//     bubble to runTopWithSource and render a single caret at the true site.
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
	canon := nameV.Data.(string)

	// ---- Uniform cycle detection (stack + in-progress record) ----
	for _, s := range ip.loadStack {
		if s == canon {
			fail(fmt.Sprintf("import cycle detected: %s", joinCyclePath(ip.loadStack, canon)))
		}
	}
	if ip.modules != nil {
		if rec, ok := ip.modules[canon]; ok && rec.state == modLoading {
			fail(fmt.Sprintf("import cycle detected: %s", joinCyclePath(append(ip.loadStack, canon), canon)))
		}
	}

	// ---- Uniform caching (success-only) ----
	if ip.modules != nil {
		if rec, ok := ip.modules[canon]; ok && rec.state == modLoaded && rec.mod != nil {
			return Value{Tag: VTModule, Data: rec.mod}
		}
	} else {
		ip.modules = map[string]*moduleRec{}
	}

	// Mark as loading and push on stack.
	ip.modules[canon] = &moduleRec{spec: canon, state: modLoading}
	ip.loadStack = append(ip.loadStack, canon)

	// Ensure we never leave a stale modLoading record or a stuck stack entry.
	// On panic/failure, delete the cache record; always pop loadStack.
	defer func() {
		// Pop load stack
		if n := len(ip.loadStack); n > 0 {
			ip.loadStack = ip.loadStack[:n-1]
		}
		// If not successfully flipped to modLoaded, remove the half-built record.
		if rec, ok := ip.modules[canon]; ok && rec.state != modLoaded {
			delete(ip.modules, canon)
		}
		// Preserve existing error semantics.
		if r := recover(); r != nil {
			panic(r)
		}
	}()

	// ---- Decode body AST and base path ----
	tv := bodyV.Data.(*TypeValue)
	bodyAst := tv.Ast

	// Decode absolute base path from [Int]
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

	// Fresh env for the module (Core is parent so builtins are visible).
	modEnv := NewEnv(ip.Core)
	modEnv.SealParentWrites()

	// SourceRef rooted at the module BODY path (absolute)
	var sr *SourceRef
	if ip.currentSrc != nil {
		sr = &SourceRef{
			Name:     ip.currentSrc.Name,
			Src:      ip.currentSrc.Src,
			Spans:    ip.currentSrc.Spans,
			PathBase: base, // IMPORTANT: absolute path into the caller source tree
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
		// Rethrow as structured inner-source error (single caret at true site).
		panicRt(msg, ch.Src, line, col)
	default:
		line, col := ip.sourcePosFromChunk(ch, ch.Src, res.pc)
		panicRt("unknown VM status", ch.Src, line, col)
	}

	// Snapshot exports
	mo := buildModuleMap(modEnv)
	m := &Module{Name: canon, Map: mo, Env: modEnv}

	// Commit cache (success-only)
	rec := ip.modules[canon]
	rec.mod = m
	rec.env = modEnv
	rec.state = modLoaded
	rec.err = nil

	return Value{Tag: VTModule, Data: m}
}
