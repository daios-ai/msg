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
//	  Map  *MapObject // ordered export surface
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
//  3. Source mapping is **precise** for *every* entry point.
//
//     - ImportFile/ImportCode: parse with spans via ParseSExprWithSpans.
//     - ImportAST: first render to source via FormatSExpr, then parse with spans.
//     - Inline `module …`: VM re-roots spans to the body using an absolute NodePath.
//
//  4. Module names are **not mutated** post-construction. The value passed as the
//     "name" argument to `nativeMakeModule` **is** the Module.Name (the canonical
//     identity). We do not overwrite it afterward.
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
//   - Resolution & fetching follow the no-MSGPATH policy:
//   - Relative specs resolve against the importing file's directory,
//     or the REPL's current working directory if there is no importer.
//   - If not found, fall back to the standard library at <install-root>/lib.
//   - If resolution/fetch fails, returns **annotated null** with nil Go error.
//   - Parses with spans for precise carets and evaluates in a fresh env.
//
// Caching:
//   - Uniform caching is centralized in nativeMakeModule.
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
	spec  string
	env   *Env
	mod   *Module
	state moduleState
	err   error
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
// modules.go: importWithBody

func (ip *Interpreter) importWithBody(canonName, display string, body S, src string, spans *SpanIndex) (Value, error) {
	var sr *SourceRef
	if src != "" && spans != nil {
		// ⬇️ use the parser-faithful wrapper
		wrapped := wrapUnderModuleLikeParser(body, spans, canonName)
		sr = &SourceRef{Name: display, Src: src, Spans: wrapped}
	}

	modAst := S{"module", S{"str", canonName}, body}
	env := NewEnv(ip.Core)
	env.SealParentWrites()
	return ip.runTopWithSource(modAst, env, false, sr)
}

// modules.go

// New signature: we need the body AST to walk it in post-order.
func wrapUnderModuleLikeParser(bodyAST S, bodyIdx *SpanIndex, canonName string) *SpanIndex {
	if bodyIdx == nil {
		return nil
	}

	// Build ("module", ("str", canonName), body) for a faithful traversal shape.
	mod := S{"module", S{"str", canonName}, bodyAST}

	// Body root span drives module extents.
	bodyRoot, _ := bodyIdx.Get(nil)

	// Synthesize a zero-width name span at the body's start (best available anchor).
	nameSpan := Span{StartByte: bodyRoot.StartByte, EndByte: bodyRoot.StartByte}
	modSpan := Span{StartByte: bodyRoot.StartByte, EndByte: bodyRoot.EndByte}

	// Gather body spans in post-order by walking the *body AST* and querying bodyIdx.
	post := make([]Span, 0, 2+len(bodyIdx.byPath)) // rough capacity
	var walk func(n S, path NodePath)
	walk = func(n S, path NodePath) {
		for i := 1; i < len(n); i++ {
			if c, ok := n[i].(S); ok {
				walk(c, append(path, i-1))
			}
		}
		if sp, ok := bodyIdx.Get(path); ok {
			post = append(post, sp)
		} else {
			// If a node lacks a span (shouldn’t happen from the parser), keep cardinality.
			post = append(post, Span{})
		}
	}

	// Post-order for the wrapper: [name] + [body subtree] + [module]
	post = append(post, nameSpan)
	walk(bodyAST, nil)
	post = append(post, modSpan)

	// Rebuild a fresh index, exactly like the parser would for `mod`.
	return BuildSpanIndexPostOrder(mod, post)
}

// parseSourceWithSpans parses src into an S-expr AST + spans and wraps errors
// with source context. Prefer this whenever you have the source text so we can
// produce precise caret diagnostics during module execution.
func parseSourceWithSpans(display string, src string) (S, *SpanIndex, error) {
	ast, spans, err := ParseSExprWithSpans(src)
	if err != nil {
		if e, ok := err.(*Error); ok {
			if e.Src == nil {
				e.Src = &SourceRef{Name: display, Src: src}
			}
			return nil, nil, e // return structured error; pretty-print at API boundary

		}
		return nil, nil, err
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
//   - Resolve relative specs against importer dir → (REPL) CWD → stdlib <install-root>/lib.
//   - If spec has no extension, try spec+defaultModuleExt then spec.
//   - Returns canonical ABSOLUTE path (cleaned) as both display and cache key.
//
// NOTE: This function returns Go errors; ImportFile is responsible for classifying
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
	// In REPL (no importer), or as a fallback, use the current working directory.
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
		// fallthrough to stdlib for completeness.
	} else {
		for _, b := range bases {
			if p, ok := try(b, spec); ok {
				return p, nil
			}
		}
	}

	// Standard library fallback: <install-root>/lib
	if installRoot != "" {
		libRoot := filepath.Join(installRoot, "lib")
		if p, ok := try(libRoot, spec); ok {
			return p, nil
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
