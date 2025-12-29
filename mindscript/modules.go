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
// RESOLUTION RULES (clean + uniform)
// ---------------------------------
//
//   - Imports are extensionless: any spec whose last path segment has an
//     extension is rejected.
//
//   - A spec names either a file stem or a directory stem:
//     stem.ms        (file form)
//     stem/init.ms   (directory form)
//     If both exist, the import is ambiguous (hard error).
//     If neither exists in the chosen search base(s), it's not found (hard error).
//
//   - Absolute specs resolve only at that exact location:
//
//   - http(s) URL: probe <url>.ms and <url>/init.ms
//
//   - abs path:    probe <path>.ms and <path>/init.ms
//
//   - Relative specs search bases in order:
//     1) importer directory (URL dir if importer is URL; filesystem dir otherwise;
//     REPL: importer dir = CWD)
//     2) <install-root>/lib (filesystem only)
//
// Cycle detection, caching, and module construction are centralized in
// nativeMakeModule (interpreter_ops.go), so all entry points share identical
// semantics for those concerns.
package mindscript

import (
	"errors"
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
func (ip *Interpreter) ImportCode(name string, src string) (Value, error) {
	ast, spans, perr := parseSourceWithSpans(name, src)
	if perr != nil {
		return Null, perr
	}
	canon := name
	return ip.importWithBody(canon, name, ast, src, spans)
}

// ImportFile resolves, fetches, parses, evaluates, *and* participates in the
// same uniform caching/cycle-detection as other entry points.
//
// Resolution is described at the top of this file (clean + uniform).
func (ip *Interpreter) ImportFile(spec string, importer string) (Value, error) {
	spec = strings.TrimSuffix(spec, "/")

	// Enforce extensionless imports (for both FS and URLs).
	if _, sugg, ok := splitSpecExt(spec); ok {
		return Null, fmt.Errorf("imports are extensionless; write import(%q), not import(%q)", sugg, spec)
	}

	// Absolute URL?
	if isHTTPURL(spec) {
		target, err := pickURLTarget(spec)
		if err != nil {
			if errors.Is(err, os.ErrNotExist) {
				return Null, fmt.Errorf("module not found: %s", spec)
			}
			return Null, err
		}
		src, _, ferr := httpFetch(target)
		if ferr != nil {
			return Null, ferr
		}
		ast, spans, perr := parseSourceWithSpans(spec, src)
		if perr != nil {
			return Null, perr
		}
		// Canonical identity is the fully resolved target URL.
		return ip.importWithBody(target, spec, ast, src, spans)
	}

	// Absolute filesystem path?
	if filepath.IsAbs(spec) {
		target, err := pickFSTarget(spec)
		if err != nil {
			if errors.Is(err, os.ErrNotExist) {
				return Null, fmt.Errorf("module not found: %s", spec)
			}
			return Null, err
		}
		b, rerr := os.ReadFile(target)
		if rerr != nil {
			return Null, rerr
		}
		src := string(b)
		ast, spans, perr := parseSourceWithSpans(spec, src)
		if perr != nil {
			return Null, perr
		}
		// Canonical identity is the absolute resolved path.
		return ip.importWithBody(target, spec, ast, src, spans)
	}

	// Relative: search (1) importer dir, then (2) <installRoot>/lib (FS only).
	type cand struct {
		target string
		isURL  bool
	}
	var candidates []cand

	// Base #1: importer directory (URL or FS).
	if isHTTPURL(importer) {
		base, berr := importerURLDir(importer)
		if berr != nil {
			return Null, berr
		}
		stem := resolveURL(base, spec)
		if stem != "" {
			if target, err := pickURLTarget(stem); err == nil {
				candidates = append(candidates, cand{target: target, isURL: true})
			} else if !errors.Is(err, os.ErrNotExist) {
				return Null, err
			}
		}
	} else {
		base := importerFSDir(importer)
		stem := filepath.Join(base, spec)
		if target, err := pickFSTarget(stem); err == nil {
			candidates = append(candidates, cand{target: target, isURL: false})
		} else if !errors.Is(err, os.ErrNotExist) {
			return Null, err
		}
	}

	// Base #2: stdlib (<installRoot>/lib), filesystem only.
	if installRoot != "" {
		libRoot := filepath.Join(installRoot, "lib")
		stem := filepath.Join(libRoot, spec)
		if target, err := pickFSTarget(stem); err == nil {
			candidates = append(candidates, cand{target: target, isURL: false})
		} else if !errors.Is(err, os.ErrNotExist) {
			return Null, err
		}
	}

	if len(candidates) == 0 {
		return Null, fmt.Errorf("module not found: %s", spec)
	}

	// Fetch + parse using display=spec, cache key=canonical target.
	chosen := candidates[0]
	if chosen.isURL {
		src, _, ferr := httpFetch(chosen.target)
		if ferr != nil {
			return Null, ferr
		}
		ast, spans, perr := parseSourceWithSpans(spec, src)
		if perr != nil {
			return Null, perr
		}
		return ip.importWithBody(chosen.target, spec, ast, src, spans)
	}

	b, rerr := os.ReadFile(chosen.target)
	if rerr != nil {
		return Null, rerr
	}
	src := string(b)
	ast, spans, perr := parseSourceWithSpans(spec, src)
	if perr != nil {
		return Null, perr
	}
	return ip.importWithBody(chosen.target, spec, ast, src, spans)
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
// Cycle detection and caching are centralized in nativeMakeModule so that
// *all* entry points (AST/Code/File/inline) share identical semantics.
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

////////////////////////////////////////////////////////////////////////////////
//                         CLEAN RESOLUTION HELPERS
////////////////////////////////////////////////////////////////////////////////

const defaultModuleExt = ".ms"

func isHTTPURL(s string) bool {
	return strings.HasPrefix(s, "http://") || strings.HasPrefix(s, "https://")
}

// splitSpecExt returns (ext, suggestion, ok) where ok indicates the spec's last
// path segment has an extension. suggestion is the same spec with that extension removed.
func splitSpecExt(spec string) (string, string, bool) {
	if isHTTPURL(spec) {
		u, err := url.Parse(spec)
		if err != nil {
			return "", "", false
		}
		p := strings.TrimSuffix(u.Path, "/")
		ext := path.Ext(p)
		if ext == "" {
			return "", "", false
		}
		u2 := *u
		u2.Path = strings.TrimSuffix(p, ext)
		return ext, u2.String(), true
	}

	s := strings.TrimSuffix(spec, "/")
	ext := filepath.Ext(s)
	if ext == "" {
		return "", "", false
	}
	return ext, strings.TrimSuffix(s, ext), true
}

// pickFSTarget chooses between stem.ms and stem/init.ms.
// Returns os.ErrNotExist if neither exists.
func pickFSTarget(stem string) (string, error) {
	c1 := stem + defaultModuleExt
	c2 := filepath.Join(stem, "init"+defaultModuleExt)

	ex1 := fileExists(c1)
	ex2 := fileExists(c2)

	if ex1 && ex2 {
		return "", fmt.Errorf("ambiguous module: both %q and %q exist", c1, c2)
	}
	if !ex1 && !ex2 {
		return "", os.ErrNotExist
	}

	var chosen string
	if ex1 {
		chosen = c1
	} else {
		chosen = c2
	}

	abs, err := filepath.Abs(chosen)
	if err != nil {
		return "", err
	}
	return filepath.Clean(abs), nil
}

func fileExists(p string) bool {
	fi, err := os.Stat(p)
	return err == nil && !fi.IsDir()
}

// pickURLTarget chooses between <url>.ms and <url>/init.ms.
// Returns os.ErrNotExist if neither exists.
func pickURLTarget(stemURL string) (string, error) {
	u, err := url.Parse(stemURL)
	if err != nil {
		return "", fmt.Errorf("invalid import url: %w", err)
	}
	p := strings.TrimSuffix(u.Path, "/")

	u1 := *u
	u1.Path = p + defaultModuleExt
	u2 := *u
	u2.Path = p + "/init" + defaultModuleExt

	ok1, e1 := httpProbe(u1.String())
	ok2, e2 := httpProbe(u2.String())

	if ok1 && ok2 {
		return "", fmt.Errorf("ambiguous module: both %q and %q exist", u1.String(), u2.String())
	}
	if ok1 {
		return u1.String(), nil
	}
	if ok2 {
		return u2.String(), nil
	}

	// Prefer surfacing an operational HTTP error if both probes failed non-404.
	if e1 != nil {
		return "", e1
	}
	if e2 != nil {
		return "", e2
	}
	return "", os.ErrNotExist
}

// importerFSDir returns the filesystem directory to resolve relative imports from.
// REPL (importer="") resolves from CWD.
func importerFSDir(importer string) string {
	if importer != "" && !isHTTPURL(importer) {
		return filepath.Dir(importer)
	}
	if cwd, err := os.Getwd(); err == nil {
		return cwd
	}
	return "."
}

// importerURLDir returns the directory URL for an importing URL (as a base for ResolveReference).
func importerURLDir(importer string) (*url.URL, error) {
	u, err := url.Parse(importer)
	if err != nil || u.Scheme == "" {
		return nil, fmt.Errorf("invalid importer url: %q", importer)
	}
	p := strings.TrimSuffix(u.Path, "/")
	u.Path = path.Dir(p) + "/"
	u.RawQuery = ""
	u.Fragment = ""
	return u, nil
}

func resolveURL(base *url.URL, rel string) string {
	ref, err := url.Parse(rel)
	if err != nil {
		return ""
	}
	return base.ResolveReference(ref).String()
}

////////////////////////////////////////////////////////////////////////////////
//                               NETWORK FETCHING
////////////////////////////////////////////////////////////////////////////////

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

// httpProbe checks existence without downloading the full body.
// Returns (exists, err). Not-found is (false, nil). Other failures are err.
func httpProbe(canonURL string) (bool, error) {
	client := &http.Client{Timeout: 15 * time.Second}
	req, err := http.NewRequest("GET", canonURL, nil)
	if err != nil {
		return false, err
	}
	// Small body, widely supported; avoids HEAD quirks.
	req.Header.Set("Range", "bytes=0-0")
	resp, err := client.Do(req)
	if err != nil {
		return false, err
	}
	defer resp.Body.Close()
	if resp.StatusCode >= 200 && resp.StatusCode <= 299 {
		return true, nil
	}
	if resp.StatusCode == 404 || resp.StatusCode == 410 {
		return false, nil
	}
	return false, fmt.Errorf("http %d", resp.StatusCode)
}

////////////////////////////////////////////////////////////////////////////////
//                        DISPLAY HELPERS (CYCLES)
////////////////////////////////////////////////////////////////////////////////

// prettySpec returns a short display name for a canonical spec:
//   - file path   -> basename without extension
//   - http(s) URL -> last segment without extension
//   - fallback: original string if parsing fails
func prettySpec(s string) string {
	// Try URL first
	if u, err := url.Parse(s); err == nil && u.Scheme != "" {
		base := path.Base(u.Path)
		name := strings.TrimSuffix(base, path.Ext(base))
		if name == "init" {
			parent := path.Base(path.Dir(u.Path))
			if parent != "" && parent != "." && parent != "/" {
				return parent
			}
		}
		if name != "" {
			return name
		}
		return base
	}
	// Filesystem path (or arbitrary name)
	base := filepath.Base(s)
	name := strings.TrimSuffix(base, filepath.Ext(base))
	if name == "init" {
		parent := filepath.Base(filepath.Dir(s))
		if parent != "" && parent != "." {
			return parent
		}
	}
	if name != "" {
		return name
	}
	return base
}
