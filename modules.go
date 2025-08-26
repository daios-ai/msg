// modules.go
//
// Module system built on the stable engine surface, now exposing modules as a
// map-like value with an attached lexical environment.
//
// • Resolves & fetches sources (FS/HTTP)
// • Parses to S-exprs
// • Evaluates in an isolated Env parented to Core
// • Caches successes, detects cycles
// • Returns VTModule whose Data is *Module { Name, Map: *MapObject, Env: *Env }
//
// Design goal: VTModule should be *ergonomically* like VTMap. We realize this
// by building the module’s public surface as a MapObject (ordered, key annots),
// and keep Env for closures/types. All VTMap behaviors are reused by small,
// central coercions VTModule→VTMap in interpreter.go (no duplicated branches).

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

// ---- Module runtime structs ------------------------------------------------

type moduleState int

const (
	modUnloaded moduleState = iota
	modLoading
	modLoaded
)

type moduleRec struct {
	spec        string
	displayName string
	src         string
	env         *Env
	mod         *Module
	state       moduleState
	err         error
}

// Module is the public payload for VTModule.
// Map holds the exported bindings (ordered, with per-key annotations).
// Env is the lexical environment where the module executed (for closures/types).
type Module struct {
	Name string
	Map  *MapObject
	Env  *Env
}

// get returns an exported binding by key. The VM can use this for property/index reads.
func (m *Module) get(key string) (Value, bool) {
	v, ok := m.Map.Entries[key]
	return v, ok
}

// ---- Autoloader ------------------------------------------------------------

const defaultModuleExt = ".ms" // adjust if you prefer a different extension

// importModule resolves, parses, evaluates, caches, and returns a VTModule.
// Evaluates in an Env parented to Core. Success is cached; failures are not.
func (ip *Interpreter) importModule(spec string, importer string) (Value, error) {
	// Resolve + fetch → (src, display, canon)
	src, display, canon, rerr := resolveAndFetch(spec, importer)
	if rerr != nil {
		return Null, rerr
	}

	// Cycle detection against canonical key
	for _, s := range ip.loadStack {
		if s == canon {
			return Null, fmt.Errorf("import cycle detected: %s", joinCyclePath(ip.loadStack, canon))
		}
	}
	ip.loadStack = append(ip.loadStack, canon)
	defer func() { ip.loadStack = ip.loadStack[:len(ip.loadStack)-1] }()

	// Cache check
	if ip.modules != nil {
		if rec, ok := ip.modules[canon]; ok {
			if rec.state == modLoading {
				return Null, fmt.Errorf("import cycle detected: %s", joinCyclePath(ip.loadStack, canon))
			}
			if rec.state == modLoaded && rec.mod != nil {
				return Value{Tag: VTModule, Data: rec.mod}, nil
			}
		}
	} else {
		ip.modules = map[string]*moduleRec{}
	}

	// Mark loading
	ip.modules[canon] = &moduleRec{spec: canon, state: modLoading}

	// Parse
	ast, perr := ParseSExpr(src)
	if perr != nil {
		perr = WrapErrorWithSource(perr, src)
		delete(ip.modules, canon) // do not cache failures
		return Null, fmt.Errorf("parse error in %s:\n%s", display, perr.Error())
	}

	// Evaluate in isolated env parented to Core (convert rtErr panics to errors)
	modEnv := NewEnv(ip.Core)
	var rterr error
	var evalRes Value
	func() {
		defer func() {
			if r := recover(); r != nil {
				switch sig := r.(type) {
				case rtErr:
					rterr = fmt.Errorf("runtime error in %s: %s", display, sig.msg)
				default:
					rterr = fmt.Errorf("runtime panic in %s: %v", display, r)
				}
			}
		}()
		if len(ast) > 0 {
			evalRes = ip.EvalASTUncaught(ast, modEnv, true)
		}
	}()

	// Treat VM-returned annotated null as a hard import failure too.
	if rterr == nil && evalRes.Tag == VTNull && evalRes.Annot != "" {
		rterr = fmt.Errorf("runtime error in %s: %s", display, evalRes.Annot)
	}
	if rterr != nil {
		delete(ip.modules, canon) // do not cache failures
		return Null, rterr
	}

	// Snapshot public surface as a MapObject (ordered + key annots).
	mo := buildModuleMap(modEnv)

	// Commit cache on success only
	rec := ip.modules[canon]
	rec.displayName = display
	rec.src = src
	rec.env = modEnv
	rec.mod = &Module{
		Name: canon, // keep canonical identity; prettySpec is for messages
		Map:  mo,
		Env:  modEnv,
	}
	rec.state = modLoaded
	rec.err = nil

	return Value{Tag: VTModule, Data: rec.mod}, nil
}

// buildModuleMap snapshots modEnv.table into a MapObject:
// • Keys are sorted for determinism (Env.table is a Go map).
// • VTType exports without a pinned env are rewrapped with TypeValIn(..., modEnv).
// • If a value carries Annot, we mirror it into KeyAnn for that key.
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

// resolveAndFetch returns (src, display, canonicalKey) for the given spec.
//
// Network:
//   - Absolute http(s) URLs are fetched via GET with a timeout.
//   - If the URL path has no extension, defaultModuleExt is appended.
//
// Filesystem:
//   - Resolve relative specs against importer dir → CWD → MINDSCRIPT_PATH.
//   - If spec has no extension, try spec+defaultModuleExt then spec.
//   - Returns canonical ABSOLUTE path (cleaned) as both display and cache key.
func resolveAndFetch(spec string, importer string) (src string, display string, canon string, err error) {
	// Network?
	if strings.HasPrefix(spec, "http://") || strings.HasPrefix(spec, "https://") {
		u, perr := url.Parse(spec)
		if perr != nil {
			return "", "", "", fmt.Errorf("invalid import url: %w", perr)
		}
		if path.Ext(u.Path) == "" && defaultModuleExt != "" {
			u.Path = strings.TrimSuffix(u.Path, "/") + defaultModuleExt
		}
		canon = u.String()
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
		// fallthrough to MINDSCRIPT_PATH for completeness
	} else {
		for _, b := range bases {
			if p, ok := try(b, spec); ok {
				return p, nil
			}
		}
	}

	// MINDSCRIPT_PATH
	if sp := os.Getenv("MINDSCRIPT_PATH"); sp != "" {
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
