// modules.go
//
// Module system built on the stable engine surface:
// - Resolves and fetches sources (FS/HTTP).
// - Parses to S-exprs.
// - Evaluates via ip.EvalASTUncaught (so runtime failures abort import).
// - Caches modules and detects cycles.
// - Exposes modules as VTModule values with Exports map.

package mindscript

import (
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"path"
	"path/filepath"
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
	exports     map[string]Value
	state       moduleState
	err         error
}

// Module value backing store for VTModule.
type Module struct {
	Name    string
	Exports map[string]Value
}

func (m *Module) get(key string) (Value, bool) {
	v, ok := m.Exports[key]
	return v, ok
}

// ---- Autoloader ------------------------------------------------------------

const defaultModuleExt = ".ms" // adjust if you prefer a different extension

// importModule resolves, parses, and evaluates a module in an isolated env whose
// parent is the interpreter's Core env. It detects import cycles and reuses a cache.
func (ip *Interpreter) importModule(spec string, importer string) (Value, error) {
	// Resolve + fetch → (src, display, canonKey)
	src, display, canon, rerr := resolveAndFetch(spec, importer)
	if rerr != nil {
		return Null, rerr
	}

	// --- cycle detection on canonical key ---
	for _, s := range ip.loadStack {
		if s == canon {
			path := joinCyclePath(ip.loadStack, canon)
			return Null, fmt.Errorf("import cycle detected: %s", path)
		}
	}
	ip.loadStack = append(ip.loadStack, canon)
	defer func() { ip.loadStack = ip.loadStack[:len(ip.loadStack)-1] }()

	// --- cached? ---
	if rec, ok := ip.modules[canon]; ok {
		// We no longer cache failures, so only hit for true successes or in-flight loads.
		if rec.state == modLoading {
			path := joinCyclePath(ip.loadStack, canon)
			return Null, fmt.Errorf("import cycle detected: %s", path)
		}
		if rec.state == modLoaded {
			return Value{Tag: VTModule, Data: &Module{Name: rec.spec, Exports: rec.exports}}, nil
		}
	}

	// --- mark loading ---
	ip.modules[canon] = &moduleRec{spec: canon, state: modLoading}

	// --- parse ---
	ast, perr := ParseSExpr(src)
	if perr != nil {
		perr = WrapErrorWithSource(perr, src)
		// DO NOT cache failures; allow retry on next import
		delete(ip.modules, canon)
		return Null, fmt.Errorf("parse error in %s:\n%s", display, perr.Error())
	}

	// --- evaluate in isolated env parented to Core ---
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
		// DO NOT cache failures; allow retry on next import
		delete(ip.modules, canon)
		return Null, rterr
	}

	// --- snapshot exports ---
	exports := make(map[string]Value, len(modEnv.table))
	for k, v := range modEnv.table {
		if v.Tag == VTType {
			tv := v.Data.(*TypeValue)
			if tv.Env == nil {
				exports[k] = TypeValIn(tv.Ast, modEnv)
				continue
			}
		}
		exports[k] = v
	}

	// --- commit cache on success only ---
	rec := ip.modules[canon]
	rec.displayName = display
	rec.src = src
	rec.env = modEnv
	rec.exports = exports
	rec.state = modLoaded
	rec.err = nil

	return Value{Tag: VTModule, Data: &Module{Name: rec.spec, Exports: rec.exports}}, nil
}

// resolveAndFetch returns (src, display, canonicalKey) for the given spec.
//
// Network:
//   - Absolute http(s) URLs are fetched via GET with a timeout.
//   - If the URL path has no extension, defaultModuleExt is appended.
//
// Filesystem:
//   - Resolve relative specs against importer dir → CWD → MINDSCRIPT\_PATH.
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
//   - file path  -> basename without extension ("/x/A.ms" -> "A")
//   - http(s) URL -> last segment without extension ("[https://.../A.ms](https://.../A.ms)" -> "A")
//   - fallback: the original string if parsing fails.
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
