// runtime.go
//
// This file implements the standard runtime/builtins *against the stable engine
// surface* defined in interpreter.go. It avoids reaching into the engine's
// internals (no ip.eval, no direct stack/VM knowledge). The only remaining
// intentional touch-point is for `spawn`, where we temporarily copy a function's
// closure to a snapshot Env to preserve isolation semantics. See the TODO there.

package mindscript

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// --- Opaque, universal handle (Lua-like userdata) + concrete boxed types ---

type Handle struct {
	Kind string
	Data any
}

func HandleVal(kind string, data any) Value {
	return Value{Tag: VTHandle, Data: &Handle{Kind: kind, Data: data}}
}

func asHandle(v Value, want string) *Handle {
	if v.Tag != VTHandle {
		fail("expected handle")
	}
	h := v.Data.(*Handle)
	if want != "" && h.Kind != want {
		fail("wrong handle kind")
	}
	return h
}

// annotate a core builtin function value with a docstring
func setBuiltinDoc(ip *Interpreter, name, doc string) {
	if v, err := ip.Core.Get(name); err == nil {
		ip.Core.Define(name, withAnnot(v, doc))
	}
}

// NewRuntime returns a fully-initialized interpreter with std builtins.
func NewRuntime() (*Interpreter, error) {
	ip := NewInterpreter()

	// standard native builtins
	registerCoreBuiltins(ip)
	registerIntrospectionBuiltins(ip)
	registerCastBuiltins(ip)
	registerIOBuiltins(ip)
	registerOsBuiltins(ip)
	registerNetBuiltins(ip)
	registerConcurrencyBuiltins(ip)
	registerProcessBuiltins(ip)
	registerEncodingURLBuiltins(ip)
	registerCryptoBuiltins(ip)
	registerCompressionBuiltins(ip)
	registerExecBuiltins(ip)
	registerTimeBuiltins(ip)
	registerPathBuiltins(ip)
	registerJsonBuiltins(ip)
	registerStringBuiltins(ip)
	registerRandomBuiltins(ip)
	registerMathBuiltins(ip)

	// if err := ip.LoadPrelude("std", ""); err != nil {
	// 	return nil, err
	// }

	return ip, nil
}

// LoadPrelude resolves `spec` (filesystem only), parses it with spans,
// and executes the prelude directly in Core (not as a module).
// Resolution order: importer directory (if provided) → CWD → each root in MSGPATH.
// If spec has no extension, try "<spec>.ms" then "<spec>".
func (ip *Interpreter) LoadPrelude(spec string, importer string) error {
	src, display, err := loadPreludeSource(spec, importer)
	if err != nil {
		return err
	}

	// Parse with spans for precise caret diagnostics
	ast, spans, perr := ParseSExprWithSpans(src)
	if perr != nil {
		return WrapErrorWithSource(perr, src) // LEXICAL/PARSE with caret
	}

	// Execute in Core. Annotated-null at top level is promoted to an error.
	v, rterr := ip.runTopWithSource(ast, ip.Core /*uncaught=*/, false, &SourceRef{
		Name:  display,
		Src:   src,
		Spans: spans,
	})
	if rterr != nil {
		return rterr // already caret-enriched RUNTIME ERROR
	}
	if v.Tag == VTNull && v.Annot != "" {
		return fmt.Errorf("runtime error in %s: %s", display, v.Annot)
	}
	return nil
}

// --- Prelude-only filesystem resolver (independent from module loaders) ---

// loadPreludeSource resolves a filesystem spec using importer dir → CWD → MSGPATH
// (MindScriptPath), reads the file, and returns (src, displayPath).
// HTTP/HTTPS are rejected; preludes are filesystem-only by design.
func loadPreludeSource(spec string, importer string) (string, string, error) {
	if spec == "" {
		return "", "", fmt.Errorf("prelude spec is empty")
	}
	if strings.HasPrefix(spec, "http://") || strings.HasPrefix(spec, "https://") {
		return "", "", fmt.Errorf("preludes must be loaded from the filesystem (got URL)")
	}

	try := func(base, s string) (string, bool) {
		var cands []string
		if filepath.Ext(s) != "" {
			cands = []string{filepath.Join(base, s)}
		} else {
			cands = []string{
				filepath.Join(base, s) + defaultModuleExt, // ".ms"
				filepath.Join(base, s),
			}
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
			b, err := os.ReadFile(p)
			if err != nil {
				return "", "", err
			}
			return string(b), p, nil
		}
		return "", "", fmt.Errorf("prelude not found: %s", spec)
	}

	// Candidate bases: importer dir (if local path) → CWD → MSGPATH roots
	var bases []string
	if importer != "" && !strings.HasPrefix(importer, "http://") && !strings.HasPrefix(importer, "https://") {
		bases = append(bases, filepath.Dir(importer))
	}
	if cwd, err := os.Getwd(); err == nil {
		bases = append(bases, cwd)
	}
	if sp := os.Getenv(MindScriptPath); sp != "" { // MSGPATH
		for _, root := range filepath.SplitList(sp) {
			if root != "" {
				bases = append(bases, root)
			}
		}
	}

	for _, b := range bases {
		if p, ok := try(b, spec); ok {
			bs, err := os.ReadFile(p)
			if err != nil {
				return "", "", err
			}
			return string(bs), p, nil
		}
	}

	return "", "", fmt.Errorf("prelude not found: %s", spec)
}
