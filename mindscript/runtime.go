// runtime.go
//
// This file implements the standard runtime/builtins *against the stable engine
// surface* defined in interpreter.go. It avoids reaching into the engine's
// internals (no ip.eval, no direct stack/VM knowledge). The only remaining
// intentional touch-point is for `spawn`, where we temporarily copy a function's
// closure to a snapshot Env to preserve isolation semantics. See the TODO there.

package mindscript

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// --- Handle helpers (use central Handle from interpreter.go) ---

// asHandle asserts v is a VTHandle (and optionally of a specific kind)
// and returns the underlying *Handle. Used by runtime builtins.
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
func setBuiltinDoc(target *Env, name, doc string) {
	if v, err := target.Get(name); err == nil {
		target.Define(name, withAnnot(v, doc))
	}
}

// RegisterRuntimeBuiltin registers a native host function into the interpreter's
// host registry (ip.native) but BINDS the callable into the provided target env.
// The function value's closure Env is ip.Core (so effects occur at call-site),
// and NativeName triggers native dispatch. Unlike RegisterNative, this does NOT
// write into Core, allowing per-namespace overrides.
func (ip *Interpreter) RegisterRuntimeBuiltin(
	target *Env, name string, params []ParamSpec, ret S, impl NativeImpl,
) {
	if ip.native == nil {
		ip.native = map[string]NativeImpl{}
	}
	ip.native[name] = impl

	names := make([]string, len(params))
	types := make([]S, len(params))
	for i, p := range params {
		names[i], types[i] = p.Name, p.Type
	}
	fn := FunVal(&Fun{
		Params:     names,
		ParamTypes: types,
		ReturnType: ret,
		Body:       S{"native", name}, // sentinel
		Env:        ip.Core,           // effects resolve at call-site
		NativeName: name,
	})
	target.Define(name, fn)
}

// SeedRuntimeInto installs the standard runtime natives and prelude into `target`.
// This makes runtime symbols overrideable per-namespace (process/module).
// RETURNS ERROR if prelude load fails (constructor MUST fail fast on this).
func (ip *Interpreter) SeedRuntimeInto(target *Env) error {
	// --- Register std natives into target (NOT Core) ---
	// Change the register* functions to accept (ip, target) and use
	// RegisterRuntimeBuiltin instead of RegisterNative inside those helpers.
	registerCoreBuiltins(ip, target)
	registerIntrospectionBuiltins(ip, target)
	registerCastBuiltins(ip, target)
	registerIOBuiltins(ip, target)
	registerOsBuiltins(ip, target)
	registerNetBuiltins(ip, target)
	registerConcurrencyBuiltins(ip, target)
	registerProcessBuiltins(ip, target)
	registerEncodingURLBuiltins(ip, target)
	registerCryptoBuiltins(ip, target)
	registerCompressionBuiltins(ip, target)
	registerExecBuiltins(ip, target)
	registerTimeBuiltins(ip, target)
	registerPathBuiltins(ip, target)
	registerJsonBuiltins(ip, target)
	registerStringBuiltins(ip, target)
	registerRandomBuiltins(ip, target)
	registerMathBuiltins(ip, target)
	registerFFIBuiltins(ip, target)
	registerActorBuiltins(ip, target)
	registerOracleBuiltins(ip, target)

	// --- Load prelude into the SAME target (overrideable within namespace) ---
	// Imports are extensionless now; prelude spec should be "std", not "std.ms".
	if err := ip.LoadPreludeInto(target, "std", ""); err != nil {
		return err
	}
	return nil
}

// LoadPreludeInto resolves `spec`, parses it with spans, and evaluates it
// into the provided `target` environment (NOT Core). Errors are pretty-printed.
func (ip *Interpreter) LoadPreludeInto(target *Env, spec string, importer string) error {
	// 1) Resolve + fetch (using the same clean import rules as ImportFile)
	src, display, err := resolveAndFetchClean(spec, importer)
	if err != nil {
		return err
	}

	// 2) Parse with spans for caret diagnostics
	ast, spans, perr := ParseSExprWithSpans(src)
	if perr != nil {
		if e, ok := perr.(*Error); ok {
			if e.Src == nil {
				e.Src = &SourceRef{Name: display, Src: src}
			}
			return fmt.Errorf("%s", FormatError(e))
		}
		return perr
	}

	// 3) Evaluate in the provided target environment. We want real errors for VM runtime failures,
	//    and we also want to treat annotated-null as an error.
	v, rterr := ip.runTopWithSource(ast, target, false, &SourceRef{
		Name:  display,
		Src:   src,
		Spans: spans,
	})
	if rterr != nil {
		if e, ok := rterr.(*Error); ok {
			// Already has proper Src/coords; just pretty-print here.
			return fmt.Errorf("%s", FormatError(e))
		}
		return rterr
	}

	// Top-level returned an annotated null → promote to a hard runtime *Error*.
	if v.Tag == VTNull && v.Annot != "" {
		return fmt.Errorf("%s", FormatError(&Error{
			Kind: DiagRuntime,
			Msg:  v.Annot,
			Src:  &SourceRef{Name: display, Src: src},
			Line: 1, Col: 1, // no precise PC; anchor at file start
		}))
	}

	return nil
}

////////////////////////////////////////////////////////////////////////////////
//                 CLEAN RESOLUTION (shared with ImportFile)
////////////////////////////////////////////////////////////////////////////////

// resolveAndFetchClean implements the same "clean" resolution order as ImportFile,
// but returns raw source (for prelude evaluation into an existing env).
//
// Returns:
//   - src:     fetched source text
//   - display: a useful name for diagnostics (canonical path/URL)
func resolveAndFetchClean(spec string, importer string) (src string, display string, err error) {
	spec = strings.TrimSuffix(spec, "/")

	// Extensionless enforcement.
	if _, sugg, ok := splitSpecExt(spec); ok {
		return "", "", fmt.Errorf("imports are extensionless; write import(%q), not import(%q)", sugg, spec)
	}

	// Absolute URL?
	if isHTTPURL(spec) {
		target, err := pickURLTarget(spec)
		if err != nil {
			if errors.Is(err, os.ErrNotExist) {
				return "", "", fmt.Errorf("module not found: %s", spec)
			}
			return "", "", err
		}
		src, _, err := httpFetch(target)
		return src, target, err
	}

	// Absolute filesystem path?
	if filepath.IsAbs(spec) {
		target, err := pickFSTarget(spec)
		if err != nil {
			if errors.Is(err, os.ErrNotExist) {
				return "", "", fmt.Errorf("module not found: %s", spec)
			}
			return "", "", err
		}
		b, err := os.ReadFile(target)
		if err != nil {
			return "", "", err
		}
		return string(b), target, nil
	}

	// Relative: base #1 importer dir (URL or FS), base #2 <installRoot>/lib (FS only).
	type cand struct {
		target string
		isURL  bool
	}
	var candidates []cand

	if isHTTPURL(importer) {
		base, berr := importerURLDir(importer)
		if berr != nil {
			return "", "", berr
		}
		stem := resolveURL(base, spec)
		if stem != "" {
			if target, err := pickURLTarget(stem); err == nil {
				candidates = append(candidates, cand{target: target, isURL: true})
			} else if !errors.Is(err, os.ErrNotExist) {
				return "", "", err
			}
		}
	} else {
		base := importerFSDir(importer)
		stem := filepath.Join(base, spec)
		if target, err := pickFSTarget(stem); err == nil {
			candidates = append(candidates, cand{target: target, isURL: false})
		} else if !errors.Is(err, os.ErrNotExist) {
			return "", "", err
		}
	}

	if installRoot != "" {
		libRoot := filepath.Join(installRoot, "lib")
		stem := filepath.Join(libRoot, spec)
		if target, err := pickFSTarget(stem); err == nil {
			candidates = append(candidates, cand{target: target, isURL: false})
		} else if !errors.Is(err, os.ErrNotExist) {
			return "", "", err
		}
	}

	if len(candidates) == 0 {
		return "", "", fmt.Errorf("module not found: %s", spec)
	}

	chosen := candidates[0]
	if chosen.isURL {
		src, _, err := httpFetch(chosen.target)
		return src, chosen.target, err
	}

	b, err := os.ReadFile(chosen.target)
	if err != nil {
		return "", "", err
	}
	return string(b), chosen.target, nil
}
