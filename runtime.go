// runtime.go
//
// This file implements the standard runtime/builtins *against the stable engine
// surface* defined in interpreter.go. It avoids reaching into the engine's
// internals (no ip.eval, no direct stack/VM knowledge). The only remaining
// intentional touch-point is for `spawn`, where we temporarily copy a function's
// closure to a snapshot Env to preserve isolation semantics. See the TODO there.

package mindscript

import "fmt"

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

	// --- Load prelude into the SAME target (overrideable within namespace) ---
	// If you need to load from a different spec, make this configurable by caller.
	if err := ip.LoadPreludeInto(target, "std.ms", ""); err != nil {
		return err
	}
	return nil
}

// LoadPrelude resolves `spec` and evaluates it into the interpreter's Global
// (kept for compatibility). Prefer LoadPreludeInto for explicit targets.
func (ip *Interpreter) LoadPrelude(spec string, importer string) error {
	return ip.LoadPreludeInto(ip.Global, spec, importer)
}

// LoadPreludeInto resolves `spec`, parses it with spans, and evaluates it
// into the provided `target` environment (NOT Core). Errors are pretty-printed.
func (ip *Interpreter) LoadPreludeInto(target *Env, spec string, importer string) error {

	// 1) Resolve + fetch
	src, display, _, err := resolveAndFetch(spec, importer)
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
