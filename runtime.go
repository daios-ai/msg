// runtime.go
//
// This file implements the standard runtime/builtins *against the stable engine
// surface* defined in interpreter.go. It avoids reaching into the engine's
// internals (no ip.eval, no direct stack/VM knowledge). The only remaining
// intentional touch-point is for `spawn`, where we temporarily copy a function's
// closure to a snapshot Env to preserve isolation semantics. See the TODO there.

package mindscript

import "fmt"

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

	if err := ip.LoadPrelude("std.ms", ""); err != nil {
		return nil, err
	}

	ip.Global.SealParentWrites()

	return ip, nil
}

// LoadPrelude resolves `spec` (filesystem or absolute http(s) URL), parses it,
// and executes the prelude directly in the interpreter's Core environment.
// On success it returns nil. On failure it returns a descriptive error
// (LEXICAL/PARSE/RUNTIME with caret snippets where available).
func (ip *Interpreter) LoadPrelude(spec string, importer string) error {
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

	// 3) Evaluate in Core. We want real errors for VM runtime failures,
	//    and we also want to treat annotated-null as an error.
	v, rterr := ip.runTopWithSource(ast, ip.Core, false, &SourceRef{
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
