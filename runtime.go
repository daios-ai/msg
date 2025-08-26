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
func NewRuntime() *Interpreter {
	ip := NewInterpreter()

	// Recreate Core/Global for a clean runtime env.
	ip.Core = NewEnv(nil)
	ip.Global = NewEnv(ip.Core)

	// registries
	ip.native = map[string]NativeImpl{}
	ip.modules = map[string]*moduleRec{}

	// engine helpers
	ip.initCore()

	// standard native builtins
	registerStandardBuiltins(ip)
	registerConcurrencyBuiltins(ip)
	registerIOBuiltins(ip)
	registerIntrospectionBuiltins(ip)
	registerUtilityBuiltins(ip)
	registerSchemaBuiltins(ip)
	registerNetBuiltins(ip)
	registerMapBuiltins(ip)
	registerStringBuiltins(ip)
	registerRegexBuiltins(ip)
	registerCastBuiltins(ip)
	registerMathBuiltins(ip)
	registerTimeExtras(ip)
	registerProcessBuiltins(ip)
	registerOsBuiltins(ip)

	// standard library
	ip.LoadPrelude("std", "")

	return ip
}

// LoadPrelude resolves `spec` (filesystem or absolute http(s) URL), parses it,
// and executes the prelude directly in the interpreter's Core environment.
// On success it returns nil. On failure it returns a descriptive error,
// mirroring the module loader's error semantics (parse/runtime/cycle messages).
func (ip *Interpreter) LoadPrelude(spec string, importer string) error {
	// 1) Resolve + fetch (supports CWD, importer dir, MINDSCRIPT_PATH, http(s))
	src, display, _, err := resolveAndFetch(spec, importer)
	if err != nil {
		return err
	}

	// 2) Parse with source-wrapped diagnostics
	ast, perr := parseSource(display, src)
	if perr != nil {
		return perr
	}

	// 3) Evaluate in Core, surfacing failures like modules do
	var rterr error
	var res Value
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
			// Uncaught mode -> annotated nulls instead of errors; we convert below.
			res = ip.EvalASTUncaught(ast, ip.Core, true)
		}
	}()

	// Annotated-null means a user/runtime failure; upgrade to error.
	if rterr == nil && res.Tag == VTNull && res.Annot != "" {
		rterr = fmt.Errorf("runtime error in %s: %s", display, res.Annot)
	}
	return rterr
}
