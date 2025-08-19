// runtime.go
//
// This file implements the standard runtime/builtins *against the stable engine
// surface* defined in interpreter.go. It avoids reaching into the engine's
// internals (no ip.eval, no direct stack/VM knowledge). The only remaining
// intentional touch-point is for `spawn`, where we temporarily copy a function's
// closure to a snapshot Env to preserve isolation semantics. See the TODO there.

package mindscript

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

	// std library
	registerStandardBuiltins(ip)
	registerConcurrencyBuiltins(ip)
	registerIOBuiltins(ip)
	registerIntrospectionBuiltins(ip)
	registerUtilityBuiltins(ip)
	registerNetBuiltins(ip)
	registerMapBuiltins(ip)
	registerStringBuiltins(ip)
	registerRegexBuiltins(ip)
	registerCastBuiltins(ip)
	registerMathBuiltins(ip)
	registerTimeExtras(ip)
	registerProcessBuiltins(ip)

	return ip
}

// Public convenience if consumers already created an Interpreter.
func RegisterBuiltins(ip *Interpreter) {
	if ip == nil {
		return
	}
	if ip.Core == nil {
		ip.Core = NewEnv(nil)
	}
	if ip.native == nil {
		ip.native = map[string]NativeImpl{}
	}
	if ip.modules == nil {
		ip.modules = map[string]*moduleRec{}
	}
	registerStandardBuiltins(ip)
	registerConcurrencyBuiltins(ip)
	registerIOBuiltins(ip)
	registerIntrospectionBuiltins(ip)
	registerUtilityBuiltins(ip)
	registerNetBuiltins(ip)
}
