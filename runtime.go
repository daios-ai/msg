// runtime.go
//
// This file implements the standard runtime/builtins *against the stable engine
// surface* defined in interpreter.go. It avoids reaching into the engine's
// internals (no ip.eval, no direct stack/VM knowledge). The only remaining
// intentional touch-point is for `spawn`, where we temporarily copy a function's
// closure to a snapshot Env to preserve isolation semantics. See the TODO there.

package mindscript

import (
	"io/fs"
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
	loadPreludeIntoMain(ip, "std.ms")

	return ip
}

func loadPreludeIntoMain(ip *Interpreter, filename string) error {
	paths := strings.Split(os.Getenv("MINDSCRIPT_PATH"), string(os.PathListSeparator))
	for _, dir := range paths {
		if dir == "" {
			continue
		}
		p := filepath.Join(dir, filename)
		if data, err := os.ReadFile(p); err == nil {
			// Evaluate directly in main env so top-level lets become globals
			if _, err := ip.EvalPersistentSource(string(data)); err != nil {
				return err
			}
			return nil
		}
	}
	return fs.ErrNotExist
}
