// === FILE: builtin_misc.go ===
package mindscript

import (
	"fmt"
	"math"
	"math/rand"
	"os"
	"runtime/debug"
	"strconv"
	"sync"
	"time"
	"unicode/utf8"
)

// --- Random Utilities ----------------------------------------------------

func registerRandomBuiltins(ip *Interpreter, target *Env) {
	// Instance-local RNG and mutex; closures capture these.
	var (
		rng   = rand.New(rand.NewSource(time.Now().UnixNano()))
		rngMu sync.Mutex
	)

	ip.RegisterRuntimeBuiltin(
		target,
		"seedRand",
		[]ParamSpec{{Name: "n", Type: S{"id", "Int"}}},
		S{"id", "Null"},
		func(_ *Interpreter, ctx CallCtx) Value {
			n := ctx.Arg("n")
			rngMu.Lock()
			rng.Seed(n.Data.(int64))
			rngMu.Unlock()
			return Null
		},
	)
	setBuiltinDoc(target, "seedRand", `Seed the pseudo-random number generator.

Use a fixed seed for reproducible sequences.

Params:
	n: Int — seed value

Returns:
	Null`)

	ip.RegisterRuntimeBuiltin(
		target,
		"randInt",
		[]ParamSpec{{Name: "n", Type: S{"id", "Int"}}},
		S{"id", "Int"},
		func(_ *Interpreter, ctx CallCtx) Value {
			n := ctx.Arg("n").Data.(int64)
			// Contractual: n must be > 0 (hard error)
			if n <= 0 {
				fail("randInt: n must be > 0")
			}
			// Guard against overflow when converting to platform int.
			intMax := int64(int(^uint(0) >> 1))
			if n > intMax {
				fail("randInt: n too large on this platform")
			}
			rngMu.Lock()
			res := rng.Intn(int(n))
			rngMu.Unlock()
			return Int(int64(res))
		},
	)
	setBuiltinDoc(target, "randInt", `Uniform random integer in [0, n).

Params:
	n: Int — upper bound (must be > 0)

Returns:
	Int`)

	ip.RegisterRuntimeBuiltin(
		target,
		"randFloat",
		[]ParamSpec{{Name: "_", Type: S{"id", "Null"}}},
		S{"id", "Num"},
		func(_ *Interpreter, ctx CallCtx) Value {
			rngMu.Lock()
			f := rng.Float64()
			rngMu.Unlock()
			return Num(f)
		},
	)
	setBuiltinDoc(target, "randFloat", `Uniform random number in [0.0, 1.0).

Params:
	_: Null

Returns:
	Num`)
}

// --- Casting Utilities ----------------------------------------------------

func registerCastBuiltins(ip *Interpreter, target *Env) {
	// pretty(src: Str) -> Str?   (caret-formatted parse errors as soft null)
	ip.RegisterRuntimeBuiltin(
		target,
		"formatCode",
		[]ParamSpec{{Name: "src", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(_ *Interpreter, ctx CallCtx) (v Value) {
			s := ctx.Arg("src").Data.(string)

			// Catch panics from Pretty / printer and turn them into a soft error.
			defer func() {
				if r := recover(); r != nil {
					// Print Go-side details to stderr for debugging.
					fmt.Fprintf(
						os.Stderr, // or os.Stderr, depending on your wiring
						"formatCode panic: %v\n%s\n",
						r,
						debug.Stack(),
					)
					v = annotNull(fmt.Sprintf("formatCode panic: %v", r))
				}
			}()

			out, err := Pretty(s)
			if err != nil {
				return annotNull(err.Error())
			}
			return Str(out)
		},
	)
	setBuiltinDoc(target, "formatCode", `Format source code.

Parses the input and pretty-prints it with normalized whitespace and minimal parentheses. Supports PRE/POST annotations (# ... lines above; trailing # ... forces newline). On parse failure, returns null with a caret-formatted error.

Params:
	src: Str

Returns:
	Str?`)

	// formatValue(x: Any) -> Str   (renders with annotations)
	ip.RegisterRuntimeBuiltin(
		target,
		"formatValue",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			return Str(FormatValue(ctx.Arg("x")))
		},
	)
	setBuiltinDoc(target, "formatValue", `Render a runtime value (with annotations).

Produces a stable, readable string: scalars are literal; arrays/maps inline when short, otherwise multi-line (maps sort keys). PRE annotations print as header lines; POST as trailing comments. Functions show as <fun: ...>, types as <type: ...>, modules as <module: ...>.

Params:
	x: Any

Returns:
	Str`)

	// str(x: Any) -> Str?   (ignores annotations; soft-error on unsupported)
	ip.RegisterRuntimeBuiltin(
		target,
		"str",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			// Normalize modules to maps so they print as their map view.
			in := AsMapValue(ctx.Arg("x"))
			// Functions, types, and handles are intentionally not printable.
			switch in.Tag {
			case VTFun, VTType, VTHandle:
				return annotNull("unsupported type, cannot convert to Str")
			}
			// Strip annotations recursively before printing.
			clean := stripAnnDeep(in)

			// Identity for plain strings (no quotes).
			if clean.Tag == VTStr {
				return clean
			}

			// Everything else uses the pretty renderer.
			return Str(FormatValue(clean))
		},
	)
	setBuiltinDoc(target, "str", `Convert to string if possible; otherwise err.

Converts values of type Null, Bool, Int, Num, Str, [...], and {...}.

Params:
	x: Any

Returns:
	Str?`)

	ip.RegisterRuntimeBuiltin(
		target,
		"int",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Int"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			v := ctx.Arg("x")
			switch v.Tag {
			case VTInt:
				return v
			case VTNum:
				return Int(int64(v.Data.(float64)))
			case VTBool:
				if v.Data.(bool) {
					return Int(1)
				}
				return Int(0)
			case VTStr:
				if n, err := strconv.ParseInt(v.Data.(string), 10, 64); err == nil {
					return Int(n)
				}
				return annotNull("unsupported type, cannot convert to Int")
			default:
				return annotNull("unsupported type, cannot convert to Int")
			}
		},
	)
	setBuiltinDoc(target, "int", `Convert to Int when possible; otherwise errs.

Rules:
	• Int → Int
	• Num → truncated toward zero
	• Bool → 1 or 0
	• Str → parsed base-10 integer, or null on failure
	• Others → null

Params:
	x: Any

Returns:
	Int?`)

	ip.RegisterRuntimeBuiltin(
		target,
		"num",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Num"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			v := ctx.Arg("x")
			switch v.Tag {
			case VTNum:
				return v
			case VTInt:
				return Num(float64(v.Data.(int64)))
			case VTBool:
				if v.Data.(bool) {
					return Num(1)
				}
				return Num(0)
			case VTStr:
				if f, err := strconv.ParseFloat(v.Data.(string), 64); err == nil {
					return Num(f)
				}
				return annotNull("unsupported type, cannot convert to Num")
			default:
				return annotNull("unsupported type, cannot convert to Num")
			}
		},
	)
	setBuiltinDoc(target, "num", `Convert to Num when possible; otherwise errs.

Rules:
	• Num → Num
	• Int → floating-point value
	• Bool → 1.0 or 0.0
	• Str → parsed as float64, or null on failure
	• Others → null

Params:
	x: Any

Returns:
	Num?`)

	ip.RegisterRuntimeBuiltin(
		target,
		"bool",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Bool"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			v := ctx.Arg("x")
			switch v.Tag {
			case VTBool:
				return v
			case VTNull:
				return Bool(false)
			case VTInt:
				return Bool(v.Data.(int64) != 0)
			case VTNum:
				return Bool(v.Data.(float64) != 0)
			case VTStr:
				return Bool(v.Data.(string) != "")
			case VTArray:
				return Bool(len(v.Data.(*ArrayObject).Elems) > 0)
			case VTMap:
				return Bool(len(v.Data.(*MapObject).Entries) > 0)
			default:
				return annotNull("unsupported type, cannot convert to Bool")
			}
		},
	)
	setBuiltinDoc(target, "bool", `Convert to Bool using common "truthiness" rules; otherwise errs.

Falsey:
	• null
	• 0, 0.0
	• "" (empty string)
	• [] (empty array)
	• {} (empty map)

Truthy:
	• Any other value for Int, Num, [...], and {...}.

Params:
	x: Any

Returns:
	Bool?`)

	ip.RegisterRuntimeBuiltin(
		target,
		"len",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Int"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			x := AsMapValue(ctx.Arg("x"))
			switch x.Tag {
			case VTArray:
				return Int(int64(len(x.Data.(*ArrayObject).Elems)))
			case VTMap:
				mo := x.Data.(*MapObject)
				// Use ordered keys length to reflect object “length”
				return Int(int64(len(mo.Keys)))
			case VTStr:
				// Unicode-aware length to match substr’s rune semantics
				return Int(int64(utf8.RuneCountInString(x.Data.(string))))
			default:
				return Null
			}
		},
	)
	setBuiltinDoc(target, "len", `Length of a value.

Rules:
	• [a, b, c] → 3
	• {k: v, ...} → number of keys (in insertion order)
	• "…unicode…" → rune count
	• Others → null

Params:
	x: Any

Returns:
	Int?`)
}

// --- Math Utilities ----------------------------------------------------

func registerMathBuiltins(ip *Interpreter, target *Env) {
	// Constants
	target.Define("PI", Num(math.Pi))
	target.Define("E", Num(math.E))
	setBuiltinDoc(target, "PI", `Mathematical constant π.

Returns:
	Num`)
	setBuiltinDoc(target, "E", `Euler's number e.

Returns:
	Num`)

	// Unary math helpers
	un1 := func(name string, f func(float64) float64, doc string) {
		ip.RegisterRuntimeBuiltin(
			target,
			name,
			[]ParamSpec{{Name: "x", Type: S{"id", "Num"}}},
			S{"id", "Num"},
			func(_ *Interpreter, ctx CallCtx) Value {
				return Num(f(ctx.Arg("x").Data.(float64)))
			},
		)
		setBuiltinDoc(target, name, doc)
	}
	un1("sin", math.Sin, `Sine of an angle in radians.

Params:
	x: Num — radians

Returns:
	Num`)
	un1("cos", math.Cos, `Cosine of an angle in radians.

Params:
	x: Num — radians

Returns:
	Num`)
	un1("tan", math.Tan, `Tangent of an angle in radians.

Params:
	x: Num — radians

Returns:
	Num`)
	un1("sqrt", math.Sqrt, `Square root.

Params:
	x: Num — non-negative

Returns:
	Num`)
	un1("log", math.Log, `Natural logarithm (base e).

Params:
	x: Num — positive

Returns:
	Num`)
	un1("exp", math.Exp, `Exponential function e^x.

Params:
	x: Num

Returns:
	Num`)

	ip.RegisterRuntimeBuiltin(
		target,
		"pow",
		[]ParamSpec{
			{Name: "base", Type: S{"id", "Num"}},
			{Name: "exp", Type: S{"id", "Num"}},
		},
		S{"id", "Num"},
		func(_ *Interpreter, ctx CallCtx) Value {
			return Num(math.Pow(ctx.Arg("base").Data.(float64), ctx.Arg("exp").Data.(float64)))
		},
	)
	setBuiltinDoc(target, "pow", `Power: base^exp.

Params:
	base: Num
	exp:  Num

Returns:
	Num`)
}

// --- Process Utilities ----------------------------------------------------

func registerProcessBuiltins(ip *Interpreter, target *Env) {
	// exit(code:Int?) -> Null (terminates the host process)
	ip.RegisterRuntimeBuiltin(
		target,
		"exit",
		[]ParamSpec{{Name: "code", Type: S{"unop", "?", S{"id", "Int"}}}},
		S{"id", "Null"},
		func(_ *Interpreter, ctx CallCtx) Value {
			codeV := ctx.Arg("code")
			code := 0
			if codeV.Tag == VTInt {
				code = int(codeV.Data.(int64))
			}
			os.Exit(code)
			return Null // unreachable
		},
	)
	setBuiltinDoc(target, "exit", `Terminate the current process with an optional status code.

By convention, 0 indicates success; non-zero indicates an error.

Params:
	code: Int? — exit status (default 0)

Returns:
	Null (never returns; process exits)`)
}

// stripAnnDeep removes annotations from a value recursively.
//   - Clears Value.Annot on all nodes.
//   - For arrays, deep-copies elements with annotations stripped.
//   - For maps, deep-copies entries (annotations stripped), preserves key order (Keys),
//     and clears KeyAnn entirely.
func stripAnnDeep(v Value) Value {
	// Always drop the annotation on this node.
	v.Annot = ""
	switch v.Tag {
	case VTArray:
		ao := v.Data.(*ArrayObject)
		elems := make([]Value, len(ao.Elems))
		for i := range ao.Elems {
			elems[i] = stripAnnDeep(ao.Elems[i])
		}
		return Arr(elems) // Arr() builds a fresh ArrayObject
	case VTMap:
		mo := v.Data.(*MapObject)
		cpE := make(map[string]Value, len(mo.Entries))
		for k, vv := range mo.Entries {
			cpE[k] = stripAnnDeep(vv)
		}
		cpK := make([]string, len(mo.Keys))
		copy(cpK, mo.Keys)
		return Value{
			Tag: VTMap,
			Data: &MapObject{
				Entries: cpE,
				Keys:    cpK,
			},
		}
	default:
		// Primitives, modules already normalized via AsMapValue, etc.
		return v
	}
}
