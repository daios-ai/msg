// === FILE: std_sys.go ===
package mindscript

import (
	"encoding/json"
	"math"
	"math/rand"
	"os"
	"strconv"
	"time"
	"unicode/utf8"
)

// --- Utilities: time, rand, json --------------------------------------------

func registerUtilityBuiltins(ip *Interpreter) {

	ip.RegisterNative(
		"clone",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			return cloneValue(ctx.MustArg("x"))
		},
	)
	setBuiltinDoc(ip, "clone", `Deep-copy arrays and maps.

For maps, preserves key order and per-key annotations. Primitive values are
returned as-is. Functions, modules, and handles are not duplicated (identity
is preserved).

Params:
  x: Any

Returns:
  Any — a structurally independent copy for arrays/maps`)

	var rng = rand.New(rand.NewSource(time.Now().UnixNano()))
	ip.RegisterNative(
		"seedRand",
		[]ParamSpec{{Name: "n", Type: S{"id", "Int"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			n := ctx.MustArg("n")
			rng.Seed(n.Data.(int64))
			return Null
		},
	)
	setBuiltinDoc(ip, "seedRand", `Seed the pseudo-random number generator.

Use a fixed seed for reproducible sequences.

Params:
  n: Int — seed value

Returns:
  Null`)

	ip.RegisterNative("randInt",
		[]ParamSpec{{Name: "n", Type: S{"id", "Int"}}},
		S{"id", "Int"},
		func(ip *Interpreter, ctx CallCtx) Value {
			n := ctx.MustArg("n").Data.(int64)
			// Contractual: n must be > 0 (hard error)
			if n <= 0 {
				fail("randInt: n must be > 0")
			}
			return Int(int64(rng.Intn(int(n))))
		},
	)
	setBuiltinDoc(ip, "randInt", `Uniform random integer in [0, n).

Params:
  n: Int — upper bound (must be > 0)

Returns:
  Int`)

	ip.RegisterNative("randFloat", nil, S{"id", "Num"}, func(ip *Interpreter, ctx CallCtx) Value {
		return Num(rng.Float64())
	})
	setBuiltinDoc(ip, "randFloat", `Uniform random number in [0.0, 1.0).

Returns:
  Num`)

}

// --- Casting Utilities --------------------------------------------

func registerCastBuiltins(ip *Interpreter) {

	// str(x) -> Str (JSON-ish for arrays/maps; quotes preserved for strings)
	ip.RegisterNative(
		"str",
		[]ParamSpec{{"x", S{"id", "Any"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			v := ctx.MustArg("x")
			switch v.Tag {
			case VTStr:
				return v
			case VTNull:
				return Str("null")
			case VTBool:
				if v.Data.(bool) {
					return Str("true")
				}
				return Str("false")
			case VTInt:
				return Str(strconv.FormatInt(v.Data.(int64), 10))
			case VTNum:
				return Str(strconv.FormatFloat(v.Data.(float64), 'g', -1, 64))
			case VTArray, VTMap:
				b, err := json.Marshal(valueToGoJSON(v))
				if err != nil {
					// Soft error: cannot encode as JSON text (e.g., NaN/Inf)
					return annotNull("stringify: " + err.Error())
				}
				return Str(string(b))
			default:
				// functions/modules/handles/types: fall back to Value.String()
				return Str(v.String())
			}
		},
	)
	setBuiltinDoc(ip, "str", `Stringify a value.

Rules:
  • Str stays as-is
  • Null → "null"
  • Bool → "true"/"false"
  • Int/Num → decimal representation
  • Arrays/Maps → JSON text
  • Functions/Modules/Handles/Types → readable debug form

Params:
  x: Any

Returns:
  Str`)

	ip.RegisterNative(
		"int",
		[]ParamSpec{{"x", S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Int"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			v := ctx.MustArg("x")
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
				return Null
			default:
				return Null
			}
		},
	)
	setBuiltinDoc(ip, "int", `Convert to Int when possible; otherwise return null.

Rules:
  • Int → Int
  • Num → truncated toward zero
  • Bool → 1/0
  • Str → parsed base-10 integer, or null on failure
  • Others → null

Params:
  x: Any

Returns:
  Int?`)

	ip.RegisterNative(
		"num",
		[]ParamSpec{{"x", S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Num"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			v := ctx.MustArg("x")
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
				return Null
			default:
				return Null
			}
		},
	)
	setBuiltinDoc(ip, "num", `Convert to Num when possible; otherwise return null.

Rules:
  • Num → Num
  • Int → floating-point value
  • Bool → 1.0/0.0
  • Str → parsed as float64, or null on failure
  • Others → null

Params:
  x: Any

Returns:
  Num?`)

	ip.RegisterNative(
		"bool",
		[]ParamSpec{{"x", S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Bool"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			v := ctx.MustArg("x")
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
				return Bool(len(v.Data.([]Value)) > 0)
			case VTMap:
				return Bool(len(v.Data.(*MapObject).Entries) > 0)
			default:
				return Bool(true) // functions, modules, handles, types → truthy
			}
		},
	)
	setBuiltinDoc(ip, "bool", `Convert to Bool using common "truthiness" rules.

Falsey:
  • null
  • 0, 0.0
  • "" (empty string)
  • [] (empty array)
  • {} (empty map)

Truthy:
  • everything else (including functions, modules, handles, types)

Params:
  x: Any

Returns:
  Bool`)

	ip.RegisterNative(
		"len",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Int"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			x := AsMapValue(ctx.MustArg("x"))
			switch x.Tag {
			case VTArray:
				return Int(int64(len(x.Data.([]Value))))
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
}

// --- Math Utilities --------------------------------------------

func registerMathBuiltins(ip *Interpreter) {
	// Constants
	ip.Core.Define("PI", Num(math.Pi))
	ip.Core.Define("E", Num(math.E))
	setBuiltinDoc(ip, "PI", `Mathematical constant π.

Returns:
  Num`)
	setBuiltinDoc(ip, "E", `Euler's number e.

Returns:
  Num`)

	// Unary math helpers
	un1 := func(name string, f func(float64) float64, doc string) {
		ip.RegisterNative(
			name,
			[]ParamSpec{{"x", S{"id", "Num"}}},
			S{"id", "Num"},
			func(_ *Interpreter, ctx CallCtx) Value { return Num(f(ctx.MustArg("x").Data.(float64))) },
		)
		setBuiltinDoc(ip, name, doc)
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

	ip.RegisterNative(
		"pow",
		[]ParamSpec{{"base", S{"id", "Num"}}, {"exp", S{"id", "Num"}}},
		S{"id", "Num"},
		func(_ *Interpreter, ctx CallCtx) Value {
			return Num(math.Pow(ctx.MustArg("base").Data.(float64), ctx.MustArg("exp").Data.(float64)))
		},
	)
	setBuiltinDoc(ip, "pow", `Power: base^exp.

Params:
  base: Num
  exp:  Num

Returns:
  Num`)
}

// --- Process Utilities --------------------------------------------

func registerProcessBuiltins(ip *Interpreter) {
	// exit(code:Int?) -> Null (terminates the host process)
	ip.RegisterNative(
		"exit",
		[]ParamSpec{{"code", S{"unop", "?", S{"id", "Int"}}}},
		S{"id", "Null"},
		func(_ *Interpreter, ctx CallCtx) Value {
			codeV := ctx.MustArg("code")
			code := 0
			if codeV.Tag == VTInt {
				code = int(codeV.Data.(int64))
			}
			os.Exit(code)
			return Null // unreachable
		},
	)
	setBuiltinDoc(ip, "exit", `Terminate the current process with an optional status code.

By convention, 0 indicates success; non-zero indicates an error.

Params:
  code: Int? — exit status (default 0)

Returns:
  Null (never returns; process exits)`)
}
