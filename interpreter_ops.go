// interpreter_ops.go — PRIVATE: language ops (built-ins, assignment, iteration)
// and the AST → bytecode emitter used by the exec layer.
//
// This file:
//  - Implements `newOps(ip)` with `initCore()` (registers all core natives).
//  - Provides assignment semantics (`assignTo`) and helpers.
//  - Normalizes collections to iterators (`__to_iter`) and drives iteration.
//  - Implements deep value equality for const interning in the emitter.
//  - Hosts the private emitter (`newEmitter`) used by exec for JIT.
//
// Public API is in interpreter.go. Exec/call engine is in interpreter_exec.go.
//
// Concurrency model (minimal, Lua-style isolates):
//  - A single *Interpreter is **not re-entrant**; do not call it from multiple
//    goroutines. For parallelism, clone via (*Interpreter).Clone() and use the
//    clone in another goroutine. Each clone has its own Core/Global/env graph,
//    module cache, and source-tracking, so no locks are required here.
//  - All state touched in this file is per-interpreter (o.ip / ip.*). There are
//    no package-level mutable singletons. As long as an Interpreter isn't shared
//    concurrently, operations here are race-free without additional locking.
//  - Host native functions you register may themselves use goroutines, but they
//    must not touch the *same* Interpreter or its Env from multiple goroutines.
//    Use isolates (clones) for truly concurrent execution.

package mindscript

import (
	"fmt"
	"strings"
)

////////////////////////////////////////////////////////////////////////////////
//                         PRIVATE PANIC / ERROR HELPERS
////////////////////////////////////////////////////////////////////////////////

type returnSig struct{ v Value }
type rtErr struct {
	msg  string
	src  *SourceRef
	line int
	col  int
}

func fail(msg string)          { panic(rtErr{msg: msg}) }
func errNull(msg string) Value { return withAnnot(Null, msg) }
func annotNull(msg string) Value {
	return Value{Tag: VTNull, Annot: msg}
}
func withAnnot(v Value, ann string) Value { v.Annot = ann; return v }

// panicRt rethrows a structured runtime error as a **value** (never a pointer).
// Always use this (or fail) to signal runtime errors within the interpreter.
func panicRt(msg string, src *SourceRef, line, col int) {
	panic(rtErr{msg: msg, src: src, line: line, col: col})
}

////////////////////////////////////////////////////////////////////////////////
//                          PRIVATE OPS FACADE (to API)
////////////////////////////////////////////////////////////////////////////////

type opsImpl struct{ ip *Interpreter }

func newOps(ip *Interpreter) opsCore { return &opsImpl{ip: ip} }

func (o *opsImpl) initCore() {
	ip := o.ip
	if ip.Core == nil {
		ip.Core = NewEnv(nil)
	}
	// sugar for native registration with a ctx-only closure
	reg := func(name string, params []ParamSpec, ret S, body func(ctx CallCtx) Value) {
		ip.RegisterNative(name, params, ret, func(_ *Interpreter, ctx CallCtx) Value { return body(ctx) })
	}

	// __assign_set(target: Type, value: Any) -> Any
	reg("__assign_set",
		[]ParamSpec{{"target", S{"id", "Type"}}, {"value", S{"id", "Any"}}},
		S{"id", "Any"},
		func(ctx CallCtx) Value {
			t := ctx.MustArg("target")
			v := ctx.MustArg("value")
			tv := t.Data.(*TypeValue)
			ip.assignTo(tv.Ast, v, ctx.Env())
			return v
		})

	// __assign_def(target: Type, value: Any) -> Any
	reg("__assign_def",
		[]ParamSpec{{"target", S{"id", "Type"}}, {"value", S{"id", "Any"}}},
		S{"id", "Any"},
		func(ctx CallCtx) Value {
			t := ctx.MustArg("target")
			v := ctx.MustArg("value")
			tv := t.Data.(*TypeValue)
			ip.assignTo(tv.Ast, v, ctx.Env(), true)
			return v
		})

	// __plus (numbers/strings/arrays/maps)
	reg("__plus",
		[]ParamSpec{{"a", S{"id", "Any"}}, {"b", S{"id", "Any"}}}, S{"id", "Any"},
		func(ctx CallCtx) Value {
			a := AsMapValue(ctx.MustArg("a"))
			b := AsMapValue(ctx.MustArg("b"))
			if isNumber(a) && isNumber(b) {
				if a.Tag == VTInt && b.Tag == VTInt {
					return Int(a.Data.(int64) + b.Data.(int64))
				}
				return Num(toFloat(a) + toFloat(b))
			}
			if a.Tag == VTStr && b.Tag == VTStr {
				return Str(a.Data.(string) + b.Data.(string))
			}
			if a.Tag == VTArray && b.Tag == VTArray {
				x := append(append([]Value{}, a.Data.([]Value)...), b.Data.([]Value)...)
				return Arr(x)
			}
			if a.Tag == VTMap && b.Tag == VTMap {
				am, bm := a.Data.(*MapObject), b.Data.(*MapObject)
				out := &MapObject{
					Entries: make(map[string]Value, len(am.Entries)+len(bm.Entries)),
					KeyAnn:  make(map[string]string, len(am.KeyAnn)+len(bm.KeyAnn)),
					Keys:    make([]string, 0, len(am.Keys)+len(bm.Keys)),
				}
				seen := make(map[string]struct{}, len(am.Keys)+len(bm.Keys))
				// LHS order/content
				for _, k := range am.Keys {
					out.Keys = append(out.Keys, k)
					seen[k] = struct{}{}
				}
				for k, v := range am.Entries {
					out.Entries[k] = v
				}
				for k, ann := range am.KeyAnn {
					out.KeyAnn[k] = ann
				}
				// overlay RHS; append new keys in RHS order
				for _, k := range bm.Keys {
					if _, ok := seen[k]; !ok {
						out.Keys = append(out.Keys, k)
						seen[k] = struct{}{}
					}
				}
				for k, v := range bm.Entries {
					out.Entries[k] = v
				}
				for k, ann := range bm.KeyAnn {
					out.KeyAnn[k] = ann
				}
				return Value{Tag: VTMap, Data: out}
			}
			return errNull("unsupported operands for '+'")
		})

	// __resolve_type: Value(Type) -> Value(Type(resolved))
	reg("__resolve_type",
		[]ParamSpec{{"t", S{"id", "Type"}}}, S{"id", "Type"},
		func(ctx CallCtx) Value {
			t := ctx.MustArg("t")
			resolved := ip.resolveTypeValue(t, ctx.Env())
			return TypeVal(resolved)
		})

	// __annotate(text: Str, v: Any) -> Any
	reg("__annotate",
		[]ParamSpec{{"text", S{"id", "Str"}}, {"v", S{"id", "Any"}}}, S{"id", "Any"},
		func(ctx CallCtx) Value { return withAnnot(ctx.MustArg("v"), ctx.MustArg("text").Data.(string)) })

	// __collect_for_elems(iter: Any) -> Any   (used by high-level mapping helpers)
	reg("__collect_for_elems",
		[]ParamSpec{{"iter", S{"id", "Any"}}}, S{"id", "Any"},
		func(ctx CallCtx) (out Value) {
			defer func() {
				if r := recover(); r != nil {
					if e, ok := r.(rtErr); ok {
						out = errNull(e.msg)
						return
					}
					panic(r)
				}
			}()
			out = Arr(ip.collectForElemsScoped(ctx.MustArg("iter"), ctx.Env()))
			return
		})

	// __map_from(keys:[Str], vals:[Any]) -> Map
	reg("__map_from",
		[]ParamSpec{{"keys", S{"array", S{"id", "Str"}}}, {"vals", S{"array", S{"id", "Any"}}}}, S{"id", "Any"},
		func(ctx CallCtx) Value {
			ka := ctx.MustArg("keys").Data.([]Value)
			va := ctx.MustArg("vals").Data.([]Value)
			if len(ka) != len(va) {
				return errNull("map_from: mismatched arity")
			}
			mo := &MapObject{
				Entries: make(map[string]Value, len(ka)),
				KeyAnn:  make(map[string]string, len(ka)),
				Keys:    make([]string, 0, len(ka)),
			}
			for i := range ka {
				if ka[i].Tag != VTStr {
					return errNull("map key must be string")
				}
				k := ka[i].Data.(string)
				mo.Entries[k] = va[i]
				mo.Keys = append(mo.Keys, k)
				if ann := ka[i].Annot; ann != "" {
					mo.KeyAnn[k] = ann
				}
			}
			return Value{Tag: VTMap, Data: mo}
		})

	// __len(array|map) -> Int
	reg("__len",
		[]ParamSpec{{"x", S{"id", "Any"}}}, S{"id", "Int"},
		func(ctx CallCtx) Value {
			x := AsMapValue(ctx.MustArg("x"))
			switch x.Tag {
			case VTArray:
				return Int(int64(len(x.Data.([]Value))))
			case VTMap:
				return Int(int64(len(x.Data.(*MapObject).Entries)))
			default:
				return errNull("len expects array or map")
			}
		})

	// __make_fun(params:[Str], types:[Type], ret:Type, body:Type, isOracle:Bool, examples:Any) -> Fun
	ip.RegisterNative("__make_fun",
		[]ParamSpec{
			{"params", S{"array", S{"id", "Str"}}},
			{"types", S{"array", S{"id", "Type"}}},
			{"ret", S{"id", "Type"}},
			{"body", S{"id", "Type"}},
			{"isOracle", S{"id", "Bool"}},
			{"examples", S{"id", "Any"}},
			{"basePath", S{"array", S{"id", "Int"}}},
		},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			namesV := ctx.MustArg("params").Data.([]Value)
			typesV := ctx.MustArg("types").Data.([]Value)
			retTV := ctx.MustArg("ret").Data.(*TypeValue)
			bodyTV := ctx.MustArg("body").Data.(*TypeValue)
			isOr := ctx.MustArg("isOracle").Data.(bool)
			exAny := ctx.MustArg("examples")
			baseAny := ctx.MustArg("basePath")

			names := make([]string, len(namesV))
			types := make([]S, len(typesV))
			for i := range namesV {
				names[i] = namesV[i].Data.(string)
			}
			for i := range typesV {
				types[i] = typesV[i].Data.(*TypeValue).Ast
			}

			hidden := false
			if len(names) == 0 {
				names = []string{"_"}
				types = []S{S{"id", "Null"}}
				hidden = true
			}

			var exVals []Value
			if exAny.Tag == VTArray {
				exVals = append([]Value(nil), exAny.Data.([]Value)...)
			}

			// Build absolute base path for the body
			var base NodePath
			if baseAny.Tag == VTArray {
				xs := baseAny.Data.([]Value)
				base = make(NodePath, 0, len(xs))
				for _, v := range xs {
					if v.Tag == VTInt {
						base = append(base, int(v.Data.(int64)))
					}
				}
			}

			retAst := retTV.Ast
			if isOr {
				retAst = ensureNullableUnlessAny(retAst)
			}

			// Clone current SourceRef and attach base path
			var sr *SourceRef
			if ip.currentSrc != nil {
				cpy := *ip.currentSrc
				// IMPORTANT: 'base' is ABSOLUTE - overwrite.
				cpy.PathBase = append(NodePath(nil), base...)
				sr = &cpy
			}

			// Build closure env that carries hidden signature metadata
			closure := NewEnv(ctx.Env())
			nameVals := make([]Value, len(names))
			for i, n := range names {
				nameVals[i] = Str(n)
			}
			typeVals := make([]Value, len(types))
			for i, t := range types {
				typeVals[i] = TypeVal(t)
			}
			closure.Define("$__sig_names", Arr(nameVals))
			closure.Define("$__sig_types", Arr(typeVals))

			// Construct the function with this closure
			return FunVal(&Fun{
				Params:     names,
				ParamTypes: types,
				ReturnType: retAst,
				Body:       bodyTV.Ast,
				Env:        closure, // <-- use the closure with hidden signature
				HiddenNull: hidden,
				IsOracle:   isOr,
				Examples:   exVals,
				Src:        sr,
			})
		})

	// __is_fun(x: Any) -> Bool
	reg("__is_fun",
		[]ParamSpec{{"x", S{"id", "Any"}}}, S{"id", "Bool"},
		func(ctx CallCtx) Value { return Bool(ctx.MustArg("x").Tag == VTFun) })

	// __iter_should_stop(x: Any) -> Bool
	reg("__iter_should_stop",
		[]ParamSpec{{"x", S{"id", "Any"}}}, S{"id", "Bool"},
		func(ctx CallCtx) Value {
			v := ctx.MustArg("x")
			if v.Tag == VTNull {
				if v.Annot != "" {
					fail(v.Annot)
				}
				return Bool(true)
			}
			return Bool(false)
		})

	// __to_iter(x: Any) -> (Null -> Any?)  |  error
	ip.RegisterNative("__to_iter",
		[]ParamSpec{{"x", S{"id", "Any"}}}, S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			x := AsMapValue(ctx.MustArg("x"))

			// Already an iterator?
			if x.Tag == VTFun {
				f := x.Data.(*Fun)
				if len(f.Params) == 1 && ip.isType(Null, f.ParamTypes[0], f.Env) {
					return x
				}
				fail("for expects array, map, or iterator function (Null -> Any?)")
			}

			// Helpers
			newIter := func(parent *Env, lenTarget S, thenBlock S) Value {
				env := NewEnv(parent)
				env.Define("$i", Int(0))
				body := S{"if",
					S{"pair",
						S{"binop", "<",
							S{"id", "$i"},
							S{"call", S{"id", "__len"}, lenTarget},
						},
						S{"block", thenBlock},
					},
					S{"block", S{"null"}},
				}
				var sr *SourceRef
				if ip.currentSrc != nil {
					cpy := *ip.currentSrc // shallow copy; Spans pointer intentionally shared
					sr = &cpy
				}
				return FunVal(&Fun{
					Params:     []string{"_"},
					ParamTypes: []S{S{"id", "Null"}},
					ReturnType: S{"unop", "?", S{"id", "Any"}},
					Body:       body,
					Env:        env,
					Src:        sr,
				})
			}
			inc := func() S {
				return S{"assign", S{"id", "$i"},
					S{"binop", "+", S{"id", "$i"}, S{"int", int64(1)}},
				}
			}

			// Array → iterator
			if x.Tag == VTArray {
				envInit := NewEnv(ctx.Env())
				envInit.Define("$arr", x)
				then := S{"block",
					inc(),
					S{"idx",
						S{"id", "$arr"},
						S{"binop", "-", S{"id", "$i"}, S{"int", int64(1)}},
					},
				}
				return newIter(envInit, S{"id", "$arr"}, then)
			}

			// Map → iterator (yields [key, value]) preserving insertion order + key annotations
			if x.Tag == VTMap {
				mo := x.Data.(*MapObject)
				envInit := NewEnv(ctx.Env())
				envInit.Define("$map", x)
				keyVals := make([]Value, 0, len(mo.Keys))
				for _, k := range mo.Keys {
					s := Str(k)
					if ann, ok := mo.KeyAnn[k]; ok && ann != "" {
						s = withAnnot(s, ann)
					}
					keyVals = append(keyVals, s)
				}
				envInit.Define("$keys", Arr(keyVals))

				then := S{"block",
					S{"assign", S{"decl", "$k"},
						S{"idx", S{"id", "$keys"}, S{"id", "$i"}},
					},
					inc(),
					S{"array",
						S{"id", "$k"},
						S{"idx", S{"id", "$map"}, S{"id", "$k"}},
					},
				}
				return newIter(envInit, S{"id", "$keys"}, then)
			}

			fail("for expects array, map, or iterator function (Null -> Any?)")
			return annotNull("__for_iter: unreachable")
		})

	ip.RegisterNative(
		"__make_module",
		[]ParamSpec{
			{Name: "name", Type: S{"id", "Str"}}, // keep lax; runtime checks enforce string
			{Name: "body", Type: S{"id", "Any"}}, // Type-carried AST
			{Name: "base", Type: S{"id", "Any"}}, // [Int] path
		},
		S{"id", "Any"}, // could be a dedicated Module type later; Any is simplest now
		nativeMakeModule,
	)
}

////////////////////////////////////////////////////////////////////////////////
//                                ASSIGNMENT
////////////////////////////////////////////////////////////////////////////////

func (ip *Interpreter) assignTo(target S, value Value, env *Env, optAllowDefine ...bool) {
	allowDefine := len(optAllowDefine) > 0 && optAllowDefine[0]
	switch target[0].(string) {
	case "id":
		name := target[1].(string)
		if err := env.Set(name, value); err != nil {
			if allowDefine {
				env.Define(name, value)
				return
			}
			fail(err.Error())
		}
	case "decl":
		env.Define(target[1].(string), value)
	case "get":
		obj := ip.evalFull(target[1].(S), env)
		// resolve key string (literal or computed)
		var keyStr string
		if ks := target[2].(S); len(ks) >= 2 && (ks[0].(string) == "id" || ks[0].(string) == "str") {
			keyStr = ks[1].(string)
		} else {
			k := ip.evalFull(target[2].(S), env)
			if k.Tag != VTStr {
				fail("object assignment requires map and string key")
			}
			keyStr = k.Data.(string)
		}
		mv := AsMapValue(obj)
		if mv.Tag == VTMap {
			mo := mv.Data.(*MapObject)
			if _, exists := mo.Entries[keyStr]; !exists {
				mo.Keys = append(mo.Keys, keyStr)
			}
			mo.Entries[keyStr] = value
			syncModuleEnv(obj, keyStr, value) // no-op for plain maps
			return
		}
		if obj.Tag == VTModule {
			fail("object assignment requires map and string key") // unreachable, safety
		}
		if obj.Tag == VTArray {
			fail("object assignment requires map and string key")
		}
		fail("object assignment requires map and string key")
	case "idx":
		obj, idx := ip.evalFull(target[1].(S), env), ip.evalFull(target[2].(S), env)
		if obj.Tag == VTArray && idx.Tag == VTInt {
			xs := obj.Data.([]Value)
			if len(xs) == 0 {
				fail("index on empty array")
			}
			i := int(idx.Data.(int64))
			if i < 0 {
				i = (i%len(xs) + len(xs)) % len(xs)
			}
			if i < 0 || i >= len(xs) {
				fail("array index out of range")
			}
			xs[i] = value
			return
		}
		mv := AsMapValue(obj)
		if mv.Tag == VTMap && idx.Tag == VTStr {
			mo := mv.Data.(*MapObject)
			k := idx.Data.(string)
			if _, exists := mo.Entries[k]; !exists {
				mo.Keys = append(mo.Keys, k)
			}
			mo.Entries[k] = value
			syncModuleEnv(obj, k, value)
			return
		}
		fail("index assignment requires array[int] or map[string]")
	case "darr":
		if value.Tag != VTArray {
			for i := 1; i < len(target); i++ {
				ip.assignTo(target[i].(S), annotNull("array pattern: RHS is not an array"), env, true)
			}
			return
		}
		xs := value.Data.([]Value)
		for i := 1; i < len(target); i++ {
			if i-1 < len(xs) {
				ip.assignTo(target[i].(S), xs[i-1], env, true)
			} else {
				ip.assignTo(target[i].(S), annotNull(fmt.Sprintf("array pattern: missing element #%d", i-1)), env, true)
			}
		}
	case "dobj":
		vmap := AsMapValue(value)
		if vmap.Tag != VTMap {
			for i := 1; i < len(target); i++ {
				p := target[i].(S) // ("pair", key, pattern)
				ip.assignTo(p[2].(S), annotNull("object pattern: RHS is not a map"), env, true)
			}
			return
		}
		mo := vmap.Data.(*MapObject)
		m := mo.Entries
		for i := 1; i < len(target); i++ {
			p := target[i].(S)
			k := unwrapKeyStr(p[1].(S))
			if v, ok := m[k]; ok {
				ip.assignTo(p[2].(S), v, env, true)
			} else {
				ip.assignTo(p[2].(S), annotNull(fmt.Sprintf("object pattern: missing key '%s'", k)), env, true)
			}
		}
	case "annot":
		text := target[1].(S)[1].(string)
		sub := target[2].(S)
		if len(sub) > 0 && sub[0].(string) == "decl" {
			env.Define(sub[1].(string), withAnnot(value, text))
			return
		}
		ip.assignTo(sub, value, env, true)
	default:
		fail("invalid assignment target")
	}
}

// syncModuleEnv keeps a module's Env consistent after a write to its map.
// NOTE (isolates): modules live within a single Interpreter instance; this
// function updates the module's *local* Env only. Do not cross-post between
// interpreters.
func syncModuleEnv(obj Value, key string, val Value) {
	if obj.Tag == VTModule {
		m := obj.Data.(*Module)
		if _, ok := m.Env.table[key]; ok {
			m.Env.table[key] = val
		} else {
			m.Env.Define(key, val)
		}
	}
}

////////////////////////////////////////////////////////////////////////////////
//                     TINY EVALUATORS (used by assignment)
////////////////////////////////////////////////////////////////////////////////

// evalFull compiles and runs a single expression in env.
// Annotated null is turned into a runtime failure (panic(rtErr)) to align with assignment.
func (ip *Interpreter) evalFull(n S, env *Env) Value {
	em := newEmitter(ip, ip.currentSrc)
	em.emitExpr(n)
	em.emit(opReturn, 0)
	ch := em.chunk()
	res := ip.runChunk(ch, env, 0)
	switch res.status {
	case vmOK, vmReturn:
		if res.value.Tag == VTNull && res.value.Annot != "" {
			fail(res.value.Annot)
		}
		return res.value
	case vmRuntimeError:
		if res.value.Tag == VTNull && res.value.Annot != "" {
			fail(res.value.Annot)
		}
		fail("runtime error")
	default:
		fail("unknown VM status")
	}
	return Null
}

////////////////////////////////////////////////////////////////////////////////
//                             ITERATOR EXPANSION
////////////////////////////////////////////////////////////////////////////////

func (ip *Interpreter) collectForElemsScoped(iter Value, scope *Env) []Value {
	iter = AsMapValue(iter)

	// Normalize to iterator function via Core's __to_iter when needed.
	if iter.Tag != VTFun {
		toIter, err := ip.Core.Get("__to_iter")
		if err != nil {
			fail("for expects array, map, or iterator function (Null -> Any?)")
		}
		iter = ip.applyArgsScoped(toIter, []Value{iter}, scope)

		// Safety: __to_iter now fails itself for bad inputs; if it ever
		// returns non-fun here, keep the user-facing invariant.
		if iter.Tag != VTFun {
			fail("for expects array, map, or iterator function (Null -> Any?)")
		}
	}

	// At this point, iter must be a function of shape (Null) -> Any?
	f, ok := iter.Data.(*Fun)
	if !ok {
		fail("for expects array, map, or iterator function (Null -> Any?)")
	}
	if len(f.Params) != 1 || !ip.isType(Null, f.ParamTypes[0], f.Env) {
		name := "_"
		if len(f.Params) > 0 {
			name = f.Params[0]
		}
		fail(fmt.Sprintf("type mismatch in parameter '%s'", name))
	}

	stopFn, err := ip.Core.Get("__iter_should_stop")
	if err != nil {
		fail("missing __iter_should_stop")
	}

	out := []Value{}
	for {
		next := ip.applyArgsScoped(iter, []Value{Null}, scope)
		if ip.applyArgsScoped(stopFn, []Value{next}, scope).Data.(bool) {
			break
		}
		out = append(out, next)
	}
	return out
}

////////////////////////////////////////////////////////////////////////////////
//                          VALUE EQUALITY (for emitter)
////////////////////////////////////////////////////////////////////////////////

func (ip *Interpreter) deepEqual(a, b Value) bool {
	if a.Tag == VTModule {
		a = Value{Tag: VTMap, Data: a.Data.(*Module).Map}
	}
	if b.Tag == VTModule {
		b = Value{Tag: VTMap, Data: b.Data.(*Module).Map}
	}
	if isNumber(a) && isNumber(b) {
		return toFloat(a) == toFloat(b)
	}
	if a.Tag != b.Tag {
		return false
	}
	switch a.Tag {
	case VTNull:
		return true
	case VTBool:
		return a.Data.(bool) == b.Data.(bool)
	case VTInt:
		return a.Data.(int64) == b.Data.(int64)
	case VTNum:
		return a.Data.(float64) == b.Data.(float64)
	case VTStr:
		return a.Data.(string) == b.Data.(string)
	case VTArray:
		ax, bx := a.Data.([]Value), b.Data.([]Value)
		if len(ax) != len(bx) {
			return false
		}
		for i := range ax {
			if !ip.deepEqual(ax[i], bx[i]) {
				return false
			}
		}
		return true
	case VTMap:
		am, bm := a.Data.(*MapObject), b.Data.(*MapObject)
		if len(am.Entries) != len(bm.Entries) {
			return false
		}
		for k, av := range am.Entries {
			bv, ok := bm.Entries[k]
			if !ok || !ip.deepEqual(av, bv) {
				return false
			}
		}
		return true
	case VTFun:
		return a.Data.(*Fun) == b.Data.(*Fun)
	case VTType:
		ta := a.Data.(*TypeValue)
		tb := b.Data.(*TypeValue)
		ea := ta.Env
		if ea == nil {
			ea = ip.Core
		}
		eb := tb.Env
		if eb == nil {
			eb = ip.Core
		}
		ra := ip.resolveType(ta.Ast, ea)
		rb := ip.resolveType(tb.Ast, eb)
		return equalS(ra, rb)
	default:
		return false
	}
}

////////////////////////////////////////////////////////////////////////////////
//                                SMALL HELPERS
////////////////////////////////////////////////////////////////////////////////

func isNumber(v Value) bool { return v.Tag == VTInt || v.Tag == VTNum }
func toFloat(v Value) float64 {
	if v.Tag == VTInt {
		return float64(v.Data.(int64))
	}
	return v.Data.(float64)
}

func unwrapKeyStr(k S) string {
	for len(k) > 0 && k[0].(string) == "annot" {
		k = k[2].(S)
	}
	if len(k) >= 2 && k[0].(string) == "str" {
		return k[1].(string)
	}
	fail("map key is not a string")
	return ""
}

// Noop detection: ("noop") and ("annot", ..., ("noop"), ...) are “noopish” and
// generate no code inside blocks.
func isNoopish(n S) bool {
	if len(n) == 0 {
		return false
	}
	switch n[0].(string) {
	case "noop":
		// Defensive: treat a stray ("noop") in expression position as plain Null.
		return true
	case "annot":
		// n[2] is the subject node; treat annot(noop) as noop
		if len(n) >= 3 {
			if sub, ok := n[2].(S); ok {
				return isNoopish(sub)
			}
		}
		return false
	default:
		return false
	}
}

// Given a VTType, resolve its AST using its own env if present; otherwise use fallback.
func (ip *Interpreter) resolveTypeValue(v Value, fallback *Env) S {
	if v.Tag != VTType {
		return S{"id", "Any"}
	}
	tv := v.Data.(*TypeValue)
	env := tv.Env
	if env == nil {
		env = fallback
	}
	return ip.resolveType(tv.Ast, env)
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

// nativeMakeModule is the implementation of the __make_module primitive.
//
// UNIFORM CACHING & CYCLE DETECTION LIVE HERE.
// This ensures AST/Code/File/inline constructions all behave the same.
//
// It receives:
//   - name: Str   — the **canonical identity** for the module (NOT overwritten).
//   - body: Type  — AST for the module body wrapped as a type value.
//   - base: [Int] — absolute NodePath indicating where the body lives in the
//     caller’s SourceRef; used to re-root spans to the body.
//
// Plumbing:
//   - We build a child SourceRef with PathBase=base so VM marks and PC→(line,col)
//     map into the module body text.
//   - Runtime errors are rethrown with exact location using panicRt, so they
//     bubble to runTopWithSource and render a single caret at the true site.
//
// Concurrency note: module load state (ip.modules, ip.loadStack) belongs to a
// single Interpreter isolate. Do not share the same Interpreter across goroutines.
func nativeMakeModule(ip *Interpreter, ctx CallCtx) Value {
	nameV := ctx.MustArg("name")
	bodyV := ctx.MustArg("body")
	baseV := ctx.MustArg("base")

	if nameV.Tag != VTStr {
		fail("module name must be a string")
	}
	if bodyV.Tag != VTType {
		fail("internal error: module body must be a Type")
	}
	canon := nameV.Data.(string)

	// ---- Uniform cycle detection (stack + in-progress record) ----
	for _, s := range ip.loadStack {
		if s == canon {
			fail(fmt.Sprintf("import cycle detected: %s", joinCyclePath(ip.loadStack, canon)))
		}
	}
	if ip.modules != nil {
		if rec, ok := ip.modules[canon]; ok && rec.state == modLoading {
			fail(fmt.Sprintf("import cycle detected: %s", joinCyclePath(append(ip.loadStack, canon), canon)))
		}
	}

	// ---- Uniform caching (success-only) ----
	if ip.modules != nil {
		if rec, ok := ip.modules[canon]; ok && rec.state == modLoaded && rec.mod != nil {
			return Value{Tag: VTModule, Data: rec.mod}
		}
	} else {
		ip.modules = map[string]*moduleRec{}
	}

	// Mark as loading and push on stack.
	ip.modules[canon] = &moduleRec{spec: canon, state: modLoading}
	ip.loadStack = append(ip.loadStack, canon)

	// Ensure we never leave a stale modLoading record or a stuck stack entry.
	// On panic/failure, delete the cache record; always pop loadStack.
	defer func() {
		// Pop load stack
		if n := len(ip.loadStack); n > 0 {
			ip.loadStack = ip.loadStack[:n-1]
		}
		// If not successfully flipped to modLoaded, remove the half-built record.
		if rec, ok := ip.modules[canon]; ok && rec.state != modLoaded {
			delete(ip.modules, canon)
		}
		// Preserve existing error semantics.
		if r := recover(); r != nil {
			panic(r)
		}
	}()

	// ---- Decode body AST and base path ----
	tv := bodyV.Data.(*TypeValue)
	bodyAst := tv.Ast

	// Decode absolute base path from [Int]
	var base NodePath
	if baseV.Tag == VTArray {
		xs := baseV.Data.([]Value)
		base = make(NodePath, 0, len(xs))
		for _, v := range xs {
			if v.Tag != VTInt {
				fail("internal error: module base path must be [Int]")
			}
			base = append(base, int(v.Data.(int64)))
		}
	}

	// Fresh env for the module (Core is parent so builtins are visible).
	modEnv := NewEnv(ip.Core)
	modEnv.SealParentWrites()

	// SourceRef rooted at the module BODY path (absolute)
	var sr *SourceRef
	if ip.currentSrc != nil {
		// Compose any existing PathBase with the module's absolute body path.
		sr = &SourceRef{
			Name:     ip.currentSrc.Name,
			Src:      ip.currentSrc.Src,
			Spans:    ip.currentSrc.Spans, // keep full index; marks are absolute
			PathBase: append(NodePath(nil), base...),
		}
	}

	// JIT + run (like runTopWithSource, but we handle errors to avoid re-wrap)
	ch := ip.jitTop(bodyAst, sr)

	prev := ip.currentSrc
	ip.currentSrc = ch.Src
	res := ip.runChunk(ch, modEnv, 0)
	ip.currentSrc = prev

	switch res.status {
	case vmOK, vmReturn:
		// ok
	case vmRuntimeError:
		line, col := ip.sourcePosFromChunk(ch, ch.Src, res.pc)
		msg := res.value.Annot
		if msg == "" {
			msg = "runtime error"
		}
		// Rethrow as structured inner-source error (single caret at true site).
		panicRt(msg, ch.Src, line, col)
	default:
		line, col := ip.sourcePosFromChunk(ch, ch.Src, res.pc)
		panicRt("unknown VM status", ch.Src, line, col)
	}

	// Snapshot exports
	mo := buildModuleMap(modEnv)
	m := &Module{Name: canon, Map: mo, Env: modEnv}

	// Commit cache (success-only)
	rec := ip.modules[canon]
	rec.mod = m
	rec.env = modEnv
	rec.state = modLoaded
	rec.err = nil

	return Value{Tag: VTModule, Data: m}
}
