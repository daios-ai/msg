// interpreter.go
//
// MindScript "interpreter" — JIT compiler + VM runner
// ----------------------------------------------------
// Public API stays the same. Internally we compile S-expr AST to bytecode
// (a Chunk) and execute it on the VM (vm.go). This version removes duplication
// by unifying call/exec paths, iterator helpers, and VM entry/exit plumbing.

package mindscript

import (
	"fmt"
	"strconv"
)

// -----------------------------------------------------------------------------
// Stable runtime model (unchanged)
// -----------------------------------------------------------------------------

type ValueTag int

const (
	VTNull ValueTag = iota
	VTBool
	VTInt
	VTNum
	VTStr
	VTArray
	VTMap
	VTFun
	VTType
	VTModule
	VTHandle
)

type Value struct {
	Tag   ValueTag
	Data  interface{}
	Annot string
}

func (v Value) String() string {
	switch v.Tag {
	case VTNull:
		return "null"
	case VTBool:
		return fmt.Sprintf("%v", v.Data.(bool))
	case VTInt:
		return strconv.FormatInt(v.Data.(int64), 10)
	case VTNum:
		return strconv.FormatFloat(v.Data.(float64), 'g', -1, 64)
	case VTStr:
		return fmt.Sprintf("%q", v.Data.(string))
	case VTArray:
		return fmt.Sprintf("<array len=%d>", len(v.Data.([]Value)))
	case VTMap:
		return "<map>"
	case VTFun:
		return "<fun>"
	case VTType:
		return "<type>"
	case VTModule:
		return "<module>"
	default:
		return "<unknown>"
	}
}

func withAnnot(v Value, ann string) Value { v.Annot = ann; return v }

var Null = Value{Tag: VTNull}

func Bool(b bool) Value    { return Value{Tag: VTBool, Data: b} }
func Int(n int64) Value    { return Value{Tag: VTInt, Data: n} }
func Num(f float64) Value  { return Value{Tag: VTNum, Data: f} }
func Str(s string) Value   { return Value{Tag: VTStr, Data: s} }
func Arr(xs []Value) Value { return Value{Tag: VTArray, Data: xs} }

// MapObject holds entries + per-key annotations, and preserves insertion order.
type MapObject struct {
	Entries map[string]Value
	KeyAnn  map[string]string
	Keys    []string // insertion order of keys (unique)
}

func Map(m map[string]Value) Value {
	// When constructing from a plain map (rare), we don't know order,
	// so keep whatever iteration order Go gives. Literals go via __map_from.
	mo := &MapObject{
		Entries: m,
		KeyAnn:  map[string]string{},
	}
	mo.Keys = make([]string, 0, len(m))
	for k := range m {
		mo.Keys = append(mo.Keys, k)
	}
	return Value{Tag: VTMap, Data: mo}
}

func TypeVal(expr S) Value { return Value{Tag: VTType, Data: expr} }

// Closures keep AST for introspection + a compiled chunk for speed.
type Fun struct {
	Params     []string
	Body       S
	Env        *Env
	ParamTypes []S
	ReturnType S
	HiddenNull bool

	Chunk      *Chunk // JIT result
	NativeName string // non-empty for natives
}

func FunVal(f *Fun) Value { return Value{Tag: VTFun, Data: f} }

// -----------------------------------------------------------------------------
// Environments (unchanged API)
// -----------------------------------------------------------------------------

type Env struct {
	parent *Env
	table  map[string]Value
}

func NewEnv(parent *Env) *Env              { return &Env{parent: parent, table: make(map[string]Value)} }
func (e *Env) Define(name string, v Value) { e.table[name] = v }
func (e *Env) Set(name string, v Value) error {
	if _, ok := e.table[name]; ok {
		e.table[name] = v
		return nil
	}
	if e.parent != nil {
		return e.parent.Set(name, v)
	}
	return fmt.Errorf("undefined variable: %s", name)
}
func (e *Env) Get(name string) (Value, error) {
	if v, ok := e.table[name]; ok {
		return v, nil
	}
	if e.parent != nil {
		return e.parent.Get(name)
	}
	return Value{}, fmt.Errorf("undefined variable: %s", name)
}

// -----------------------------------------------------------------------------
// Control & errors (unchanged surface)
// -----------------------------------------------------------------------------

type returnSig struct{ v Value }
type breakSig struct{ v Value }
type contSig struct{ v Value }
type rtErr struct{ msg string }

func fail(msg string)          { panic(rtErr{msg: msg}) }
func errNull(msg string) Value { return withAnnot(Null, msg) }
func annotNull(msg string) Value {
	return Value{Tag: VTNull, Annot: msg}
}

// -----------------------------------------------------------------------------
// Public engine interfaces (unchanged)
// -----------------------------------------------------------------------------

type ParamSpec struct {
	Name string
	Type S
}

type Callable interface {
	Arity() int
	ParamSpecs() []ParamSpec
	ReturnType() S
	Doc() string
	ClosureEnv() *Env
}

type CallCtx interface {
	Arg(name string) (Value, bool)
	MustArg(name string) Value
	Env() *Env
}

type NativeImpl func(ip *Interpreter, ctx CallCtx) Value

type Interpreter struct {
	Global    *Env
	Core      *Env
	modules   map[string]*moduleRec
	native    map[string]NativeImpl
	loadStack []string
}

func NewInterpreter() *Interpreter {
	ip := &Interpreter{}
	ip.Core = NewEnv(nil)
	ip.Global = NewEnv(ip.Core) // built-ins visible in program env
	ip.initCore()
	return ip
}

// -----------------------------------------------------------------------------
// Public API — thin wrappers around unified engine
// -----------------------------------------------------------------------------

func (ip *Interpreter) EvalSource(src string) (Value, error) {
	ast, err := ParseSExpr(src)
	if err != nil {
		return Value{}, WrapErrorWithSource(err, src)
	}
	return ip.Eval(ast)
}
func (ip *Interpreter) Eval(root S) (Value, error) {
	return ip.runTop(root, NewEnv(ip.Global), true, false)
}

func (ip *Interpreter) EvalPersistentSource(src string) (Value, error) {
	ast, err := ParseSExpr(src)
	if err != nil {
		return Value{}, WrapErrorWithSource(err, src)
	}
	return ip.EvalPersistent(ast)
}
func (ip *Interpreter) EvalPersistent(root S) (Value, error) {
	return ip.runTop(root, ip.Global, true, false)
}

func (ip *Interpreter) EvalAST(ast S, env *Env) (Value, error) {
	return ip.runTop(ast, env, false, false)
}
func (ip *Interpreter) EvalASTUncaught(ast S, env *Env, topBlockToSameEnv bool) Value {
	v, _ := ip.runTop(ast, env, topBlockToSameEnv, true)
	return v
}

// Apply/Call — preserved API; use unified scoped engine underneath.
func (ip *Interpreter) Apply(fn Value, args []Value) Value { return ip.applyArgsScoped(fn, args, nil) }
func (ip *Interpreter) Call0(fn Value) Value               { return ip.applyArgsScoped(fn, nil, nil) }
func (ip *Interpreter) FunMeta(fn Value) (Callable, bool) {
	if fn.Tag != VTFun {
		return nil, false
	}
	return &funCallable{f: fn.Data.(*Fun), doc: fn.Annot}, true
}
func (ip *Interpreter) ResolveType(t S, env *Env) S        { return ip.resolveType(t, env) }
func (ip *Interpreter) IsType(v Value, t S, env *Env) bool { return ip.isType(v, t, env) }
func (ip *Interpreter) IsSubtype(a, b S, env *Env) bool    { return ip.isSubtype(a, b, env) }
func (ip *Interpreter) UnifyTypes(a, b S, env *Env) S      { return ip.unifyTypes(a, b, env) }
func (ip *Interpreter) ValueToType(v Value, env *Env) S    { return ip.valueToTypeS(v, env) }
func withScope(parent, override *Env) *Env {
	if override != nil {
		return override
	}
	return parent
}
func isNumber(v Value) bool { return v.Tag == VTInt || v.Tag == VTNum }
func toFloat(v Value) float64 {
	if v.Tag == VTInt {
		return float64(v.Data.(int64))
	}
	return v.Data.(float64)
}

// -----------------------------------------------------------------------------
// Callable/CallCtx
// -----------------------------------------------------------------------------

type funCallable struct {
	f   *Fun
	doc string
}

func (c *funCallable) Arity() int { return len(c.f.Params) }
func (c *funCallable) ParamSpecs() []ParamSpec {
	ps := make([]ParamSpec, len(c.f.Params))
	for i := range c.f.Params {
		ps[i] = ParamSpec{Name: c.f.Params[i], Type: c.f.ParamTypes[i]}
	}
	return ps
}
func (c *funCallable) ReturnType() S    { return c.f.ReturnType }
func (c *funCallable) Doc() string      { return c.doc }
func (c *funCallable) ClosureEnv() *Env { return c.f.Env }

type callCtx struct {
	argEnv *Env // holds bound arguments
	scope  *Env // where side effects should land (program/call-site env)
}

func (c *callCtx) Arg(name string) (Value, bool) { v, err := c.argEnv.Get(name); return v, err == nil }
func (c *callCtx) MustArg(name string) Value {
	if v, ok := c.Arg(name); ok {
		return v
	}
	fail("missing argument: " + name)
	return Null
}
func (c *callCtx) Env() *Env { return c.scope }

// -----------------------------------------------------------------------------
// Unified VM entry/exit plumbing
// -----------------------------------------------------------------------------

func (ip *Interpreter) runTop(ast S, env *Env, topBlockToSameEnv bool, uncaught bool) (out Value, err error) {
	if !uncaught {
		defer func() {
			if r := recover(); r != nil {
				switch sig := r.(type) {
				case returnSig:
					out, err = sig.v, nil
				case breakSig:
					out, err = sig.v, nil
				case contSig:
					out, err = errNull("continue outside of loop"), nil
				case rtErr:
					out, err = errNull(sig.msg), nil
				case error:
					out, err = Null, sig
				default:
					out, err = errNull(fmt.Sprintf("runtime panic: %v", r)), nil
				}
			}
		}()
	}

	ch := ip.jitTop(ast, topBlockToSameEnv)
	res := ip.runChunk(ch, env, 0)
	switch res.status {
	case vmOK, vmReturn:
		return res.value, nil
	case vmBreak:
		return res.value, nil
	case vmContinue:
		return errNull("continue outside of loop"), nil
	case vmRuntimeError:
		return res.value, nil
	default:
		return errNull("unknown VM status"), nil
	}
}

// Build a one-off top-level function: optionally execute ("block", …) directly in env.
func (ip *Interpreter) jitTop(ast S, topBlockToSameEnv bool) *Chunk {
	f := &Fun{
		ReturnType: S{"id", "Any"},
		Body:       ast,
	}
	ip.ensureChunk(f, topBlockToSameEnv)
	return f.Chunk
}

func (ip *Interpreter) ensureChunk(f *Fun, topBlockToSameEnv bool) {
	if f.Chunk != nil || f.NativeName != "" {
		return
	}
	em := newEmitter(ip)
	em.topBlockToSameEnv = topBlockToSameEnv
	em.emitFunBody(f.Body)
	f.Chunk = em.chunk()
}

// -----------------------------------------------------------------------------
// Calls / currying — unified “scoped” engine used by public wrappers
// -----------------------------------------------------------------------------

func (ip *Interpreter) applyArgsScoped(fnVal Value, args []Value, callSite *Env) Value {
	if fnVal.Tag != VTFun {
		fail("not a function")
	}
	f := fnVal.Data.(*Fun)

	// Zero-arg application
	if len(args) == 0 {
		switch len(f.Params) {
		case 0:
			return ip.execFunBodyScoped(f, callSite)
		case 1:
			if ip.isType(Null, f.ParamTypes[0], f.Env) {
				return ip.applyOneScoped(fnVal, Null, callSite)
			}
			fail(fmt.Sprintf("arity mismatch: expected %d, got 0", len(f.Params)))
		default:
			fail(fmt.Sprintf("arity mismatch: expected %d, got 0", len(f.Params)))
		}
	}

	cur := fnVal
	for i := 0; i < len(args); i++ {
		cur = ip.applyOneScoped(cur, args[i], callSite)
	}
	return cur
}

func (ip *Interpreter) applyOneScoped(fnVal Value, arg Value, callSite *Env) Value {
	if fnVal.Tag != VTFun {
		fail("not a function")
	}
	f := fnVal.Data.(*Fun)

	// Already saturated → execute then keep applying (currying chains).
	if len(f.Params) == 0 {
		res := ip.execFunBodyScoped(f, callSite)
		if res.Tag != VTFun {
			fail("too many arguments")
		}
		return ip.applyOneScoped(res, arg, callSite)
	}

	// Type check against next parameter.
	paramName := f.Params[0]
	paramType := f.ParamTypes[0]
	if !ip.isType(arg, paramType, f.Env) {
		fail(fmt.Sprintf("type mismatch in parameter '%s'", paramName))
	}

	// Bind argument into a fresh call env. For natives, the binding/side effects
	// should land under the *call-site* env (program scope). For user functions,
	// keep closure semantics.
	parent := f.Env
	if f.NativeName != "" && callSite != nil {
		if f.Env == nil || f.Env == ip.Core {
			parent = callSite
		}
	}
	callEnv := NewEnv(parent)
	callEnv.Define(paramName, arg)

	// More params left → return partially-applied closure.
	if len(f.Params) > 1 {
		return FunVal(&Fun{
			Params:     append([]string{}, f.Params[1:]...),
			ParamTypes: append([]S{}, f.ParamTypes[1:]...),
			ReturnType: f.ReturnType,
			Body:       f.Body,
			Env:        callEnv,
			HiddenNull: f.HiddenNull,
			Chunk:      f.Chunk,
			NativeName: f.NativeName,
		})
	}

	// Last arg supplied → execute on VM.
	return ip.execFunBodyScoped(&Fun{
		ReturnType: f.ReturnType,
		Body:       f.Body,
		Env:        callEnv,
		Chunk:      f.Chunk,
		NativeName: f.NativeName,
	}, callSite)
}

func (ip *Interpreter) execFunBodyScoped(f *Fun, callSite *Env) Value {
	// Native fast path
	if f.NativeName != "" {
		impl, ok := ip.native[f.NativeName]
		if !ok {
			fail(fmt.Sprintf("unknown native %q", f.NativeName))
		}
		scope := withScope(f.Env, callSite) // where side effects land
		res := impl(ip, &callCtx{argEnv: f.Env, scope: scope})
		if !ip.isType(res, f.ReturnType, f.Env) {
			fail("return type mismatch")
		}
		return res
	}

	// User-defined function
	ip.ensureChunk(f, false)
	res := ip.runChunk(f.Chunk, f.Env, 0)
	switch res.status {
	case vmOK, vmReturn:
		if !ip.isType(res.value, f.ReturnType, f.Env) {
			fail("return type mismatch")
		}
		return res.value
	case vmBreak:
		panic(breakSig{res.value})
	case vmContinue:
		panic(contSig{res.value})
	case vmRuntimeError:
		return res.value
	default:
		return errNull("unknown VM status")
	}
}

// -----------------------------------------------------------------------------
// Internal natives (concise registration helpers + same semantics)
// -----------------------------------------------------------------------------

func (ip *Interpreter) RegisterNative(name string, params []ParamSpec, ret S, impl NativeImpl) {
	if ip.native == nil {
		ip.native = map[string]NativeImpl{}
	}
	ip.native[name] = impl

	if ip.Core == nil {
		ip.Core = NewEnv(nil)
	}
	names := make([]string, len(params))
	types := make([]S, len(params))
	for i, p := range params {
		names[i], types[i] = p.Name, p.Type
	}
	ip.Core.Define(name, FunVal(&Fun{
		Params:     names,
		ParamTypes: types,
		ReturnType: ret,
		Body:       S{"native", name},
		Env:        ip.Core,
		NativeName: name,
	}))
}

// tiny sugar to cut native boilerplate
func (ip *Interpreter) reg(name string, params []ParamSpec, ret S, body func(ctx CallCtx) Value) {
	ip.RegisterNative(name, params, ret, func(_ *Interpreter, ctx CallCtx) Value { return body(ctx) })
}

func (ip *Interpreter) initCore() {
	if ip.Core == nil {
		ip.Core = NewEnv(nil)
	}

	// __assign_set(target: Type, value: Any) -> Any
	ip.reg("__assign_set",
		[]ParamSpec{{"target", S{"id", "Type"}}, {"value", S{"id", "Any"}}},
		S{"id", "Any"},
		func(ctx CallCtx) Value {
			t := ctx.MustArg("target")
			v := ctx.MustArg("value")
			ip.assignTo(t.Data.(S), v, ctx.Env())
			return v
		})

	// __assign_def(target: Type, value: Any) -> Any
	ip.reg("__assign_def",
		[]ParamSpec{{"target", S{"id", "Type"}}, {"value", S{"id", "Any"}}},
		S{"id", "Any"},
		func(ctx CallCtx) Value {
			t := ctx.MustArg("target")
			v := ctx.MustArg("value")
			ip.assignTo(t.Data.(S), v, ctx.Env(), true)
			return v
		})

	// __plus (numbers/strings/arrays/maps)
	ip.reg("__plus",
		[]ParamSpec{{"a", S{"id", "Any"}}, {"b", S{"id", "Any"}}}, S{"id", "Any"},
		func(ctx CallCtx) Value {
			a, b := ctx.MustArg("a"), ctx.MustArg("b")
			// NOTE: annotations are meta; equality ignores .Annot on values and on map keys.
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

				// start with LHS order/content
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

				// overlay RHS values/annotations; append new keys in RHS order
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

	// resolve type at runtime
	ip.reg("__resolve_type",
		[]ParamSpec{{"t", S{"id", "Type"}}}, S{"id", "Type"},
		func(ctx CallCtx) Value {
			t := ctx.MustArg("t").Data.(S)
			return TypeVal(ip.resolveType(t, ctx.Env()))
		})

	// annotate a value
	ip.reg("__annotate",
		[]ParamSpec{{"text", S{"id", "Str"}}, {"v", S{"id", "Any"}}}, S{"id", "Any"},
		func(ctx CallCtx) Value {
			return withAnnot(ctx.MustArg("v"), ctx.MustArg("text").Data.(string))
		})

	// collect for-each elements
	ip.reg("__collect_for_elems",
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

	// map from key/value arrays (keys are Str; preserve annotations and insertion order)
	ip.reg("__map_from",
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

	// len(array|map)
	ip.reg("__len",
		[]ParamSpec{{"x", S{"id", "Any"}}}, S{"id", "Int"},
		func(ctx CallCtx) Value {
			x := ctx.MustArg("x")
			switch x.Tag {
			case VTArray:
				return Int(int64(len(x.Data.([]Value))))
			case VTMap:
				return Int(int64(len(x.Data.(*MapObject).Entries)))
			default:
				return errNull("len expects array or map")
			}
		})

	// make_fun(params:[str], types:[Type], ret:Type, body:Type) -> Fun
	ip.reg("__make_fun",
		[]ParamSpec{
			{"params", S{"array", S{"id", "Str"}}},
			{"types", S{"array", S{"id", "Type"}}},
			{"ret", S{"id", "Type"}},
			{"body", S{"id", "Type"}},
		},
		S{"id", "Any"},
		func(ctx CallCtx) Value {
			namesV := ctx.MustArg("params").Data.([]Value)
			typesV := ctx.MustArg("types").Data.([]Value)
			retT := ctx.MustArg("ret").Data.(S)
			body := ctx.MustArg("body").Data.(S)

			names := make([]string, len(namesV))
			types := make([]S, len(typesV))
			for i := range namesV {
				names[i] = namesV[i].Data.(string)
			}
			for i := range typesV {
				types[i] = typesV[i].Data.(S)
			}
			hidden := false
			if len(names) == 0 {
				names = []string{"_"}
				types = []S{S{"id", "Null"}}
				hidden = true
			}
			return FunVal(&Fun{
				Params:     names,
				ParamTypes: types,
				ReturnType: retT,
				Body:       body,
				Env:        ctx.Env(),
				HiddenNull: hidden,
			})
		})

	// is function?
	ip.reg("__is_fun",
		[]ParamSpec{{"x", S{"id", "Any"}}}, S{"id", "Bool"},
		func(ctx CallCtx) Value { return Bool(ctx.MustArg("x").Tag == VTFun) })

	// iterator control: stop on plain null; annotated-null is error
	ip.reg("__iter_should_stop",
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

	// __to_iter: coerce arrays/maps to iterators; pass through properly-shaped fun
	ip.RegisterNative("__to_iter",
		[]ParamSpec{{"x", S{"id", "Any"}}}, S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			x := ctx.MustArg("x")

			// Already iterator? must be Null -> Any?
			if x.Tag == VTFun {
				f := x.Data.(*Fun)
				if len(f.Params) == 1 && ip.isType(Null, f.ParamTypes[0], f.Env) {
					return x
				}
				return annotNull("for expects array, map, or iterator function (Null -> Any?)")
			}

			// Array → iterator
			if x.Tag == VTArray {
				env := NewEnv(ctx.Env())
				env.Define("$arr", x)
				env.Define("$i", Int(0))

				body := S{"if",
					S{"pair",
						S{"binop", "<",
							S{"id", "$i"},
							S{"call", S{"id", "__len"}, S{"id", "$arr"}},
						},
						S{"block",
							S{"assign", S{"id", "$i"},
								S{"binop", "+", S{"id", "$i"}, S{"int", int64(1)}},
							},
							S{"idx",
								S{"id", "$arr"},
								S{"binop", "-", S{"id", "$i"}, S{"int", int64(1)}},
							},
						},
					},
					S{"block", S{"null"}},
				}

				return FunVal(&Fun{
					Params:     []string{"_"},
					ParamTypes: []S{S{"id", "Null"}},
					ReturnType: S{"unop", "?", S{"id", "Any"}},
					Body:       body,
					Env:        env,
				})
			}

			// Map → iterator (yields [key, value]) preserving insertion order;
			// keys are Str values that carry their annotation (if any).
			if x.Tag == VTMap {
				mo := x.Data.(*MapObject)

				env := NewEnv(ctx.Env())
				env.Define("$map", x)
				// Precompute ordered keys as Values (with annotations)
				keyVals := make([]Value, 0, len(mo.Keys))
				for _, k := range mo.Keys {
					s := Str(k)
					if ann, ok := mo.KeyAnn[k]; ok && ann != "" {
						s = withAnnot(s, ann)
					}
					keyVals = append(keyVals, s)
				}
				env.Define("$keys", Arr(keyVals))
				env.Define("$i", Int(0))

				body := S{"if",
					S{"pair",
						S{"binop", "<",
							S{"id", "$i"},
							S{"call", S{"id", "__len"}, S{"id", "$keys"}},
						},
						S{"block",
							// k := keys[i]
							S{"assign", S{"decl", "$k"},
								S{"idx", S{"id", "$keys"}, S{"id", "$i"}},
							},
							// i := i + 1
							S{"assign", S{"id", "$i"},
								S{"binop", "+", S{"id", "$i"}, S{"int", int64(1)}},
							},
							// [k, map[k]]
							S{"array",
								S{"id", "$k"},
								S{"idx", S{"id", "$map"}, S{"id", "$k"}},
							},
						},
					},
					S{"block", S{"null"}},
				}

				return FunVal(&Fun{
					Params:     []string{"_"},
					ParamTypes: []S{S{"id", "Null"}},
					ReturnType: S{"unop", "?", S{"id", "Any"}},
					Body:       body,
					Env:        env,
				})
			}

			return annotNull("for expects array, map, or iterator function (Null -> Any?)")
		})
}

// -----------------------------------------------------------------------------
// Deep equal & helpers (unchanged semantics)
// -----------------------------------------------------------------------------

func (ip *Interpreter) deepEqual(a, b Value) bool {
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
		return a.Data.(string) == b.Data.(string) // ignore a.Annot / b.Annot
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
		return equalS(a.Data.(S), b.Data.(S))
	default:
		return false
	}
}

// assignTo (unchanged error texts & semantics).
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
		obj := ip.evalSimple(target[1].(S), env)
		var keyStr string
		switch target[2].(S)[0].(string) {
		case "id", "str":
			keyStr = target[2].(S)[1].(string)
		default:
			k := ip.evalSimple(target[2].(S), env)
			if k.Tag != VTStr {
				fail("object assignment requires map and string key")
			}
			keyStr = k.Data.(string)
		}
		if obj.Tag == VTMap {
			mo := obj.Data.(*MapObject)
			if _, exists := mo.Entries[keyStr]; !exists {
				mo.Keys = append(mo.Keys, keyStr)
			}
			mo.Entries[keyStr] = value
			return
		}
		if obj.Tag == VTModule {
			fail("cannot assign to module exports")
		}
		fail("object assignment requires map and string key")
	case "idx":
		obj, idx := ip.evalSimple(target[1].(S), env), ip.evalSimple(target[2].(S), env)
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
		fail("index assignment requires array[int]")
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
		if value.Tag != VTMap {
			for i := 1; i < len(target); i++ {
				p := target[i].(S) // ("pair", key, pattern)
				ip.assignTo(p[2].(S), annotNull("object pattern: RHS is not a map"), env, true)
			}
			return
		}
		mo := value.Data.(*MapObject)
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

// evalSimple: minimal subset used by assignTo.
func (ip *Interpreter) evalSimple(n S, env *Env) Value {
	switch n[0].(string) {
	case "id":
		v, err := env.Get(n[1].(string))
		if err != nil {
			fail(err.Error())
		}
		return v
	case "str":
		return Str(n[1].(string))
	case "int":
		return Int(n[1].(int64))
	case "num":
		return Num(n[1].(float64))
	case "bool":
		return Bool(n[1].(bool))
	case "null":
		return Null
	default:
		fail("unsupported simple eval")
		return Null
	}
}

// Iterator expansion (single scoped implementation).
func (ip *Interpreter) collectForElemsScoped(iter Value, scope *Env) []Value {
	switch iter.Tag {
	case VTArray:
		return append([]Value(nil), iter.Data.([]Value)...)
	case VTMap:
		m := iter.Data.(map[string]Value)
		out := make([]Value, 0, len(m))
		for k, v := range m {
			out = append(out, Arr([]Value{Str(k), v}))
		}
		return out
	case VTFun:
		f := iter.Data.(*Fun)
		if len(f.Params) != 1 || !ip.isType(Null, f.ParamTypes[0], f.Env) {
			name := "_"
			if len(f.Params) > 0 {
				name = f.Params[0]
			}
			fail(fmt.Sprintf("type mismatch in parameter '%s'", name))
		}
		out := []Value{}
		for {
			next := ip.applyArgsScoped(iter, []Value{Null}, scope)
			if next.Tag == VTNull {
				if next.Annot != "" {
					fail(next.Annot)
				}
				break
			}
			out = append(out, next)
		}
		return out
	default:
		fail("for expects array, map, or iterator function (Null -> Any)")
		return nil
	}
}

// -----------------------------------------------------------------------------
// JIT compiler (emitter helpers to reduce boilerplate)
// -----------------------------------------------------------------------------

type emitter struct {
	ip                *Interpreter
	code              []uint32
	consts            []Value
	topBlockToSameEnv bool
	loopStack         []loopLbls
}

type loopLbls struct {
	head       int
	contTarget int
	breakJumps []int
	contJumps  []int
}

func newEmitter(ip *Interpreter) *emitter { return &emitter{ip: ip} }
func (e *emitter) k(v Value) uint32 {
	for i := range e.consts {
		if e.ip.deepEqual(e.consts[i], v) {
			return uint32(i)
		}
	}
	e.consts = append(e.consts, v)
	return uint32(len(e.consts) - 1)
}
func (e *emitter) ks(s string) uint32         { return e.k(Str(s)) }
func (e *emitter) emit(op opcode, imm uint32) { e.code = append(e.code, pack(op, imm)) }
func (e *emitter) patch(at int, to int)       { e.code[at] = pack(uop(e.code[at]), uint32(to)) }
func (e *emitter) here() int                  { return len(e.code) }
func (e *emitter) chunk() *Chunk              { return &Chunk{Code: e.code, Consts: e.consts} }

// small helpers
func (e *emitter) callBuiltin(name string, args ...S) {
	e.emit(opLoadGlobal, e.ks(name))
	for _, a := range args {
		e.emitExpr(a)
	}
	e.emit(opCall, uint32(len(args)))
}

// unwrapKeyStr returns the string name for a map key S-node.
// Accepts ("str", name) or ("annot", ("str", text), <key>), recursively.
func unwrapKeyStr(k S) string {
	for len(k) > 0 && k[0].(string) == "annot" {
		k = k[2].(S) // skip to the wrapped key
	}
	// Parser emits keys as ("str", name).
	if len(k) >= 2 && k[0].(string) == "str" {
		return k[1].(string)
	}
	fail("map key is not a string")
	return ""
}

// Entry
func (e *emitter) emitFunBody(body S) {
	e.emitExpr(body)
	e.emit(opReturn, 0)
}

// ---- Expressions ----

func (e *emitter) emitExpr(n S) {
	if len(n) == 0 {
		e.emit(opConst, e.k(Null))
		return
	}
	switch n[0].(string) {

	// literals
	case "int":
		e.emit(opConst, e.k(Int(n[1].(int64))))
	case "num":
		e.emit(opConst, e.k(Num(n[1].(float64))))
	case "str":
		e.emit(opConst, e.k(Str(n[1].(string))))
	case "bool":
		e.emit(opConst, e.k(Bool(n[1].(bool))))
	case "null":
		e.emit(opConst, e.k(Null))

	case "id":
		e.emit(opLoadGlobal, e.ks(n[1].(string)))

	case "block":
		for i := 1; i < len(n); i++ {
			e.emitExpr(n[i].(S))
		}

	// unary
	case "unop":
		op := n[1].(string)
		if op == "?" { // type-only; signal runtime error if appears in terms
			e.emit(opConst, e.k(errNull("postfix '?' invalid here")))
			return
		}
		e.emitExpr(n[2].(S))
		switch op {
		case "not":
			e.emit(opNot, 0)
		case "-":
			e.emit(opNeg, 0)
		default:
			e.emit(opConst, e.k(errNull("unknown unary op")))
		}

	// binary (short-circuit for and/or; others via helpers)
	case "binop":
		op := n[1].(string)
		if op == "and" || op == "or" {
			e.emitExpr(n[2].(S))
			if op == "and" {
				jf := e.here()
				e.emit(opJumpIfFalse, 0)
				e.emitExpr(n[3].(S))
				jend := e.here()
				e.emit(opJump, 0)
				lfalse := e.here()
				e.emit(opConst, e.k(Bool(false)))
				lend := e.here()
				e.patch(jf, lfalse)
				e.patch(jend, lend)
			} else {
				jf := e.here()
				e.emit(opJumpIfFalse, 0)
				e.emit(opConst, e.k(Bool(true)))
				jend := e.here()
				e.emit(opJump, 0)
				lrhs := e.here()
				e.patch(jf, lrhs)
				e.emitExpr(n[3].(S))
				lend := e.here()
				e.patch(jend, lend)
			}
			return
		}
		a, b := n[2].(S), n[3].(S)
		switch op {
		case "==":
			e.emitExpr(a)
			e.emitExpr(b)
			e.emit(opEq, 0)
		case "!=":
			e.emitExpr(a)
			e.emitExpr(b)
			e.emit(opNe, 0)
		case "+":
			e.callBuiltin("__plus", a, b)
		case "-":
			e.emitExpr(a)
			e.emitExpr(b)
			e.emit(opSub, 0)
		case "*":
			e.emitExpr(a)
			e.emitExpr(b)
			e.emit(opMul, 0)
		case "/":
			e.emitExpr(a)
			e.emitExpr(b)
			e.emit(opDiv, 0)
		case "%":
			e.emitExpr(a)
			e.emitExpr(b)
			e.emit(opMod, 0)
		case "<":
			e.emitExpr(a)
			e.emitExpr(b)
			e.emit(opLt, 0)
		case "<=":
			e.emitExpr(a)
			e.emitExpr(b)
			e.emit(opLe, 0)
		case ">":
			e.emitExpr(a)
			e.emitExpr(b)
			e.emit(opGt, 0)
		case ">=":
			e.emitExpr(a)
			e.emitExpr(b)
			e.emit(opGe, 0)
		default:
			e.emit(opConst, e.k(errNull("unsupported operator")))
		}

	case "assign":
		lhs := n[1].(S)
		opName := "__assign_set"
		switch lhs[0].(string) {
		case "decl", "darr", "dobj", "annot":
			opName = "__assign_def"
		}
		e.emit(opLoadGlobal, e.ks(opName))
		e.emit(opConst, e.k(TypeVal(lhs)))
		e.emitExpr(n[2].(S))
		e.emit(opCall, 2)

	case "decl": // let x → define null
		e.callBuiltin("__assign_def", S{"type", n}, S{"null"})

	// arrays & maps
	case "array":
		for i := 1; i < len(n); i++ {
			e.emitExpr(n[i].(S))
		}
		e.emit(opMakeArr, uint32(len(n)-1))

	case "map":
		// Build keys/vals arrays; for keys, emit the key node as-is
		// (can be "str" or "annot(..., str)"), so annotations become part of the Str Value.
		keys := S{"array"}
		vals := S{"array"}
		for i := 1; i < len(n); i++ {
			p := n[i].(S)       // ("pair"|"pair!", keyNode, valueExpr)
			keyNode := p[1].(S) // "str" or "annot"
			valExpr := p[2].(S) // value expr
			keys = append(keys, keyNode)
			vals = append(vals, valExpr)
		}
		// call __map_from(keys, vals)
		e.emit(opLoadGlobal, e.ks("__map_from"))
		for i := 1; i < len(keys); i++ {
			e.emitExpr(keys[i].(S))
		}
		e.emit(opMakeArr, uint32(len(keys)-1))
		for i := 1; i < len(vals); i++ {
			e.emitExpr(vals[i].(S))
		}
		e.emit(opMakeArr, uint32(len(vals)-1))
		e.emit(opCall, 2)

	// property / index
	case "get":
		e.emitExpr(n[1].(S))
		e.emit(opGetProp, e.ks(n[2].(S)[1].(string)))
	case "idx":
		e.emitExpr(n[1].(S))
		e.emitExpr(n[2].(S))
		e.emit(opGetIdx, 0)

	// calls
	case "call":
		e.emitExpr(n[1].(S))
		for i := 2; i < len(n); i++ {
			e.emitExpr(n[i].(S))
		}
		e.emit(opCall, uint32(len(n)-2))

	case "fun":
		params := n[1].(S)
		namesArr := make([]Value, 0, len(params)-1)
		typesArr := make([]Value, 0, len(params)-1)
		for i := 1; i < len(params); i++ {
			p := params[i].(S)
			namesArr = append(namesArr, Str(p[1].(S)[1].(string)))
			t := p[2].(S)
			if len(t) == 0 {
				t = S{"id", "Any"}
			}
			typesArr = append(typesArr, TypeVal(t))
		}
		retT := n[2].(S)
		if len(retT) == 0 {
			retT = S{"id", "Any"}
		}

		e.emit(opLoadGlobal, e.ks("__make_fun"))
		for _, v := range namesArr {
			e.emit(opConst, e.k(v))
		}
		e.emit(opMakeArr, uint32(len(namesArr)))
		for _, v := range typesArr {
			e.emit(opConst, e.k(v))
		}
		e.emit(opMakeArr, uint32(len(typesArr)))
		e.emit(opConst, e.k(TypeVal(retT)))
		e.emit(opConst, e.k(TypeVal(n[3].(S))))
		e.emit(opCall, 4)

	// control
	case "return":
		e.emitExpr(n[1].(S))
		e.emit(opReturn, 0)
	case "break":
		e.emitExpr(n[1].(S))
		if len(e.loopStack) == 0 {
			e.emit(opBreak, 0)
		} else {
			i := len(e.loopStack) - 1
			lbl := e.loopStack[i]
			at := e.here()
			e.emit(opJump, 0)
			lbl.breakJumps = append(lbl.breakJumps, at)
			e.loopStack[i] = lbl
		}
	case "continue":
		e.emitExpr(n[1].(S))
		if len(e.loopStack) == 0 {
			e.emit(opContinue, 0)
		} else {
			i := len(e.loopStack) - 1
			lbl := e.loopStack[i]
			if lbl.contTarget >= 0 {
				e.emit(opJump, uint32(lbl.contTarget))
			} else {
				at := e.here()
				e.emit(opJump, 0)
				lbl.contJumps = append(lbl.contJumps, at)
				e.loopStack[i] = lbl
			}
		}

	// if/elif/else
	case "if":
		arms := n[1:]
		jends := []int{}
		hasElse := false
		if len(arms) > 0 {
			if last, ok := arms[len(arms)-1].(S); ok && last[0].(string) == "block" {
				hasElse = true
			}
		}
		limit := len(arms)
		if hasElse {
			limit--
		}
		for i := 0; i < limit; i++ {
			p := arms[i].(S)
			e.emitExpr(p[1].(S))
			jf := e.here()
			e.emit(opJumpIfFalse, 0)
			e.emitExpr(p[2].(S))
			jend := e.here()
			e.emit(opJump, 0)
			jends = append(jends, jend)
			e.patch(jf, e.here())
		}
		if hasElse {
			e.emitExpr(arms[len(arms)-1].(S))
		} else {
			e.emit(opConst, e.k(Null))
		}
		tail := e.here()
		for _, at := range jends {
			e.patch(at, tail)
		}

	// while
	case "while":
		cond := n[1].(S)
		body := n[2].(S)
		head := e.here()
		e.emitExpr(cond)
		jf := e.here()
		e.emit(opJumpIfFalse, 0)

		l := loopLbls{head: head, contTarget: head}
		e.loopStack = append(e.loopStack, l)

		e.emitExpr(body)
		e.emit(opJump, uint32(head))

		end := e.here()
		e.patch(jf, end)
		i := len(e.loopStack) - 1
		lbl := e.loopStack[i]
		for _, at := range lbl.breakJumps {
			e.patch(at, end)
		}
		e.loopStack = e.loopStack[:i]

	// for (iterator lowering)
	case "for":
		target := n[1].(S)
		iterExpr := n[2].(S)
		body := n[3].(S)

		iterName := fmt.Sprintf("$iter_%d", e.here())
		e.callBuiltin("__assign_def", S{"type", S{"decl", iterName}},
			S{"call", S{"id", "__to_iter"}, iterExpr})

		tmpName := fmt.Sprintf("$tmp_%d", e.here())
		e.callBuiltin("__assign_def", S{"type", S{"decl", tmpName}}, S{"null"})

		head := e.here()

		// tmp = iter(null)
		e.emit(opLoadGlobal, e.ks("__assign_set"))
		e.emit(opConst, e.k(TypeVal(S{"id", tmpName})))
		e.emit(opLoadGlobal, e.ks(iterName))
		e.emit(opConst, e.k(Null))
		e.emit(opCall, 1)
		e.emit(opCall, 2)

		// stop?
		e.emit(opLoadGlobal, e.ks("__iter_should_stop"))
		e.emit(opLoadGlobal, e.ks(tmpName))
		e.emit(opCall, 1)
		jBody := e.here()
		e.emit(opJumpIfFalse, 0)
		jEnd := e.here()
		e.emit(opJump, 0)

		bodyStart := e.here()
		e.patch(jBody, bodyStart)

		assignName := "__assign_set"
		switch target[0].(string) {
		case "decl", "darr", "dobj", "annot":
			assignName = "__assign_def"
		}
		e.callBuiltin(assignName, S{"type", target}, S{"id", tmpName})

		lbl := loopLbls{head: head, contTarget: head}
		e.loopStack = append(e.loopStack, lbl)

		e.emitExpr(body)
		e.emit(opJump, uint32(head))

		end := e.here()
		e.patch(jEnd, end)
		i := len(e.loopStack) - 1
		lrec := e.loopStack[i]
		for _, at := range lrec.breakJumps {
			e.patch(at, end)
		}
		e.loopStack = e.loopStack[:i]

		e.emit(opConst, e.k(Null)) // for-expression value is null

	// type / annotation
	case "type":
		e.emit(opConst, e.k(TypeVal(n[1].(S))))
	case "annot":
		e.callBuiltin("__annotate", S{"str", n[1].(S)[1].(string)}, n[2].(S))

	default:
		e.emit(opConst, e.k(errNull("unknown AST tag")))
	}
}
