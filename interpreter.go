// interpreter.go: JIT-compiled interpreter + VM runner for MindScript.
//
// OVERVIEW
// --------
// This file implements the execution engine for MindScript in two phases:
//  1. **Compilation** — S-expression AST (see parser.go) is lowered to bytecode
//     (a *Chunk) by a small emitter.
//  2. **Execution**   — the resulting bytecode runs on a simple stack VM
//     (see vm.go), producing a runtime Value.
//
// The **public API** is intentionally stable and compact:
//   - Runtime value model: Value/ValueTag and helpers (Bool, Int, Num, Str, Arr, Map).
//   - Environments: Env with lexical scoping (Define/Set/Get).
//   - Functions/closures: Fun and FunVal, plus Callable/CallCtx introspection.
//   - The Interpreter: construction (NewInterpreter / NewInterpreterWithOptions),
//     evaluation entry points (Eval*/Eval*Persistent/EvalAST), native registration
//     (RegisterNative), call helpers (Apply/Call0), and type helpers
//     (ResolveType/IsType/…).
//
// NEW (RUNTIME ERROR REPORTING)
// -----------------------------
// When enabled via Options.RuntimeErrorsAsGoError, failures that reach runTop
// (VM runtime errors or panic(rtErr)) are surfaced as **Go errors** rendered by
// WrapErrorWithSource using **RUNTIME ERROR** headers and caret snippets.
// This is implemented by carrying a SourceRef on compiled chunks and recording
// PC→AST NodePath marks in the emitter. On failure, runTop resolves PC→Span→(line,col)
// and returns a wrapped *RuntimeError. Legacy behavior (annotated VTNull “soft
// errors”) remains available when the option is disabled.
//
// INTERNALS (PRIVATE SECTION BELOW)
// ---------------------------------
//   - A JIT emitter compiles S-expr nodes to bytecode (Chunk).
//   - A VM entry (runTopWithSource) runs chunks and converts failures into either
//     caret-style runtime errors (new) or annotated-null Values (legacy), based
//     on options.
//   - Unified “scoped” call engine supports currying and native calls.
//   - Core natives (__assign_set/def, __plus, __len, __map_from, __to_iter, …)
//     are registered in initCore().
//   - Assignment semantics (destructuring, object/array) are centralized in assignTo.
//
// DEPENDENCIES ON OTHER FILES
// ---------------------------
// • lexer.go / parser.go
//   - ParseSExpr / ParseSExprInteractive / ParseSExprWithSpans to build S-expr AST.
//   - Tokenization rules and source positions used for error reporting upstream.
//
// • vm.go (bytecode & virtual machine)
//   - Chunk { Code []uint32, Consts []Value, Src *SourceRef, Marks []PCMark }
//   - opcode packing/unpacking and enum (opConst, opLoadGlobal, opCall, …)
//   - runChunk(*Chunk, *Env, gas) -> { status vmOK|vmReturn|vmRuntimeError, value Value, pc int }
//
// • types.go (type system)
//   - (ip *Interpreter) resolveType / isType / isSubtype / unifyTypes / valueToTypeS
//   - ensureNullableUnlessAny(t S) S              (used for oracle return types)
//   - equalS(a, b S) bool                         (structural type equality)
//
// • errors.go (error reporting)
//   - WrapErrorWithSource(err, src string) error  (enrich lexer/parser/runtime errors)
//   - type RuntimeError { Line, Col int; Msg string } (rendered as caret snippet)
//
// • oracles.go (oracle implementation)
//   - (ip *Interpreter) execOracle(funVal Value, callSite *Env) Value
//
// The PUBLIC API is documented exhaustively below, so consumers need not read
// the private implementation to understand behavior.
package mindscript

import (
	"fmt"
	"strconv"
)

////////////////////////////////////////////////////////////////////////////////
//                                   PUBLIC API
////////////////////////////////////////////////////////////////////////////////

// ValueTag enumerates all runtime kinds a Value may hold.
type ValueTag int

const (
	VTNull   ValueTag = iota // null value (may carry an annotation for error reporting)
	VTBool                   // Go bool in Data
	VTInt                    // Go int64 in Data
	VTNum                    // Go float64 in Data
	VTStr                    // Go string in Data (Annotations may be used as metadata)
	VTArray                  // []Value
	VTMap                    // *MapObject (ordered map with key annotations)
	VTFun                    // *Fun (closure, may be native or user-defined)
	VTType                   // *TypeValue (type AST + definition env)
	VTModule                 // module handle (opaque)
	VTHandle                 // opaque handle for host integrations
)

// Value is the universal runtime carrier.
//
//	Tag   — discriminant
//	Data  — Go value appropriate for Tag (see ValueTag)
//	Annot — optional annotation used for doc/error propagation; never affects equality
type Value struct {
	Tag   ValueTag
	Data  interface{}
	Annot string
}

// String renders a human-friendly debug representation; annotations are not shown.
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

// Null is the singleton null Value (no annotation).
var Null = Value{Tag: VTNull}

// Constructors for primitive runtime Values.
func Bool(b bool) Value    { return Value{Tag: VTBool, Data: b} }
func Int(n int64) Value    { return Value{Tag: VTInt, Data: n} }
func Num(f float64) Value  { return Value{Tag: VTNum, Data: f} }
func Str(s string) Value   { return Value{Tag: VTStr, Data: s} }
func Arr(xs []Value) Value { return Value{Tag: VTArray, Data: xs} }

// MapObject is an ordered map that preserves insertion order and per-key annotations.
//
//	Entries — key/value storage
//	KeyAnn  — key → annotation text (if any), preserved in round-trips
//	Keys    — insertion order (unique keys)
//
// The Value for a Map is Value{Tag: VTMap, Data: *MapObject}.
type MapObject struct {
	Entries map[string]Value
	KeyAnn  map[string]string
	Keys    []string
}

// Map constructs a VTMap from a plain Go map. Literal maps (from source) should
// be constructed via the runtime native __map_from to preserve key order exactly.
func Map(m map[string]Value) Value {
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

// TypeValue carries a type AST (S) and the lexical Env where it was defined.
// This allows subsequent resolution under the correct scope.
type TypeValue struct {
	Ast S
	Env *Env
}

// TypeVal builds a VTType from a type expression AST.
func TypeVal(expr S) Value { return Value{Tag: VTType, Data: &TypeValue{Ast: expr}} }

// TypeValIn builds a VTType and pins its resolution environment.
func TypeValIn(expr S, env *Env) Value {
	return Value{Tag: VTType, Data: &TypeValue{Ast: expr, Env: env}}
}

// Fun represents a function/closure.
//
//	Params      — parameter names in order
//	Body        — S-expr body
//	Env         — closure environment (where free vars resolve)
//	ParamTypes  — declared parameter types (S)
//	ReturnType  — declared return type (S). Oracles are automatically made nullable.
//	HiddenNull  — internal: zero-arg placeholder; not part of public semantics
//
//	Chunk       — compiled bytecode (set on-demand)
//	NativeName  — non-empty if implemented by a registered native
//
//	IsOracle    — marks oracle functions (different return-type semantics)
//	Examples    — optional example pairs for tooling (opaque to runtime)
//
//	Src         — source metadata for caret-runtime-errors (optional)
type Fun struct {
	Params     []string
	Body       S
	Env        *Env
	ParamTypes []S
	ReturnType S
	HiddenNull bool

	Chunk      *Chunk // JIT result (from vm.go)
	NativeName string // non-empty for natives

	IsOracle bool    // oracle marker
	Examples []Value // optional examples

	Src *SourceRef // where this function's body came from (may be nil)
}

// FunVal wraps *Fun into a Value.
func FunVal(f *Fun) Value { return Value{Tag: VTFun, Data: f} }

// Env is a lexical environment with parent link.
//
// Lookup flows parent-ward; Define creates/overwrites in the current frame;
// Set mutates an existing binding (nearest defining frame) or errors.
type Env struct {
	parent *Env
	table  map[string]Value
}

// NewEnv creates a new lexical frame.
func NewEnv(parent *Env) *Env { return &Env{parent: parent, table: make(map[string]Value)} }

// Define binds name to v in the current frame (shadowing any outer binding).
func (e *Env) Define(name string, v Value) { e.table[name] = v }

// Set updates the nearest existing binding of name to v.
// If name does not exist in any visible scope, returns an error.
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

// Get retrieves the nearest visible binding for name or returns an error.
func (e *Env) Get(name string) (Value, error) {
	if v, ok := e.table[name]; ok {
		return v, nil
	}
	if e.parent != nil {
		return e.parent.Get(name)
	}
	return Value{}, fmt.Errorf("undefined variable: %s", name)
}

// ParamSpec documents a function parameter (name + declared type).
type ParamSpec struct {
	Name string
	Type S
}

// Callable exposes metadata about a function Value.
type Callable interface {
	Arity() int
	ParamSpecs() []ParamSpec
	ReturnType() S
	Doc() string
	ClosureEnv() *Env
}

// CallCtx is passed to native functions to access arguments and the effect scope.
//
//	Arg(name) / MustArg(name) — retrieve bound argument values
//	Env() — environment where side effects should land (call-site/program scope)
type CallCtx interface {
	Arg(name string) (Value, bool)
	MustArg(name string) Value
	Env() *Env
}

// NativeImpl is the implementation signature for registered host/native functions.
//
// A native receives the interpreter (for re-entrancy if needed) and a CallCtx.
// It must return a Value of the declared return type; the engine enforces types.
type NativeImpl func(ip *Interpreter, ctx CallCtx) Value

// Options configures interpreter behavior.
type Options struct {
	// If true, runtime failures at the top level are reported as Go errors
	// using caret snippets (RUNTIME ERROR). If false (default), the legacy
	// annotated-null “soft errors” are returned.
	RuntimeErrorsAsGoError bool
}

// RuntimeError represents an execution-time failure with a source location.
// Line/Col are 1-based.
type RuntimeError struct {
	Line int
	Col  int
	Msg  string
}

func (e *RuntimeError) Error() string {
	return fmt.Sprintf("RUNTIME ERROR at %d:%d: %s", e.Line, e.Col, e.Msg)
}

// Interpreter owns global/core environments, registered natives, and module state.
// Construct with NewInterpreter or NewInterpreterWithOptions. Most users interact
// via Eval*/Apply/Call0, etc.
//
// Fields for consumers:
//
//	Core   — built-in, shared environment; parent of Global
//	Global — program-global environment (persistent across EvalPersistent calls)
type Interpreter struct {
	Global    *Env
	Core      *Env
	modules   map[string]*moduleRec // private module system (not shown)
	native    map[string]NativeImpl // registered natives
	loadStack []string              // import guard (not shown)

	// Oracle observability (reserved for tooling)
	oracleLastPrompt string

	// Options / current source
	runtimeErrorsAsGoError bool
	currentSrc             *SourceRef // set while running a chunk; visible to natives
}

// NewInterpreter constructs an engine with defaults.
func NewInterpreter() *Interpreter {
	return NewInterpreterWithOptions(Options{})
}

// NewInterpreterWithOptions constructs an engine with options and core natives.
func NewInterpreterWithOptions(o Options) *Interpreter {
	ip := &Interpreter{runtimeErrorsAsGoError: o.RuntimeErrorsAsGoError}
	ip.Core = NewEnv(nil)
	ip.Global = NewEnv(ip.Core) // built-ins visible from user programs
	ip.modules = map[string]*moduleRec{}
	ip.runtimeErrorsAsGoError = true
	ip.initCore()
	return ip
}

// EvalSource parses (via ParseSExprWithSpans) and evaluates source in a fresh child
// of Global. The result does not mutate Global scope.
func (ip *Interpreter) EvalSource(src string) (Value, error) {
	ast, spans, err := ParseSExprWithSpans(src)
	if err != nil {
		return Value{}, WrapErrorWithSource(err, src)
	}
	return ip.runTopWithSource(ast, NewEnv(ip.Global), false, &SourceRef{Name: "main", Src: src, Spans: spans})
}

// Eval evaluates an AST in a fresh child of Global. Global is not mutated by
// top-level bindings (they go into the fresh child), unless the program itself
// mutates Global via explicit assignment.
func (ip *Interpreter) Eval(root S) (Value, error) {
	return ip.runTopWithSource(root, NewEnv(ip.Global), false, nil)
}

// EvalPersistentSource parses and evaluates source **in Global**.
// This is intended for REPLs or scripts that add to the global scope.
func (ip *Interpreter) EvalPersistentSource(src string) (Value, error) {
	ast, spans, err := ParseSExprWithSpans(src)
	if err != nil {
		return Value{}, WrapErrorWithSource(err, src)
	}
	return ip.runTopWithSource(ast, ip.Global, false, &SourceRef{Name: "main", Src: src, Spans: spans})
}

// EvalPersistent evaluates an AST **in Global**.
func (ip *Interpreter) EvalPersistent(root S) (Value, error) {
	return ip.runTopWithSource(root, ip.Global, false, nil)
}

// EvalAST evaluates an AST in the provided environment.
func (ip *Interpreter) EvalAST(ast S, env *Env) (Value, error) {
	return ip.runTopWithSource(ast, env, false, nil)
}

// EvalASTUncaught evaluates an AST and **never returns an error**.
// Runtime failures are returned as annotated null Values; this mirrors legacy
// behavior in some integrations. The topBlockToSameEnv flag is kept for API
// compatibility and is ignored.
func (ip *Interpreter) EvalASTUncaught(ast S, env *Env, topBlockToSameEnv bool) Value {
	v, _ := ip.runTopWithSource(ast, env, true, nil)
	return v
}

// Apply applies a function Value to a list of arguments with correct scoping,
// type-checking, and currying semantics.
//
// If fewer arguments than parameters are provided, a partially-applied Fun is
// returned. If more are provided, arguments are applied in sequence (currying
// chain) to the results.
func (ip *Interpreter) Apply(fn Value, args []Value) Value {
	return ip.applyArgsScoped(fn, args, nil)
}

// Call0 invokes a function with zero arguments (shortcut for Apply(fn, nil)).
func (ip *Interpreter) Call0(fn Value) Value { return ip.applyArgsScoped(fn, nil, nil) }

// FunMeta exposes a function Value as a Callable (arity/param specs/return type
// and closure env). The returned doc string is taken from v.Annot.
func (ip *Interpreter) FunMeta(fn Value) (Callable, bool) {
	if fn.Tag != VTFun {
		return nil, false
	}
	return &funCallable{f: fn.Data.(*Fun), doc: fn.Annot}, true
}

// ResolveType expands a type expression by resolving identifiers bound to
// user-defined types in the provided environment. Implementation lives in
// types.go (resolveType). See types.go header for full semantics.
func (ip *Interpreter) ResolveType(t S, env *Env) S { return ip.resolveType(t, env) }

// IsType reports whether runtime value v conforms to type t. Delegates to
// types.go (isType). Handles Int <: Num, optionals (T?), arrays, open-world
// maps with required/optional fields, enums by literal equality, and functions
// via structural subtyping.
func (ip *Interpreter) IsType(v Value, t S, env *Env) bool { return ip.isType(v, t, env) }

// IsSubtype reports whether a is a structural subtype of b. Delegates to
// types.go (isSubtype). Arrays are covariant; function params contravariant
// and returns covariant; maps must preserve requiredness; enums use set
// inclusion; Any is top; Null interacts with T? as expected.
func (ip *Interpreter) IsSubtype(a, b S, env *Env) bool { return ip.isSubtype(a, b, env) }

// UnifyTypes computes a least common supertype (LUB) of t1 and t2. Delegates
// to types.go (unifyTypes). Handles Any absorption, null/nullable normalization,
// Int ⊔ Num = Num, arrays element-wise, maps field-wise with requiredness=OR,
// enums by set union (or Type if all members fit), and function param GLB /
// return LUB.
func (ip *Interpreter) UnifyTypes(t1 S, t2 S, env *Env) S { return ip.unifyTypes(t1, t2, env) }

// ValueToType infers a pragmatic structural type for v (JSON-friendly). Delegates
// to types.go (valueToTypeS). Arrays unify element types; maps produce optional
// fields for observed keys; functions reconstruct A1 -> ... -> Ret.
func (ip *Interpreter) ValueToType(v Value, env *Env) S { return ip.valueToTypeS(v, env) }

// RegisterNative installs a host/native function into Core and exposes it as a
// first-class function Value. The function is available to programs by `name`.
// Natives are type-checked on call and on return.
//
// Example:
//
//	ip.RegisterNative("add1",
//	  []ParamSpec{{"x", S{"id","Int"}}},
//	  S{"id","Int"},
//	  func(ip *Interpreter, ctx CallCtx) Value {
//	    return Int(ctx.MustArg("x").Data.(int64) + 1)
//	  })
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
		Body:       S{"native", name}, // sentinel for debugging; not executed
		Env:        ip.Core,
		NativeName: name,
	}))
}

// AsMapValue returns a VTMap view for VTMap/VTModule (same MapObject), else the input.
func AsMapValue(v Value) Value {
	if v.Tag == VTModule {
		return Value{Tag: VTMap, Data: v.Data.(*Module).Map}
	}
	return v
}

//// END_OF_PUBLIC

////////////////////////////////////////////////////////////////////////////////
//                             PRIVATE IMPLEMENTATION
////////////////////////////////////////////////////////////////////////////////

func withAnnot(v Value, ann string) Value { v.Annot = ann; return v }

type returnSig struct{ v Value }
type rtErr struct{ msg string }

func fail(msg string)          { panic(rtErr{msg: msg}) }
func errNull(msg string) Value { return withAnnot(Null, msg) }
func annotNull(msg string) Value {
	return Value{Tag: VTNull, Annot: msg}
}

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

// syncModuleEnv keeps a module's Env consistent after a write to its map.
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

// -------- Callable & CallCtx adapters --------

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

// -------- VM entry/exit plumbing (with runtime error surfacing) --------

func (ip *Interpreter) runTopWithSource(ast S, env *Env, uncaught bool, sr *SourceRef) (out Value, err error) {
	if !uncaught {
		defer func() {
			if r := recover(); r != nil {
				switch sig := r.(type) {
				case returnSig:
					out, err = sig.v, nil
				case rtErr:
					if ip.runtimeErrorsAsGoError {
						line, col := ip.sourcePosFromChunk(nil, sr, 0)
						err = WrapErrorWithSource(&RuntimeError{Line: line, Col: col, Msg: sig.msg}, ip.fallbackSrc(sr, ast))
						out = Value{}
					} else {
						out, err = errNull(sig.msg), nil
					}
				case error:
					out, err = Null, sig
				default:
					if ip.runtimeErrorsAsGoError {
						line, col := ip.sourcePosFromChunk(nil, sr, 0)
						err = WrapErrorWithSource(&RuntimeError{Line: line, Col: col, Msg: fmt.Sprintf("runtime panic: %v", r)}, ip.fallbackSrc(sr, ast))
						out = Value{}
					} else {
						out, err = errNull(fmt.Sprintf("runtime panic: %v", r)), nil
					}
				}
			}
		}()
	}

	ch := ip.jitTop(ast, sr)
	prev := ip.currentSrc
	ip.currentSrc = ch.Src
	res := ip.runChunk(ch, env, 0)
	ip.currentSrc = prev

	switch res.status {
	case vmOK, vmReturn:
		return res.value, nil
	case vmRuntimeError:
		if ip.runtimeErrorsAsGoError {
			line, col := ip.sourcePosFromChunk(ch, ch.Src, res.pc)
			msg := res.value.Annot
			if msg == "" {
				msg = "runtime error"
			}
			return Value{}, WrapErrorWithSource(&RuntimeError{Line: line, Col: col, Msg: msg}, ip.fallbackSrc(ch.Src, ast))
		}
		return res.value, nil
	default:
		return errNull("unknown VM status"), nil
	}
}

// Build a one-off top-level function body and ensure it is compiled.
func (ip *Interpreter) jitTop(ast S, sr *SourceRef) *Chunk {
	f := &Fun{
		ReturnType: S{"id", "Any"},
		Body:       ast,
		Src:        sr,
	}
	ip.ensureChunkWithSource(f, sr)
	return f.Chunk
}

func (ip *Interpreter) ensureChunk(f *Fun) { ip.ensureChunkWithSource(f, f.Src) }

func (ip *Interpreter) ensureChunkWithSource(f *Fun, sr *SourceRef) {
	if f.Chunk != nil || f.NativeName != "" || f.IsOracle {
		return
	}
	em := newEmitter(ip, sr)
	em.emitFunBody(f.Body)
	ch := em.chunk()
	ch.Src = sr
	f.Chunk = ch
}

// -------- Calls / currying (scoped engine) --------

func (ip *Interpreter) applyArgsScoped(fnVal Value, args []Value, callSite *Env) Value {
	if fnVal.Tag != VTFun {
		fail("not a function")
	}
	f := fnVal.Data.(*Fun)

	// Zero-arg application
	if len(args) == 0 {
		switch len(f.Params) {
		case 0:
			return ip.execFunBodyScoped(fnVal, callSite)
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

	// Already saturated → execute, then keep applying to the result (currying chains).
	if len(f.Params) == 0 {
		res := ip.execFunBodyScoped(fnVal, callSite)
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

	// Bind argument into a fresh call env.
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
			Src:        f.Src,
		})
	}

	// Last arg supplied → execute.
	execFun := &Fun{
		Params:     nil,
		ParamTypes: append([]S(nil), f.ParamTypes...),
		ReturnType: f.ReturnType,
		Body:       f.Body,
		Env:        callEnv,
		Chunk:      f.Chunk,
		NativeName: f.NativeName,
		IsOracle:   f.IsOracle,
		Examples:   f.Examples,
		HiddenNull: f.HiddenNull,
		Src:        f.Src,
	}
	execVal := FunVal(execFun)
	execVal.Annot = fnVal.Annot // keep doc
	return ip.execFunBodyScoped(execVal, callSite)
}

func (ip *Interpreter) execFunBodyScoped(funVal Value, callSite *Env) Value {
	if funVal.Tag != VTFun {
		fail("not a function")
	}
	f := funVal.Data.(*Fun)
	// Native fast path
	if f.NativeName != "" {
		impl, ok := ip.native[f.NativeName]
		if !ok {
			fail(fmt.Sprintf("unknown native %q", f.NativeName))
		}
		scope := withScope(f.Env, callSite) // where side effects land
		prev := ip.currentSrc
		if f.Src != nil {
			ip.currentSrc = f.Src
		}
		res := impl(ip, &callCtx{argEnv: f.Env, scope: scope})
		ip.currentSrc = prev
		if !ip.isType(res, f.ReturnType, f.Env) {
			fail("return type mismatch")
		}
		return res
	}

	if f.IsOracle {
		return ip.execOracle(funVal, callSite)
	}

	// User-defined function
	ip.ensureChunkWithSource(f, f.Src)
	prev := ip.currentSrc
	if f.Src != nil {
		ip.currentSrc = f.Src
	}
	res := ip.runChunk(f.Chunk, f.Env, 0)
	ip.currentSrc = prev

	switch res.status {
	case vmOK, vmReturn:
		if !ip.isType(res.value, f.ReturnType, f.Env) {
			fail("return type mismatch")
		}
		return res.value
	case vmRuntimeError:
		return res.value
	default:
		return errNull("unknown VM status")
	}
}

// -------- sugar for native registration --------

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
			tv := t.Data.(*TypeValue)
			ip.assignTo(tv.Ast, v, ctx.Env())
			return v
		})

	// __assign_def(target: Type, value: Any) -> Any
	ip.reg("__assign_def",
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
	ip.reg("__plus",
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
	ip.reg("__resolve_type",
		[]ParamSpec{{"t", S{"id", "Type"}}}, S{"id", "Type"},
		func(ctx CallCtx) Value {
			t := ctx.MustArg("t")
			resolved := ip.resolveTypeValue(t, ctx.Env())
			return TypeVal(resolved)
		})

	// __annotate(text: Str, v: Any) -> Any
	ip.reg("__annotate",
		[]ParamSpec{{"text", S{"id", "Str"}}, {"v", S{"id", "Any"}}}, S{"id", "Any"},
		func(ctx CallCtx) Value {
			return withAnnot(ctx.MustArg("v"), ctx.MustArg("text").Data.(string))
		})

	// __collect_for_elems(iter: Any) -> Any   (used by high-level mapping helpers)
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

	// __map_from(keys:[Str], vals:[Any]) -> Map
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

	// __len(array|map) -> Int
	ip.reg("__len",
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
		},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			namesV := ctx.MustArg("params").Data.([]Value)
			typesV := ctx.MustArg("types").Data.([]Value)

			retTV := ctx.MustArg("ret").Data.(*TypeValue)
			bodyTV := ctx.MustArg("body").Data.(*TypeValue)

			isOr := ctx.MustArg("isOracle").Data.(bool)
			exAny := ctx.MustArg("examples")

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

			retAst := retTV.Ast
			if isOr {
				retAst = ensureNullableUnlessAny(retAst)
			}

			return FunVal(&Fun{
				Params:     names,
				ParamTypes: types,
				ReturnType: retAst,
				Body:       bodyTV.Ast,
				Env:        ctx.Env(),
				HiddenNull: hidden,
				IsOracle:   isOr,
				Examples:   exVals,
				Src:        ip.currentSrc, // inherit source of the running chunk
			})
		})

	// __is_fun(x: Any) -> Bool
	ip.reg("__is_fun",
		[]ParamSpec{{"x", S{"id", "Any"}}}, S{"id", "Bool"},
		func(ctx CallCtx) Value { return Bool(ctx.MustArg("x").Tag == VTFun) })

	// __iter_should_stop(x: Any) -> Bool
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
					Src:        ip.currentSrc,
				})
			}

			// Map → iterator (yields [key, value]) preserving insertion order + key annotations
			if x.Tag == VTMap {
				mo := x.Data.(*MapObject)

				env := NewEnv(ctx.Env())
				env.Define("$map", x)
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
							S{"assign", S{"decl", "$k"},
								S{"idx", S{"id", "$keys"}, S{"id", "$i"}},
							},
							S{"assign", S{"id", "$i"},
								S{"binop", "+", S{"id", "$i"}, S{"int", int64(1)}},
							},
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
					Src:        ip.currentSrc,
				})
			}

			return annotNull("for expects array, map, or iterator function (Null -> Any?)")
		})
}

// -------- deep equality (Value) --------

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

// -------- assignment semantics --------

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

// -------- tiny evaluators used by assignment --------

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

// -------- iterator expansion --------

func (ip *Interpreter) collectForElemsScoped(iter Value, scope *Env) []Value {
	iter = AsMapValue(iter)
	switch iter.Tag {
	case VTArray:
		return append([]Value(nil), iter.Data.([]Value)...)
	case VTMap:
		mo := iter.Data.(*MapObject)
		out := make([]Value, 0, len(mo.Keys))
		for _, k := range mo.Keys {
			keyV := Str(k)
			if ann, ok := mo.KeyAnn[k]; ok && ann != "" {
				keyV = withAnnot(keyV, ann)
			}
			out = append(out, Arr([]Value{keyV, mo.Entries[k]}))
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

// -------- JIT: AST → bytecode emitter (with PC marks) --------

type emitter struct {
	ip        *Interpreter
	code      []uint32
	consts    []Value
	ctrlStack []ctrlCtx // generic block/loop control stack

	// Source mapping
	src   *SourceRef
	marks []PCMark
	path  NodePath
}

// Control-jump bookkeeping for blocks/loops (break/continue implemented via jumps).
type ctrlCtx struct {
	isLoop     bool
	breakJumps []int
	contJumps  []int
}

func (e *emitter) pushBlockCtx() { e.ctrlStack = append(e.ctrlStack, ctrlCtx{isLoop: false}) }
func (e *emitter) pushLoopCtx()  { e.ctrlStack = append(e.ctrlStack, ctrlCtx{isLoop: true}) }
func (e *emitter) popCtx() ctrlCtx {
	i := len(e.ctrlStack) - 1
	c := e.ctrlStack[i]
	e.ctrlStack = e.ctrlStack[:i]
	return c
}

// Prefer nearest loop; otherwise nearest block.
func (e *emitter) addBreakJump(at int) {
	for i := len(e.ctrlStack) - 1; i >= 0; i-- {
		if e.ctrlStack[i].isLoop {
			c := e.ctrlStack[i]
			c.breakJumps = append(c.breakJumps, at)
			e.ctrlStack[i] = c
			return
		}
	}
	i := len(e.ctrlStack) - 1
	c := e.ctrlStack[i]
	c.breakJumps = append(c.breakJumps, at)
	e.ctrlStack[i] = c
}
func (e *emitter) addContJump(at int) {
	for i := len(e.ctrlStack) - 1; i >= 0; i-- {
		if e.ctrlStack[i].isLoop {
			c := e.ctrlStack[i]
			c.contJumps = append(c.contJumps, at)
			e.ctrlStack[i] = c
			return
		}
	}
	i := len(e.ctrlStack) - 1
	c := e.ctrlStack[i]
	c.contJumps = append(c.contJumps, at)
	e.ctrlStack[i] = c
}

// Bytecode helpers used by loops/blocks.

func (e *emitter) preloadAssignToLast(lastName string) {
	e.emit(opLoadGlobal, e.ks("__assign_set"))
	e.emit(opConst, e.k(TypeVal(S{"id", lastName})))
}
func (e *emitter) saveLastAndJumpHead(head int) {
	e.emit(opCall, 2)
	e.emit(opPop, 0)
	e.emit(opJump, uint32(head))
}
func (e *emitter) patchGateAndSaveLast(jumps []int, gate int) {
	for _, at := range jumps {
		e.patch(at, gate)
	}
	e.emit(opCall, 2)
	e.emit(opPop, 0)
}

func newEmitter(ip *Interpreter, src *SourceRef) *emitter { return &emitter{ip: ip, src: src} }

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
func (e *emitter) chunk() *Chunk {
	return &Chunk{Code: e.code, Consts: e.consts, Marks: e.marks, Src: e.src}
}

// path helpers for source mapping
func (e *emitter) mark() {
	e.marks = append(e.marks, PCMark{PC: e.here(), Path: append(NodePath(nil), e.path...)})
}
func (e *emitter) withChild(childIdx int, f func()) {
	e.path = append(e.path, childIdx)
	f()
	e.path = e.path[:len(e.path)-1]
}

func (e *emitter) callBuiltin(name string, args ...S) {
	e.emit(opLoadGlobal, e.ks(name))
	for _, a := range args {
		e.emitExpr(a)
	}
	e.emit(opCall, uint32(len(args)))
}

// unwrapKeyStr returns the string name for a map key S-node (accepts "str" or
// annotation-wrapped "str").
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

// Entry: emit whole function body and return.
func (e *emitter) emitFunBody(body S) {
	e.path = e.path[:0] // root
	e.emitExpr(body)
	e.emit(opReturn, 0)
}

// Emit an expression node.
func (e *emitter) emitExpr(n S) {
	e.mark() // record PC → current AST node
	if len(n) == 0 {
		e.emit(opConst, e.k(Null))
		return
	}
	switch n[0].(string) {
	case "int":
		e.emit(opConst, e.k(Int(n[1].(int64))))
	case "num":
		e.emit(opConst, e.k(Num(n[1].(float64))))
	case "str":
		e.emit(opConst, e.k(Str(n[1].(string))))
	case "bool":
		e.emit(opConst, e.k(Bool(n[1].(bool))))
	// case "noop":
	// 	e.emit(opConst, e.k(Null))
	case "null":
		e.emit(opConst, e.k(Null))

	case "id":
		e.emit(opLoadGlobal, e.ks(n[1].(string)))

	case "block":
		e.pushBlockCtx()
		nItems := len(n) - 1
		if nItems <= 0 {
			e.emit(opConst, e.k(Null))
		} else {
			for i := 1; i <= nItems; i++ {
				j := i - 1
				e.withChild(j, func() { e.emitExpr(n[i].(S)) })
				if i < nItems {
					e.emit(opPop, 0)
				}
			}
		}
		exit := e.here()
		ctx := e.popCtx()
		for _, at := range ctx.breakJumps {
			e.patch(at, exit)
		}
		for _, at := range ctx.contJumps {
			e.patch(at, exit)
		}

	case "break":
		e.emitExpr(n[1].(S))
		at := e.here()
		e.emit(opJump, 0)
		e.addBreakJump(at)
		return

	case "continue":
		e.emitExpr(n[1].(S))
		at := e.here()
		e.emit(opJump, 0)
		e.addContJump(at)
		return

	case "unop":
		op := n[1].(string)
		if op == "?" {
			e.emit(opConst, e.k(errNull("postfix '?' invalid here")))
			return
		}
		e.withChild(1, func() { e.emitExpr(n[2].(S)) })
		switch op {
		case "not":
			e.emit(opNot, 0)
		case "-":
			e.emit(opNeg, 0)
		default:
			e.emit(opConst, e.k(errNull("unknown unary op")))
		}

	case "binop":
		op := n[1].(string)
		if op == "and" || op == "or" {
			e.withChild(1, func() { e.emitExpr(n[2].(S)) })
			if op == "and" {
				jf := e.here()
				e.emit(opJumpIfFalse, 0)
				e.withChild(2, func() { e.emitExpr(n[3].(S)) })
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
				e.withChild(2, func() { e.emitExpr(n[3].(S)) })
				lend := e.here()
				e.patch(jend, lend)
			}
			return
		}
		a, b := n[2].(S), n[3].(S)
		switch op {
		case "==":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emit(opEq, 0)
		case "!=":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emit(opNe, 0)
		case "+":
			// builtin plus; preserve child mapping
			e.emit(opLoadGlobal, e.ks("__plus"))
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emit(opCall, 2)
		case "-":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emit(opSub, 0)
		case "*":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emit(opMul, 0)
		case "/":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emit(opDiv, 0)
		case "%":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emit(opMod, 0)
		case "<":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emit(opLt, 0)
		case "<=":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emit(opLe, 0)
		case ">":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emit(opGt, 0)
		case ">=":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
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

	case "array":
		for i := 1; i < len(n); i++ {
			e.withChild(i-1, func() { e.emitExpr(n[i].(S)) })
		}
		e.emit(opMakeArr, uint32(len(n)-1))

	case "map":
		keys := S{"array"}
		vals := S{"array"}
		for i := 1; i < len(n); i++ {
			p := n[i].(S)
			keys = append(keys, p[1].(S))
			vals = append(vals, p[2].(S))
		}
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

	case "get":
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
		e.emit(opGetProp, e.ks(n[2].(S)[1].(string)))
	case "idx":
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
		e.withChild(1, func() { e.emitExpr(n[2].(S)) })
		e.emit(opGetIdx, 0)

	case "call":
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
		for i := 2; i < len(n); i++ {
			e.withChild(i-1, func() { e.emitExpr(n[i].(S)) })
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
		e.emit(opConst, e.k(Bool(false)))
		e.emit(opConst, e.k(Null))
		e.emit(opCall, 6)

	case "oracle":
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
		e.emit(opConst, e.k(TypeVal(S{"oracle"})))
		e.emit(opConst, e.k(Bool(true)))
		e.emitExpr(n[3].(S))
		e.emit(opCall, 6)

	case "return":
		e.emitExpr(n[1].(S))
		e.emit(opReturn, 0)

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
			e.withChild(i, func() {
				e.withChild(0, func() { e.emitExpr(p[1].(S)) }) // cond
			})
			jf := e.here()
			e.emit(opJumpIfFalse, 0)
			e.withChild(i, func() { e.withChild(1, func() { e.emitExpr(p[2].(S)) }) }) // then
			jend := e.here()
			e.emit(opJump, 0)
			jends = append(jends, jend)
			e.patch(jf, e.here())
		}
		if hasElse {
			e.withChild(len(arms)-1, func() { e.emitExpr(arms[len(arms)-1].(S)) })
		} else {
			e.emit(opConst, e.k(Null))
		}
		tail := e.here()
		for _, at := range jends {
			e.patch(at, tail)
		}

	case "while":
		cond := n[1].(S)
		body := n[2].(S)

		lastName := fmt.Sprintf("$last_%d", e.here())
		e.callBuiltin("__assign_def", S{"type", S{"decl", lastName}}, S{"null"})

		head := e.here()
		e.withChild(0, func() { e.emitExpr(cond) })
		jf := e.here()
		e.emit(opJumpIfFalse, 0)

		e.preloadAssignToLast(lastName)

		e.pushLoopCtx()
		e.withChild(1, func() { e.emitExpr(body) })
		loopCtx := e.popCtx()

		e.saveLastAndJumpHead(head)

		lcont := e.here()
		e.patchGateAndSaveLast(loopCtx.contJumps, lcont)
		e.emit(opJump, uint32(head))

		lbreak := e.here()
		e.patchGateAndSaveLast(loopCtx.breakJumps, lbreak)
		jEnd := e.here()
		e.emit(opJump, 0)

		end := e.here()
		e.patch(jf, end)
		e.patch(jEnd, end)

		e.emit(opLoadGlobal, e.ks(lastName))

	case "for":
		target := n[1].(S)
		iterExpr := n[2].(S)
		body := n[3].(S)

		iterName := fmt.Sprintf("$iter_%d", e.here())
		e.callBuiltin("__assign_def", S{"type", S{"decl", iterName}},
			S{"call", S{"id", "__to_iter"}, iterExpr})

		tmpName := fmt.Sprintf("$tmp_%d", e.here())
		e.callBuiltin("__assign_def", S{"type", S{"decl", tmpName}}, S{"null"})

		lastName := fmt.Sprintf("$last_%d", e.here())
		e.callBuiltin("__assign_def", S{"type", S{"decl", lastName}}, S{"null"})

		head := e.here()

		e.emit(opLoadGlobal, e.ks("__assign_set"))
		e.emit(opConst, e.k(TypeVal(S{"id", tmpName})))
		e.emit(opLoadGlobal, e.ks(iterName))
		e.emit(opConst, e.k(Null))
		e.emit(opCall, 1)
		e.emit(opCall, 2)

		e.emit(opLoadGlobal, e.ks("__iter_should_stop"))
		e.emit(opLoadGlobal, e.ks(tmpName))
		e.emit(opCall, 1)
		jBody := e.here()
		e.emit(opJumpIfFalse, 0)
		jEnd := e.here()
		e.emit(opJump, 0)

		bodyStart := e.here()
		e.patch(jBody, bodyStart)

		e.preloadAssignToLast(lastName)

		assignName := "__assign_set"
		switch target[0].(string) {
		case "decl", "darr", "dobj", "annot":
			assignName = "__assign_def"
		}
		e.callBuiltin(assignName, S{"type", target}, S{"id", tmpName})
		e.emit(opPop, 0)

		e.pushLoopCtx()
		e.withChild(2, func() { e.emitExpr(body) })
		loopCtx := e.popCtx()

		e.saveLastAndJumpHead(head)

		lcont := e.here()
		e.patchGateAndSaveLast(loopCtx.contJumps, lcont)
		e.emit(opJump, uint32(head))

		lbreak := e.here()
		e.patchGateAndSaveLast(loopCtx.breakJumps, lbreak)
		jToEnd := e.here()
		e.emit(opJump, 0)

		end := e.here()
		e.patch(jEnd, end)
		e.patch(jToEnd, end)

		e.emit(opLoadGlobal, e.ks(lastName))

	case "type":
		e.emit(opConst, e.k(TypeVal(n[1].(S))))

	case "annot":
		text := n[1].(S)[1].(string)
		subj := n[2].(S)

		// #(doc) (lhs = rhs)  ==>  lhs = #(doc) rhs
		if len(subj) > 0 && subj[0].(string) == "assign" {
			lhs := subj[1].(S)
			rhs := subj[2].(S)

			opName := "__assign_set"
			switch lhs[0].(string) {
			case "decl", "darr", "dobj", "annot":
				opName = "__assign_def"
			}

			e.emit(opLoadGlobal, e.ks(opName))
			e.emit(opConst, e.k(TypeVal(lhs)))
			e.emit(opLoadGlobal, e.ks("__annotate"))
			e.emit(opConst, e.k(Str(text)))
			e.emitExpr(rhs)
			e.emit(opCall, 2)
			e.emit(opCall, 2)
			return
		}

		// #(doc) (let x)  ==>  let x = #(doc) null
		if len(subj) > 0 && subj[0].(string) == "decl" {
			e.emit(opLoadGlobal, e.ks("__assign_def"))
			e.emit(opConst, e.k(TypeVal(subj)))
			e.emit(opLoadGlobal, e.ks("__annotate"))
			e.emit(opConst, e.k(Str(text)))
			e.emit(opConst, e.k(Null))
			e.emit(opCall, 2)
			e.emit(opCall, 2)
			return
		}

		// default: #(doc) expr  ==>  __annotate(doc, expr)
		e.callBuiltin("__annotate", S{"str", text}, subj)

	default:
		e.emit(opConst, e.k(errNull(fmt.Sprintf("unknown AST tag: %s", n[0].(string)))))
	}
}

// -------- source mapping helpers (PC → (line,col)) --------

// sourcePosFromChunk finds a best-effort (line,col) from PC marks + spans.
func (ip *Interpreter) sourcePosFromChunk(ch *Chunk, sr *SourceRef, pc int) (int, int) {
	src := ""
	if sr != nil {
		src = sr.Src
	}
	// Unknown → 1:1
	if ch == nil || sr == nil || sr.Spans == nil || len(ch.Marks) == 0 || src == "" {
		return 1, 1
	}
	// last mark with mark.PC <= pc
	i := -1
	for j := range ch.Marks {
		if ch.Marks[j].PC <= pc {
			i = j
		} else {
			break
		}
	}
	if i < 0 {
		return 1, 1
	}
	if span, ok := sr.Spans.Get(ch.Marks[i].Path); ok {
		return offsetToLineCol(src, span.StartByte)
	}
	return 1, 1
}

// fallbackSrc chooses source text to render a snippet, even if spans are missing.
func (ip *Interpreter) fallbackSrc(sr *SourceRef, ast S) string {
	if sr != nil && sr.Src != "" {
		return sr.Src
	}
	// As a last resort, pretty-print the AST (line:col will be coarse).
	return FormatSExpr(ast)
}

// offsetToLineCol converts a byte offset into 1-based (line, col).
func offsetToLineCol(src string, off int) (int, int) {
	if off < 0 {
		return 1, 1
	}
	line, col := 1, 1
	i := 0
	for i < len(src) && i < off {
		if src[i] == '\n' {
			line++
			col = 1
			i++
			continue
		}
		col++
		i++
	}
	return line, col
}
