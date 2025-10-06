// interpreter.go — SINGLE PUBLIC API SURFACE for the MindScript interpreter.
//
// OVERVIEW
// ========
// This file exposes the **entire public surface** of the MindScript runtime. It
// deliberately contains **only exported types and thin methods**. All behavior
// is specified here in enough detail that a consumer can use the interpreter
// without reading any private implementation.
//
// What you get in this file:
//   • The **runtime value model** (`Value`, `ValueTag`, constructors like `Int/Str/Arr`).
//   • **Ordered maps** with per-key annotations (`MapObject`) and helpers.
//   • **Functions / closures** (`Fun`) as first-class values.
//   • **Environments** (`Env`) with lexical scoping.
//   • The **Interpreter** type with the canonical entry points:
//        - parsing+evaluation of source/AST (ephemeral vs persistent),
//        - function application (`Apply`, `Call0`),
//        - function introspection (`FunMeta`),
//        - native registration (`RegisterNative`),
//        - type helpers (`ResolveType`, `IsType`, `IsSubtype`, `UnifyTypes`, `ValueToType`).
//   • Hard errors bubble as a single `*Error` (see errors.go). Entry points
//     format them with caret snippets; internals do **not** format.
//
// What this file does **not** include:
//   • Any algorithmic implementation, bytecode generation, or the VM. Those live
//     in private files and are intentionally hidden behind narrow private
//     interfaces that are wired up during `NewInterpreter()`.
//
// EXECUTION & SCOPING SEMANTICS
// -----------------------------
// MindScript code evaluates in **environments** (`*Env`) that form a lexical
// chain via `parent`. The Interpreter exposes two well-known frames:
//   • `Core`: built-ins and registered natives (read-only to user code).
//   • `Global`: user-visible program state (REPL/module globals).
//
// Entry points differ only in *which* environment they target:
//   • Ephemeral (sandboxed) runs: `EvalSource` and `Eval` create a **fresh child
//     of Global**; names bound during evaluation land in that throwaway child,
//     so `Global` remains unchanged unless the program **explicitly** mutates it.
//   • Persistent (REPL-style) runs: `EvalPersistentSource` and `EvalPersistent`
//     evaluate **in Global** itself, so `let`/assignment update the persistent
//     state.
//   • Advanced embedding: `EvalAST(ast, env)` evaluates exactly in the provided
//     environment, letting hosts control scoping explicitly.
//
// ERROR MODEL
// -----------
// MindScript distinguishes:
//   1) **Soft errors** for operational conditions (file-not-found, no space,
//      etc.). These surface as annotated-null **Values** (not Go errors).
//   2) **Hard errors** for contractual failures (lex/parse mistakes, wrong
//      arity/type contracts, runtime invariants). These bubble as a single
//      `*Error` with (Kind, Msg, Line, Col, Src). Internals NEVER pretty print.
//      Public entry points attach the correct SourceRef and format them.
//
// All `Eval*` methods return `(Value, error)`. On hard failure they return a Go
// `error` whose message is a caret-formatted snippet produced **only here** at
// the API surface via `FormatError` (see errors.go). Internals pass `*Error` up
// unformatted.
//
// VALUES & MAPS
// -------------
// `Value` is a tagged sum covering: null, bool, int64, float64, string, arrays,
// ordered maps, functions, type values, modules, and opaque handles. `MapObject`
// preserves **insertion order** (`Keys`) and supports **per-key annotations**
// (`KeyAnn`). Its `Entries` is a string→Value map; order-sensitive operations
// must consult `Keys`. The helper `AsMapValue` exposes a module’s map view.
//
// FUNCTIONS & NATIVES
// -------------------
// `Fun` carries parameter names/types, a body (as an S-expression), its closure
// environment, and an optional `NativeName` when the function is implemented in
// the host. Natives are registered via `RegisterNative(name, params, ret, impl)`
// and are **type-checked both on call and on return** using the MindScript type
// engine (see Type Helpers below). `Apply` performs call/currying semantics and
// enforces parameter types. `FunMeta` exposes arity, parameter specs, return
// type, docs, and closure environment for tooling.
//
// TYPES (STRUCTURAL)
// ------------------
// Types are S-expressions (`type S = []any`, defined in parser.go). The public
// helpers delegate to the private type engine:
//   • `ResolveType(t, env)` — resolve identifiers within a type expression.
//   • `IsType(v, t, env)`   — runtime value check against a type.
//   • `IsSubtype(a, b, env)`— structural subtyping.
//   • `UnifyTypes(t1, t2, env)` — least common supertype (LUB).
//   • `ValueToType(v, env)` — pragmatic structural type inference.
// The precise structural rules (nullable, arrays, maps with required/optional,
// functions contravariant/covariant, enums, `Int <: Num`, etc.) are defined in
// `types.go` and fully respected by this API.
//
// PROCESSES & CONCURRENCY (MINIMAL, LUA-STYLE)
// --------------------------------------------
// The simplest safe model is **isolates**: each `Interpreter` instance is an
// independent “process” with its own `Global`, module cache, stacks, and
// ephemeral state. The `Core` environment is shared **read-only** across
// isolates (user code cannot mutate Core; see sealing notes below).
//
// You can spawn concurrent work by **cloning** an interpreter and running code
// in a new goroutine. This file exposes:
//   • `(*Interpreter) Clone()` — snapshot the current interpreter into a new
//     isolate (shares a read-only Core; copies native registry; new Global).
//   • Host-level spawns: `SpawnSource` / `SpawnAST` returning a `ProcessHandle`.
//   • A helper `HandleVal` to wrap/unwrap process handles as `Value` (VTHandle),
//     so `spawn`/`join` natives can be implemented in `interpreter_ops.go`.
//
// Concurrency contract:
//   • Instantiate and register natives **before** spawning concurrent processes.
//     (Clones copy the native registry; mutating a Go `map` concurrently is not
//     allowed. Core is treated as immutable after init.)
//   • Each `Interpreter` instance is not re-entrant; run at most one evaluation
//     at a time per instance. For parallel work, use `Clone()`.
//   • `Global`/module caches are **not** shared between clones.
//
// DEPENDENCIES (OTHER FILES)
// --------------------------
//   • lexer.go / parser.go: tokenization and Pratt parser that produce S-expr ASTs.
//     (Public alias `type S = []any` is defined in parser.go.)
//   • spans.go: sidecar source spans used for caret-style runtime errors.
//   • vm.go: bytecode `Chunk`, opcodes, and VM execution (internal).
//   • interpreter_exec.go (private): parsing, JIT, VM dispatch, calls/currying.
//   • interpreter_ops.go  (private): built-ins, assignment, iteration, emitter.
//   • types.go: structural type system (used via public wrappers here).
//   • errors.go: unified diagnostic (`*Error`) and pretty-printing at API surface.
//   • oracles.go, modules.go: optional features used internally (opaque here).
//
// DESIGN INTENT
// -------------
// The API is intentionally **narrow and predictable**. You can:
//   • Choose source or AST inputs.
//   • Choose ephemeral (child) or persistent (Global) scope.
//   • Register natives with explicit param/return types.
//   • Call functions and introspect them.
//   • Ask type questions and perform type inference.
//   • Clone interpreters and spawn concurrent processes safely.
//
// Everything else—parsing details, bytecode shapes, opcodes, cache strategies,
// optimization passes—remains private and may evolve without breaking this API.

package mindscript

import (
	"fmt"
	"strconv"
)

////////////////////////////////////////////////////////////////////////////////
//                              PUBLIC TYPES & CTORS
////////////////////////////////////////////////////////////////////////////////

// ValueTag enumerates all runtime kinds a Value may hold.
// The tag determines which field of Value.Data is valid (see Value docs).
type ValueTag int

const (
	VTNull   ValueTag = iota // null (no payload)
	VTBool                   // bool
	VTInt                    // int64
	VTNum                    // float64
	VTStr                    // string
	VTArray                  // *ArrayObject
	VTMap                    // *MapObject (ordered map)
	VTFun                    // *Fun (closure; native or user-defined)
	VTType                   // *TypeValue (type AST + definition env)
	VTModule                 // module handle (opaque; maps to a MapObject view)
	VTHandle                 // opaque host handle (integration-specific)
)

// Value is the universal runtime carrier used by the interpreter.
//
// Fields:
//   - Tag   — discriminant indicating which case is active.
//   - Data  — Go value appropriate for Tag (see ValueTag; e.g., int64 for VTInt).
//   - Annot — optional annotation used by the runtime to propagate user-facing
//     documentation or error context. Annotations never affect equality.
//
// Invariants:
//   - When Tag==VTNull, Data is nil.
//   - When Tag==VTMap, Data is *MapObject preserving insertion order.
//   - Modules (VTModule) can be viewed as maps via AsMapValue.
type Value struct {
	Tag   ValueTag
	Data  interface{}
	Annot string
}

// ArrayObject gives arrays reference semantics (like MapObject), so mutations
// (push/pop/shift/unshift/index-assign) are observed by all aliases.
type ArrayObject struct {
	Elems []Value
}

// Handle is the single opaque/userdata-like carrier (Lua-style).
// Hosts can box arbitrary data behind a "kind" discriminator.
type Handle struct {
	Kind string
	Data any
}

// HandleVal boxes host data into an opaque runtime Value.
func HandleVal(kind string, data any) Value {
	return Value{Tag: VTHandle, Data: &Handle{Kind: kind, Data: data}}
}

// String renders a human-friendly debug representation (annotations are omitted).
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
		return fmt.Sprintf("<array len=%d>", len(v.Data.(*ArrayObject).Elems))
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

// Null is the singleton null Value (no annotation, no payload).
var Null = Value{Tag: VTNull}

// Primitive constructors for convenience. They do not attach annotations.
func Bool(b bool) Value    { return Value{Tag: VTBool, Data: b} }
func Int(n int64) Value    { return Value{Tag: VTInt, Data: n} }
func Num(f float64) Value  { return Value{Tag: VTNum, Data: f} }
func Str(s string) Value   { return Value{Tag: VTStr, Data: s} }
func Arr(xs []Value) Value { return Value{Tag: VTArray, Data: &ArrayObject{Elems: xs}} }

// MapObject is an ordered map preserving insertion order and per-key annotations.
//
// Fields:
//   - Entries — the key/value storage (by string key).
//   - KeyAnn  — optional per-key annotation text (preserved on round-trips).
//   - Keys    — insertion order (unique keys); use this to iterate predictably.
//
// Semantics:
//   - Insert order is the iteration order.
//   - Setting a value for a new key appends that key to Keys.
//   - Removing keys (if implemented in hosts) must also update Keys.
//
// Values of map type are represented as Value{Tag: VTMap, Data: *MapObject}.
type MapObject struct {
	Entries map[string]Value
	KeyAnn  map[string]string
	Keys    []string
}

// Map constructs a VTMap from a plain Go map. Note: Literal maps constructed
// from source preserve exact key order via internal built-ins; hosts building
// maps programmatically can use Map for convenience (order equals Go map
// iteration order if Keys is not supplied, so this helper synthesizes Keys
// from the initial map contents).
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

// TypeValue carries a type expression AST (S) and the lexical Env where it was
// defined. Resolution uses the stored Env when available.
type TypeValue struct {
	Ast S
	Env *Env
}

// TypeValIn builds a VTType and pins its resolution environment explicitly.
// Use this when exporting user-defined types from specific scopes.
func TypeValIn(expr S, env *Env) Value {
	return Value{Tag: VTType, Data: &TypeValue{Ast: expr, Env: env}}
}

// Fun represents a function/closure. Functions are first-class Values (VTFun).
//
// Fields (stable API; implementation specifics like bytecode are private):
//   - Params      — parameter names in order.
//   - ParamTypes  — declared parameter types (S-expression per param).
//   - ReturnType  — declared return type (S). Oracles are made nullable internally.
//   - Body        — function body as an S-expression (opaque to callers).
//   - Env         — closure environment captured at definition time.
//   - NativeName  — non-empty iff implemented by a registered native.
//   - Examples    — optional example values for tooling; ignored by runtime.
//   - IsOracle    — marks oracle functions (different return-type semantics).
//   - HiddenNull  — internal arity placeholder for zero-arg construction (not API).
//   - Src         — optional source metadata for enriched runtime errors.
//
// Note: `Chunk` is an internal JIT product stored here for caching; callers
// should treat it as opaque and never rely on it.
type Fun struct {
	Params     []string
	Body       S
	Env        *Env
	ParamTypes []S
	ReturnType S
	HiddenNull bool

	Chunk      *Chunk // JIT result (from vm.go) — internal use only
	NativeName string // non-empty for registered natives

	IsOracle bool  // oracle marker
	Examples Value // VTArray of [input, output] pairs, or Null when none

	Src *SourceRef // source metadata (optional)

	// Original declaration signature (names/types): tools/oracles can
	// reference it even after currying changes Params.
	Sig *SigMeta
}

// SigMeta is an immutable, engine-internal carrier of the original signature.
// It replaces the old $__sig_names / $__sig_types closure bindings for oracles.
type SigMeta struct {
	Names []string // original parameter names, in order
	Types []S      // original declared parameter types, in order
}

// FunVal wraps *Fun into a Value (Tag=VTFun).
func FunVal(f *Fun) Value { return Value{Tag: VTFun, Data: f} }

// Env is a lexical environment frame with a parent link. Lookups walk parent-ward.
// Use Define to bind in the current frame, Set to update an existing visible
// binding (nearest frame), and Get to retrieve.
type Env struct {
	parent           *Env
	table            map[string]Value
	sealParentWrites bool
}

// ---- Builtin type names/constructors (value-namespace guards) ----
var builtinTypeAtoms = map[string]struct{}{
	"Any": {}, "Null": {}, "Bool": {}, "Int": {}, "Num": {}, "Str": {}, "Type": {}, "Enum": {},
}

func isBuiltinTypeAtom(name string) bool { _, ok := builtinTypeAtoms[name]; return ok }

// SealParentWrites prevents Set from climbing into parent frames.
// Lookups (Get) still traverse parents as usual.
func (e *Env) SealParentWrites() { e.sealParentWrites = true }

// NewEnv creates a new lexical frame with the given parent (which may be nil).
func NewEnv(parent *Env) *Env { return &Env{parent: parent, table: make(map[string]Value)} }

// Define binds name to v in the current frame, shadowing any outer binding.
func (e *Env) Define(name string, v Value) {
	e.table[name] = v
}

// Set updates the nearest existing binding of name to v. If no binding exists
// in any visible frame, Set returns an error (it does not implicitly define).
func (e *Env) Set(name string, v Value) error {
	// Disallow assignment to language-level builtins (even if not present in envs).
	if isBuiltinTypeAtom(name) {
		return fmt.Errorf("cannot assign to type atom/contructor: %s", name)
	}
	if _, ok := e.table[name]; ok {
		e.table[name] = v
		return nil
	}
	// If this frame is sealed, do not climb; emit a friendlier message
	// when the name exists in an ancestor (e.g., Core builtins).
	if e.sealParentWrites {
		for p := e.parent; p != nil; p = p.parent {
			if _, ok := p.table[name]; ok {
				return fmt.Errorf("cannot assign to builtin: %s", name)
			}
		}
		return fmt.Errorf("undefined variable: %s", name)
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
	// If the miss is a type atom/ctor, explain how to obtain a runtime Type.
	if isBuiltinTypeAtom(name) {
		return Value{}, fmt.Errorf("'%s' is a type expression, not a value. Use 'type %s' to obtain a runtime Type, or use it in a type annotation", name, name)
	}
	return Value{}, fmt.Errorf("undefined variable: %s", name)
}

// ParamSpec documents a function parameter (name + declared type). Used by
// native registration and function introspection.
type ParamSpec struct {
	Name string
	Type S
}

// Callable exposes metadata about a function Value (for tooling, docs, REPLs).
// The returned values reflect the function’s declared signature and closure env.
type Callable interface {
	Arity() int
	ParamSpecs() []ParamSpec
	ReturnType() S
	Doc() string
	ClosureEnv() *Env
}

// CallCtx is passed to native functions, providing access to bound arguments
// (by parameter name) and the effect scope (where side effects should land).
type CallCtx interface {
	Arg(name string) Value
	Env() *Env
}

// NativeImpl is the implementation signature for registered host/native functions.
// Implementations must return a Value conforming to the declared return type;
// the interpreter enforces parameter and return types on every call.
type NativeImpl func(ip *Interpreter, ctx CallCtx) Value

////////////////////////////////////////////////////////////////////////////////
//                          PROCESSES (MINIMAL CONCURRENCY)
////////////////////////////////////////////////////////////////////////////////

// ProcessHandle is a tiny, host-visible handle for a spawned evaluation.
// Join blocks until the process completes and returns the same (Value, error)
// shape as Eval* entry points (errors already pretty-printed).
type ProcessHandle struct {
	ch chan processResult
}

type processResult struct {
	v   Value
	err error
}

// Join waits for the spawned evaluation to finish and returns its result.
func (h *ProcessHandle) Join() (Value, error) {
	if h == nil || h.ch == nil {
		return Null, fmt.Errorf("invalid process handle")
	}
	r := <-h.ch
	return r.v, r.err
}

////////////////////////////////////////////////////////////////////////////////
//                               PUBLIC INTERPRETER
////////////////////////////////////////////////////////////////////////////////

// Interpreter is the entry point for evaluating MindScript programs.
//
// Public fields:
//   - Core   — built-in environment; parent of Global. Populated by NewInterpreter.
//   - Global — persistent program environment (REPL/module state).
//
// Construction:
//   - Use NewInterpreter() to obtain a ready-to-use instance. Core natives are
//     installed automatically; Global is an empty child of Core.
//
// Behavior summary:
//   - EvalSource/Eval run in a **fresh child of Global** (ephemeral).
//   - EvalPersistentSource/EvalPersistent run **in Global** (persistent).
//   - EvalAST runs in the environment you pass.
//   - Apply/Call0 invoke function Values with type-checking & currying.
//   - FunMeta returns a Callable to inspect signatures/docs.
//   - (New) Clone returns an isolated interpreter suitable for concurrent runs.
//   - (New) SpawnSource/SpawnAST run code in a fresh isolate on a goroutine.
//
// Hard-error discipline:
//   - Internals bubble `*Error` up **unformatted**.
//   - Public entry points attach the correct `SourceRef` and return a Go error
//     whose message is a pretty, caret-labeled snippet via `FormatError`.
type Interpreter struct {
	// Publicly visible environments:
	Global *Env // user-visible namespace (per-interpreter)
	Base   *Env // per-namespace runtime/prelude layer (overwritable)
	Core   *Env // engine-critical intrinsics (read-only)

	// Immutable, pre-seeded Base template used for fast namespace snapshots.
	baseTemplate *Env

	// Private internals (opaque to callers):
	modules   map[string]*moduleRec // private module system (defined elsewhere)
	native    map[string]NativeImpl // registered natives
	loadStack []string              // import guard

	currentSrc *SourceRef

	// Private facades implemented in private files:
	_exec execCore
}

// Private contracts the internals satisfy (wired by NewInterpreter).
// These are intentionally unexported and may evolve.
type execCore interface {
	// Parse + evaluate source into the given env (fresh or persistent).
	evalSource(src string, env *Env) (Value, error)
	// Evaluate AST in the given env.
	evalAST(ast S, env *Env) (Value, error)
	// Calls & metadata
	applyArgsScoped(fn Value, args []Value, callSite *Env) Value
	funMeta(fn Value) (Callable, bool)
}

// NewInterpreter constructs an engine with core natives and a seeded Base,
// failing fast if the standard prelude cannot be loaded. After construction:
//   - Core is populated with built-ins and any subsequently registered natives.
//   - Global is empty and inherits from Core.
//   - Global is sealed from mutating Core (user code cannot overwrite builtins).
//   - The interpreter is ready for Eval*/Apply/FunMeta/etc.
func NewInterpreter() (*Interpreter, error) {
	ip := &Interpreter{}
	ip.Core = NewEnv(nil)
	ip.modules = map[string]*moduleRec{}
	ip.native = map[string]NativeImpl{}

	// Wire private implementations (defined in private files).
	ip._exec = newExec(ip)

	// Install core built-ins.
	initCore(ip)

	// Build a pre-seeded, immutable Base template once, then snapshot per namespace.
	if err := ip.buildBaseTemplate(); err != nil {
		return nil, err
	}
	ip.Base = ip.newBaseFromTemplate()
	// Global is the user frame. It may climb into Base to overwrite runtime/prelude.
	ip.Global = NewEnv(ip.Base)

	return ip, nil
}

////////////////////////////////////////////////////////////////////////////////
//                         PUBLIC METHODS (THIN DELEGATIONS)
////////////////////////////////////////////////////////////////////////////////

// ensureErrorHasSource attaches sr to e if e is a *Error and Src is nil.
// Returns the possibly-updated error.
func ensureErrorHasSource(err error, sr *SourceRef) error {
	if err == nil || sr == nil {
		return err
	}
	if e, ok := err.(*Error); ok && e.Src == nil {
		e.Src = sr
	}
	return err
}

// formatAtAPI pretty-prints a hard error to a Go error (string message).
// If err is not a *Error, it is passed through unchanged.
func formatAtAPI(err error) error {
	if err == nil {
		return nil
	}
	if e, ok := err.(*Error); ok {
		return fmt.Errorf("%s", FormatError(e))
	}
	return err
}

// EvalSource parses and evaluates source **in a fresh child of Global**.
// Effects (lets/assignments) land in that ephemeral child; Global is unchanged
// unless the program explicitly mutates it.
//
// Returns the resulting Value; on hard failure returns a Go error with a
// caret-formatted snippet (LEXICAL/PARSE/RUNTIME) produced at the API surface.
func (ip *Interpreter) EvalSource(src string) (Value, error) {
	ast, spans, err := ParseSExprWithSpans(src)
	if err != nil {
		sr := &SourceRef{Name: "<main>", Src: src, Spans: spans}
		err = ensureErrorHasSource(err, sr)
		return Null, formatAtAPI(err)
	}
	sr := &SourceRef{Name: "<main>", Src: src, Spans: spans}
	val, err := ip.runTopWithSource(ast, NewEnv(ip.Global), false, sr)
	if err != nil {
		err = ensureErrorHasSource(err, sr)
		return Null, formatAtAPI(err)
	}
	return val, nil
}

// Eval evaluates a pre-parsed AST **in a fresh child of Global**.
// See EvalSource for scoping and error semantics.
//
// Implementation detail:
//
//	We format the AST back to source to construct a SourceRef with spans
//	(via ParseSExprWithSpans) so hard errors can be shown with carets.
func (ip *Interpreter) Eval(root S) (Value, error) {
	src := FormatSExpr(root)
	ast, spans, err := ParseSExprWithSpans(src)
	if err != nil {
		sr := &SourceRef{Name: "<main>", Src: src, Spans: spans}
		err = ensureErrorHasSource(err, sr)
		return Null, formatAtAPI(err)
	}
	sr := &SourceRef{Name: "<main>", Src: src, Spans: spans}
	val, err := ip.runTopWithSource(ast, NewEnv(ip.Global), false, sr)
	if err != nil {
		err = ensureErrorHasSource(err, sr)
		return Null, formatAtAPI(err)
	}
	return val, nil
}

// EvalPersistentSource parses and evaluates source **in Global** (REPL-style).
// Effects directly mutate Global. Returns Value; on hard failure returns a
// caret-formatted Go error produced at the API surface.
func (ip *Interpreter) EvalPersistentSource(src string) (Value, error) {
	ast, spans, err := ParseSExprWithSpans(src)
	if err != nil {
		sr := &SourceRef{Name: "<repl>", Src: src, Spans: spans}
		err = ensureErrorHasSource(err, sr)
		return Null, formatAtAPI(err)
	}
	sr := &SourceRef{Name: "<repl>", Src: src, Spans: spans}
	val, err := ip.runTopWithSource(ast, ip.Global, false, sr)
	if err != nil {
		err = ensureErrorHasSource(err, sr)
		return Null, formatAtAPI(err)
	}
	return val, nil
}

// EvalPersistent evaluates a pre-parsed AST **in Global** (REPL-style).
// Effects directly mutate Global. Returns Value; on hard failure returns a
// caret-formatted Go error produced at the API surface.
func (ip *Interpreter) EvalPersistent(root S) (Value, error) {
	src := FormatSExpr(root)
	ast, spans, err := ParseSExprWithSpans(src)
	if err != nil {
		sr := &SourceRef{Name: "<repl>", Src: src, Spans: spans}
		err = ensureErrorHasSource(err, sr)
		return Null, formatAtAPI(err)
	}
	sr := &SourceRef{Name: "<repl>", Src: src, Spans: spans}
	val, err := ip.runTopWithSource(ast, ip.Global, false, sr)
	if err != nil {
		err = ensureErrorHasSource(err, sr)
		return Null, formatAtAPI(err)
	}
	return val, nil
}

// EvalAST evaluates an AST in the provided environment exactly as given.
// Hosts use this to control scoping (e.g., per-request envs, sandboxes).
// Returns Value; on hard failure returns a caret-formatted Go error if the
// bubbled *Error already carries a SourceRef; otherwise the error is passed
// through unchanged (callers without source can still inspect *Error fields).
func (ip *Interpreter) EvalAST(ast S, env *Env) (Value, error) {
	val, err := ip._exec.evalAST(ast, env)
	if err != nil {
		// If the error already has a SourceRef, pretty print; else pass through.
		if _, ok := err.(*Error); ok {
			return Null, formatAtAPI(err)
		}
		return Null, err
	}
	return val, nil
}

// Apply applies a function Value to the provided argument Values.
//
// Semantics:
//   - Performs arity/type checking against the function’s declared ParamTypes.
//   - Supports currying: if fewer args than parameters are provided, returns a
//     partially-applied function Value; if more are provided, they are applied
//     in sequence to the results.
//   - Side effects from natives occur in the call-site/program scope.
//
// Panics inside the engine are caught by Eval* callers; Apply itself follows
// the runtime’s internal error discipline. Use via evaluated programs or in
// hosts that handle runtime errors accordingly.
func (ip *Interpreter) Apply(fn Value, args []Value) Value {
	return ip._exec.applyArgsScoped(fn, args, nil)
}

// Call0 invokes a function with zero arguments (equivalent to Apply(fn, nil)).
func (ip *Interpreter) Call0(fn Value) Value { return ip._exec.applyArgsScoped(fn, nil, nil) }

// FunMeta exposes a function Value as a Callable for introspection (arity,
// parameter specs with declared types, return type, documentation string taken
// from Value.Annot, and the closure environment). Returns (nil, false) if the
// Value is not a function.
func (ip *Interpreter) FunMeta(fn Value) (Callable, bool) {
	return ip._exec.funMeta(fn)
}

// ResolveType expands a type expression by resolving identifiers bound to
// user-defined types in the provided environment. See types.go for semantics.
func (ip *Interpreter) ResolveType(t S, env *Env) S { return ip.resolveType(t, env) }

// IsType reports whether runtime value v conforms to type t.
// Structural rules are defined in types.go (Int<:Num, nullable, arrays/maps,
// function subtyping, enums, open-world objects, etc.).
func (ip *Interpreter) IsType(v Value, t S, env *Env) bool { return ip.isType(v, t, env) }

// IsSubtype reports whether type a is a structural subtype of type b.
func (ip *Interpreter) IsSubtype(a, b S, env *Env) bool { return ip.isSubtype(a, b, env) }

// UnifyTypes computes a least common supertype (LUB) of t1 and t2.
func (ip *Interpreter) UnifyTypes(t1 S, t2 S, env *Env) S { return ip.unifyTypes(t1, t2, env) }

// ValueToType infers a pragmatic structural type for v (JSON-friendly).
// Arrays unify element types; maps become open-world with observed fields.
func (ip *Interpreter) ValueToType(v Value, env *Env) S { return ip.valueToTypeS(v, env) }

// RegisterNative installs a host/native function into Core and exposes it as a
// first-class function Value available by `name` to programs.
//
// Contract:
//   - `params` declares parameter names and types (enforced on call).
//   - `ret` declares the return type (enforced on return).
//   - `impl` is invoked with (ip, CallCtx) at runtime.
//   - The created function is placed in Core under `name`.
//
// Notes:
//   - Natives participate in currying and type-checking like user functions.
//   - The doc string for introspection is taken from the Value’s Annot (callers
//     may annotate after registration if desired).
//
// Concurrency:
//   - Register natives **before** using Clone/Spawn for concurrent work.
//     Clones copy the native registry; mutating maps concurrently is not supported.
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

	hidden := false
	if len(names) == 0 {
		names = []string{"_"}
		types = []S{S{"id", "Null"}}
		hidden = true
	}

	ip.Core.Define(name, FunVal(&Fun{
		Params:     names,
		ParamTypes: types,
		ReturnType: ret,
		Body:       S{"native", name}, // sentinel for debugging; not executed
		Env:        ip.Core,
		NativeName: name,
		HiddenNull: hidden, // <-- set this
		Sig: &SigMeta{
			Names: append([]string{}, names...),
			Types: append([]S{}, types...),
		},
	}))
}

// AsMapValue returns a VTMap view for VTMap/VTModule (sharing the same MapObject),
// else returns the input unchanged. This is useful when callers want uniform map
// handling for modules and plain maps.
func AsMapValue(v Value) Value {
	if v.Tag == VTModule {
		return Value{Tag: VTMap, Data: v.Data.(*Module).Map}
	}
	return v
}

// Clone creates an isolated Interpreter that shares no mutable runtime state
// with the original. It shares the read-only Core, reuses the prebuilt
// baseTemplate, creates a fresh Base + Global, and copies the native registry
// (name -> implementation) without mutating Core.
//
// Notes:
//   - Core is shared and treated as immutable after init (no writes here).
//   - Base is recreated from the immutable template; Global is a fresh child.
//   - Module cache and loader state are fresh per-clone.
//   - The exec facade is rebound to the clone.
func (ip *Interpreter) Clone() *Interpreter {
	// New interpreter shell
	cl := &Interpreter{}

	// Share read-only Core; do not mutate it here.
	cl.Core = ip.Core

	// Fresh per-clone state
	cl.modules = map[string]*moduleRec{}
	cl.native = make(map[string]NativeImpl, len(ip.native))
	cl._exec = newExec(cl)

	// Reuse immutable template to create a fresh Base, then make a fresh Global.
	cl.baseTemplate = ip.baseTemplate
	cl.Base = cl.newBaseFromTemplate()
	cl.Global = NewEnv(cl.Base)

	// Copy native registry implementations for this clone (no Core writes).
	for name, impl := range ip.native {
		cl.native[name] = impl
	}

	// Fresh loader/source tracking
	cl.modules = map[string]*moduleRec{}
	cl.loadStack = nil
	cl.currentSrc = nil

	return cl
}

////////////////////////////////////////////////////////////////////////////////
//                      ISOLATES: CLONE & HOST-LEVEL SPAWN
////////////////////////////////////////////////////////////////////////////////

// SpawnSource clones the interpreter and evaluates `src` persistently (in the
// child’s Global) on a new goroutine. It returns a ProcessHandle whose Join()
// yields the same (Value, error) contract as EvalPersistentSource.
func (ip *Interpreter) SpawnSource(src string) *ProcessHandle {
	child := ip.Clone()
	h := &ProcessHandle{ch: make(chan processResult, 1)}
	go func() {
		v, err := child.EvalPersistentSource(src)
		h.ch <- processResult{v: v, err: err}
	}()
	return h
}

// SpawnAST clones the interpreter and evaluates `ast` persistently (in the
// child’s Global) on a new goroutine. It returns a ProcessHandle whose Join()
// yields the same (Value, error) contract as EvalPersistent.
func (ip *Interpreter) SpawnAST(ast S) *ProcessHandle {
	child := ip.Clone()
	h := &ProcessHandle{ch: make(chan processResult, 1)}
	go func() {
		v, err := child.EvalPersistent(ast)
		h.ch <- processResult{v: v, err: err}
	}()
	return h
}

//// END_OF_PUBLIC

////////////////////////////////////////////////////////////////////////////////
//                    BASE TEMPLATE + SNAPSHOT HELPERS (PRIVATE)
////////////////////////////////////////////////////////////////////////////////

// Build a pre-seeded, sealed Base template parented to Core. Treat as immutable.
// RETURNS ERROR if the runtime/prelude fails to load.
func (ip *Interpreter) buildBaseTemplate() error {
	tmpl := NewEnv(ip.Core)
	tmpl.SealParentWrites()

	ip.baseTemplate = tmpl

	ip.SeedRuntimeInto(tmpl)
	if err := ip.SeedRuntimeInto(tmpl); err != nil {
		return err
	}
	ip.baseTemplate = tmpl
	return nil
}

// Create a fresh Base(ns) by cloning the template and rebinding closures/types.
func (ip *Interpreter) newBaseFromTemplate() *Env {
	// Base template must be constructed during NewInterpreter(). Do not
	// silently build it here (that would swallow prelude errors).
	if ip.baseTemplate == nil {
		panic("base template not initialized (constructor should have failed)")
	}
	return cloneEnvRebinding(ip.baseTemplate, ip.Core)
}

// cloneEnvRebinding clones src into a new Env with newParent, rebinding
// closures and type envs that *directly* referenced src -> to the new Env.
// We do not rebuild entire env chains anymore; functions/types now capture
// the real parent env (Base/module), so a simple pointer equality rebind
// is sufficient and avoids deep recursion/stack growth.
func cloneEnvRebinding(src *Env, newParent *Env) *Env {
	dst := NewEnv(newParent)
	dst.sealParentWrites = src.sealParentWrites
	dst.table = make(map[string]Value, len(src.table))
	for k, v := range src.table {
		dst.table[k] = rebindValue(v, src, dst)
	}
	return dst
}

// rebindValue: if a function/type closes over 'from', re-pin to 'to'.
// For arrays/maps we deep-copy the container shape and rebind contained values
// (to avoid aliasing across namespaces). Primitives pass through unchanged.
func rebindValue(v Value, from, to *Env) Value {
	switch v.Tag {
	case VTFun:
		f := *v.Data.(*Fun) // copy
		if f.Env == from {
			f.Env = to
		}
		nv := FunVal(&f)
		nv.Annot = v.Annot
		return nv
	case VTType:
		tv := *v.Data.(*TypeValue) // copy
		// Pin envs that were defined in the template (or env-less) to 'to'.
		if tv.Env == nil || tv.Env == from {
			tv.Env = to
		}
		return Value{Tag: VTType, Data: &tv, Annot: v.Annot}
	case VTArray:
		src := v.Data.(*ArrayObject).Elems
		out := make([]Value, len(src))
		for i := range src {
			out[i] = rebindValue(src[i], from, to)
		}
		return Arr(out)
	case VTMap:
		sm := v.Data.(*MapObject)
		nm := &MapObject{
			Entries: make(map[string]Value, len(sm.Entries)),
			KeyAnn:  make(map[string]string, len(sm.KeyAnn)),
			Keys:    append([]string(nil), sm.Keys...),
		}
		for k, vv := range sm.Entries {
			nm.Entries[k] = rebindValue(vv, from, to)
		}
		for k, a := range sm.KeyAnn {
			nm.KeyAnn[k] = a
		}
		nv := Value{Tag: VTMap, Data: nm, Annot: v.Annot}
		return nv
	default:
		return v
	}
}
