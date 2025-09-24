// types.go: runtime type & schema system for MindScript.
//
// IMPLEMENTATION-ONLY FILE
// ------------------------
// This file contains the *private implementation* of the type engine used by
// the public methods on *Interpreter* that live in interpreter.go:
//
//   ResolveType, IsType, IsSubtype, UnifyTypes, ValueToType.
//
// Those exported methods are thin wrappers that delegate to the lower-case
// functions defined here: resolveType, isType, isSubtype, unifyTypes,
// valueToTypeS (plus helpers like litToValue/equalS).
//
// Goals / design (practical, JSON-friendly, duck-typed):
//
// OVERVIEW
// --------
// This file implements MindScript’s *runtime* type system. Types are represented
// as S-expressions (the same light-weight shape the parser produces) and remain
// entirely structural. The system is designed for practical, JSON-friendly
// validation and inference:
//
//   - `Value → Type` inference (`ValueToType`) infers loose shapes, e.g.
//     {name:"Raffa"} → {"map", {"pair", {"str","name"}, {"id","Str"}}}
//     Arrays unify element types conservatively; objects are “open-world”.
//   - `IsType(v, T)` checks whether a runtime `Value` conforms to `T`.
//   - `IsSubtype(A, B)` is a *structural* subtyping relation.
//   - Arrays are covariant in element type.
//   - Objects require that all *required* fields of `B` exist, with compatible
//     types, and *requiredness cannot be relaxed*.
//   - Extra fields on values are allowed (open-world).
//   - Functions use parameter contravariance and return covariance.
//   - `Int <: Num`, and `T?` means nullable.
//   - `UnifyTypes(A, B)` computes a least common supertype (LUB) used by
//     inference (e.g., arrays with mixed contents).
//   - **Modules:** Runtime values tagged `VTModule` are treated as **maps** for
//     type-checking and inference purposes (they normalize via `AsMapValue`).
//
// CANONICALIZATION (alias nodes)
// ------------------------------
// A key improvement in this implementation is *alias canonicalization*.
//
//  • Any non-builtin type reference (local or module-qualified) is resolved to a
//    stable, pointer-identified alias node:   ("alias", *TypeValue)
//
//  • Builtin type names remain as ("id", "Int"|"Num"|"Str"|"Bool"|"Null"|"Any"|"Type").
//
//  • Structural nodes ("array", "map", "unop ?","binop ->", "enum") are resolved
//    recursively while keeping alias nodes *opaque* — we *do not* inline-expand
//    aliases during ResolveType. This produces a canonical, name-agnostic
//    representation so that `M.T` and a local `T = M.T` unify by *pointer
//    identity* of the exported `*TypeValue`.
//
// EQUIRECURSIVE COMPARISON
// ------------------------
// Both `isType` and `isSubtype` are implemented coinductively with cycle
// breaking using memo tables keyed on:
//   • value-pointer × type-node-pointer (for isType), and
//   • (left-node, right-node) identity (for isSubtype),
// augmented so that ("alias", *TypeValue) keys by the *TypeValue pointer*.
// This makes recursive types and module-qualified aliases compare correctly.
//
// DEPENDENCIES (other files)
// --------------------------
// • parser.go
//   - `type S = []any`  (S-expression node type)
//
// • interpreter.go
//   - Runtime value model: `Value`, tags (VTNull/VTBool/VTInt/...),
//     constructors (`Null`, `Bool`, `Int`, `Num`, `Str`, `Arr`)
//   - `MapObject` (map entries + key annotations), `Fun`, `TypeValue`
//   - Environments: `Env` with `Get/Set/Define`
//   - `type Interpreter` (receiver for public API methods)
//
// • vm.go (indirect): no direct calls here, but shares `Value` semantics.
//
// PUBLIC VS PRIVATE
// -----------------
// PUBLIC  : Nothing.
//
// PRIVATE : All concrete algorithms and helpers: resolveType, isType,
//           isSubtype, unifyTypes, valueToTypeS, literal conversion,
//           structural S-equality, and field extraction.

package mindscript

import (
	"unsafe"
)

//// END_OF_PUBLIC

////////////////////////////////////////////////////////////////////////////////
//                             PRIVATE IMPLEMENTATION
////////////////////////////////////////////////////////////////////////////////

// -----------------------------
// Helpers
// -----------------------------

// stripAnnot unwraps annotation wrappers inside type ASTs:
//
//	("annot", ("str", doc), T)  =>  T
//
// It repeats until the outer node is not "annot".
func stripAnnot(t S) S {
	for len(t) >= 3 {
		tag, ok := t[0].(string)
		if !ok || tag != "annot" {
			break
		}
		t = t[2].(S)
	}
	return t
}

func isBuiltinTypeName(name string) bool {
	switch name {
	case "Any", "Null", "Bool", "Int", "Num", "Str", "Type":
		return true
	default:
		return false
	}
}

func isId(t S, name string) bool {
	return len(t) >= 2 && t[0].(string) == "id" && t[1].(string) == name
}

// ("map", ("pair"| "pair!", ("str",k), T)...)
type objField struct {
	required bool
	typ      S
}

func mapTypeFields(t S) map[string]objField {
	fs := map[string]objField{}
	for i := 1; i < len(t); i++ {
		p := t[i].(S)
		if len(p) < 3 {
			continue
		}
		ptag := p[0].(string) // "pair" or "pair!"
		keyNode := p[1].(S)   // ("str", k)
		if len(keyNode) < 2 || keyNode[0].(string) != "str" {
			continue
		}
		k := keyNode[1].(string)
		required := ptag == "pair!"
		fs[k] = objField{required: required, typ: stripAnnot(p[2].(S))}
	}
	return fs
}

// Structural equality for S-exprs with special cases:
//   - "map" and "enum" stay order-insensitive as before.
//   - NEW: ("alias", *TypeValue) compares by pointer identity.
func equalLiteralS(a, b S) bool {

	// Compares two primitive S nodes or scalars used inside S.
	equalPrimitiveS := func(x, y any) bool {
		switch xv := x.(type) {
		case []any: // covers S too
			yv, ok := y.([]any)
			if !ok {
				return false
			}
			return equalLiteralS(xv, yv)
		case string:
			ys, ok := y.(string)
			return ok && xv == ys
		case int64:
			yi, ok := y.(int64)
			return ok && xv == yi
		case float64:
			yf, ok := y.(float64)
			return ok && xv == yf
		case bool:
			yb, ok := y.(bool)
			return ok && xv == yb
		default:
			return x == y
		}
	}

	if len(a) != len(b) {
		return false
	}
	if len(a) == 0 {
		return true
	}

	ta, ok := a[0].(string)
	if !ok {
		return false
	}
	tb, ok := b[0].(string)
	if !ok || ta != tb {
		return false
	}

	if ta == "alias" {
		if len(a) < 2 || len(b) < 2 {
			return false
		}
		atv, ok1 := a[1].(*TypeValue)
		btv, ok2 := b[1].(*TypeValue)
		return ok1 && ok2 && atv == btv
	}

	// Order-insensitive equality for maps.
	if ta == "map" {
		fa := mapTypeFields(a)
		fb := mapTypeFields(b)
		if len(fa) != len(fb) {
			return false
		}
		for k, va := range fa {
			vb, ok := fb[k]
			if !ok || va.required != vb.required || !equalLiteralS(va.typ, vb.typ) {
				return false
			}
		}
		return true
	}

	// Order-insensitive equality for enums.
	if ta == "enum" {
		if len(a) != len(b) { // quick length check
			return false
		}
	outer:
		for i := 1; i < len(a); i++ {
			for j := 1; j < len(b); j++ {
				if equalLiteralS(a[i].(S), b[j].(S)) {
					continue outer
				}
			}
			return false
		}
		return true
	}

	for i := 1; i < len(a); i++ {
		if !equalPrimitiveS(a[i], b[i]) {
			return false
		}
	}
	return true
}

// Convert a literal S-expr (incl. arrays/maps) to a runtime Value.
// Allowed forms: null/bool/int/num/str/array/map with literal children.
// Map pairs may be "pair" or "pair!" and keys must be ("str", <k>).
func (ip *Interpreter) litToValue(lit S) (Value, bool) {
	if len(lit) == 0 {
		return Null, false
	}
	switch lit[0].(string) {
	case "null":
		return Null, true
	case "bool":
		return Bool(lit[1].(bool)), true
	case "int":
		return Int(lit[1].(int64)), true
	case "num":
		return Num(lit[1].(float64)), true
	case "str":
		return Str(lit[1].(string)), true
	case "array":
		out := make([]Value, 0, len(lit)-1)
		for i := 1; i < len(lit); i++ {
			v, ok := ip.litToValue(lit[i].(S))
			if !ok {
				return Null, false
			}
			out = append(out, v)
		}
		return Arr(out), true
	case "map":
		m := make(map[string]Value, len(lit)-1)
		for i := 1; i < len(lit); i++ {
			p := lit[i].(S)
			if len(p) < 3 {
				return Null, false
			}
			keyNode := p[1].(S)
			if len(keyNode) < 2 || keyNode[0].(string) != "str" {
				return Null, false
			}
			k := keyNode[1].(string)
			v, ok := ip.litToValue(p[2].(S))
			if !ok {
				return Null, false
			}
			m[k] = v
		}
		return Map(m), true
	default:
		return Null, false
	}
}

// -----------------------------
// Value → Type inference (typeOf-like behavior)
// -----------------------------

// valueToTypeS infers a structural type for v. This wrapper seeds cycle guards.
func (ip *Interpreter) valueToTypeS(v Value, env *Env) S {
	// Cycle guards for arrays and maps so self-references don't produce
	// infinitely deep or self-nested element/object types. Any back-edge
	// collapses to Any (conservative, JSON-friendly).
	seenArr := map[*ArrayObject]bool{}
	seenMap := map[*MapObject]bool{}

	var infer func(Value) S
	infer = func(v Value) S {
		switch v.Tag {
		case VTNull:
			return S{"id", "Null"}
		case VTBool:
			return S{"id", "Bool"}
		case VTInt:
			return S{"id", "Int"}
		case VTNum:
			return S{"id", "Num"}
		case VTStr:
			return S{"id", "Str"}

		case VTArray:
			ao := v.Data.(*ArrayObject)

			// If we're already walking this array, it's a cycle → Any.
			if seenArr[ao] {
				return S{"id", "Any"}
			}
			seenArr[ao] = true
			defer func() { delete(seenArr, ao) }()

			xs := ao.Elems
			if len(xs) == 0 {
				return S{"array", S{"id", "Any"}}
			}

			// If any element is exactly this array → element type Any.
			for _, e := range xs {
				if e.Tag == VTArray && e.Data.(*ArrayObject) == ao {
					return S{"array", S{"id", "Any"}}
				}
			}

			elem := infer(xs[0])
			for i := 1; i < len(xs); i++ {
				elem = ip.unifyTypes(elem, infer(xs[i]), env)
				if isId(elem, "Any") {
					// Can't get more specific; bail early.
					break
				}
			}
			return S{"array", elem}

		case VTMap:
			mo := v.Data.(*MapObject)

			// Cycle guard for maps.
			if seenMap[mo] {
				return S{"id", "Any"}
			}
			seenMap[mo] = true
			defer func() { delete(seenMap, mo) }()

			out := S{"map"}
			// Open-world: infer observed fields as optional ("pair").
			for k, vv := range mo.Entries {
				out = append(out, S{"pair", S{"str", k}, infer(vv)})
			}
			return out

		case VTModule:
			// Treat modules structurally as maps.
			return infer(AsMapValue(v))

		case VTFun:
			f := v.Data.(*Fun)
			// Resolve each piece in the function's own env to avoid <type>
			rt := ip.resolveType(f.ReturnType, f.Env)
			for i := len(f.ParamTypes) - 1; i >= 0; i-- {
				pt := ip.resolveType(f.ParamTypes[i], f.Env)
				rt = S{"binop", "->", pt, rt}
			}
			return rt

		case VTType:
			return S{"id", "Type"}

		default:
			return S{"id", "Any"}
		}
	}

	return infer(v)
}

// -----------------------------
// Alias-based canonical resolution
// -----------------------------

// --- replace the old resolveModuleTypeAlias with these helpers ---

// peelGetChain collects rightward keys from nested ("get", base, "str") nodes.
// It returns (baseNode, keysRightToLeft).
func peelGetChain(t S) (S, []string) {
	t = stripAnnot(t)
	keys := []string{}
	node := t
	for len(node) >= 3 && node[0].(string) == "get" {
		keyNode, _ := node[2].(S)
		if len(keyNode) < 2 || keyNode[0].(string) != "str" {
			return t, nil
		}
		keys = append(keys, keyNode[1].(string))
		base, _ := node[1].(S)
		node = base
	}
	return node, keys
}

// resolveTypePath walks a ("get", ...) chain against runtime values, switching
// the lookup view when encountering a Type (use its Env) and stepping through
// modules by reading their exported map. On success returns the terminal *TypeValue.
func (ip *Interpreter) resolveTypePath(env *Env, t S) (*TypeValue, bool) {
	base, keysRL := peelGetChain(t)
	if len(keysRL) == 0 {
		return nil, false
	}

	// Resolve the base to a runtime Value in the given env.
	var cur Value
	switch {
	case len(base) >= 2 && base[0].(string) == "id":
		v, err := env.Get(base[1].(string))
		if err != nil {
			return nil, false
		}
		cur = v

	case len(base) >= 3 && base[0].(string) == "get":
		// Recurse: resolve the inner path first.
		tv, ok := ip.resolveTypePath(env, base)
		if !ok {
			return nil, false
		}
		// The base resolved to a Type; switch view to its Env to continue.
		cur = TypeValIn(tv.Ast, tv.Env)

	default:
		return nil, false
	}

	// Walk keys left→right (we collected right→left).
	for i := len(keysRL) - 1; i >= 0; i-- {
		switch cur.Tag {
		case VTModule:
			mod := cur.Data.(*Module)
			v, ok := mod.get(keysRL[i])
			if !ok {
				return nil, false
			}
			cur = v

		case VTType:
			// Switch lookup view to the type's own environment.
			tv := cur.Data.(*TypeValue)
			if tv.Env == nil {
				return nil, false
			}
			v, err := tv.Env.Get(keysRL[i])
			if err != nil {
				return nil, false
			}
			cur = v

		default:
			return nil, false
		}
	}

	if cur.Tag != VTType {
		return nil, false
	}
	return cur.Data.(*TypeValue), true
}

// resolveType canonicalizes a type S-expression in env:
//   - Builtins stay as ("id", "...").
//   - Any non-builtin type identifier or qualified get resolves to a stable,
//     *pointer-identified* alias node: ("alias", *TypeValue).
//   - Structure nodes ("array", "map", "unop ?","binop ->", "enum") are
//     recursively resolved while *keeping* alias nodes opaque (no inline
//     expansion). This gives us a canonical, name-agnostic graph suitable for
//     equirecursive comparison across modules.
//
// NOTE: By avoiding inline expansion and using ("alias", *TypeValue), two
// references to the same exported type (e.g., M.T and a local T = M.T) become
// literally the same anchor, regardless of spelling.
func (ip *Interpreter) resolveType(t S, env *Env) S {
	t = stripAnnot(t)
	if len(t) == 0 {
		return t
	}

	// Helper: build alias if a local name resolves to a VTType.
	// Self-cycle guard: let T = type T  must not aliasify; keep ("id","T").
	aliasOf := func(e *Env, name string) (S, bool) {
		if e == nil || isBuiltinTypeName(name) {
			return nil, false
		}
		v, err := e.Get(name)
		if err != nil || v.Tag != VTType {
			return nil, false
		}
		tv := v.Data.(*TypeValue)
		root := stripAnnot(tv.Ast)
		if len(root) >= 2 && root[0].(string) == "id" && root[1].(string) == name {
			return nil, false
		}
		return S{"alias", tv}, true
	}

	switch t[0].(string) {
	case "id":
		if a, ok := aliasOf(env, t[1].(string)); ok {
			return a
		}
		return t // builtin or unknown stays as-is

	case "get":
		// Robust qualified resolution: walk through modules and types (env switch on type).
		if tv, ok := ip.resolveTypePath(env, t); ok {
			return S{"alias", tv}
		}
		return t

	case "unop":
		if len(t) >= 3 && t[1].(string) == "?" {
			return S{"unop", "?", ip.resolveType(t[2].(S), env)}
		}
		return t

	case "array":
		if len(t) == 2 {
			return S{"array", ip.resolveType(t[1].(S), env)}
		}
		out := S{"array"}
		for i := 1; i < len(t); i++ {
			out = append(out, ip.resolveType(t[i].(S), env))
		}
		return out

	case "map":
		out := S{"map"}
		for i := 1; i < len(t); i++ {
			p := t[i].(S) // ("pair"|"pair!", ("str",k), T)
			tag := p[0].(string)
			key := p[1].(S)
			out = append(out, S{tag, key, ip.resolveType(p[2].(S), env)})
		}
		return out

	case "enum":
		// Enum literals stay literal; we don't resolve inside.
		return t

	case "binop":
		if len(t) >= 4 && t[1].(string) == "->" {
			return S{"binop", "->", ip.resolveType(t[2].(S), env), ip.resolveType(t[3].(S), env)}
		}
		return t

	default:
		return t
	}
}

// -----------------------------
// Runtime type checking
// -----------------------------

// isType checks whether runtime value v conforms to type t under env.
// We canonicalize t once (resolveType), then check structurally.
// It fully supports nested function types and alias nodes.
func (ip *Interpreter) isType(v Value, t S, env *Env) bool {
	t = stripAnnot(ip.resolveType(t, env))

	// Coinductive memo: (valuePtr, nodeKey(t))
	// nodeKey mirrors isSubtype: aliases key by *TypeValue, others by &t[0].
	seen := make(map[[2]unsafe.Pointer]struct{})

	nodeKey := func(t S) unsafe.Pointer {
		if len(t) >= 2 && t[0].(string) == "alias" {
			if tv, ok := t[1].(*TypeValue); ok {
				return unsafe.Pointer(tv)
			}
		}
		return unsafe.Pointer(&t[0])
	}

	// Build value pointer key for arrays/maps/funs for memoization.
	valKey := func(v Value) unsafe.Pointer {
		switch v.Tag {
		case VTMap:
			return unsafe.Pointer(v.Data.(*MapObject))
		case VTArray:
			return unsafe.Pointer(v.Data.(*ArrayObject))
		case VTFun:
			return unsafe.Pointer(v.Data.(*Fun))
		default:
			return nil
		}
	}

	var check func(Value, S) bool
	check = func(v Value, t S) bool {
		t = stripAnnot(t)
		if len(t) == 0 {
			return false
		}

		// Modules behave structurally as maps.
		v = AsMapValue(v)

		vp := valKey(v)
		nk := nodeKey(t)
		// For structured values (vp != nil), memoize all nodes.
		// For scalars (vp == nil), memoize only alias nodes to break unfold cycles.
		if vp != nil || (len(t) >= 2 && t[0].(string) == "alias") {
			k := [2]unsafe.Pointer{vp, nk}
			if _, ok := seen[k]; ok {
				return true
			}
			seen[k] = struct{}{}
		}

		switch t[0].(string) {
		case "id":
			switch t[1].(string) {
			case "Any":
				return true
			case "Null":
				return v.Tag == VTNull
			case "Bool":
				return v.Tag == VTBool
			case "Int":
				return v.Tag == VTInt
			case "Num":
				return v.Tag == VTInt || v.Tag == VTNum
			case "Str":
				return v.Tag == VTStr
			case "Type":
				return v.Tag == VTType
			default:
				// Non-builtin unresolved id: reject (canonical resolver leaves only builtins or aliases)
				return false
			}

		case "alias":
			// Unfold against the alias's own env; the unified memo above
			// prevents both structured and scalar infinite unfolding.
			tv := t[1].(*TypeValue)
			return check(v, ip.resolveType(tv.Ast, tv.Env))

		case "unop": // nullable
			if t[1].(string) != "?" {
				return false
			}
			if v.Tag == VTNull {
				return true
			}
			return check(v, t[2].(S))

		case "array":
			if v.Tag != VTArray {
				return false
			}
			elemT := S{"id", "Any"}
			if len(t) == 2 {
				elemT = t[1].(S)
			}
			for _, e := range v.Data.(*ArrayObject).Elems {
				if !check(e, elemT) {
					return false
				}
			}
			return true

		case "map":
			if v.Tag != VTMap {
				return false
			}
			fs := mapTypeFields(t)
			m := v.Data.(*MapObject).Entries
			for name, f := range fs {
				val, ok := m[name]
				if !ok {
					if f.required {
						return false
					}
					continue
				}
				if !check(val, f.typ) {
					return false
				}
			}
			return true

		case "enum":
			for i := 1; i < len(t); i++ {
				if lit, ok := ip.litToValue(t[i].(S)); ok && ip.deepEqual(lit, v) {
					return true
				}
			}
			return false

		case "binop": // function type expected
			if t[1].(string) != "->" || v.Tag != VTFun {
				return false
			}
			// Build the function value's fully-resolved signature in its own env.
			f := v.Data.(*Fun)
			rt := stripAnnot(ip.resolveType(f.ReturnType, f.Env))
			for i := len(f.ParamTypes) - 1; i >= 0; i-- {
				pt := stripAnnot(ip.resolveType(f.ParamTypes[i], f.Env))
				rt = S{"binop", "->", pt, rt}
			}
			// CRITICAL: Compare under the *expected-type's* env (the env passed into isType),
			// so module-qualified names/aliases from the expected side resolve properly.
			// 'rt' already contains alias nodes for the value side and does not require this env.
			return ip.isSubtype(rt, t, env)
		}

		return false
	}

	return check(v, t)
}

// -----------------------------
// Structural subtyping  t1 <: t2
// -----------------------------

// isSubtype checks a <: b structurally. Both sides are first canonicalized by
// resolveType(t, env). It supports equirecursive types via a coinductive memo
// keyed by the underlying node identity; for ("alias", *TypeValue) it keys on
// the *TypeValue pointer*, so module-qualified names unify with local aliases.
func (ip *Interpreter) isSubtype(a, b S, env *Env) bool {
	a = stripAnnot(ip.resolveType(a, env))
	b = stripAnnot(ip.resolveType(b, env))

	// Produce a stable pointer identity for memo keys:
	//  • for ("alias", *TypeValue) use the tv pointer,
	//  • otherwise use &node[0] (address of tag cell).
	nodeKey := func(t S) unsafe.Pointer {
		if len(t) >= 2 && t[0].(string) == "alias" {
			if tv, ok := t[1].(*TypeValue); ok {
				return unsafe.Pointer(tv)
			}
		}
		return unsafe.Pointer(&t[0])
	}

	seen := make(map[[2]unsafe.Pointer]struct{})

	var sub func(S, S) bool
	sub = func(x, y S) bool {
		x = stripAnnot(x)
		y = stripAnnot(y)
		if len(x) == 0 || len(y) == 0 {
			return false
		}

		k := [2]unsafe.Pointer{nodeKey(x), nodeKey(y)}
		if _, ok := seen[k]; ok {
			return true
		}
		seen[k] = struct{}{}

		// Fast equality
		if equalLiteralS(x, y) {
			return true
		}

		// Top
		if isId(y, "Any") {
			return true
		}

		// Nullable helpers
		isOpt := func(t S) bool { return len(t) >= 3 && t[0].(string) == "unop" && t[1].(string) == "?" }
		unwrap := func(t S) S { return t[2].(S) }

		if isOpt(y) {
			if sub(x, unwrap(y)) {
				return true
			}
			if isId(x, "Null") {
				return true
			}
		}
		// Int <: Num
		if isId(x, "Int") && isId(y, "Num") {
			return true
		}
		// A? <: B? iff A <: B
		if isOpt(x) && isOpt(y) {
			return sub(unwrap(x), unwrap(y))
		}
		// Identical primitive ids
		if len(x) >= 2 && x[0].(string) == "id" && len(y) >= 2 && y[0].(string) == "id" {
			return x[1].(string) == y[1].(string)
		}

		// Alias unfolding (lazily)
		if len(x) >= 2 && x[0].(string) == "alias" {
			tv := x[1].(*TypeValue)
			return sub(ip.resolveType(tv.Ast, tv.Env), y)
		}
		if len(y) >= 2 && y[0].(string) == "alias" {
			tv := y[1].(*TypeValue)
			return sub(x, ip.resolveType(tv.Ast, tv.Env))
		}

		switch x[0].(string) {
		case "array":
			if y[0].(string) != "array" {
				return false
			}
			xe := S{"id", "Any"}
			ye := S{"id", "Any"}
			if len(x) == 2 {
				xe = x[1].(S)
			}
			if len(y) == 2 {
				ye = y[1].(S)
			}
			return sub(xe, ye) // covariance

		case "map":
			if y[0].(string) != "map" {
				return false
			}
			reqY := mapTypeFields(y)
			haveX := mapTypeFields(x)
			for name, fy := range reqY {
				fx, ok := haveX[name]
				if !ok {
					if fy.required {
						return false
					}
					continue
				}
				if fy.required && !fx.required {
					return false
				}
				if !sub(fx.typ, fy.typ) {
					return false
				}
			}
			return true

		case "enum":
			// Enum⊆Enum by literal inclusion
			if y[0].(string) == "enum" {
				for i := 1; i < len(x); i++ {
					found := false
					for j := 1; j < len(y); j++ {
						if equalLiteralS(x[i].(S), y[j].(S)) {
							found = true
							break
						}
					}
					if !found {
						return false
					}
				}
				return true
			}
			// Enum <: T if each member fits T
			for i := 1; i < len(x); i++ {
				lv, ok := ip.litToValue(x[i].(S))
				if !ok || !ip.isType(lv, y, nil) {
					return false
				}
			}
			return true

		case "binop":
			if x[1].(string) != "->" || y[0].(string) != "binop" || y[1].(string) != "->" {
				return false
			}
			xp, xr := x[2].(S), x[3].(S)
			yp, yr := y[2].(S), y[3].(S)
			// Param contravariance, return covariance
			return sub(yp, xp) && sub(xr, yr)
		}

		return false
	}

	return sub(a, b)
}

// -----------------------------
// Unification (least common supertype)
// -----------------------------

func (ip *Interpreter) unifyTypes(t1 S, t2 S, env *Env) S {
	// Canonicalize both sides first (alias-aware)
	t1 = stripAnnot(ip.resolveType(t1, env))
	t2 = stripAnnot(ip.resolveType(t2, env))

	// Helper: expand a single alias once (for LUB we can unfold to compare)
	expandAlias := func(t S) S {
		if len(t) >= 2 && t[0].(string) == "alias" {
			tv := t[1].(*TypeValue)
			return stripAnnot(ip.resolveType(tv.Ast, tv.Env))
		}
		return t
	}

	// Any absorbs
	if isId(t1, "Any") {
		return t1
	}
	if isId(t2, "Any") {
		return t2
	}

	// ---- Null / Nullable ----
	if isId(t1, "Null") && isId(t2, "Null") {
		return t1
	}
	if isId(t1, "Null") && len(t2) >= 3 && t2[0].(string) == "unop" && t2[1].(string) == "?" {
		return t2
	}
	if isId(t2, "Null") && len(t1) >= 3 && t1[0].(string) == "unop" && t1[1].(string) == "?" {
		return t1
	}
	if isId(t1, "Null") {
		return S{"unop", "?", t2}
	}
	if isId(t2, "Null") {
		return S{"unop", "?", t1}
	}

	// Nullable normalization
	isOpt1 := len(t1) >= 3 && t1[0].(string) == "unop" && t1[1].(string) == "?"
	isOpt2 := len(t2) >= 3 && t2[0].(string) == "unop" && t2[1].(string) == "?"
	if isOpt1 && isOpt2 {
		u := ip.unifyTypes(t1[2].(S), t2[2].(S), env)
		if isId(u, "Any") {
			return u
		}
		return S{"unop", "?", u}
	}
	if isOpt1 {
		u := ip.unifyTypes(t1[2].(S), t2, env)
		if isId(u, "Any") {
			return u
		}
		return S{"unop", "?", u}
	}
	if isOpt2 {
		u := ip.unifyTypes(t1, t2[2].(S), env)
		if isId(u, "Any") {
			return u
		}
		return S{"unop", "?", u}
	}

	// If either side is an alias, unfold once for comparison.
	if len(t1) >= 2 && t1[0].(string) == "alias" {
		t1 = expandAlias(t1)
	}
	if len(t2) >= 2 && t2[0].(string) == "alias" {
		t2 = expandAlias(t2)
	}

	// ---- Primitives (incl. Int ⊔ Num = Num) ----
	if len(t1) >= 2 && len(t2) >= 2 && t1[0].(string) == "id" && t2[0].(string) == "id" {
		n1 := t1[1].(string)
		n2 := t2[1].(string)
		if n1 == n2 {
			return t1
		}
		if (n1 == "Int" && n2 == "Num") || (n1 == "Num" && n2 == "Int") {
			return S{"id", "Num"}
		}
		return S{"id", "Any"}
	}

	// ---- Arrays ----
	if t1[0].(string) == "array" && t2[0].(string) == "array" {
		e1 := S{"id", "Any"}
		e2 := S{"id", "Any"}
		if len(t1) == 2 {
			e1 = t1[1].(S)
		}
		if len(t2) == 2 {
			e2 = t2[1].(S)
		}
		elem := ip.unifyTypes(e1, e2, env)
		return S{"array", elem}
	}
	if t1[0].(string) == "array" || t2[0].(string) == "array" {
		return S{"id", "Any"}
	}

	// ---- Maps: fieldwise unify; required OR ----
	if t1[0].(string) == "map" && t2[0].(string) == "map" {
		f1 := mapTypeFields(t1)
		f2 := mapTypeFields(t2)
		keys := map[string]struct{}{}
		for k := range f1 {
			keys[k] = struct{}{}
		}
		for k := range f2 {
			keys[k] = struct{}{}
		}
		out := S{"map"}
		for k := range keys {
			s1, ok1 := f1[k]
			s2, ok2 := f2[k]
			switch {
			case ok1 && ok2:
				ut := ip.unifyTypes(s1.typ, s2.typ, env)
				req := s1.required && s2.required
				tag := "pair"
				if req {
					tag = "pair!"
				}
				out = append(out, S{tag, S{"str", k}, ut})
			case ok1 && !ok2:
				out = append(out, S{"pair", S{"str", k}, s1.typ})
			case !ok1 && ok2:
				out = append(out, S{"pair", S{"str", k}, s2.typ})
			}
		}
		return out
	}
	if t1[0].(string) == "map" || t2[0].(string) == "map" {
		return S{"id", "Any"}
	}

	// ---- Enums ----
	if t1[0].(string) == "enum" && t2[0].(string) == "enum" {
		union := S{"enum"}
		seen := func(x S) bool {
			for i := 1; i < len(union); i++ {
				if equalLiteralS(union[i].(S), x) {
					return true
				}
			}
			return false
		}
		for i := 1; i < len(t1); i++ {
			x := t1[i].(S)
			if !seen(x) {
				union = append(union, x)
			}
		}
		for i := 1; i < len(t2); i++ {
			x := t2[i].(S)
			if !seen(x) {
				union = append(union, x)
			}
		}
		return union
	}
	// enum ⊔ Type → Type if all members fit; else Any
	if t1[0].(string) == "enum" {
		all := true
		for i := 1; i < len(t1); i++ {
			lv, ok := ip.litToValue(t1[i].(S))
			if !ok || !ip.isType(lv, t2, env) {
				all = false
				break
			}
		}
		if all {
			return t2
		}
		return S{"id", "Any"}
	}
	if t2[0].(string) == "enum" {
		all := true
		for i := 1; i < len(t2); i++ {
			lv, ok := ip.litToValue(t2[i].(S))
			if !ok || !ip.isType(lv, t1, env) {
				all = false
				break
			}
		}
		if all {
			return t1
		}
		return S{"id", "Any"}
	}

	// ---- Functions: pointwise ----
	if t1[0].(string) == "binop" && t2[0].(string) == "binop" && t1[1].(string) == "->" && t2[1].(string) == "->" {
		// f1 = A1 -> B1, f2 = A2 -> B2
		a1, b1 := t1[2].(S), t1[3].(S)
		a2, b2 := t2[2].(S), t2[3].(S)

		// Param GLB (contravariant):
		// if A1 <: A2 → A1; else if A2 <: A1 → A2; else no GLB → give up to Any
		var param S
		if ip.isSubtype(a1, a2, env) {
			param = a1
		} else if ip.isSubtype(a2, a1, env) {
			param = a2
		} else {
			return S{"id", "Any"}
		}

		// Return LUB (covariant):
		ret := ip.unifyTypes(b1, b2, env)
		return S{"binop", "->", param, ret}
	}
	if t1[0].(string) == "binop" || t2[0].(string) == "binop" {
		return S{"id", "Any"}
	}

	return S{"id", "Any"}
}

// validateTypeShape checks that an S-expression is a *type-shaped* AST.
// It is purely syntactic: it does not resolve identifiers or paths.
// Returns "" if OK; otherwise a short human-friendly error message.
//
// Allowed forms (with arity):
//
//	("id", name)
//	("get", base=id|get, ("str", key))
//	("unop","?", T)
//	("binop","->", A, B)
//	("array", T)
//	("map", ("pair"| "pair!", ("str", key), T)*)
//	("enum", <JSON-literal>...)
//	("annot", ("str", doc), T)   // stripped and T validated
//
// Disallowed:
//
//	raw literals as types (e.g., ("int", 1)), ("fun", ...), ("type", ...),
//	("module", ...), raw ("alias", *TypeValue) (internal canonical form),
//	malformed arities, non-string map keys, duplicate keys within one map type.
//
// validateTypeShape checks that an S-expression is a syntactically valid *type*
// shape (purely structural; no name resolution). It returns "" if OK, else a
// short human-readable error message. Aliases/self-references are allowed
// because ("id", ...) and ("get", ...) are accepted without resolution.
//
// Allowed nodes:
//
//	("id", name)
//	("get", base, ("str", key))        // base must be id|get
//	("unop", "?", T)                   // nullable
//	("binop", "->", A, B)              // function type
//	("array", T)                       // exactly one elem type
//	("map", ("pair"| "pair!", ("str",k), T)...)  // no duplicate keys
//	("enum", <anything>)               // JSON-literal check is done elsewhere
//	("annot", ("str", doc), T)         // allowed wrapper anywhere
//
// Disallowed: literal value nodes ("int"/"num"/"str"/... as types), "fun",
// "module", raw internal "alias" nodes, wrong arities, non-string keys, etc.
func validateTypeShape(t S) string {
	// Local helper to strip "annot" wrappers *and* validate their doc fields.
	strip := func(n S) (S, string) {
		for len(n) >= 3 {
			tag, ok := n[0].(string)
			if !ok || tag != "annot" {
				break
			}
			// validate ("annot", ("str", doc), subj)
			doc, ok := n[1].(S)
			if !ok || len(doc) < 2 || doc[0].(string) != "str" {
				return nil, "annotation must have a string doc"
			}
			subj, _ := n[2].(S)
			n = subj
		}
		return n, ""
	}

	var walk func(S) string
	walk = func(n S) string {
		if len(n) == 0 {
			return "invalid type: empty"
		}
		// peel/validate annot wrappers
		var msg string
		if n, msg = strip(n); msg != "" {
			return msg
		}
		if len(n) == 0 {
			return "invalid type: empty"
		}

		tag, ok := n[0].(string)
		if !ok {
			return "invalid type: corrupt node tag"
		}

		switch tag {
		case "id":
			if len(n) != 2 {
				return "id type must have exactly one name"
			}
			if _, ok := n[1].(string); !ok {
				return "id name must be a string"
			}
			return ""

		case "get":
			if len(n) != 3 {
				return `qualified type must be ("get", base, ("str", key))`
			}
			base, okb := n[1].(S)
			if !okb || len(base) == 0 {
				return "qualified type has invalid base"
			}
			if btag, _ := base[0].(string); btag != "id" && btag != "get" && btag != "annot" {
				return "qualified type base must be id or get"
			}
			// walk the base (allows nested get/annot/id)
			if err := walk(base); err != "" {
				return err
			}
			key, okk := n[2].(S)
			if !okk || len(key) < 2 || key[0].(string) != "str" {
				return "qualified type key must be a string literal"
			}
			return ""

		case "unop":
			if len(n) != 3 || n[1] != "?" {
				return `nullable type must be ("unop", "?", T)`
			}
			sub, _ := n[2].(S)
			return walk(sub)

		case "binop":
			if len(n) != 4 || n[1] != "->" {
				return `function type must be ("binop", "->", A, B)`
			}
			if err := walk(n[2].(S)); err != "" {
				return err
			}
			return walk(n[3].(S))

		case "array":
			if len(n) != 2 {
				return "array type must have exactly one element type"
			}
			return walk(n[1].(S))

		case "map":
			seen := map[string]struct{}{}
			for i := 1; i < len(n); i++ {
				p, okp := n[i].(S)
				if !okp || len(p) < 3 {
					return `map field must be ("pair"|"pair!", ("str", key), T)`
				}
				ptag, ok := p[0].(string)
				if !ok || (ptag != "pair" && ptag != "pair!") {
					return `map field must start with "pair" or "pair!"`
				}
				key, okk := p[1].(S)
				if !okk || len(key) < 2 || key[0].(string) != "str" {
					return "map field key must be a string literal"
				}
				k := key[1].(string)
				if _, dup := seen[k]; dup {
					return "duplicate field '" + k + "' in map type"
				}
				seen[k] = struct{}{}
				if err := walk(p[2].(S)); err != "" {
					return err
				}
			}
			return ""

		case "enum":
			// Shape is fine here; members being JSON-literals is validated
			// by validateEnumsJSONOnly at construction call-sites.
			return ""

		case "annot":
			// Should have been stripped; treat as error to avoid silent loops.
			return "internal error: unexpected annot after strip"

		// Explicitly disallow internal/user-nonsensical nodes in type shapes.
		case "int", "num", "str", "bool", "null", "fun", "module", "type", "alias":
			return "invalid type: unexpected node " + tag

		default:
			return "invalid type: unexpected node " + tag
		}
	}

	return walk(t)
}

// -----------------------------
// Enum literal-only validation
// -----------------------------

// isJSONLiteralNode reports whether n is one of the literal forms that
// litToValue accepts: null/bool/int/num/str/array/map (with literal children).
func isJSONLiteralNode(n S) bool {
	if len(n) == 0 {
		return false
	}
	switch n[0].(string) {
	case "null", "bool", "int", "num", "str":
		return true
	case "array":
		for i := 1; i < len(n); i++ {
			child, ok := n[i].(S)
			if !ok || !isJSONLiteralNode(child) {
				return false
			}
		}
		return true
	case "map":
		for i := 1; i < len(n); i++ {
			p, ok := n[i].(S)
			if !ok || len(p) < 3 {
				return false
			}
			// p = ("pair" | "pair!", ("str", key), valueLit)
			tag := p[0].(string)
			if tag != "pair" && tag != "pair!" {
				return false
			}
			key, ok := p[1].(S)
			if !ok || len(key) < 2 || key[0].(string) != "str" {
				return false
			}
			val, ok := p[2].(S)
			if !ok || !isJSONLiteralNode(val) {
				return false
			}
		}
		return true
	default:
		return false
	}
}

// validateEnumsJSONOnly walks a type AST and ensures any ("enum", ...)
// contains only JSON-style literals. It returns an empty string if OK,
// otherwise a short human message.
func validateEnumsJSONOnly(t S) string {
	if len(t) == 0 {
		return ""
	}
	switch t[0].(string) {
	case "annot":
		if len(t) >= 3 {
			if sub, ok := t[2].(S); ok {
				return validateEnumsJSONOnly(sub)
			}
		}
		return ""
	case "enum":
		for i := 1; i < len(t); i++ {
			member, ok := t[i].(S)
			if !ok || !isJSONLiteralNode(member) {
				return "Enum members must be JSON literals (null/bool/int/num/str/array/map of literals)"
			}
		}
		return ""
	case "unop", "binop", "array", "map", "get", "id", "alias", "type", "module":
		for i := 1; i < len(t); i++ {
			if sub, ok := t[i].(S); ok {
				if msg := validateEnumsJSONOnly(sub); msg != "" {
					return msg
				}
			}
		}
		return ""
	default:
		return ""
	}
}
