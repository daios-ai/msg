// types.go: runtime type & schema system for MindScript.
//
// IMPLEMENTATION-ONLY FILE
// ------------------------
// This file contains the *private implementation* of the type engine used by
// the public methods on *Interpreter* that live in interpreter.go:
//
//	ResolveType, IsType, IsSubtype, UnifyTypes, ValueToType.
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
//   - `ResolveType(T, env)` resolves identifiers bound to `VTType` in `env`,
//     with cycle protection and resolution using the alias’s own environment.
//   - Enums are finite sets of *literal* values (null/bool/int/num/str/array/map).
//     Their typing, subtyping, and unification follow intuitive set semantics.
//   - **Modules:** Runtime values tagged `VTModule` are treated as **maps** for
//     all type-checking and inference purposes (they normalize via `AsMapValue`).
//
// TYPE SYNTAX (as S)
// ------------------
//
//	("id","Int"|"Num"|"Str"|"Bool"|"Null"|"Any"|"Type")
//	("unop","?", T)                                // nullable T?
//	("array", T)                                   // homogeneous arrays
//	("map", ("pair" | "pair!", ("str",k), T) ...)  // object schema; "pair!" = required
//	("enum", literalS, ...)                        // finite set of literal values
//	("binop","->", A, B)                           // function A -> B (right-assoc)
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
//
//	isSubtype, unifyTypes, valueToTypeS, literal conversion,
//	structural S-equality, and field extraction.
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

// Structural equality for S-exprs (no fmt, no allocs).
func equalS(a, b S) bool {
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

	// Order-insensitive equality for maps: compare by field name/required/type.
	if ta == "map" {
		fa := mapTypeFields(a)
		fb := mapTypeFields(b)
		if len(fa) != len(fb) {
			return false
		}
		for k, va := range fa {
			vb, ok := fb[k]
			if !ok || va.required != vb.required || !equalS(va.typ, vb.typ) {
				return false
			}
		}
		return true
	}

	// Order-insensitive equality for enums: set comparison on literal S-exprs.
	if ta == "enum" {
		if len(a) != len(b) { // quick length check
			return false
		}
	outer:
		for i := 1; i < len(a); i++ {
			for j := 1; j < len(b); j++ {
				if equalS(a[i].(S), b[j].(S)) {
					continue outer
				}
			}
			return false
		}
		return true
	}

	for i := 1; i < len(a); i++ {
		if !equalNode(a[i], b[i]) {
			return false
		}
	}
	return true
}

// Compares two S nodes or scalars used inside S.
func equalNode(x, y any) bool {
	switch xv := x.(type) {
	case []any: // covers S too
		yv, ok := y.([]any)
		if !ok {
			return false
		}
		return equalS(xv, yv)
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

// Deep equality on Values for the literal space used by enums
// (null/bool/int/num/str/array/map). Named *Lit* to avoid confusion with
// (*Interpreter).deepEqual used elsewhere.
func deepEqualLit(a, b Value) bool {
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
		ax := a.Data.(*ArrayObject).Elems
		bx := b.Data.(*ArrayObject).Elems
		if len(ax) != len(bx) {
			return false
		}
		for i := range ax {
			if !deepEqualLit(ax[i], bx[i]) {
				return false
			}
		}
		return true
	case VTMap:
		am := a.Data.(*MapObject)
		bm := b.Data.(*MapObject)
		if len(am.Entries) != len(bm.Entries) {
			return false
		}
		for k, av := range am.Entries {
			bv, ok := bm.Entries[k]
			if !ok || !deepEqualLit(av, bv) {
				return false
			}
		}
		return true
	default:
		// Not expected for enum literals
		return false
	}
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

	var go1 func(Value) S
	go1 = func(v Value) S {
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

			elem := go1(xs[0])
			for i := 1; i < len(xs); i++ {
				elem = ip.unifyTypes(elem, go1(xs[i]), env)
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
				out = append(out, S{"pair", S{"str", k}, go1(vv)})
			}
			return out

		case VTModule:
			// Treat modules structurally as maps.
			return go1(AsMapValue(v))

		case VTFun:
			f := v.Data.(*Fun)
			t := f.ReturnType
			for i := len(f.ParamTypes) - 1; i >= 0; i-- {
				t = S{"binop", "->", f.ParamTypes[i], t}
			}
			return t

		case VTType:
			return S{"id", "Type"}

		default:
			return S{"id", "Any"}
		}
	}

	return go1(v)
}

// -----------------------------
// Alias resolution for types
// -----------------------------

func (ip *Interpreter) resolveType(t S, env *Env) S {
	t = stripAnnot(t)
	seen := map[string]bool{}
	var go1 func(S, *Env) S
	go1 = func(x S, e *Env) S {
		// IMPORTANT: strip on the current node (x), not the outer t.
		x = stripAnnot(x)
		if len(x) == 0 {
			return x
		}
		switch x[0].(string) {
		case "id":
			name := x[1].(string)
			if isBuiltinTypeName(name) {
				return x
			}
			if seen[name] {
				return x // guard cycles
			}
			if e != nil {
				if v, err := e.Get(name); err == nil && v.Tag == VTType {
					tv := v.Data.(*TypeValue)
					seen[name] = true
					defer func() { delete(seen, name) }()
					// recurse with the alias's own env (not the caller's env)
					return go1(tv.Ast, tv.Env)
				}
			}
			return x
		case "unop":
			if len(x) >= 3 && x[1].(string) == "?" {
				return S{"unop", "?", go1(x[2].(S), e)}
			}
			return x
		case "array":
			if len(x) == 2 {
				return S{"array", go1(x[1].(S), e)}
			}
			out := S{"array"}
			for i := 1; i < len(x); i++ {
				out = append(out, go1(x[i].(S), e))
			}
			return out
		case "map":
			out := S{"map"}
			for i := 1; i < len(x); i++ {
				p := x[i].(S)
				ttag := p[0].(string)
				key := p[1].(S)
				typ := go1(p[2].(S), e)
				out = append(out, S{ttag, key, typ})
			}
			return out
		case "enum":
			return x
		case "binop":
			if len(x) >= 4 && x[1].(string) == "->" {
				return S{"binop", "->", go1(x[2].(S), e), go1(x[3].(S), e)}
			}
			return x
		default:
			return x
		}
	}
	return go1(t, env)
}

// -----------------------------
// Runtime type checking
// -----------------------------

// isType is the public-impl entry; it seeds cycle guards for runtime values.
// isType checks whether runtime value v conforms to type t, with cycle guards
// for arrays/maps to avoid non-termination on cyclic graphs. Modules are treated
// structurally as maps. Annotations in type ASTs are ignored.
// isType checks whether runtime value v conforms to type t in env.
// Cycle-safe via coinductive memo on (value,type).
// isType checks whether runtime value v conforms to type t in env.
// It is cycle-safe via a coinductive memo keyed by (value-pointer, type-node-pointer).
func (ip *Interpreter) isType(v Value, t S, env *Env) bool {
	t = stripAnnot(ip.resolveType(t, env))

	// Coinductive memo: only set for pointer-backed values (maps/arrays/funs).
	seen := make(map[[2]unsafe.Pointer]struct{})

	var check func(Value, S) bool
	check = func(v Value, t S) bool {
		t = stripAnnot(t)
		if len(t) == 0 {
			return false
		}

		// Modules behave structurally as maps.
		v = AsMapValue(v)

		// Build memo key (value pointer, type-node pointer).
		var vp unsafe.Pointer
		switch v.Tag {
		case VTMap:
			vp = unsafe.Pointer(v.Data.(*MapObject))
		case VTArray:
			vp = unsafe.Pointer(v.Data.(*ArrayObject))
		case VTFun:
			vp = unsafe.Pointer(v.Data.(*Fun))
		default:
			vp = nil // scalars don’t participate in cycles; skip memo for them
		}
		tp := unsafe.Pointer(&t[0]) // address of this AST node’s tag
		if vp != nil {
			key := [2]unsafe.Pointer{vp, tp}
			if _, ok := seen[key]; ok {
				return true // coinductive success on the same (v,t) pair
			}
			seen[key] = struct{}{}
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
				return false
			}

		case "unop": // nullable T?
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
				if lit, ok := ip.litToValue(t[i].(S)); ok && deepEqualLit(lit, v) {
					return true
				}
			}
			return false

		case "binop": // function type
			if t[1].(string) != "->" || v.Tag != VTFun {
				return false
			}
			f := v.Data.(*Fun)
			ft := f.ReturnType
			for i := len(f.ParamTypes) - 1; i >= 0; i-- {
				ft = S{"binop", "->", f.ParamTypes[i], ft}
			}
			return ip.isSubtype(ft, t, env)
		}

		return false
	}

	return check(v, t)
}

// -----------------------------
// Structural subtyping  t1 <: t2
// -----------------------------

// isSubtype checks structural subtyping with a tiny coinductive memo to break cycles.
// isSubtype — compact, coinductive (cycle-safe) structural subtyping.
// Uses a tiny memo keyed by the memory addresses of the compared S-nodes.
// isSubtype — compact, cycle-safe (coinductive) structural subtyping.
// Uses address-based memo keys for recursion, keeping it hot-path friendly.
// isSubtype checks structural subtyping a <: b with coinductive memoization.
// It memoizes on the pair of underlying S nodes using raw pointers for speed.
func (ip *Interpreter) isSubtype(a, b S, env *Env) bool {
	a = stripAnnot(ip.resolveType(a, env))
	b = stripAnnot(ip.resolveType(b, env))

	seen := make(map[[2]unsafe.Pointer]struct{})

	var sub func(S, S) bool
	sub = func(x, y S) bool {
		x = stripAnnot(x)
		y = stripAnnot(y)
		if len(x) == 0 || len(y) == 0 {
			return false
		}

		// Coinductive memo key: (address of x[0], address of y[0])
		key := [2]unsafe.Pointer{unsafe.Pointer(&x[0]), unsafe.Pointer(&y[0])}
		if _, ok := seen[key]; ok {
			return true
		}
		seen[key] = struct{}{}

		// Fast-path equal
		if equalS(x, y) {
			return true
		}

		// Top
		if isId(y, "Any") {
			return true
		}

		// Nullable helpers
		isOpt := func(t S) bool { return len(t) >= 3 && t[0].(string) == "unop" && t[1].(string) == "?" }
		unwrap := func(t S) S { return t[2].(S) }

		// A <: B?  if  A <: B  or  A == Null
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
						if equalS(x[i].(S), y[j].(S)) {
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
				if !ok || !ip.isType(lv, y, env) {
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
	t1 = stripAnnot(ip.resolveType(t1, env))
	t2 = stripAnnot(ip.resolveType(t2, env))

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
				req := s1.required || s2.required
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
				if equalS(union[i].(S), x) {
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
