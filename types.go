// types.go
//
// MindScript Type & Schema system (runtime-focused)
//
// Goals / design (practical, JSON-friendly, duck-typed):
//  1. “Natural” typeOf: For concrete values, infer loose shapes. Example:
//     typeOf({name:"Pedro"}) = {name: Str}          // optional field
//     Arrays unify element types conservatively (e.g., [1, null] → [Int?]).
//  2. Precise runtime checks:
//     - isType(v, T) checks v conforms to T.
//     - isSubtype(A, B) is structural and sound.
//     - Objects: required fields must not be relaxed; optional fields can be absent.
//     - Extra fields in values are allowed (open-world objects).
//  3. Enums as finite literal sets (not general sum types):
//     Enum members can be arbitrary literals (null/bool/int/num/str/array/map).
//     isType compares by deep equality to literal values.
//     Subtyping:
//     • Enum[X...] <: Enum[Y...]  iff every X is in Y (by structural equality).
//     • Enum[X...] <: T           iff every literal X conforms to T (great for duck-typing).
//     Unification:
//     • Enum ∪ Enum  = structural set union.
//     • Enum ∪ T     = T if all literals ∈ T; otherwise Any.
//  4. No open sum types beyond T? (nullable) and Any (top).
//  5. Function types use parameter **contravariance** and return **covariance**.
//     (Matches soundness for callable subtyping; works well with currying).
//
// Implementation notes:
//   - Types are S-exprs (same shape as AST):
//     ("id","Int"|"Num"|"Str"|"Bool"|"Null"|"Any"|"Type")
//     ("unop","?", T)                         // nullable
//     ("array", T)                            // homogeneous arrays
//     ("map", ("pair" | "pair!", ("str",k), T) ...)
//     ("enum", literalS, literalS, ...)
//     ("binop","->", A, B)                    // function A -> B (right-assoc chains)
//   - Aliases: ("id", name) resolved from env when bound to VTType (spliced S).
//   - This file provides: resolveType, isType, isSubtype, unifyTypes, valueToTypeS,
//     and helpers (equalS/mapTypeFields/deopt/...).
package mindscript

// NOTE: no imports needed

// -----------------------------
// Helpers
// -----------------------------

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
		fs[k] = objField{required: required, typ: p[2].(S)}
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

// deopt: ("unop","?", X) => (X, true);  Null => (Any, true) for unification purposes
func deopt(t S) (base S, opt bool) {
	if len(t) == 0 {
		return S{"id", "Any"}, false
	}
	switch t[0].(string) {
	case "unop":
		if len(t) >= 3 && t[1].(string) == "?" {
			return t[2].(S), true
		}
	case "id":
		if t[1].(string) == "Null" {
			return S{"id", "Any"}, true
		}
	}
	return t, false
}

func reopt(base S, opt bool) S {
	if !opt {
		return base
	}
	return S{"unop", "?", base}
}

// Deep equality on Values for the literal space used by enums (null/bool/int/num/str/array/map).
func deepEqual(a, b Value) bool {
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
		ax := a.Data.([]Value)
		bx := b.Data.([]Value)
		if len(ax) != len(bx) {
			return false
		}
		for i := range ax {
			if !deepEqual(ax[i], bx[i]) {
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
			if !ok || !deepEqual(av, bv) {
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

func (ip *Interpreter) valueToTypeS(v Value, env *Env) S {
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
		xs := v.Data.([]Value)
		if len(xs) == 0 {
			return S{"array", S{"id", "Any"}}
		}
		elem := ip.valueToTypeS(xs[0], env)
		for i := 1; i < len(xs); i++ {
			elem = ip.unifyTypes(elem, ip.valueToTypeS(xs[i], env), env)
		}
		return S{"array", elem}

	case VTMap:
		mo := v.Data.(*MapObject)
		out := S{"map"}
		// For a single observed map, infer optional fields (no required).
		for k, vv := range mo.Entries {
			out = append(out, S{"pair", S{"str", k}, ip.valueToTypeS(vv, env)})
		}
		return out

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

// -----------------------------
// Alias resolution for types
// -----------------------------

func (ip *Interpreter) resolveType(t S, env *Env) S {
	seen := map[string]bool{}
	var go1 func(S, *Env) S
	go1 = func(x S, e *Env) S {
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

func (ip *Interpreter) isType(v Value, t S, env *Env) bool {
	t = ip.resolveType(t, env)
	if len(t) == 0 {
		return false
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
			return v.Tag == VTInt || v.Tag == VTNum // Int <: Num
		case "Str":
			return v.Tag == VTStr
		case "Type":
			return v.Tag == VTType
		default:
			return false
		}

	case "unop":
		if t[1].(string) != "?" {
			return false
		}
		if v.Tag == VTNull {
			return true
		}
		return ip.isType(v, t[2].(S), env)

	case "array":
		if v.Tag != VTArray {
			return false
		}
		// If no element type provided, treat as [Any]
		elemT := S{"id", "Any"}
		if len(t) == 2 {
			elemT = t[1].(S)
		}
		for _, elem := range v.Data.([]Value) {
			if !ip.isType(elem, elemT, env) {
				return false
			}
		}
		return true

	case "map":
		if v.Tag != VTMap {
			return false
		}
		fs := mapTypeFields(t)
		mo := v.Data.(*MapObject)
		for name, f := range fs {
			val, ok := mo.Entries[name]
			if !ok {
				if f.required {
					return false
				}
				continue
			}
			if !ip.isType(val, f.typ, env) {
				return false
			}
		}
		return true

	case "enum":
		// Deeply equal to one of the literal values
		for i := 1; i < len(t); i++ {
			lv, ok := ip.litToValue(t[i].(S))
			if !ok {
				continue
			}
			if deepEqual(lv, v) {
				return true
			}
		}
		return false

	case "binop":
		if t[1].(string) != "->" || v.Tag != VTFun {
			return false
		}
		// value's function type
		f := v.Data.(*Fun)
		ft := f.ReturnType
		for i := len(f.ParamTypes) - 1; i >= 0; i-- {
			ft = S{"binop", "->", f.ParamTypes[i], ft}
		}
		return ip.isSubtype(ft, t, env)
	}

	return false
}

// -----------------------------
// Structural subtyping  t1 <: t2
// -----------------------------

func (ip *Interpreter) isSubtype(a, b S, env *Env) bool {
	a = ip.resolveType(a, env)
	b = ip.resolveType(b, env)

	if equalS(a, b) {
		return true
	}

	isOpt := func(t S) bool { return len(t) >= 2 && t[0].(string) == "unop" && t[1].(string) == "?" }
	unwrapOpt := func(t S) S { return t[2].(S) }

	// Top
	if b[0].(string) == "id" && b[1].(string) == "Any" {
		return true
	}
	// Optional RHS
	if isOpt(b) {
		inner := unwrapOpt(b)
		if ip.isSubtype(a, inner, env) {
			return true
		}
		if a[0].(string) == "id" && a[1].(string) == "Null" {
			return true
		}
	}
	// Int <: Num
	if a[0].(string) == "id" && a[1].(string) == "Int" &&
		b[0].(string) == "id" && b[1].(string) == "Num" {
		return true
	}
	// A? <: B?  iff  A <: B
	if isOpt(a) && isOpt(b) {
		return ip.isSubtype(unwrapOpt(a), unwrapOpt(b), env)
	}
	// Identical primitive IDs
	if a[0].(string) == "id" && b[0].(string) == "id" {
		return a[1].(string) == b[1].(string)
	}

	// Arrays: elementwise covariance
	if a[0].(string) == "array" && b[0].(string) == "array" {
		elemA := S{"id", "Any"}
		if len(a) == 2 {
			elemA = a[1].(S)
		}
		elemB := S{"id", "Any"}
		if len(b) == 2 {
			elemB = b[1].(S)
		}
		return ip.isSubtype(elemA, elemB, env)
	}

	// Maps: must provide all required fields of b with compatible types,
	// and cannot relax requiredness.
	if a[0].(string) == "map" && b[0].(string) == "map" {
		reqB := mapTypeFields(b)
		haveA := mapTypeFields(a)
		for name, fb := range reqB {
			fa, ok := haveA[name]
			if !ok {
				if fb.required {
					return false
				}
				continue
			}
			if fb.required && !fa.required {
				return false
			}
			if !ip.isSubtype(fa.typ, fb.typ, env) {
				return false
			}
		}
		return true
	}

	// Enums:
	// 1) Enum ⊆ Enum by literal inclusion (structural equality on literal S)
	if a[0].(string) == "enum" && b[0].(string) == "enum" {
		for i := 1; i < len(a); i++ {
			found := false
			for j := 1; j < len(b); j++ {
				if equalS(a[i].(S), b[j].(S)) {
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
	// 2) Enum <: T if every member conforms to T
	if a[0].(string) == "enum" {
		for i := 1; i < len(a); i++ {
			lv, ok := ip.litToValue(a[i].(S))
			if !ok || !ip.isType(lv, b, env) {
				return false
			}
		}
		return true
	}

	// Functions: param contravariant, return covariant (right-assoc)
	if a[0].(string) == "binop" && a[1].(string) == "->" &&
		b[0].(string) == "binop" && b[1].(string) == "->" {
		aParam, aRest := a[2].(S), a[3].(S)
		bParam, bRest := b[2].(S), b[3].(S)
		if !ip.isSubtype(bParam, aParam, env) { // contra
			return false
		}
		return ip.isSubtype(aRest, bRest, env) // co (right)
	}

	return false
}

// -----------------------------
// Unification (least common supertype)
// -----------------------------

func (ip *Interpreter) unifyTypes(t1 S, t2 S, env *Env) S {
	t1 = ip.resolveType(t1, env)
	t2 = ip.resolveType(t2, env)

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
		inU := ip.unifyTypes(t1[2].(S), t2[2].(S), env)
		outU := ip.unifyTypes(t1[3].(S), t2[3].(S), env)
		return S{"binop", "->", inU, outU}
	}
	if t1[0].(string) == "binop" || t2[0].(string) == "binop" {
		return S{"id", "Any"}
	}

	return S{"id", "Any"}
}
