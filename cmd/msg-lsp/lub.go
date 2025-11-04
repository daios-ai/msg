// cmd/msg-lsp/lub.go
//
// Package-local LUB/GLB/Subtyping for MindScript S-expr types.
//
// What this file does
// -------------------
// Implements a **single-pass, static** type **Least Upper Bound (LUB)**,
// a minimal **Greatest Lower Bound (GLB)** (only needed for function-parameter
// contravariance), and a **structural subtyping predicate** over MindScript’s
// S-expr type nodes.
//
// Public surface for analysis.go
// ------------------------------
//   • func LUB(a, b []any) []any
//       – Spec-faithful LUB over type nodes, including nullability sugar,
//         arrays (covariant), maps (field-wise on intersection; required iff
//         required in both), enums (set union / widen), and arrows
//         (GLB on params, LUB on returns). Opaque non-builtin IDs are treated
//         as atoms: same-ID ⇒ itself, otherwise widen to Any.
//
//   • func GLB(a, b []any) ([]any, bool)
//       – Minimal GLB used by LUB for function parameters. Handles primitives,
//         arrays, type-maps (union of keys; required if either; per-field GLB
//         with Any fallback), enums (set intersection), and nullability rules.
//         Returns ok=false when no meaningful meet exists.
//
//   • func IsSubtypeStatic(a, b []any) bool
//       – **Structural** subtyping check used by the analyzer for conformance
//         tests (e.g., return-site checks and call-site argument checks).
//         Obeys the spec: Int <: Num; arrays covariant; maps require all
//         required fields and check optionals if present; arrows are
//         contravariant in parameters and covariant in returns; enums are
//         subset-checked; nullable is handled uniformly; non-builtin IDs are
//         opaque atoms (identical-id only).
//
// Data structures visible to analysis.go
// --------------------------------------
// None beyond the S-expr `[]any` nodes it already uses.
//
// Notes
// -----
// • This file does **not** unfold user aliases; only built-ins (`Any, Null,
//   Bool, Int, Num, Str, Type`) are special. Keep results compact (no shape
//   invention beyond the spec rules).
// • The public functions are thin shims; implementation details live below
//   the `//// END_OF_PUBLIC` marker.

package main

import "reflect"

// -----------------------------
// Public shims (stable surface)
// -----------------------------

// LUB computes the least upper bound of two MindScript type S-exprs.
func LUB(a, b []any) []any { return lubImpl(a, b) }

// GLB computes a minimal greatest lower bound. ok=false means no GLB exists.
func GLB(a, b []any) ([]any, bool) { return glbImpl(a, b) }

// IsSubtypeStatic reports whether a <: b under the analyzer's static,
// structural rules (uniform nullability, arrays covariant, maps with
// required/optional semantics, arrows contra/co, enums by set-inclusion,
// opaque aliases compare by atom identity).
func IsSubtypeStatic(a, b []any) bool { return isSubtypeImpl(a, b) }

//// END_OF_PUBLIC

// -----------------------------
// Implementation (private)
// -----------------------------

// --- tiny predicates ---------------------------------------------------------

func isID(t []any, name string) bool { return len(t) >= 2 && t[0] == "id" && t[1] == name }
func isAny(t []any) bool             { return isID(t, "Any") }
func isNull(t []any) bool            { return isID(t, "Null") }

func isPrim(t []any) bool {
	if len(t) < 2 || t[0] != "id" {
		return false
	}
	switch t[1] {
	case "Any", "Null", "Bool", "Int", "Num", "Str", "Type":
		return true
	default:
		return false
	}
}

func primLUB(a, b []any) []any {
	// equal handled by caller; Any handled by caller.
	if (isID(a, "Int") && isID(b, "Num")) || (isID(a, "Num") && isID(b, "Int")) {
		return typeID("Num")
	}
	// unrelated primitives widen to Any
	return typeID("Any")
}

func primSubtype(a, b []any) bool {
	if reflect.DeepEqual(a, b) {
		return true
	}
	if isID(a, "Int") && isID(b, "Num") {
		return true
	}
	if isAny(b) {
		return true
	}
	return false
}

func peelNullable(t []any) (base []any, nullable bool, wasNull bool) {
	if len(t) >= 3 && t[0] == "unop" && t[1] == "?" {
		if inner, ok := t[2].([]any); ok {
			return inner, true, false
		}
	}
	if isNull(t) {
		return nil, true, true
	}
	return t, false, false
}
func addNullable(t []any, q bool) []any {
	if q {
		return []any{"unop", "?", t}
	}
	return t
}

func isArray(t []any) bool { return len(t) >= 2 && t[0] == "array" }
func arrElem(t []any) []any {
	if len(t) >= 2 {
		if e, ok := t[1].([]any); ok {
			return e
		}
	}
	return typeID("Any")
}

type fieldInfo struct {
	typ      []any
	required bool
}

func isMap(t []any) bool { return len(t) >= 1 && t[0] == "map" }
func mapFields(t []any) map[string]fieldInfo {
	m := map[string]fieldInfo{}
	if !isMap(t) {
		return m
	}
	for i := 1; i < len(t); i++ {
		pr, ok := t[i].([]any)
		if !ok || len(pr) < 3 {
			continue
		}
		tag, _ := pr[0].(string)
		if tag != "pair" && tag != "pair!" {
			continue
		}
		k, _ := pr[1].([]any)
		if len(k) < 2 || k[0] != "str" {
			continue
		}
		key, _ := k[1].(string)
		tv, _ := pr[2].([]any)
		m[key] = fieldInfo{typ: tv, required: tag == "pair!"}
	}
	return m
}
func makeMapFrom(fields map[string]fieldInfo) []any {
	out := []any{"map"}
	for k, fi := range fields {
		tag := "pair"
		if fi.required {
			tag = "pair!"
		}
		out = append(out, []any{tag, []any{"str", k}, fi.typ})
	}
	return out
}

func isEnum(t []any) bool { return len(t) >= 1 && t[0] == "enum" }
func enumKind(t []any) string {
	if !isEnum(t) || len(t) < 2 {
		return ""
	}
	for i := 1; i < len(t); i++ {
		if m, ok := t[i].([]any); ok && len(m) >= 2 {
			if k, _ := m[0].(string); k == "str" || k == "int" || k == "num" || k == "bool" {
				return k
			}
		}
	}
	return ""
}
func enumMembers(t []any) []any {
	if !isEnum(t) {
		return nil
	}
	out := make([]any, 0, len(t)-1)
	for i := 1; i < len(t); i++ {
		if m, ok := t[i].([]any); ok && len(m) >= 2 {
			out = append(out, m)
		}
	}
	return out
}
func enumOf(_kind string, members []any) []any {
	out := []any{"enum"}
	out = append(out, members...)
	return out
}
func enumBasePrim(kind string) []any {
	switch kind {
	case "str":
		return typeID("Str")
	case "int":
		return typeID("Int")
	case "num":
		return typeID("Num")
	case "bool":
		return typeID("Bool")
	default:
		return typeID("Any")
	}
}

func isArrow(t []any) bool { return len(t) >= 4 && t[0] == "binop" && t[1] == "->" }
func arrowParts(t []any) (param, ret []any) {
	if !isArrow(t) {
		return typeID("Any"), typeID("Any")
	}
	p, _ := t[2].([]any)
	r, _ := t[3].([]any)
	return p, r
}

func eq(a, b []any) bool { return reflect.DeepEqual(a, b) }

// --- Subtyping (structural; analyzer-side) ----------------------------------

func isSubtypeImpl(a, b []any) bool {
	// Any is top
	if isAny(b) {
		return true
	}
	// Exact equality
	if eq(a, b) {
		return true
	}

	// Uniform nullable handling
	ab, aq, anull := peelNullable(a)
	bb, bq, bnull := peelNullable(b)

	// Null <: T? (and only then)
	if anull {
		return bnull || bq
	}
	// T? ⊄ U unless U is also nullable
	if aq && !bq {
		return false
	}
	// T? <: U? iff T <: U
	if aq && bq {
		return isSubtypeImpl(ab, bb)
	}
	// T <: U? iff T <: U
	if !aq && bq {
		return isSubtypeImpl(ab, bb)
	}
	// From here both are non-nullable base types ab, bb

	// Primitives (Int <: Num)
	if isPrim(ab) && isPrim(bb) {
		return primSubtype(ab, bb)
	}

	// Arrays covariant
	if isArray(ab) && isArray(bb) {
		return isSubtypeImpl(arrElem(ab), arrElem(bb))
	}

	// Maps (open-world): required keys must exist and subtype; optional keys
	// must subtype only if present on the left.
	if isMap(ab) && isMap(bb) {
		ma, mb := mapFields(ab), mapFields(bb)
		for k, fb := range mb {
			fa, ok := ma[k]
			if fb.required {
				if !ok || !isSubtypeImpl(fa.typ, fb.typ) {
					return false
				}
			} else {
				if ok && !isSubtypeImpl(fa.typ, fb.typ) {
					return false
				}
			}
		}
		return true
	}

	// Enums: subset (same literal kind)
	if isEnum(ab) && isEnum(bb) {
		ka, kb := enumKind(ab), enumKind(bb)
		if ka == "" || ka != kb {
			return false
		}
		A, B := enumMembers(ab), enumMembers(bb)
		for _, x := range A {
			found := false
			for _, y := range B {
				if reflect.DeepEqual(x, y) {
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
	// Enum <: base primitive (e.g., Enum["r","g"] <: Str)
	if isEnum(ab) && isPrim(bb) && eq(enumBasePrim(enumKind(ab)), bb) {
		return true
	}

	// Arrows: contravariant in params, covariant in return
	if isArrow(ab) && isArrow(bb) {
		ap, ar := arrowParts(ab)
		bp, br := arrowParts(bb)
		return isSubtypeImpl(bp, ap) && isSubtypeImpl(ar, br)
	}

	// Opaque non-builtin aliases: identical id only
	if len(ab) >= 2 && ab[0] == "id" && !isPrim(ab) {
		return reflect.DeepEqual(ab, bb)
	}
	return false
}

// --- GLB (minimal; for function params) --------------------------------------

func glbImpl(a, b []any) ([]any, bool) {
	ab, aq, anull := peelNullable(a)
	bb, bq, bnull := peelNullable(b)

	// Null rules
	if anull && bnull {
		return typeID("Null"), true
	}
	// both nullable: GLB(inner)?
	if aq && bq {
		if g, ok := glbImpl(ab, bb); ok {
			return addNullable(g, true), true
		}
		return nil, false
	}
	// mixed nullable vs non-nullable: inner GLB must exist
	if aq != bq || anull || bnull {
		if g, ok := glbImpl(ab, bb); ok {
			return g, true
		}
		return nil, false
	}

	if eq(ab, bb) {
		return ab, true
	}
	// Any identity
	if isAny(ab) {
		return bb, true
	}
	if isAny(bb) {
		return ab, true
	}

	// Primitives
	if isPrim(ab) && isPrim(bb) {
		if (isID(ab, "Int") && isID(bb, "Num")) || (isID(ab, "Num") && isID(bb, "Int")) {
			return typeID("Int"), true
		}
		if primSubtype(ab, bb) {
			return ab, true
		}
		if primSubtype(bb, ab) {
			return bb, true
		}
		return nil, false
	}

	// Arrays
	if isArray(ab) && isArray(bb) {
		ge, ok := glbImpl(arrElem(ab), arrElem(bb))
		if !ok {
			return nil, false
		}
		return []any{"array", ge}, true
	}

	// Type maps (contravariant): union of keys; field GLB (fallback Any); required if either
	if isMap(ab) && isMap(bb) {
		ma, mb := mapFields(ab), mapFields(bb)
		keys := map[string]struct{}{}
		for k := range ma {
			keys[k] = struct{}{}
		}
		for k := range mb {
			keys[k] = struct{}{}
		}
		out := map[string]fieldInfo{}
		for k := range keys {
			fa, oka := ma[k]
			fb, okb := mb[k]
			var ta, tb []any
			req := false
			if oka {
				ta = fa.typ
				req = req || fa.required
			}
			if okb {
				tb = fb.typ
				req = req || fb.required
			}
			g, ok := glbImpl(ta, tb)
			if !ok {
				g = typeID("Any")
			}
			out[k] = fieldInfo{typ: g, required: req}
		}
		return makeMapFrom(out), true
	}

	// Enums (same kind): intersection (if empty → no GLB)
	if isEnum(ab) && isEnum(bb) {
		ka, kb := enumKind(ab), enumKind(bb)
		if ka != "" && ka == kb {
			ma, mb := enumMembers(ab), enumMembers(bb)
			inter := []any{}
			for _, x := range ma {
				found := false
				for _, y := range mb {
					if reflect.DeepEqual(x, y) {
						found = true
						break
					}
				}
				if found {
					inter = append(inter, x)
				}
			}
			if len(inter) == 0 {
				return nil, false
			}
			return enumOf(ka, inter), true
		}
		return nil, false
	}

	// Enums with primitive of same base: GLB is the enum (subset of primitive)
	if isEnum(ab) && isPrim(bb) {
		k := enumKind(ab)
		switch {
		case k == "str" && isID(bb, "Str"),
			k == "int" && isID(bb, "Int"),
			k == "num" && isID(bb, "Num"),
			k == "bool" && isID(bb, "Bool"):
			return ab, true
		}
	}
	if isEnum(bb) && isPrim(ab) {
		k := enumKind(bb)
		switch {
		case k == "str" && isID(ab, "Str"),
			k == "int" && isID(ab, "Int"),
			k == "num" && isID(ab, "Num"),
			k == "bool" && isID(ab, "Bool"):
			return bb, true
		}
	}

	// Opaque alias: same id → itself; else no GLB
	if len(ab) >= 2 && ab[0] == "id" && !isPrim(ab) && reflect.DeepEqual(ab, bb) {
		return ab, true
	}

	// Arrow GLB not defined (contravariant meet of arrows rarely needed here)
	return nil, false
}

// --- LUB (spec-faithful) -----------------------------------------------------

func lubImpl(a, b []any) []any {
	ab, aq, anull := peelNullable(a)
	bb, bq, bnull := peelNullable(b)

	// Null ⊔ Null = Null
	if anull && bnull {
		return typeID("Null")
	}
	nullable := aq || bq || anull || bnull

	// base shortcuts
	if ab == nil {
		if bb == nil {
			return typeID("Null")
		}
		return addNullable(bb, nullable)
	}
	if bb == nil {
		return addNullable(ab, nullable)
	}

	if eq(ab, bb) {
		return addNullable(ab, nullable)
	}
	if isAny(ab) || isAny(bb) {
		return addNullable(typeID("Any"), nullable)
	}

	// Primitives
	if isPrim(ab) && isPrim(bb) {
		return addNullable(primLUB(ab, bb), nullable)
	}

	// Arrays (covariant)
	if isArray(ab) && isArray(bb) {
		le := lubImpl(arrElem(ab), arrElem(bb))
		return addNullable([]any{"array", le}, nullable)
	}

	// Maps: intersection of keys; field LUB; required iff both required
	if isMap(ab) && isMap(bb) {
		ma, mb := mapFields(ab), mapFields(bb)
		out := map[string]fieldInfo{}
		for k, fa := range ma {
			if fb, ok := mb[k]; ok {
				req := fa.required && fb.required
				out[k] = fieldInfo{typ: lubImpl(fa.typ, fb.typ), required: req}
			}
		}
		return addNullable(makeMapFrom(out), nullable)
	}

	// Enums (same kind → union; different kinds → Any)
	if isEnum(ab) && isEnum(bb) {
		ka, kb := enumKind(ab), enumKind(bb)
		if ka != "" && ka == kb {
			ua := enumMembers(ab)
			ub := enumMembers(bb)
			union := []any{}
			add := func(x any) {
				for _, u := range union {
					if reflect.DeepEqual(u, x) {
						return
					}
				}
				union = append(union, x)
			}
			for _, x := range ua {
				add(x)
			}
			for _, x := range ub {
				add(x)
			}
			return addNullable(enumOf(ka, union), nullable)
		}
		return addNullable(typeID("Any"), nullable)
	}

	// Enums with primitive of same base: widen to the primitive
	if isEnum(ab) && isPrim(bb) {
		k := enumKind(ab)
		switch {
		case k == "str" && isID(bb, "Str"),
			k == "int" && isID(bb, "Int"),
			k == "num" && isID(bb, "Num"),
			k == "bool" && isID(bb, "Bool"):
			return addNullable(bb, nullable)
		}
	}
	if isEnum(bb) && isPrim(ab) {
		k := enumKind(bb)
		switch {
		case k == "str" && isID(ab, "Str"),
			k == "int" && isID(ab, "Int"),
			k == "num" && isID(ab, "Num"),
			k == "bool" && isID(ab, "Bool"):
			return addNullable(ab, nullable)
		}
	}

	// Arrows: param GLB (if exists), return LUB
	if isArrow(ab) && isArrow(bb) {
		ap1, ar1 := arrowParts(ab)
		ap2, ar2 := arrowParts(bb)
		gp, ok := glbImpl(ap1, ap2)
		if !ok {
			return addNullable(typeID("Any"), nullable)
		}
		return addNullable([]any{"binop", "->", gp, lubImpl(ar1, ar2)}, nullable)
	}

	// Opaque alias atoms: same id → itself; else Any
	if len(ab) >= 2 && ab[0] == "id" && !isPrim(ab) && reflect.DeepEqual(ab, bb) {
		return addNullable(ab, nullable)
	}

	// Fallback: different shapes → Any
	return addNullable(typeID("Any"), nullable)
}
