package mindscript

// introspection.go — reflection & transformation utilities for MindScript
//
// WHAT THIS FILE DOES (HIGH-LEVEL CONTRACT)
// -----------------------------------------
// This module provides a *public*, well-documented bridge between:
//
//   1) Internal AST nodes (S-expressions, type `S = []any` as defined in
//      parser.go) and a *runtime S-expression* representation encoded as
//      nested runtime Values (`Value`, from interpreter.go).
//
//   2) General runtime Values (boxed, annotated, possibly functions or types)
//      and that same *runtime S-expression* representation, so tools can
//      inspect, traverse, rewrite, and reify values in a uniform tree shape.
//
//   3) Minimal tree traversal and rewriting helpers that operate on the
//      runtime S-expression Value trees.
//
// A *runtime S-expression* is a `Value` with `Tag == VTArray` whose first
// element is a `VTStr` tag (e.g., "int", "map", "fun", "annot"), followed
// by zero or more child nodes, each of which is either a scalar `Value`
// (VTStr/VTInt/VTNum/VTBool) or another runtime S-expression (VTArray).
//
// ANNOTATIONS (IMPORTANT)
// -----------------------
// MindScript Values carry an optional `Annot` string for documentation and
// error-context. When we convert a Value to a runtime S-expression, we *wrap*
// the produced node in an annotation node if `Annot` is non-empty:
//
//     ("annot", ("str", text), <node>)
//
// Conversely, when reifying from a runtime S-expression back to a Value,
// that wrapper is peeled and its text is restored into `Value.Annot` (merged
// with any existing annotation using a `""` join).
//
// FUNCTIONS & TYPES
// -----------------
// • User functions (non-native) reflect to their AST forms:
//     ("fun",    paramsArray, returnType, bodyBlock)
//     ("oracle", paramsArray, outType,    sourceExpr)
//   "paramsArray" is `( "array", ("pair", ("id", name), type), ... )`.
//   Reifying these constructs creates a *new* `*Fun` Value (closure env is
//   provided by the caller) with `Chunk=nil` to force re-JIT on first call.
//
// • Native functions (`Fun.NativeName != ""`) reflect to `( "native", name )`
//   stubs and are *not* reifiable (decoding returns an error).
//
// • Types (`VTType`) reflect to their embedded type S-expression (`TypeValue.Ast`).
//   Reifying a type node yields a `VTType` using `TypeValIn` if an env is given,
//   otherwise `TypeVal`.
//
// MAPS & KEY ANNOTATIONS
// ----------------------
// Runtime maps use `*MapObject` which preserves insertion order and per-key
// annotations via `KeyAnn`. Reflection follows printer/type conventions:
//   - Ordinary fields become `( "pair",  ("str", key), valueNode )`.
//   - Required fields render as   `( "pair!", ("str", key), valueNode )`.
//   - Any *other* key annotation text (e.g., rich docs) wraps the *key* with
//     `("annot", ("str", text), ("str", key))` so the information survives.
//   - When decoding back to a map Value, `pair!` implies `KeyAnn[key] = "!"`.
//
// NUMBERS & LITERALS
// ------------------
// Literals round-trip with tags: `null`, `bool`, `int` (int64), `num` (float64),
// `str`. Arrays are `( "array", elem1, ... )`. Maps are as above. The runtime
// S-expression for a literal `null` is `( "null" )` — *not* a bare `VTNull`.
//
// NON-ROUNDTRIPPABLE VALUES
// --------------------------
// `VTModule` and `VTHandle` reflect to `( "module" )` / `( "handle" )` stubs
// and are not reifiable. Attempting to reify those or `( "native", ... )`
// yields an error.
//
// DEPENDENCIES (other files)
// --------------------------
// • parser.go
//   - `type S = []any`
//   - `ParseSExpr`, `ParseSExprInteractive`     (for source→AST helpers)
//
// • printer.go
//   - `FormatSExpr`                             (for AST→source helper)
//
// • interpreter.go
//   - Runtime model: `Value`, `ValueTag`, constructors (`Null`, `Bool`, `Int`,
//     `Num`, `Str`, `Arr`), `MapObject`, `Fun`, `TypeValue`, `Env`.
//   - Type helpers: `TypeVal`, `TypeValIn`.
//
// PUBLIC vs PRIVATE LAYOUT
// ------------------------
// The rest of the file is split into:
//   1) PUBLIC  — exported `Ix*` functions with exhaustive documentation.
//   2) PRIVATE — implementation helpers and internal utilities.

import (
	"fmt"
)

// ==============================
// ========== PUBLIC ============
// ==============================

// IxToS converts an internal AST node (S) into a *runtime S-expression* Value.
//
// Input shape:
//
//	S is the AST node form produced by parser.go (e.g., ("block", ...)).
//
// Output shape:
//
//	The returned Value is a nested VTArray whose first element is a VTStr tag
//	and whose children are either scalars (VTStr/VTInt/VTNum/VTBool) or nested
//	runtime S nodes (VTArray). No annotations are introduced here — this is a
//	structural conversion only.
//
// Errors:
//
//	IxToS never errors; unknown leaf payloads are rendered as strings
//	`"<unsupported:...>"` to avoid panics.
func IxToS(n S) Value { return ix_toS(n) }

// IxFromS converts a *runtime S-expression* Value back into an internal AST (S).
//
// Requirements:
//
//	v must be a VTArray with leading VTStr tag; children must be either
//	VTArray (sub-nodes) or scalar literals (VTStr/VTInt/VTNum/VTBool).
//
// Returns:
//
//	A reconstructed AST node (S) matching the structural shape of v.
//
// Errors:
//
//	Returns an error if the shape is malformed (missing tag, wrong scalar kinds).
func IxFromS(v Value) (S, error) { return ix_fromS(v) }

// IxReflect converts a general runtime Value into a *runtime S-expression*
// Value suitable for inspection and transformation. If v carries a non-empty
// `v.Annot`, the result is wrapped as `( "annot", ("str", v.Annot), node )`.
//
// Mapping highlights:
//   - Scalars → ("null"), ("bool", b), ("int", i), ("num", f), ("str", s)
//   - Arrays  → ("array", ...)
//   - Maps    → ("map", ("pair"|"pair!", keyStrExpr, valNode)*) with key
//     annotations encoded as an `annot` wrapper around the key string.
//   - Functions:
//   - User fun:    ("fun",    paramsArray, returnTypeS, bodyS)
//   - Oracle fun:  ("oracle", paramsArray, outTypeS,    sourceS)
//   - Native fun:  ("native", name)  — *not* reifiable.
//   - Types    → the embedded type AST (S).
//   - Module/Handle → ("module") / ("handle") stubs (not reifiable).
func IxReflect(v Value) Value {
	rs := ix_toS(ix_valueToAST(v))
	if v.Annot != "" {
		// Wrap at the runtime-S layer to guarantee preservation.
		return Arr([]Value{
			Str("annot"),
			Arr([]Value{Str("str"), Str(v.Annot)}),
			rs,
		})
	}
	return rs
}

// IxReify decodes a *runtime S-expression* into a runtime Value using no
// special environment. For functions and types you likely want IxReifyIn.
//
// Behavior:
//   - Peels a leading ("annot", ("str", text), node) and restores the text into
//     Value.Annot of the produced Value.
//   - Reifies literals, arrays, maps, user/oracle functions, and types.
//   - Rejects native/module/handle stubs.
func IxReify(v Value) (Value, error) { return IxReifyIn(v, nil) }

// IxReifyIn decodes a *runtime S-expression* into a runtime Value, using env as
// the closure/type resolution environment.
//
// Environment usage:
//   - For user/oracle functions, the produced *Fun captures `env` as its
//     closure; its `Chunk` is set to nil so it will re-JIT on first call.
//   - For VTType, env is used by `TypeValIn`; if env is nil, `TypeVal` is used.
//
// Errors:
//   - Malformed shapes (wrong arity/kinds) yield descriptive errors.
//   - Attempting to reify ("native"|"module"|"handle") yields an error.
func IxReifyIn(v Value, env *Env) (Value, error) {
	s, ann, err := ix_stripAnnotToS(v)
	if err != nil {
		return Value{}, err
	}
	val, err := ix_valueFromAST(s, env)
	if err != nil {
		return Value{}, err
	}
	if ann != "" {
		val.Annot = ix_joinAnnot(val.Annot, ann)
	}
	return val, nil
}

// IxVisit performs a preorder traversal of a *runtime S-expression* Value.
//
// The callback receives the current index path and node. If it returns false,
// that node's children are skipped.
func IxVisit(v Value, visit func(path []int, node Value) bool) { ixVisit(v, visit) }

// IxRewrite performs a bottom-up rewrite over a *runtime S-expression* Value.
// If the callback returns (newNode, true), the node is replaced with newNode.
// Children are rewritten before the callback runs at a node.
func IxRewrite(v Value, f func(node Value) (Value, bool)) Value { return ixRewrite(v, f) }

// IxGet retrieves the node at `path` (sequence of indices) within a *runtime
// S-expression* Value. Returns (zero, false) if the path is invalid.
func IxGet(v Value, path []int) (Value, bool) { return ixGet(v, path) }

// IxSet returns a *new* runtime S-expression Value with the node at `path`
// replaced by `newNode`. If the path is invalid, it returns (v, false).
func IxSet(v Value, path []int, newNode Value) (Value, bool) { return ixSet(v, path, newNode) }

// IxFromSource parses MindScript source into an AST (S) using normal mode.
// It forwards to ParseSExpr from parser.go. On parse errors, the error is
// returned as-is (callers may wrap it with source if desired).
func IxFromSource(src string) (S, error) { return ParseSExpr(src) }

// IxFromSourceInteractive parses in interactive mode (unterminated constructs
// become *IncompleteError). It forwards to ParseSExprInteractive.
func IxFromSourceInteractive(src string) (S, error) { return ParseSExprInteractive(src) }

// IxToSource formats an AST (S) back to MindScript source code using the
// pretty-printer from printer.go (FormatSExpr). Output is stable.
func IxToSource(ast S) string { return FormatSExpr(ast) }

//// END_OF_PUBLIC

// ==============================
// ========== PRIVATE ===========
// ==============================

// --- AST ↔ runtime S ---

func ix_toS(n S) Value {
	arr := make([]Value, 0, len(n))
	tag, _ := n[0].(string)
	arr = append(arr, Str(tag))
	for i := 1; i < len(n); i++ {
		switch x := n[i].(type) {
		case []any: // S is a type alias of []any; handle both via this single case
			arr = append(arr, ix_toS(x))
		case string:
			arr = append(arr, Str(x))
		case int64:
			arr = append(arr, Int(x))
		case float64:
			arr = append(arr, Num(x))
		case bool:
			arr = append(arr, Bool(x))
		default:
			arr = append(arr, Str(fmt.Sprintf("<unsupported:%T>", x)))
		}
	}
	return Arr(arr)
}

func ix_fromS(v Value) (S, error) {
	if v.Tag != VTArray {
		return nil, fmt.Errorf("ixFromS: expected array node, got %v", v.Tag)
	}
	node := v.Data.([]Value)
	if len(node) == 0 || node[0].Tag != VTStr {
		return nil, fmt.Errorf("ixFromS: malformed node: missing tag")
	}
	tag := node[0].Data.(string)
	out := make(S, 0, len(node))
	out = append(out, tag)
	for i := 1; i < len(node); i++ {
		c := node[i]
		switch c.Tag {
		case VTArray:
			child, err := ix_fromS(c)
			if err != nil {
				return nil, err
			}
			out = append(out, child)
		case VTStr:
			out = append(out, c.Data.(string))
		case VTInt:
			out = append(out, c.Data.(int64))
		case VTNum:
			out = append(out, c.Data.(float64))
		case VTBool:
			out = append(out, c.Data.(bool))
		default:
			return nil, fmt.Errorf("ixFromS: unsupported scalar tag in node %q: %v", tag, c.Tag)
		}
	}
	return out, nil
}

// --- Value ↔ AST (S) ---

func ix_valueFromAST(n S, env *Env) (Value, error) {
	// ("annot", ("str", text), node)
	if len(n) > 0 {
		if tag, _ := n[0].(string); tag == "annot" && len(n) == 3 {
			annNode, ok := n[1].(S)
			if !ok || len(annNode) != 2 || annNode[0] != "str" {
				return Value{}, fmt.Errorf("annot expects ('str', text)")
			}
			ann, _ := annNode[1].(string)
			inner, ok := n[2].(S)
			if !ok {
				return Value{}, fmt.Errorf("annot inner must be a node")
			}
			v, err := ix_valueFromAST(inner, env)
			if err != nil {
				return Value{}, err
			}
			v.Annot = ix_joinAnnot(v.Annot, ann)
			return v, nil
		}
	}

	if ix_isTypeS(n) {
		if env != nil {
			return TypeValIn(n, env), nil
		}
		return TypeVal(n), nil
	}

	tag, _ := n[0].(string)
	switch tag {
	case "null":
		return Null, nil
	case "bool":
		if len(n) != 2 {
			return Value{}, fmt.Errorf("bool expects 1 arg")
		}
		return Bool(n[1].(bool)), nil
	case "int":
		if len(n) != 2 {
			return Value{}, fmt.Errorf("int expects 1 arg")
		}
		return Int(n[1].(int64)), nil
	case "num":
		if len(n) != 2 {
			return Value{}, fmt.Errorf("num expects 1 arg")
		}
		return Num(n[1].(float64)), nil
	case "str":
		if len(n) != 2 {
			return Value{}, fmt.Errorf("str expects 1 arg")
		}
		return Str(n[1].(string)), nil
	case "array":
		vals := make([]Value, 0, len(n)-1)
		for i := 1; i < len(n); i++ {
			child, ok := n[i].(S)
			if !ok {
				return Value{}, fmt.Errorf("array elements must be nodes")
			}
			v, err := ix_valueFromAST(child, env)
			if err != nil {
				return Value{}, err
			}
			vals = append(vals, v)
		}
		return Arr(vals), nil
	case "map":
		mo := &MapObject{Entries: map[string]Value{}, KeyAnn: map[string]string{}, Keys: []string{}}
		for i := 1; i < len(n); i++ {
			p, ok := n[i].(S)
			if !ok || len(p) != 3 {
				return Value{}, fmt.Errorf("map entries must be ('pair'|'pair!', key, value)")
			}
			ptag, _ := p[0].(string)
			k, kAnn, err := ix_decodeKeyStr(p[1])
			if err != nil {
				return Value{}, err
			}
			if ptag == "pair!" {
				if kAnn == "" {
					kAnn = "!"
				} else if kAnn != "!" {
					kAnn = kAnn + " !"
				}
			}
			vv, ok := p[2].(S)
			if !ok {
				return Value{}, fmt.Errorf("map value for key %q must be a node", k)
			}
			val, err := ix_valueFromAST(vv, env)
			if err != nil {
				return Value{}, err
			}
			if _, exists := mo.Entries[k]; !exists {
				mo.Keys = append(mo.Keys, k)
			}
			mo.Entries[k] = val
			if kAnn != "" {
				mo.KeyAnn[k] = kAnn
			}
		}
		return Value{Tag: VTMap, Data: mo}, nil
	case "fun":
		if len(n) != 4 {
			return Value{}, fmt.Errorf("fun expects (params, retType, body)")
		}
		names, ptypes, err := ix_decodeParams(n[1])
		if err != nil {
			return Value{}, err
		}
		ret, ok := n[2].(S)
		if !ok {
			return Value{}, fmt.Errorf("fun return type must be a node")
		}
		body, ok := n[3].(S)
		if !ok {
			return Value{}, fmt.Errorf("fun body must be a node")
		}
		f := &Fun{Params: names, ParamTypes: ptypes, ReturnType: ret, Body: body, Env: env, Chunk: nil}
		return FunVal(f), nil
	case "oracle":
		if len(n) != 4 {
			return Value{}, fmt.Errorf("oracle expects (params, outType, source)")
		}
		names, ptypes, err := ix_decodeParams(n[1])
		if err != nil {
			return Value{}, err
		}
		outT, ok := n[2].(S)
		if !ok {
			return Value{}, fmt.Errorf("oracle out type must be a node")
		}
		src, ok := n[3].(S)
		if !ok {
			return Value{}, fmt.Errorf("oracle source must be a node")
		}
		f := &Fun{Params: names, ParamTypes: ptypes, ReturnType: outT, Body: src, Env: env, IsOracle: true, Chunk: nil}
		return FunVal(f), nil
	case "native":
		return Value{}, fmt.Errorf("cannot reify native function stubs")
	case "module":
		return Value{}, fmt.Errorf("cannot reify module stubs")
	case "handle":
		return Value{}, fmt.Errorf("cannot reify handle stubs")
	default:
		return Value{}, fmt.Errorf("cannot reify non-literal node: %q", tag)
	}
}

func ix_valueToAST(v Value) S {
	switch v.Tag {
	case VTNull:
		return S{"null"}
	case VTBool:
		return S{"bool", v.Data.(bool)}
	case VTInt:
		return S{"int", v.Data.(int64)}
	case VTNum:
		return S{"num", v.Data.(float64)}
	case VTStr:
		return S{"str", v.Data.(string)}
	case VTArray:
		elts := v.Data.([]Value)
		out := make(S, 1, 1+len(elts))
		out[0] = "array"
		for _, e := range elts {
			out = append(out, ix_valueToAST(e))
		}
		return out
	case VTMap:
		mo := v.Data.(*MapObject)
		out := make(S, 1, 1+len(mo.Keys))
		out[0] = "map"
		for _, k := range mo.Keys {
			vv := mo.Entries[k]
			ptag := "pair"
			if ann := mo.KeyAnn[k]; ann == "!" {
				ptag = "pair!"
			}
			keyNode := S{"str", k}
			if ann := mo.KeyAnn[k]; ann != "" && ann != "!" {
				keyNode = S{"annot", S{"str", ann}, keyNode}
			}
			out = append(out, S{ptag, keyNode, ix_valueToAST(vv)})
		}
		return out
	case VTFun:
		f := v.Data.(*Fun)
		if f.NativeName != "" {
			return S{"native", f.NativeName}
		}
		parr := S{"array"}
		for i, name := range f.Params {
			var t S
			if i < len(f.ParamTypes) && f.ParamTypes[i] != nil {
				t = f.ParamTypes[i]
			} else {
				t = S{"id", "Any"}
			}
			parr = append(parr, S{"pair", S{"id", name}, t})
		}
		ret := f.ReturnType
		if ret == nil {
			ret = S{"id", "Any"}
		}
		if f.IsOracle {
			return S{"oracle", parr, ret, f.Body}
		}
		return S{"fun", parr, ret, f.Body}
	case VTType:
		tv := v.Data.(*TypeValue)
		return tv.Ast
	case VTModule:
		return S{"module"}
	case VTHandle:
		return S{"handle"}
	default:
		return S{"str", fmt.Sprintf("<unsupported:%v>", v.Tag)}
	}
}

// --- runtime S helpers ---

func ix_stripAnnotToS(v Value) (S, string, error) {
	s, err := ix_fromS(v)
	if err != nil {
		return nil, "", err
	}
	if len(s) >= 1 {
		if tag, _ := s[0].(string); tag == "annot" && len(s) == 3 {
			annNode, ok := s[1].(S)
			if !ok || len(annNode) != 2 || annNode[0] != "str" {
				return nil, "", fmt.Errorf("annot expects ('str', text)")
			}
			ann := annNode[1].(string)
			inner, ok := s[2].(S)
			if !ok {
				return nil, "", fmt.Errorf("annot inner must be a node")
			}
			return inner, ann, nil
		}
	}
	return s, "", nil
}

func ix_decodeKeyStr(x any) (string, string, error) {
	n, ok := x.(S)
	if !ok {
		return "", "", fmt.Errorf("map key must be a node")
	}
	if len(n) == 3 && n[0] == "annot" {
		annNode, ok := n[1].(S)
		if !ok || len(annNode) != 2 || annNode[0] != "str" {
			return "", "", fmt.Errorf("annot on key expects ('str', text)")
		}
		ann := annNode[1].(string)
		kNode, ok := n[2].(S)
		if !ok || len(kNode) != 2 || kNode[0] != "str" {
			return "", "", fmt.Errorf("annotated key must wrap a ('str', k)")
		}
		return kNode[1].(string), ann, nil
	}
	if len(n) == 2 && n[0] == "str" {
		return n[1].(string), "", nil
	}
	return "", "", fmt.Errorf("invalid key node: %v", n)
}

func ix_decodeParams(x any) ([]string, []S, error) {
	arr, ok := x.(S)
	if !ok || len(arr) == 0 || arr[0] != "array" {
		return nil, nil, fmt.Errorf("params must be ('array', ...) node")
	}
	names := make([]string, 0, len(arr)-1)
	types := make([]S, 0, len(arr)-1)
	for i := 1; i < len(arr); i++ {
		p, ok := arr[i].(S)
		if !ok || len(p) != 3 || p[0] != "pair" {
			return nil, nil, fmt.Errorf("param %d must be ('pair', ('id', name), type)", i-1)
		}
		idn, ok := p[1].(S)
		if !ok || len(idn) != 2 || idn[0] != "id" {
			return nil, nil, fmt.Errorf("param %d must have ('id', name)", i-1)
		}
		name := idn[1].(string)
		T, ok := p[2].(S)
		if !ok {
			return nil, nil, fmt.Errorf("param %s type must be a node", name)
		}
		names = append(names, name)
		types = append(types, T)
	}
	return names, types, nil
}

// --- Type detection helpers (on AST S) ---

func ix_isTypeS(n S) bool {
	if len(n) == 0 {
		return false
	}
	tag, _ := n[0].(string)
	switch tag {
	case "annot":
		if len(n) != 3 {
			return false
		}
		inner, ok := n[2].(S)
		return ok && ix_isTypeS(inner)
	case "id":
		// Any identifier may denote a type alias; accept as type form.
		return len(n) == 2
	case "unop":
		if len(n) != 3 {
			return false
		}
		op, _ := n[1].(string)
		if op != "?" {
			return false
		}
		inner, ok := n[2].(S)
		return ok && ix_isTypeS(inner)
	case "array":
		if len(n) != 2 {
			return false
		}
		inner, ok := n[1].(S)
		return ok && ix_isTypeS(inner)
	case "map":
		for i := 1; i < len(n); i++ {
			p, ok := n[i].(S)
			if !ok || len(p) != 3 {
				return false
			}
			ptag, _ := p[0].(string)
			if ptag != "pair" && ptag != "pair!" {
				return false
			}
			if _, _, err := ix_decodeKeyStr(p[1]); err != nil {
				return false
			}
			val, ok := p[2].(S)
			if !ok {
				return false
			}
			if !ix_isTypeS(val) {
				return false
			}
		}
		return true
	case "enum":
		if len(n) < 2 {
			return false
		}
		for i := 1; i < len(n); i++ {
			if !ix_isValueLiteralS(n[i]) {
				return false
			}
		}
		return true
	case "binop":
		if len(n) != 4 {
			return false
		}
		op, _ := n[1].(string)
		if op != "->" {
			return false
		}
		a, okA := n[2].(S)
		b, okB := n[3].(S)
		return okA && okB && ix_isTypeS(a) && ix_isTypeS(b)
	default:
		return false
	}
}

func ix_isValueLiteralS(x any) bool {
	n, ok := x.(S)
	if !ok || len(n) == 0 {
		return false
	}
	tag, _ := n[0].(string)
	switch tag {
	case "annot":
		return len(n) == 3 && ix_isValueLiteralS(n[2])
	case "null":
		return len(n) == 1
	case "bool", "int", "num", "str":
		return len(n) == 2
	case "array":
		for i := 1; i < len(n); i++ {
			if !ix_isValueLiteralS(n[i]) {
				return false
			}
		}
		return true
	case "map":
		for i := 1; i < len(n); i++ {
			p, ok := n[i].(S)
			if !ok || len(p) != 3 {
				return false
			}
			ptag, _ := p[0].(string)
			if ptag != "pair" && ptag != "pair!" {
				return false
			}
			if _, _, err := ix_decodeKeyStr(p[1]); err != nil {
				return false
			}
			if !ix_isValueLiteralS(p[2]) {
				return false
			}
		}
		return true
	default:
		return false
	}
}

func asS(x any) bool { _, ok := x.(S); return ok }

// --- Tree utils (PRIVATE impl) ---

func ixVisit(v Value, visit func(path []int, node Value) bool) {
	var walk func(cur Value, path []int)
	walk = func(cur Value, path []int) {
		if !visit(path, cur) {
			return
		}
		if cur.Tag != VTArray {
			return
		}
		n := cur.Data.([]Value)
		for i := range n {
			walk(n[i], append(path, i))
		}
	}
	walk(v, nil)
}

func ixRewrite(v Value, f func(node Value) (Value, bool)) Value {
	if v.Tag == VTArray {
		old := v.Data.([]Value)
		cpy := make([]Value, len(old))
		for i := range old {
			cpy[i] = ixRewrite(old[i], f)
		}
		v = Arr(cpy)
	}
	if nv, ok := f(v); ok {
		return nv
	}
	return v
}

func ixGet(v Value, path []int) (Value, bool) {
	cur := v
	for _, idx := range path {
		if cur.Tag != VTArray {
			return Value{}, false
		}
		a := cur.Data.([]Value)
		if idx < 0 || idx >= len(a) {
			return Value{}, false
		}
		cur = a[idx]
	}
	return cur, true
}

func ixSet(v Value, path []int, newNode Value) (Value, bool) {
	if len(path) == 0 {
		return newNode, true
	}
	if v.Tag != VTArray {
		return v, false
	}
	a := v.Data.([]Value)
	idx := path[0]
	if idx < 0 || idx >= len(a) {
		return v, false
	}
	repl, ok := ixSet(a[idx], path[1:], newNode)
	if !ok {
		return v, false
	}
	cpy := make([]Value, len(a))
	copy(cpy, a)
	cpy[idx] = repl
	return Arr(cpy), true
}

func ix_joinAnnot(a, b string) string {
	if a == "" {
		return b
	}
	if b == "" {
		return a
	}
	return a + "\n" + b
}
