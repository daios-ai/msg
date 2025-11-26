// cmd/msg-lsp/analysis_engine.go
//
// MindScript Static Analyzer — engine (private implementation)
//
// ------------------------------------------------------------
// This file contains the private folding engine and helpers used by
// Analyzer.Analyze(). It depends ONLY on data structures and helpers
// declared in analysis_frontend.go (same package), and does not
// introduce any new exported data structures.

package main

import (
	"fmt"
	"strings"

	mindscript "github.com/daios-ai/msg/internal/mindscript"
)

//// END_OF_PUBLIC

////////////////////////////////////////////////////////////////////////////////
// foldCtx, block modes & flow (engine-internal)
////////////////////////////////////////////////////////////////////////////////

type foldCtx struct {
	idx *FileIndex
	ip  *mindscript.Interpreter
	env *mindscript.Env

	topLevel bool
	path     mindscript.NodePath
}

// ---------------------------------------------------------------------------
// Centralized resolvers/stampers (single source of truth)
// ---------------------------------------------------------------------------

// resolveName resolves an identifier to a payload (VTType or VTSymbol) and its type.
// It also lazily seeds ambient runtime values into the current env as VTSymbols,
// so subsequent lookups hit a uniform representation without re-conversion.
func resolveName(ip *mindscript.Interpreter, env *mindscript.Env, name string) (payload mindscript.Value, typ mindscript.S, isType bool, ok bool) {
	v, err := env.Get(name)
	if err != nil {
		return mindscript.Value{}, nil, false, false
	}
	// Keep VTType values as-is (preserves alias docs).
	if v.Tag == mindscript.VTType {
		if tv, _ := v.Data.(*mindscript.TypeValue); tv != nil {
			return v, tv.Ast, true, true
		}
		return v, mindscript.S{"id", "Type"}, true, true
	}
	// Keep VTSymbol as-is (locals/ambient already promoted).
	if sym, ok := asSymbol(v); ok {
		return v, sym.Type, false, true
	}
	// Ambient runtime value: convert once to a VTSymbol (preserve v.Annot) and seed it.
	var t mindscript.S
	if ip != nil {
		t = ip.ValueToType(v, env)
	} else {
		t = mindscript.S{"id", "Any"}
	}
	s := newSymbolValIn(env, t, strings.TrimSpace(v.Annot))
	// Seed into the *current* env to avoid repeated conversions and to unify def–use.
	// Define is safe here because lookups reached the parent; this creates a child binding shadow.
	env.Define(name, s)
	return s, t, false, true
}

type fieldInfo struct {
	Type     mindscript.S
	Required bool
	Doc      string
	Found    bool
}

// resolveField extracts field info from RAW and RESOLVED receiver types.
func resolveField(ip *mindscript.Interpreter, env *mindscript.Env, recvTRaw, recvT mindscript.S, key string) fieldInfo {
	var doc string
	// Normalize helper: peel ("alias", *TypeValue) and outer ("annot", ..., inner)
	unwrap := func(t mindscript.S) mindscript.S {
		for {
			if len(t) == 0 {
				return t
			}
			switch tag := t[0].(string); tag {
			case "alias":
				if len(t) >= 2 {
					if tv, _ := t[1].(*mindscript.TypeValue); tv != nil {
						t = tv.Ast
						continue
					}
				}
				return t
			case "annot":
				if len(t) >= 3 {
					if inner, ok := t[2].(mindscript.S); ok {
						t = inner
						continue
					}
				}
				return t
			default:
				return t
			}
		}
	}
	get := func(t mindscript.S) (ft mindscript.S, req bool, ok bool) {
		t = unwrap(t)
		if len(t) == 0 || t[0] != "map" {
			return nil, false, false
		}
		for i := 1; i < len(t); i++ {
			p, _ := t[i].(mindscript.S)
			if len(p) < 3 {
				continue
			}
			ptag, _ := p[0].(string)
			if ptag != "pair" && ptag != "pair!" {
				continue
			}
			k, _ := p[1].(mindscript.S)
			if len(k) >= 2 && k[0] == "str" {
				if s, _ := k[1].(string); s == key {
					ft, _ := p[2].(mindscript.S)
					if ft == nil {
						ft = mindscript.S{"id", "Any"}
					}
					return ft, ptag == "pair!", true
				}
			}
		}
		return nil, false, false
	}
	if ftRaw, _, ok := get(recvTRaw); ok {
		doc = outerDocFromType(ftRaw)
	}
	if ftRes, req, ok := get(recvT); ok {
		return fieldInfo{Type: ftRes, Required: req, Doc: doc, Found: true}
	}
	return fieldInfo{Doc: doc}
}

type blockMode int

const (
	blockPlain blockMode = iota
	blockTopLevel
	blockFunBody
	blockLoopBody
)

// ---------------- Centralized diagnostic helpers ----------------
// All engine diagnostics should go through these foldCtx methods to ensure
// consistent span selection and fallback behavior.

// diagBytes emits a diagnostic for an explicit byte range.
func (c *foldCtx) diagBytes(s, e int, code, msg string) {
	c.idx.addDiag(Diag{StartByte: s, EndByte: e, Code: code, Message: msg})
}

// diagInternalNoSpanf emits a severe internal diagnostic anchored at 0,0.
func (c *foldCtx) diagInternalNoSpanf(format string, a ...any) {
	c.idx.addDiag(Diag{
		StartByte: 0, EndByte: 0,
		Code:    "MS-INTERNAL-SPAN-MISMATCH",
		Message: fmt.Sprintf(format, a...),
	})
}

// diagAtPath emits a diagnostic anchored at an explicit AST NodePath.
func (c *foldCtx) diagAtPath(p mindscript.NodePath, code, msg string) {
	if sp, ok := c.idx.Spans.Get(p); ok {
		c.diagBytes(sp.StartByte, sp.EndByte, code, msg)
		return
	}
	c.diagInternalNoSpanf("no span for node path %v while emitting %s: %s", p, code, msg)
}

// ---------------------------------------------------------------

// flow is the engine’s internal result carrier.
type flow struct {
	Val                 mindscript.Value
	Ret, Brk, Cont      mindscript.Value
	HasRet, HasBrk      bool
	HasCont, Terminated bool
}

// constructors that always stamp the current environment
func (c *foldCtx) newFlow(t mindscript.S) flow {
	if t == nil {
		t = mindscript.S{"id", "Any"}
	}
	return flow{Val: newSymbolValIn(c.env, t, "")}
}

func (c *foldCtx) newFlowFromType(t mindscript.S) flow { return c.newFlow(t) }

func typeOf(ip *mindscript.Interpreter, env *mindscript.Env, v mindscript.Value) mindscript.S {
	if sym, ok := asSymbol(v); ok {
		return sym.Type
	}
	if v.Tag == mindscript.VTType {
		return mindscript.S{"id", "Type"}
	}
	if ip != nil {
		return ip.ValueToType(v, env)
	}
	return mindscript.S{"id", "Any"}
}

////////////////////////////////////////////////////////////////////////////////
// Dispatcher
////////////////////////////////////////////////////////////////////////////////

func (c *foldCtx) fold(n mindscript.S) flow {
	if len(n) == 0 {
		return c.newFlow(nil)
	}
	tag, _ := n[0].(string)

	switch tag {
	case "return":
		return c.foldReturn(n)
	case "break":
		return c.foldBreak(n)
	case "continue":
		return c.foldContinue(n)
	case "annot":
		return c.foldAnnot(n)
	case "str", "int", "num", "bool", "null":
		return c.foldLiteral(n)
	case "id":
		return c.foldId(n)
	case "array":
		return c.foldArray(n)
	case "map":
		return c.foldMap(n)
	case "get":
		return c.foldGet(n)
	case "idx":
		return c.foldIdx(n)
	case "call":
		return c.foldCall(n)
	case "binop":
		return c.foldBinop(n)
	case "unop":
		return c.foldUnop(n)
	case "type":
		return c.foldTypeExpr(n)
	case "assign":
		return c.foldAssign(n)
	case "decl":
		return c.foldDecl(n)
	case "block":
		return c.foldBlock(n)
	case "if":
		return c.foldIf(n)
	case "while":
		return c.foldWhile(n)
	case "for":
		return c.foldFor(n)
	case "fun", "oracle":
		return c.foldFunLike(n)
	case "module":
		return c.foldModule(n)
	}

	return c.newFlow(nil)
}

////////////////////////////////////////////////////////////////////////////////
// Span & child helpers
////////////////////////////////////////////////////////////////////////////////

func appendPath(base mindscript.NodePath, child int) mindscript.NodePath {
	out := make(mindscript.NodePath, 0, len(base)+1)
	out = append(out, base...)
	out = append(out, child)
	return out
}

func (c *foldCtx) spanFor() (int, int, bool) {
	if sp, ok := c.idx.Spans.Get(c.path); ok {
		return sp.StartByte, sp.EndByte, true
	}
	return 0, 0, false
}

func (c *foldCtx) spanForChildSlot(slot int) (int, int, bool) {
	if slot < 1 {
		return 0, 0, false
	}
	cp := appendPath(c.path, slot-1)
	if sp, ok := c.idx.Spans.Get(cp); ok {
		return sp.StartByte, sp.EndByte, true
	}
	return 0, 0, false
}

func (c *foldCtx) childCtx(slot int) *foldCtx {
	cc := *c
	if slot >= 1 {
		cc.path = appendPath(c.path, slot-1)
	}
	return &cc
}

// ----- Centralized span + stamping + diagnostics (no fallbacks) -------------
func (c *foldCtx) leafSpanAtSlot(slot int) (int, int, bool) {
	if slot == 0 {
		if s, e, ok := c.spanFor(); ok {
			return s, e, true
		}
	} else if s, e, ok := c.spanForChildSlot(slot); ok {
		return s, e, true
	}
	c.diagInternalNoSpanf("no leaf span for node path %v (slot %d)", c.path, slot)
	return 0, 0, false
}

func (c *foldCtx) stampIdent(slot int, name string, isDecl bool, payload mindscript.Value) {
	s, _, ok := c.leafSpanAtSlot(slot)
	if !ok {
		return
	}
	c.idx.emitIdentAtStart(s, name, isDecl, payload)
}

func (c *foldCtx) diagAt(slot int, code, msg string) {
	s, e, ok := c.leafSpanAtSlot(slot)
	if !ok {
		return
	}
	c.diagBytes(s, e, code, msg)
}

// stampTypeID stamps a type identifier leaf, reusing stored VTType when present.
func (c *foldCtx) stampTypeID(name string, p mindscript.NodePath, typeAst mindscript.S) {
	if sp, ok := c.idx.Spans.Get(p); ok {
		if payload, _, isType, ok := resolveName(c.ip, c.env, name); ok && isType && payload.Tag == mindscript.VTType {
			c.idx.emitIdentAtStart(sp.StartByte, name, false, payload)
			return
		}
		c.idx.emitIdentAtStart(sp.StartByte, name, false, mindscript.TypeValIn(typeAst, c.env))
	}
}

// ---- Type AST stamping ------------------------------------------------------
// stampTypeIDs walks a *type* AST 't' whose root lives at node-path 'p' and
// stamps every ("id", name) leaf with a VTType payload so hover shows
//
//	name: Type (type <structure>)
//
// rather than a generic "symbol".
func (c *foldCtx) stampTypeIDs(t mindscript.S, p mindscript.NodePath) {
	if len(t) == 0 {
		return
	}
	tag, _ := t[0].(string)
	switch tag {
	case "id":
		if len(t) >= 2 {
			if name, ok := t[1].(string); ok && name != "" {
				c.stampTypeID(name, p, t)
				return
			}
		}
	case "annot":
		// ("annot", ["str", ...], inner)
		if inner, ok := t[2].(mindscript.S); ok {
			c.stampTypeIDs(inner, appendPath(p, 1))
		}
	case "unop":
		// ("unop", "?", T)
		if len(t) >= 3 {
			if inner, ok := t[2].(mindscript.S); ok {
				c.stampTypeIDs(inner, appendPath(p, 1))
			}
		}
	case "array":
		// ("array", T)
		if len(t) >= 2 {
			if et, ok := t[1].(mindscript.S); ok {
				c.stampTypeIDs(et, appendPath(p, 0))
			}
		}
	case "map":
		// ("map", ("pair"| "pair!", ("str", key), T) ...)
		for i := 1; i < len(t); i++ {
			pair, ok := t[i].(mindscript.S)
			if !ok || len(pair) < 3 {
				continue
			}
			ptag, _ := pair[0].(string)
			// Stamp value type identifiers inside the field type T.
			if vt, ok := pair[2].(mindscript.S); ok {
				c.stampTypeIDs(vt, appendPath(p, i-1))
			}
			// Also stamp the field *key* token inside the type map so hovers show "key: T".
			keyNode, _ := pair[1].(mindscript.S)
			if len(keyNode) >= 2 && keyNode[0] == "str" {
				if key, _ := keyNode[1].(string); key != "" {
					// Use the field type (possibly annotated) for the symbol payload; prefer its outer doc.
					ft := mindscript.S(nil)
					if vt, ok := pair[2].(mindscript.S); ok {
						ft = vt
					}
					if ft == nil {
						ft = mindscript.S{"id", "Any"}
					}
					doc := outerDocFromType(ft)
					label := key
					if ptag == "pair!" {
						label = key + "!"
					}
					kPath := appendPath(appendPath(p, i-1), 0) // path to ["str", key]
					if sp, ok := c.idx.Spans.Get(kPath); ok {
						c.idx.emitIdentAtStart(sp.StartByte, label, false, newSymbolValIn(c.env, ft, doc))
					}
				}
			}
		}
		// skip other tags
	case "binop":
		// ("binop","->", A, B)
		if len(t) >= 4 && t[1] == "->" {
			if a, ok := t[2].(mindscript.S); ok {
				c.stampTypeIDs(a, appendPath(p, 1))
			}
			if b, ok := t[3].(mindscript.S); ok {
				c.stampTypeIDs(b, appendPath(p, 2))
			}
		}
	case "enum":
		// literals only; nothing to stamp
	}
}

////////////////////////////////////////////////////////////////////////////////
// Per-tag handlers
////////////////////////////////////////////////////////////////////////////////

// --------- Small doc/type helpers (non-destructive wrt types) ---------------
// outerDocFromType returns the top-level annotation string if `t` is ("annot", ["str", ...], inner),
// otherwise "".
func outerDocFromType(t mindscript.S) string {
	if len(t) >= 3 {
		if tag, _ := t[0].(string); tag == "annot" {
			if docNode, _ := t[1].(mindscript.S); len(docNode) >= 2 {
				if dtag, _ := docNode[0].(string); dtag == "str" {
					if s, ok := docNode[1].(string); ok {
						return strings.TrimSpace(s)
					}
				}
			}
		}
	}
	return ""
}

// preferDoc returns v.Annot if present, otherwise the outer doc on t (if any).
func preferDoc(v mindscript.Value, t mindscript.S) string {
	if strings.TrimSpace(v.Annot) != "" {
		return strings.TrimSpace(v.Annot)
	}
	return outerDocFromType(t)
}

func (c *foldCtx) foldReturn(n mindscript.S) flow {
	t := mindscript.S{"id", "Null"}
	if len(n) >= 2 {
		if expr, ok := n[1].(mindscript.S); ok {
			ef := c.childCtx(1).fold(expr)
			if tv := typeOf(c.ip, c.env, ef.Val); tv != nil {
				t = tv
			}
		}
	}
	return flow{
		Val:        newSymbolValIn(c.env, mindscript.S{"id", "Null"}, ""),
		Terminated: true,
		Ret:        newSymbolValIn(c.env, t, ""),
		HasRet:     true,
	}
}

func (c *foldCtx) foldBreak(n mindscript.S) flow {
	_ = n
	v := newSymbolValIn(c.env, mindscript.S{"id", "Null"}, "")
	return flow{Val: v, Terminated: true, Brk: v, HasBrk: true}
}

func (c *foldCtx) foldContinue(n mindscript.S) flow {
	_ = n
	v := newSymbolValIn(c.env, mindscript.S{"id", "Null"}, "")
	return flow{Val: v, Terminated: true, Cont: v, HasCont: true}
}

func (c *foldCtx) foldAnnot(n mindscript.S) flow {
	if len(n) < 3 {
		return c.newFlow(nil)
	}
	docNode, _ := n[1].(mindscript.S)
	expr, _ := n[2].(mindscript.S)

	doc := ""
	if len(docNode) >= 2 {
		if tag, _ := docNode[0].(string); tag == "str" {
			if s, ok := docNode[1].(string); ok {
				doc = s
			}
		}
	}

	inner := c.childCtx(2).fold(expr)
	if doc != "" {
		v := inner.Val
		v.Annot = doc
		inner.Val = v
	}
	return inner
}

func (c *foldCtx) foldLiteral(n mindscript.S) flow {
	if len(n) == 0 {
		return c.newFlow(nil)
	}
	switch n[0].(string) {
	case "int", "num", "str", "bool", "null":
		return c.newFlowFromType(mindscript.S{"enum", n})
	default:
		return c.newFlow(nil)
	}
}

func (c *foldCtx) foldId(n mindscript.S) flow {
	if len(n) < 2 {
		return c.newFlow(nil)
	}
	name, _ := n[1].(string)
	if name == "" || c.env == nil {
		return c.newFlow(nil)
	}

	payload, typ, _, ok := resolveName(c.ip, c.env, name)
	if !ok {
		c.stampIdent(0, name, false, mindscript.Value{})
		c.diagAt(0, "MS-UNKNOWN-NAME", "unknown name: "+name)
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}
	// Stamp the resolved payload (VTType or VTSymbol) for proper hover.
	c.stampIdent(0, name, false, payload)
	// Return the same payload as the value so downstream sees the exact type.
	if typ == nil {
		typ = mindscript.S{"id", "Any"}
	}
	return flow{Val: payload}
}

func (c *foldCtx) foldArray(n mindscript.S) flow {
	if len(n) == 1 {
		return c.newFlowFromType(mindscript.S{"array", mindscript.S{"id", "Any"}})
	}
	ip := c.ip
	elemT := mindscript.S{"id", "Any"}
	first := true

	for i := 1; i < len(n); i++ {
		sub, ok := n[i].(mindscript.S)
		if !ok {
			continue
		}
		sf := c.childCtx(i).fold(sub)
		t := typeOf(ip, c.env, sf.Val)
		if t == nil {
			t = mindscript.S{"id", "Any"}
		}
		if ip == nil {
			elemT = mindscript.S{"id", "Any"}
			continue
		}
		if first {
			elemT, first = t, false
		} else {
			elemT = ip.UnifyTypes(elemT, t, c.env)
		}
	}
	if first {
		elemT = mindscript.S{"id", "Any"}
	}
	return c.newFlowFromType(mindscript.S{"array", elemT})
}

func (c *foldCtx) foldMap(n mindscript.S) flow {
	if len(n) == 1 {
		return c.newFlowFromType(mindscript.S{"map"})
	}
	out := mindscript.S{"map"}
	for i := 1; i < len(n); i++ {
		p, ok := n[i].(mindscript.S)
		if !ok || len(p) < 3 {
			continue
		}
		tag, _ := p[0].(string)
		if tag != "pair" && tag != "pair!" {
			continue
		}
		keyNode, _ := p[1].(mindscript.S)
		if len(keyNode) < 2 || keyNode[0].(string) != "str" {
			continue
		}
		key, _ := keyNode[1].(string)
		if key == "" {
			continue
		}
		valNode, _ := p[2].(mindscript.S)
		valFlow := c.childCtx(i).fold(valNode)
		valType := typeOf(c.ip, c.env, valFlow.Val)
		if valType == nil {
			valType = mindscript.S{"id", "Any"}
		}
		// Propagate value docs onto the stored field type so later hovers can see them.
		doc := preferDoc(valFlow.Val, valType)
		valTypeWithDoc := valType
		if strings.TrimSpace(doc) != "" {
			valTypeWithDoc = mindscript.S{"annot", mindscript.S{"str", doc}, valType}
		}
		// Stamp the map-literal key token so hovering the key at the declaration site shows field type + doc.
		c.childCtx(i).stampIdent(1, key, false, newSymbolValIn(c.env, valType, doc))
		out = append(out, mindscript.S{"pair!", mindscript.S{"str", key}, valTypeWithDoc})
	}
	return c.newFlowFromType(out)
}

func mapFieldType(t mindscript.S, key string) (mindscript.S, bool) {
	if len(t) == 0 {
		return nil, false
	}
	if tag, _ := t[0].(string); tag != "map" {
		return nil, false
	}
	for i := 1; i < len(t); i++ {
		p, ok := t[i].(mindscript.S)
		if !ok || len(p) < 3 {
			continue
		}
		if ptag, _ := p[0].(string); ptag != "pair" && ptag != "pair!" {
			continue
		}
		k, ok := p[1].(mindscript.S)
		if !ok || len(k) < 2 || k[0].(string) != "str" {
			continue
		}
		name, _ := k[1].(string)
		if name != key {
			continue
		}
		ft, _ := p[2].(mindscript.S)
		if ft == nil {
			ft = mindscript.S{"id", "Any"}
		}
		return ft, true
	}
	return nil, false
}

func arrayElemType(t mindscript.S) (mindscript.S, bool) {
	if len(t) == 0 {
		return nil, false
	}
	if tag, _ := t[0].(string); tag != "array" {
		return nil, false
	}
	if len(t) == 2 {
		if et, ok := t[1].(mindscript.S); ok {
			return et, true
		}
	}
	return mindscript.S{"id", "Any"}, true
}

func (c *foldCtx) foldGet(n mindscript.S) flow {
	if len(n) < 3 {
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}
	recvNode, _ := n[1].(mindscript.S)
	keyNode, _ := n[2].(mindscript.S)

	recvFlow := c.childCtx(1).fold(recvNode)
	recvTRaw := typeOf(c.ip, c.env, recvFlow.Val)
	if recvTRaw == nil {
		recvTRaw = mindscript.S{"id", "Any"}
	}
	recvT := recvTRaw
	if c.ip != nil {
		recvT = c.ip.ResolveType(recvTRaw, c.env)
	}

	if len(keyNode) < 2 || keyNode[0].(string) != "str" {
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}
	key, _ := keyNode[1].(string)

	fi := resolveField(c.ip, c.env, recvTRaw, recvT, key)
	label := key
	if fi.Required {
		label = key + "!"
	}

	// Stamp only when the field is known; do not fabricate payloads on misses.
	if fi.Found {
		c.childCtx(2).stampIdent(0, label, false, newSymbolValIn(c.env, fi.Type, fi.Doc))
	}

	if fi.Found {
		return c.newFlowFromType(fi.Type)
	}

	if len(recvT) > 0 {
		if tag, _ := recvT[0].(string); tag == "map" {
			c.diagAt(2, "MS-MAP-MISSING-KEY", "missing key '"+key+"' on map/module")
		}
	}
	return c.newFlowFromType(mindscript.S{"id", "Any"})
}

func (c *foldCtx) foldIdx(n mindscript.S) flow {
	if len(n) < 3 {
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}
	recvNode, _ := n[1].(mindscript.S)
	idxNode, _ := n[2].(mindscript.S)

	recvFlow := c.childCtx(1).fold(recvNode)
	idxFlow := c.childCtx(2).fold(idxNode)

	// Keep RAW and RESOLVED receiver types (docs may live only on RAW).
	recvTRaw := typeOf(c.ip, c.env, recvFlow.Val)
	if recvTRaw == nil {
		recvTRaw = mindscript.S{"id", "Any"}
	}
	recvT := recvTRaw
	idxT := typeOf(c.ip, c.env, idxFlow.Val)
	if idxT == nil {
		idxT = mindscript.S{"id", "Any"}
	}

	ip := c.ip
	if ip != nil {
		recvT = ip.ResolveType(recvTRaw, c.env)
		idxT = ip.ResolveType(idxT, c.env)
	}

	isId := func(t mindscript.S, name string) bool {
		return len(t) >= 2 && t[0].(string) == "id" && t[1].(string) == name
	}

	// Arrays
	if elemT, ok := arrayElemType(recvT); ok {
		okInt := isId(idxT, "Int")
		if !okInt && ip != nil {
			okInt = ip.IsSubtype(idxT, mindscript.S{"id", "Int"}, c.env)
		}
		if !okInt {
			c.diagAt(2, "MS-ARG-TYPE-MISMATCH", "array index must be Int")
			return c.newFlowFromType(mindscript.S{"id", "Any"})
		}
		// Enrich the index token hover with the element type payload (best-effort).
		// Only stamp when the index is a literal int so we know the token starts here.
		if len(idxNode) >= 2 && idxNode[0].(string) == "int" {
			if v, ok := idxNode[1].(int64); ok {
				name := fmt.Sprintf("%d", v)
				doc := outerDocFromType(elemT)
				c.stampIdent(2, name, false, newSymbolValIn(c.env, elemT, doc))
			}
		}
		return c.newFlowFromType(elemT)
	}

	// Maps
	if len(recvT) > 0 {
		if tag, _ := recvT[0].(string); tag == "map" {
			if len(idxNode) >= 2 && idxNode[0].(string) == "str" {
				key, _ := idxNode[1].(string)
				fi := resolveField(c.ip, c.env, recvTRaw, recvT, key)
				label := key
				if fi.Required {
					label = key + "!"
				}
				if fi.Found {
					c.childCtx(2).stampIdent(0, label, false, newSymbolValIn(c.env, fi.Type, fi.Doc))
					return c.newFlowFromType(fi.Type)
				}
				// Unknown key: diagnose but do not stamp a payload.
				c.diagAt(2, "MS-MAP-MISSING-KEY", "missing key '"+key+"' on map/module")
				return c.newFlowFromType(mindscript.S{"id", "Any"})
			}
			return c.newFlowFromType(mindscript.S{"id", "Any"})
		}
	}

	return c.newFlowFromType(mindscript.S{"id", "Any"})
}

func (c *foldCtx) foldCall(n mindscript.S) flow {
	if len(n) < 2 {
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}

	calleeNode, _ := n[1].(mindscript.S)
	cf := c.childCtx(1).fold(calleeNode)
	calleeT := typeOf(c.ip, c.env, cf.Val)
	if calleeT == nil {
		calleeT = mindscript.S{"id", "Any"}
	}

	argTypes := make([]mindscript.S, 0, len(n)-2)
	for i := 2; i < len(n); i++ {
		if arg, ok := n[i].(mindscript.S); ok {
			cc := c.childCtx(i)
			af := cc.fold(arg)
			t := typeOf(c.ip, c.env, af.Val)
			if t == nil {
				t = mindscript.S{"id", "Any"}
			}
			argTypes = append(argTypes, t)
		}
	}

	ip := c.ip
	if ip == nil {
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}

	resT := calleeT
	for i, at := range argTypes {
		if len(resT) >= 4 {
			if tag, _ := resT[0].(string); tag == "binop" {
				if op, _ := resT[1].(string); op == "->" {
					pt, _ := resT[2].(mindscript.S)
					next, _ := resT[3].(mindscript.S)
					if !ip.IsSubtype(at, pt, c.env) {
						c.diagAt(2+i, "MS-ARG-TYPE-MISMATCH",
							fmt.Sprintf("argument %d type mismatch: got %s, expected %s (callee %s)",
								i+1, mindscript.FormatType(at), mindscript.FormatType(pt), mindscript.FormatType(calleeT)))
					}
					resT = next
					continue
				}
			}
		}
		// Too many arguments: anchor to the first extra arg slot.
		c.diagAt(2+i, "MS-ARG-OVERFLOW", "too many arguments")
		break
	}
	return c.newFlowFromType(resT)
}

func (c *foldCtx) foldBinop(n mindscript.S) flow {
	if len(n) < 4 {
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}
	op, _ := n[1].(string)
	lhs, _ := n[2].(mindscript.S)
	rhs, _ := n[3].(mindscript.S)

	lf := c.childCtx(2).fold(lhs)
	rf := c.childCtx(3).fold(rhs)

	lt := typeOf(c.ip, c.env, lf.Val)
	rt := typeOf(c.ip, c.env, rf.Val)
	if lt == nil {
		lt = mindscript.S{"id", "Any"}
	}
	if rt == nil {
		rt = mindscript.S{"id", "Any"}
	}

	// Const div/mod by zero
	if op == "/" || op == "%" {
		if len(rhs) >= 2 {
			if tag, _ := rhs[0].(string); tag == "int" {
				if v, ok := rhs[1].(int64); ok && v == 0 {
					// Anchor at the RHS child token span (slot 3).
					c.diagAt(3, "MS-DIV-BY-ZERO-CONST", "division or modulo by constant zero")
				}
			}
		}
	}

	ip := c.ip
	if ip == nil {
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}

	isId := func(t mindscript.S, name string) bool {
		return len(t) >= 2 && t[0].(string) == "id" && t[1].(string) == name
	}

	switch op {
	case "-", "*", "%", "/", "**":
		// Nullable-operand hint (conservative, single-pass).
		isNullable := func(t mindscript.S) bool {
			return len(t) >= 3 && t[0] == "unop" && t[1] == "?"
		}
		if isNullable(lt) || isNullable(rt) {
			// Anchor on RHS by convention (mirrors other binop diags).
			c.diagAt(3, "MS-MAYBE-NULL-UNSAFE", "nullable value used in numeric operator")
		}
		// Numeric typing: Int ⊖ Int → Int; any Num involvement → Num; else mismatch.
		if isId(lt, "Int") && isId(rt, "Int") {
			return c.newFlowFromType(mindscript.S{"id", "Int"})
		}
		if (isId(lt, "Int") && isId(rt, "Num")) ||
			(isId(lt, "Num") && isId(rt, "Int")) ||
			(isId(lt, "Num") && isId(rt, "Num")) {
			return c.newFlowFromType(mindscript.S{"id", "Num"})
		}
		c.diagAt(3, "MS-ARG-TYPE-MISMATCH", "argument type mismatch")
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	case "+":
		// strings
		ltR := ip.ResolveType(lt, c.env)
		rtR := ip.ResolveType(rt, c.env)
		if ip.IsSubtype(ltR, mindscript.S{"id", "Str"}, c.env) &&
			ip.IsSubtype(rtR, mindscript.S{"id", "Str"}, c.env) {
			return c.newFlowFromType(mindscript.S{"id", "Str"})
		}
		// arrays
		if lElem, okL := arrayElemType(ltR); okL {
			if rElem, okR := arrayElemType(rtR); okR {
				elem := ip.UnifyTypes(lElem, rElem, c.env)
				if elem == nil {
					elem = mindscript.S{"id", "Any"}
				}
				return c.newFlowFromType(mindscript.S{"array", elem})
			}
		}
		// numeric
		if isId(lt, "Int") && isId(rt, "Int") {
			return c.newFlowFromType(mindscript.S{"id", "Int"})
		}
		if (isId(lt, "Int") && isId(rt, "Num")) ||
			(isId(lt, "Num") && isId(rt, "Int")) ||
			(isId(lt, "Num") && isId(rt, "Num")) {
			return c.newFlowFromType(mindscript.S{"id", "Num"})
		}
		// otherwise: type mismatch — anchor to RHS expression (slot 3)
		c.diagAt(3, "MS-ARG-TYPE-MISMATCH", "argument type mismatch")
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}
	return c.newFlowFromType(mindscript.S{"id", "Any"})
}

func (c *foldCtx) foldUnop(n mindscript.S) flow {
	if len(n) < 3 {
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}
	op, _ := n[1].(string)
	expr, _ := n[2].(mindscript.S)
	tf := c.childCtx(2).fold(expr)
	t := typeOf(c.ip, c.env, tf.Val)
	if t == nil {
		t = mindscript.S{"id", "Any"}
	}
	switch op {
	case "-":
		return c.newFlowFromType(t)
	case "!":
		return c.newFlowFromType(mindscript.S{"id", "Bool"})
	default:
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}
}

func (c *foldCtx) foldTypeExpr(n mindscript.S) flow {
	if len(n) < 2 {
		return c.newFlowFromType(mindscript.S{"id", "Type"})
	}
	typeAst, _ := n[1].(mindscript.S)
	// Stamp identifiers and map-field keys inside this type AST for hover.
	c.stampTypeIDs(typeAst, appendPath(c.path, 0))
	tv := mindscript.TypeValIn(typeAst, c.env)
	return flow{Val: tv}
}

func (c *foldCtx) foldDecl(n mindscript.S) flow {
	if len(n) < 2 || c.env == nil {
		return c.newFlowFromType(mindscript.S{"id", "Null"})
	}
	name, _ := n[1].(string)
	if name == "" {
		return c.newFlowFromType(mindscript.S{"id", "Null"})
	}
	v := newSymbolValIn(c.env, mindscript.S{"id", "Null"}, "")
	c.env.Define(name, v)
	// ("decl", name) is terminal → slot 0.
	c.stampIdent(0, name, true, v)
	return c.newFlowFromType(mindscript.S{"id", "Null"})
}

func (c *foldCtx) foldAssign(n mindscript.S) flow {
	if len(n) < 2 {
		return c.newFlow(nil)
	}
	lhs, _ := n[1].(mindscript.S)

	invalidTarget := func(msg string, t mindscript.S) flow {
		if msg == "" {
			msg = "invalid assignment target"
		}
		if s, e, ok := c.spanFor(); ok {
			c.diagBytes(s, e, "MS-INVALID-ASSIGN-TARGET", msg)
		} else {
			c.diagInternalNoSpanf("no span for assignment at %v (emitting MS-INVALID-ASSIGN-TARGET: %s)", c.path, msg)
		}
		if t == nil {
			t = mindscript.S{"id", "Any"}
		}
		return c.newFlowFromType(t)
	}

	if len(n) == 2 {
		return c.newFlow(nil)
	}

	rhs, _ := n[2].(mindscript.S)
	rhsFlow := c.childCtx(2).fold(rhs)
	rhsVal := rhsFlow.Val
	if (rhsVal == mindscript.Value{}) {
		rhsVal = newSymbolValIn(c.env, mindscript.S{"id", "Any"}, "")
	}
	ip := c.ip

	// let name = rhs
	if len(lhs) >= 2 && lhs[0].(string) == "decl" {
		name, _ := lhs[1].(string)
		if name != "" && c.env != nil {
			c.env.Define(name, rhsVal)
			declPath := appendPath(c.path, 0)
			if sp, ok := c.idx.Spans.Get(declPath); ok {
				if rhsVal.Tag == mindscript.VTSymbol || rhsVal.Tag == mindscript.VTType {
					c.idx.emitIdentAtStart(sp.StartByte, name, true, rhsVal)
				} else {
					c.idx.emitIdentAtStart(sp.StartByte, name, true, mindscript.Value{})
				}
			}
		}
		return flow{Val: rhsVal}
	}

	// name = rhs
	if len(lhs) >= 2 && lhs[0].(string) == "id" {
		name, _ := lhs[1].(string)
		if name == "" || c.env == nil {
			return flow{Val: rhsVal}
		}
		v, err := c.env.Get(name)
		if err != nil {
			c.stampIdent(1, name, false, mindscript.Value{})
			c.diagAt(1, "MS-UNKNOWN-NAME", "unknown name: "+name)
			return flow{Val: rhsVal}
		}

		if sym, ok := asSymbol(v); ok {
			newT := typeOf(ip, c.env, rhsVal)
			if newT == nil {
				newT = mindscript.S{"id", "Any"}
			}
			sym.Type = newT
			// If RHS carries a doc, update the symbol's doc.
			if strings.TrimSpace(rhsVal.Annot) != "" {
				v.Annot = strings.TrimSpace(rhsVal.Annot)
			}
			if err := c.env.Set(name, v); err != nil {
				return invalidTarget(err.Error(), newT)
			}
			c.stampIdent(1, name, false, v)
			return flow{Val: v}
		}

		if err := c.env.Set(name, rhsVal); err != nil {
			return invalidTarget(err.Error(), typeOf(ip, c.env, rhsVal))
		}
		c.stampIdent(1, name, false, mindscript.Value{})
		return flow{Val: rhsVal}
	}

	// destructuring helpers
	var hasDecl func(p mindscript.S) bool
	hasDecl = func(p mindscript.S) bool {
		if len(p) == 0 {
			return false
		}
		if tag, _ := p[0].(string); tag == "decl" {
			return true
		}
		for i := 1; i < len(p); i++ {
			if sub, ok := p[i].(mindscript.S); ok && hasDecl(sub) {
				return true
			}
		}
		return false
	}

	// bind recursively assigns types to pattern leaves, optionally
	// carrying the specific RHS AST node for better precision (e.g., array literals).
	var bind func(
		pat mindscript.S,
		pth mindscript.NodePath,
		t mindscript.S, decl bool, doc string,
		rhsNode mindscript.S)
	bind = func(p mindscript.S, pth mindscript.NodePath, t mindscript.S, decl bool, doc string, rhsNode mindscript.S) {
		if t == nil {
			t = mindscript.S{"id", "Any"}
		}
		if len(p) == 0 {
			return
		}
		tag, _ := p[0].(string)

		switch tag {
		case "decl":
			if len(p) < 2 {
				return
			}
			if name, ok := p[1].(string); ok && name != "" {
				v := newSymbolValIn(c.env, t, doc)
				c.env.Define(name, v)
				if sp, ok := c.idx.Spans.Get(pth); ok {
					c.idx.emitIdentAtStart(sp.StartByte, name, true, v)
				}
				return
			}
			if sub, ok := p[1].(mindscript.S); ok {
				bind(sub, appendPath(pth, 0), t, true, doc, rhsNode)
			}

		case "id":
			if len(p) < 2 {
				return
			}
			name, _ := p[1].(string)
			if name == "" {
				return
			}
			if decl {
				v := newSymbolValIn(c.env, t, doc)
				c.env.Define(name, v)
				if sp, ok := c.idx.Spans.Get(pth); ok {
					c.idx.emitIdentAtStart(sp.StartByte, name, true, v)
				}
				return
			}
			v, err := c.env.Get(name)
			if err != nil {
				c.diagAtPath(pth, "MS-UNKNOWN-NAME", "unknown name: "+name)
				return
			}
			if sym, ok := asSymbol(v); ok {
				sym.Type = t
				// Update doc on reassignment if provided.
				if strings.TrimSpace(doc) != "" {
					v.Annot = strings.TrimSpace(doc)
				}
				if err := c.env.Set(name, v); err != nil {
					c.diagAtPath(pth, "MS-INVALID-ASSIGN-TARGET", err.Error())
				}
				if sp, ok := c.idx.Spans.Get(pth); ok {
					c.idx.emitIdentAtStart(sp.StartByte, name, false, v)
				}
			} else if err := c.env.Set(name, newSymbolValIn(c.env, t, doc)); err != nil {
				c.diagAtPath(pth, "MS-INVALID-ASSIGN-TARGET", err.Error())
			}

		case "darr":
			// Default element type from the array type (fallback for non-literal RHS).
			elemT, okElem := arrayElemType(t)
			if !okElem || elemT == nil {
				elemT = mindscript.S{"id", "Any"}
			}

			pats := len(p) - 1
			// If we have the concrete RHS node and it is an array literal,
			// we can type each target by its corresponding element expression.
			isRHSLiteralArray := len(rhsNode) > 0
			if isRHSLiteralArray {
				if tag, _ := rhsNode[0].(string); tag != "array" {
					isRHSLiteralArray = false
				}
			}
			if pats > 0 && isRHSLiteralArray {
				// Too few elements → diagnostic (kept from previous behavior).
				if len(rhsNode)-1 < pats {
					c.diagAt(2, "MS-INVALID-ASSIGN-TARGET", "array destructuring RHS has too few elements")
				}
			}

			for i := 1; i < len(p); i++ {
				sub, ok := p[i].(mindscript.S)
				if !ok {
					continue
				}
				// Start with the fallback element type.
				tForSlot := elemT
				var subRHS mindscript.S
				docForSlot := ""

				// If RHS is an array literal and has the i-th element, compute its precise type.
				if isRHSLiteralArray && (1+i-1) < len(rhsNode) {
					if sr, ok := rhsNode[i].(mindscript.S); ok {
						subRHS = sr
						// Build a child context whose path points at rhs element (slot 2 for RHS, then element index).
						cc := c.childCtx(2)
						cc.path = appendPath(cc.path, i-1)
						sf := cc.fold(subRHS)
						if tv := typeOf(c.ip, cc.env, sf.Val); tv != nil {
							tForSlot = tv
						}
						// Prefer value doc from the actual element; else from the element type's outer annot.
						docForSlot = preferDoc(sf.Val, tForSlot)
					}
				}
				if docForSlot == "" {
					docForSlot = outerDocFromType(tForSlot)
				}
				bind(sub, appendPath(pth, i-1), tForSlot, decl, docForSlot, subRHS)
			}

		case "dobj":
			for i := 1; i < len(p); i++ {
				pair, ok := p[i].(mindscript.S)
				if !ok || len(pair) < 3 {
					continue
				}
				keyNode, _ := pair[1].(mindscript.S)
				if len(keyNode) < 2 || keyNode[0].(string) != "str" {
					continue
				}
				key, _ := keyNode[1].(string)
				sub, _ := pair[2].(mindscript.S)

				ft, found := mapFieldType(t, key)
				if !found && decl {
					bind(sub,
						appendPath(appendPath(pth, i-1), 1),
						mindscript.S{"id", "Null"},
						true,
						"object pattern: missing key '"+key+"'",
						nil,
					)
				} else {
					if ft == nil {
						ft = mindscript.S{"id", "Any"}
					}
					// If RHS is a map literal, try to find the concrete field expression to refine type and doc.
					docForField := outerDocFromType(ft)
					var subRHS mindscript.S
					isRHSLiteralMap := len(rhsNode) > 0 && rhsNode[0] == "map"
					if isRHSLiteralMap {
						for j := 1; j < len(rhsNode); j++ {
							rp, ok := rhsNode[j].(mindscript.S)
							if !ok || len(rp) < 3 {
								continue
							}
							ptag, _ := rp[0].(string)
							if ptag != "pair" && ptag != "pair!" {
								continue
							}
							rk, _ := rp[1].(mindscript.S)
							if len(rk) < 2 || rk[0].(string) != "str" {
								continue
							}
							if k, _ := rk[1].(string); k == key {
								if rv, ok := rp[2].(mindscript.S); ok {
									subRHS = rv
									cc := c.childCtx(2)
									// element j is at (assign slot 2) -> (map element j-1)
									cc.path = appendPath(cc.path, j-1)
									sf := cc.fold(subRHS)
									if tv := typeOf(c.ip, cc.env, sf.Val); tv != nil {
										ft = tv
									}
									if d := preferDoc(sf.Val, ft); d != "" {
										docForField = d
									}
								}
							}
						}
					}
					bind(sub, appendPath(appendPath(pth, i-1), 1), ft, decl, docForField, subRHS)
				}
			}
		default:
			return
		}
	}

	if len(lhs) > 0 {
		if tag, _ := lhs[0].(string); tag == "darr" || tag == "dobj" {
			bind(lhs, appendPath(c.path, 0), typeOf(ip, c.env, rhsVal), hasDecl(lhs), "", rhs)
			return flow{Val: rhsVal}
		}
	}

	if len(lhs) > 0 {
		if tag, _ := lhs[0].(string); tag == "get" || tag == "idx" || tag == "darr" || tag == "dobj" {
			return flow{Val: rhsVal}
		}
	}

	return invalidTarget("", typeOf(ip, c.env, rhsVal))
}

func (c *foldCtx) foldBlock(n mindscript.S) flow {
	mode := blockPlain
	if c.topLevel {
		mode = blockTopLevel
	}
	return c.foldBlockWithMode(n, mode)
}

func (c *foldCtx) foldBlockWithMode(n mindscript.S, mode blockMode) flow {
	if len(n) == 0 {
		return c.newFlowFromType(mindscript.S{"id", "Null"})
	}

	env := c.env
	if mode != blockTopLevel {
		env = mindscript.NewEnv(env)
	}
	child := *c
	child.env = env
	child.topLevel = false

	ip := c.ip

	var out flow
	var haveVal bool

	unifyVals := func(a, b mindscript.Value) mindscript.Value {
		if (a == mindscript.Value{}) {
			return b
		}
		if (b == mindscript.Value{}) {
			return a
		}
		ta := typeOf(ip, env, a)
		tb := typeOf(ip, env, b)
		if ip == nil {
			return newSymbolValIn(env, mindscript.S{"id", "Any"}, "")
		}
		u := ip.UnifyTypes(ta, tb, env)
		return newSymbolValIn(env, u, "")
	}

	var funRet, loopBrk, loopCont mindscript.Value

	for i := 1; i < len(n); i++ {
		sub, ok := n[i].(mindscript.S)
		if !ok {
			continue
		}
		cc := child
		cc.path = appendPath(c.path, i-1)
		sf := cc.fold(sub)

		if sf.HasRet {
			if mode == blockFunBody {
				funRet = unifyVals(funRet, sf.Ret)
			} else {
				out.Ret = unifyVals(out.Ret, sf.Ret)
				out.HasRet = true
			}
			out.Terminated = true
		}
		if sf.HasBrk {
			if mode == blockLoopBody {
				loopBrk = unifyVals(loopBrk, sf.Brk)
			} else {
				out.Brk = unifyVals(out.Brk, sf.Brk)
				out.HasBrk = true
			}
			out.Terminated = true
		}
		if sf.HasCont {
			if mode == blockLoopBody {
				loopCont = unifyVals(loopCont, sf.Cont)
			} else {
				out.Cont = unifyVals(out.Cont, sf.Cont)
				out.HasCont = true
			}
			out.Terminated = true
		}

		if !sf.Terminated && (sf.Val != mindscript.Value{}) {
			out.Val = sf.Val
			haveVal = true
		}
		if sf.Terminated {
			break
		}
	}

	switch mode {
	case blockFunBody:
		if (funRet != mindscript.Value{}) {
			out.Ret = funRet
			out.HasRet = true
		}
	case blockLoopBody:
		if (loopBrk != mindscript.Value{}) {
			out.Brk = loopBrk
			out.HasBrk = true
		}
		if (loopCont != mindscript.Value{}) {
			out.Cont = loopCont
			out.HasCont = true
		}
	}

	if !haveVal {
		out.Val = newSymbolValIn(env, mindscript.S{"id", "Null"}, "")
	}
	return out
}

func (c *foldCtx) foldIf(n mindscript.S) flow {
	if len(n) < 2 {
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}

	ip := c.ip
	env := c.env

	unifyVals := func(a, b mindscript.Value) mindscript.Value {
		if (a == mindscript.Value{}) {
			return b
		}
		if (b == mindscript.Value{}) {
			return a
		}
		ta := typeOf(ip, env, a)
		tb := typeOf(ip, env, b)
		if ip == nil {
			return newSymbolValIn(env, mindscript.S{"id", "Any"}, "")
		}
		u := ip.UnifyTypes(ta, tb, env)
		return newSymbolValIn(env, u, "")
	}

	var out flow
	var armsVal mindscript.Value
	hasElse := false

	for i := 1; i < len(n); i++ {
		arm, ok := n[i].(mindscript.S)
		if !ok || len(arm) == 0 {
			continue
		}
		tag, _ := arm[0].(string)

		if tag == "pair" && len(arm) >= 3 {
			cond, _ := arm[1].(mindscript.S)
			thenBlock, _ := arm[2].(mindscript.S)
			_ = c.childCtx(i).childCtx(1).fold(cond)
			tf := c.childCtx(i).childCtx(2).foldBlockWithMode(thenBlock, blockPlain)

			if tf.HasRet {
				out.Ret = unifyVals(out.Ret, tf.Ret)
				out.HasRet = true
			}
			if tf.HasBrk {
				out.Brk = unifyVals(out.Brk, tf.Brk)
				out.HasBrk = true
			}
			if tf.HasCont {
				out.Cont = unifyVals(out.Cont, tf.Cont)
				out.HasCont = true
			}
			if !tf.Terminated && (tf.Val != mindscript.Value{}) {
				armsVal = unifyVals(armsVal, tf.Val)
			}
			continue
		}

		if tag == "block" && i == len(n)-1 {
			hasElse := true
			_ = hasElse
			ef := c.childCtx(i).foldBlockWithMode(arm, blockPlain)

			if ef.HasRet {
				out.Ret = unifyVals(out.Ret, ef.Ret)
				out.HasRet = true
			}
			if ef.HasBrk {
				out.Brk = unifyVals(out.Brk, ef.Brk)
				out.HasBrk = true
			}
			if ef.HasCont {
				out.Cont = unifyVals(out.Cont, ef.Cont)
				out.HasCont = true
			}
			if !ef.Terminated && (ef.Val != mindscript.Value{}) {
				armsVal = unifyVals(armsVal, ef.Val)
			}
		}
	}

	if !hasElse {
		armsVal = unifyVals(armsVal, newSymbolValIn(env, mindscript.S{"id", "Null"}, ""))
	}
	if (armsVal == mindscript.Value{}) {
		armsVal = newSymbolValIn(env, mindscript.S{"id", "Null"}, "")
	}

	out.Val = armsVal
	return out
}

func (c *foldCtx) foldWhile(n mindscript.S) flow {
	if len(n) < 3 {
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}
	cond, _ := n[1].(mindscript.S)
	body, _ := n[2].(mindscript.S)
	_ = c.childCtx(1).fold(cond)

	bf := c.childCtx(2).foldBlockWithMode(body, blockLoopBody)

	ip := c.ip
	env := c.env

	unifyVals := func(a, b mindscript.Value) mindscript.Value {
		if (a == mindscript.Value{}) {
			return b
		}
		if (b == mindscript.Value{}) {
			return a
		}
		ta := typeOf(ip, env, a)
		tb := typeOf(ip, env, b)
		if ip == nil {
			return newSymbolValIn(env, mindscript.S{"id", "Any"}, "")
		}
		u := ip.UnifyTypes(ta, tb, env)
		return newSymbolValIn(env, u, "")
	}

	var tv mindscript.Value
	if (bf.Val != mindscript.Value{}) {
		tv = unifyVals(tv, bf.Val)
	}
	if bf.HasBrk {
		tv = unifyVals(tv, bf.Brk)
	}
	tv = unifyVals(tv, newSymbolValIn(env, mindscript.S{"id", "Null"}, ""))
	if (tv == mindscript.Value{}) {
		tv = newSymbolValIn(env, mindscript.S{"id", "Null"}, "")
	}

	return flow{
		Val:    tv,
		Ret:    bf.Ret,
		HasRet: bf.HasRet,
	}
}

func (c *foldCtx) foldFor(n mindscript.S) flow {
	// ("for", target, iter, bodyBlock)
	if len(n) < 4 {
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}
	target, _ := n[1].(mindscript.S)
	iter, _ := n[2].(mindscript.S)
	body, _ := n[3].(mindscript.S)

	// Create loop scope and predeclare any ("decl", name) leaves in target.
	loopEnv := mindscript.NewEnv(c.env)

	var walk func(p mindscript.S, pth mindscript.NodePath)
	walk = func(p mindscript.S, pth mindscript.NodePath) {
		if len(p) == 0 {
			return
		}
		switch p[0] {
		case "decl", "id":
			if len(p) >= 2 {
				// Atomic name → predeclare and emit a *declaration* token.
				if name, ok := p[1].(string); ok && name != "" {
					val := newSymbolValIn(loopEnv, mindscript.S{"id", "Any"}, "")
					loopEnv.Define(name, val)
					// Use centralized stamping to ensure correct token/isDecl/payload handling.
					cc := *c
					cc.env = loopEnv
					cc.path = pth
					cc.stampIdent(0, name, true, val)
					return
				}
				// Structured subpattern: recurse for both decl and id.
				if sub, ok := p[1].(mindscript.S); ok {
					walk(sub, appendPath(pth, 0))
					return
				}
			}
		case "darr":
			for i := 1; i < len(p); i++ {
				if sub, ok := p[i].(mindscript.S); ok {
					walk(sub, appendPath(pth, i-1))
				}
			}
		case "dobj":
			for i := 1; i < len(p); i++ {
				if pair, ok := p[i].(mindscript.S); ok && len(pair) >= 3 {
					if sub, ok := pair[2].(mindscript.S); ok {
						walk(sub, appendPath(appendPath(pth, i-1), 1))
					}
				}
			}
		}
	}
	// Target is child slot 1 of "for".
	walk(target, appendPath(c.path, 0))

	_ = c.childCtx(2).fold(iter)
	child := *c
	child.env = loopEnv
	bf := child.childCtx(3).foldBlockWithMode(body, blockLoopBody)

	ip := c.ip
	env := c.env

	unifyVals := func(a, b mindscript.Value) mindscript.Value {
		if (a == mindscript.Value{}) {
			return b
		}
		if (b == mindscript.Value{}) {
			return a
		}
		ta := typeOf(ip, env, a)
		tb := typeOf(ip, env, b)
		if ip == nil {
			return newSymbolValIn(env, mindscript.S{"id", "Any"}, "")
		}
		u := ip.UnifyTypes(ta, tb, env)
		return newSymbolValIn(env, u, "")
	}

	var tv mindscript.Value
	if (bf.Val != mindscript.Value{}) {
		tv = unifyVals(tv, bf.Val)
	}
	if bf.HasBrk {
		tv = unifyVals(tv, bf.Brk)
	}
	tv = unifyVals(tv, newSymbolValIn(env, mindscript.S{"id", "Null"}, ""))
	if (tv == mindscript.Value{}) {
		tv = newSymbolValIn(env, mindscript.S{"id", "Null"}, "")
	}

	return flow{
		Val:    tv,
		Ret:    bf.Ret,
		HasRet: bf.HasRet,
	}
}

func (c *foldCtx) foldFunLike(n mindscript.S) flow {
	if len(n) < 3 {
		return c.newFlowFromType(mindscript.S{"id", "Any"})
	}

	tag, _ := n[0].(string)
	isOracle := tag == "oracle"

	// Params array node (child slot 1)
	paramsNode, _ := n[1].(mindscript.S)
	paramsPath := appendPath(c.path, 0)

	var paramNames []string
	var paramTypes []mindscript.S
	var paramIDPaths []mindscript.NodePath
	if len(paramsNode) >= 1 && paramsNode[0] == "array" {
		for i := 1; i < len(paramsNode); i++ {
			p, ok := paramsNode[i].(mindscript.S)
			if !ok || len(p) < 2 {
				continue
			}
			nameNode, _ := p[1].(mindscript.S)
			if len(nameNode) < 2 || nameNode[0].(string) != "id" {
				continue
			}
			name, _ := nameNode[1].(string)
			if name == "" {
				continue
			}
			pt := mindscript.S{"id", "Any"}
			if len(p) >= 3 {
				if ts, ok := p[2].(mindscript.S); ok {
					pt = ts
				}
			}
			idPath := appendPath(appendPath(paramsPath, i-1), 0)
			paramIDPaths = append(paramIDPaths, idPath)

			paramNames = append(paramNames, name)
			paramTypes = append(paramTypes, pt)
		}
	}

	// Stamp type identifiers inside parameter type ASTs for hover.
	if len(paramTypes) > 0 {
		for i := range paramTypes {
			// paramsPath -> element i -> slot 1 is the type AST
			tp := appendPath(appendPath(paramsPath, i), 1)
			c.stampTypeIDs(paramTypes[i], tp)
		}
	}

	retT := mindscript.S{"id", "Any"}
	if rt, ok := n[2].(mindscript.S); ok {
		retT = rt
	}

	// Apply oracle nullability quirk:
	effRet := retT
	if isOracle {
		isNullable := len(retT) >= 3 && retT[0] == "unop" && retT[1] == "?"
		isAny := len(retT) >= 2 && retT[0] == "id" && retT[1] == "Any"
		if !isNullable && !isAny {
			effRet = mindscript.S{"unop", "?", retT}
		}
	}

	// Stamp type identifiers inside return type AST for hover.
	rtPath := appendPath(c.path, 1)
	c.stampTypeIDs(retT, rtPath)

	funType := effRet
	if len(paramTypes) == 0 {
		funType = mindscript.S{"binop", "->", mindscript.S{"id", "Null"}, effRet}
	} else {
		for i := len(paramTypes) - 1; i >= 0; i-- {
			funType = mindscript.S{"binop", "->", paramTypes[i], funType}
		}
	}

	if !isOracle && len(n) >= 4 {
		if body, ok := n[3].(mindscript.S); ok && len(body) > 0 && body[0] == "block" {
			funEnv := mindscript.NewEnv(c.env)
			for i := range paramNames {
				// Keep full (possibly annotated) param type; extract outer doc for symbol.
				pdoc := outerDocFromType(paramTypes[i])
				val := newSymbolValIn(funEnv, paramTypes[i], pdoc)
				funEnv.Define(paramNames[i], val)
				if sp, ok := c.idx.Spans.Get(paramIDPaths[i]); ok {
					c.idx.emitIdentAtStart(sp.StartByte, paramNames[i], true, val)
				}
			}
			child := *c
			child.env = funEnv
			child.topLevel = false
			child.path = appendPath(c.path, 2)
			bf := child.foldBlockWithMode(body, blockFunBody)

			if c.ip != nil {
				var observed mindscript.S
				if bf.HasRet {
					observed = typeOf(c.ip, funEnv, bf.Ret)
				}
				if !bf.Terminated && (bf.Val != mindscript.Value{}) {
					tv := typeOf(c.ip, funEnv, bf.Val)
					if observed == nil {
						observed = tv
					} else {
						observed = c.ip.UnifyTypes(observed, tv, funEnv)
					}
				}
				if observed != nil && !c.ip.IsSubtype(observed, effRet, funEnv) {
					c.diagAt(2, "MS-RET-TYPE-MISMATCH",
						fmt.Sprintf("return type mismatch: got %s, expected %s (function %s)",
							mindscript.FormatType(observed),
							mindscript.FormatType(effRet),
							mindscript.FormatType(funType),
						))
				}
			}
		}
	} else if isOracle {
		oracleEnv := mindscript.NewEnv(c.env)
		for i := range paramNames {
			pdoc := outerDocFromType(paramTypes[i])
			val := newSymbolValIn(oracleEnv, paramTypes[i], pdoc)
			oracleEnv.Define(paramNames[i], val)
			if sp, ok := c.idx.Spans.Get(paramIDPaths[i]); ok {
				c.idx.emitIdentAtStart(sp.StartByte, paramNames[i], true, val)
			}
		}
	}

	return c.newFlowFromType(funType)
}

func (c *foldCtx) foldModule(n mindscript.S) flow {
	// Placeholder; future: module env & exports map type.
	_ = n
	return c.newFlow(nil)
}
