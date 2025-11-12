// cmd/msg-lsp/analysis.go
//
// New MindScript static analyzer.
//
// Key constraints:
//   - Pure static, single-pass, abstract interpreter.
//   - Uses ONLY mindscript APIs visible in interpreter.go and types.go.
//   - Uses real *mindscript.Env* for all scopes.
//   - Analysis domain:
//       • VTType  (as in runtime) for type aliases / schema values.
//       • VTSymbol (analysis-only) summarizing inferred type/docs for values.
//   - No Scope / Symbol / NameRef graph like the old implementation.
//   - Dispatcher-style fold() with per-tag handlers, kept modular & extensible.
//   - Never Eval/Apply user code; all reasoning goes through Interpreter helpers.

package main

import (
	mindscript "github.com/DAIOS-AI/msg/internal/mindscript"
)

////////////////////////////////////////////////////////////////////////////////
// Core analysis result types
////////////////////////////////////////////////////////////////////////////////

// Diag is a single diagnostic attached to a byte range in the current file.
type Diag struct {
	StartByte int
	EndByte   int
	Code      string
	Message   string
	// Severity, related info, quick-fixes, etc. can be added later.
}

// FileIndex is the per-file analysis product.
type FileIndex struct {
	URI  string
	Text string

	// Parsed artifacts.
	AST   mindscript.S
	Spans *mindscript.SpanIndex // from parser/spans; used only for ranges.

	// RootEnv is the file’s root analysis environment:
	//   RootEnv -> ambient/prelude (ip.Global/Base/Core)
	RootEnv *mindscript.Env

	// NodeTypes holds synthesized expression types for tooling.
	// Keying strategy (e.g. by span or stable node-id) is left for later.
	NodeTypes map[interface{}]mindscript.S

	// Diagnostics collected during analysis.
	Diags []Diag
}

// addDiag appends a diagnostic.
func (idx *FileIndex) addDiag(d Diag) {
	idx.Diags = append(idx.Diags, d)
}

// rememberType records the type for an expression node.
func (idx *FileIndex) rememberType(key interface{}, t mindscript.S) {
	if idx.NodeTypes == nil {
		idx.NodeTypes = make(map[interface{}]mindscript.S)
	}
	idx.NodeTypes[key] = t
}

////////////////////////////////////////////////////////////////////////////////
// Analyzer: shared interpreter + entrypoint
////////////////////////////////////////////////////////////////////////////////

type Analyzer struct {
	IP *mindscript.Interpreter
}

// ensureIP lazily constructs an Interpreter for ambient types/prelude.
func (a *Analyzer) ensureIP() *mindscript.Interpreter {
	if a == nil {
		return nil
	}
	if a.IP != nil {
		return a.IP
	}
	ip, err := mindscript.NewInterpreter()
	if err != nil {
		// Hard failure to build ambient; analyzer should degrade gracefully.
		return nil
	}
	a.IP = ip
	return ip
}

// Analyze is the main entry:
//   - parse with spans
//   - build file RootEnv (child of ambient/prelude)
//   - single-pass fold over AST using dispatcher
func (a *Analyzer) Analyze(uri, text string) *FileIndex {
	idx := &FileIndex{
		URI:  uri,
		Text: text,
	}

	ip := a.ensureIP()

	ast, spans, err := mindscript.ParseSExprWithSpans(text)
	if err != nil {
		// Map parse error into a single diagnostic; span mapping TBD.
		idx.addDiag(Diag{
			StartByte: 0,
			EndByte:   0,
			Code:      "MS-PARSE",
			Message:   err.Error(),
		})
		return idx
	}
	idx.AST = ast
	idx.Spans = spans

	// Build RootEnv for this file: a real Env that chains into ambient.
	var root *mindscript.Env
	if ip != nil && ip.Global != nil {
		root = mindscript.NewEnv(ip.Global)
	} else {
		root = mindscript.NewEnv(nil)
	}
	idx.RootEnv = root

	// Single-pass abstract interpretation.
	fc := &foldCtx{
		idx:      idx,
		ip:       ip,
		env:      root,
		topLevel: true,
	}
	fc.fold(idx.AST)

	return idx
}

////////////////////////////////////////////////////////////////////////////////
// Analysis value domain
////////////////////////////////////////////////////////////////////////////////

// VTSymbol is an analysis-only summary value, stored in Env table.
type VTSymbol struct {
	Type mindscript.S
	Doc  string
	// Flags like IsTopLevel / IsAmbient / etc. can be added later.
}

// newSymbolVal wraps a VTSymbol into a runtime Value for Env storage.
func newSymbolVal(sym VTSymbol) mindscript.Value {
	return mindscript.Value{
		Tag:   mindscript.VTHandle, // analysis-only payload
		Data:  sym,
		Annot: sym.Doc,
	}
}

// asSymbol tries to view a Value as VTSymbol; returns (sym, ok).
func asSymbol(v mindscript.Value) (VTSymbol, bool) {
	if v.Data == nil {
		return VTSymbol{}, false
	}
	sym, ok := v.Data.(VTSymbol)
	return sym, ok
}

////////////////////////////////////////////////////////////////////////////////
// foldCtx, block modes & flow
////////////////////////////////////////////////////////////////////////////////

// foldCtx carries analysis state during folding.
type foldCtx struct {
	idx *FileIndex
	ip  *mindscript.Interpreter
	env *mindscript.Env

	// topLevel reports whether we're in the file's outermost block.
	topLevel bool
}

// blockMode controls how a block handles control-flow from its children.
type blockMode int

const (
	blockPlain    blockMode = iota
	blockTopLevel           // file root
	blockFunBody            // catches returns for a function/oracle
	blockLoopBody           // catches break/continue for a loop
)

// flow summarizes the abstract result of an expression / construct.
// All semantic information is carried via Values:
//
//   - VTSymbol (VTHandle+VTSymbol) for inferred expression values.
//   - VTType   for type aliases / schema values.
//   - Ambient/runtime values are allowed; typeOf() collapses them.
type flow struct {
	// Val is the value along the normal fallthrough path.
	// For expressions this is usually a VTSymbol; for ("type") it is VTType.
	Val mindscript.Value

	// Control-flow payloads; guarded by Has* flags so null-return is representable.
	Ret, Brk, Cont mindscript.Value
	HasRet         bool
	HasBrk         bool
	HasCont        bool

	// Terminated means control does not fall through past this construct.
	Terminated bool
}

// newFlow wraps a static type into a fallthrough VTSymbol.
func newFlow(t mindscript.S) flow {
	if t == nil {
		t = mindscript.S{"id", "Any"}
	}
	return flow{Val: newSymbolVal(VTSymbol{Type: t})}
}

// newFlowFromType is an alias for newFlow when starting from an S-type.
func newFlowFromType(t mindscript.S) flow {
	return newFlow(t)
}

// typeOf returns the static type S of v as seen by the analyzer.
func typeOf(ip *mindscript.Interpreter, env *mindscript.Env, v mindscript.Value) mindscript.S {
	if sym, ok := asSymbol(v); ok {
		return sym.Type
	}
	if v.Tag == mindscript.VTType {
		// Value is a schema/type; its expression type is `Type`.
		return mindscript.S{"id", "Type"}
	}
	if ip != nil {
		return ip.ValueToType(v, env)
	}
	return mindscript.S{"id", "Any"}
}

////////////////////////////////////////////////////////////////////////////////
// Dispatcher-style fold
////////////////////////////////////////////////////////////////////////////////

func (c *foldCtx) fold(n mindscript.S) flow {
	if len(n) == 0 {
		return newFlow(nil)
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

	// Default: visit children for side-effects/types (to be filled in later if needed).
	return newFlow(nil)
}

////////////////////////////////////////////////////////////////////////////////
// Per-tag handlers (current subset)
////////////////////////////////////////////////////////////////////////////////

// ("return") or ("return", expr)
func (c *foldCtx) foldReturn(n mindscript.S) flow {
	t := mindscript.S{"id", "Null"}
	if len(n) >= 2 {
		if expr, ok := n[1].(mindscript.S); ok {
			ef := c.fold(expr)
			tv := typeOf(c.ip, c.env, ef.Val)
			if tv != nil {
				t = tv
			}
		}
	}
	return flow{
		Val:        newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Null"}}),
		Terminated: true,
		Ret:        newSymbolVal(VTSymbol{Type: t}),
		HasRet:     true,
	}
}

// ("break")
func (c *foldCtx) foldBreak(n mindscript.S) flow {
	_ = n
	v := newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Null"}})
	return flow{
		Val:        v,
		Terminated: true,
		Brk:        v,
		HasBrk:     true,
	}
}

// ("continue")
func (c *foldCtx) foldContinue(n mindscript.S) flow {
	_ = n
	v := newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Null"}})
	return flow{
		Val:        v,
		Terminated: true,
		Cont:       v,
		HasCont:    true,
	}
}

// ("annot", ("str", doc), expr)
// Parser has already normalized/merged related comments into this node.
func (c *foldCtx) foldAnnot(n mindscript.S) flow {
	if len(n) < 3 {
		return newFlow(nil)
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

	inner := c.fold(expr)

	if doc != "" {
		v := inner.Val
		if sym, ok := asSymbol(v); ok {
			sym.Doc = doc
			v = newSymbolVal(sym)
		}
		v.Annot = doc
		inner.Val = v
	}

	return inner
}

func (c *foldCtx) foldLiteral(n mindscript.S) flow {
	if len(n) == 0 {
		return newFlow(nil)
	}
	switch n[0].(string) {
	case "int":
		return newFlowFromType(mindscript.S{"id", "Int"})
	case "num":
		return newFlowFromType(mindscript.S{"id", "Num"})
	case "str":
		return newFlowFromType(mindscript.S{"id", "Str"})
	case "bool":
		return newFlowFromType(mindscript.S{"id", "Bool"})
	case "null":
		return newFlowFromType(mindscript.S{"id", "Null"})
	default:
		return newFlow(nil)
	}
}

func (c *foldCtx) foldId(n mindscript.S) flow {
	if len(n) < 2 {
		return newFlow(nil)
	}
	name, _ := n[1].(string)
	if name == "" || c.env == nil {
		return newFlow(nil)
	}

	v, err := c.env.Get(name)
	if err != nil {
		// Unknown name: report and treat as Any so analysis can continue.
		c.idx.addDiag(Diag{
			StartByte: 0, // TODO: use Spans to target the id.
			EndByte:   0,
			Code:      "MS-UNKNOWN-NAME",
			Message:   "unknown name: " + name,
		})
		return newFlowFromType(mindscript.S{"id", "Any"})
	}

	// VTSymbol: analysis symbol.
	if _, ok := asSymbol(v); ok {
		return flow{Val: v}
	}

	// VTType: using a type value as an expression → keep VTType (typeOf=Type).
	if v.Tag == mindscript.VTType {
		return flow{Val: v}
	}

	// Ambient/runtime values: approximate via ValueToType when possible.
	if c.ip != nil {
		return newFlowFromType(c.ip.ValueToType(v, c.env))
	}

	return newFlowFromType(mindscript.S{"id", "Any"})
}

func (c *foldCtx) foldArray(n mindscript.S) flow {
	// [] → [Any]
	if len(n) == 1 {
		return newFlowFromType(mindscript.S{"array", mindscript.S{"id", "Any"}})
	}

	ip := c.ip
	elemT := mindscript.S{"id", "Any"}
	first := true

	for i := 1; i < len(n); i++ {
		sub, ok := n[i].(mindscript.S)
		if !ok {
			continue
		}
		sf := c.fold(sub)
		t := typeOf(ip, c.env, sf.Val)
		if t == nil {
			t = mindscript.S{"id", "Any"}
		}
		if ip == nil {
			elemT = mindscript.S{"id", "Any"}
			continue
		}
		if first {
			elemT = t
			first = false
		} else {
			elemT = ip.UnifyTypes(elemT, t, c.env)
		}
	}

	if first {
		elemT = mindscript.S{"id", "Any"}
	}

	return newFlowFromType(mindscript.S{"array", elemT})
}

func (c *foldCtx) foldMap(n mindscript.S) flow {
	// ("map", ("pair" | "pair!", ("str", key), valueExpr)...)
	if len(n) == 1 {
		return newFlowFromType(mindscript.S{"map"})
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
		valFlow := c.fold(valNode)
		valType := typeOf(c.ip, c.env, valFlow.Val)
		if valType == nil {
			valType = mindscript.S{"id", "Any"}
		}

		out = append(out, mindscript.S{tag, mindscript.S{"str", key}, valType})
	}

	return newFlowFromType(out)
}

// mapFieldType returns the field type for key in a structural ("map", ...) type.
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

// arrayElemType returns the element type from a structural ("array", T) type.
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
	// ("get", recvExpr, ("str", key))
	if len(n) < 3 {
		return newFlowFromType(mindscript.S{"id", "Any"})
	}

	recvNode, _ := n[1].(mindscript.S)
	keyNode, _ := n[2].(mindscript.S)

	recvFlow := c.fold(recvNode)
	recvT := typeOf(c.ip, c.env, recvFlow.Val)
	if recvT == nil {
		recvT = mindscript.S{"id", "Any"}
	}

	if c.ip != nil {
		recvT = c.ip.ResolveType(recvT, c.env)
	}

	// Only handle literal string keys here.
	if len(keyNode) < 2 || keyNode[0].(string) != "str" {
		return newFlowFromType(mindscript.S{"id", "Any"})
	}
	key, _ := keyNode[1].(string)

	if ft, ok := mapFieldType(recvT, key); ok {
		return newFlowFromType(ft)
	}

	// Known map/module type without that key → warning + Any.
	if len(recvT) > 0 {
		if tag, _ := recvT[0].(string); tag == "map" {
			c.idx.addDiag(Diag{
				StartByte: 0,
				EndByte:   0,
				Code:      "MS-MAP-MISSING-KEY",
				Message:   "missing key '" + key + "' on map/module",
			})
		}
	}

	return newFlowFromType(mindscript.S{"id", "Any"})
}

func (c *foldCtx) foldIdx(n mindscript.S) flow {
	// ("idx", recvExpr, indexExpr)
	if len(n) < 3 {
		return newFlowFromType(mindscript.S{"id", "Any"})
	}

	recvNode, _ := n[1].(mindscript.S)
	idxNode, _ := n[2].(mindscript.S)

	recvFlow := c.fold(recvNode)
	idxFlow := c.fold(idxNode)

	recvT := typeOf(c.ip, c.env, recvFlow.Val)
	if recvT == nil {
		recvT = mindscript.S{"id", "Any"}
	}
	idxT := typeOf(c.ip, c.env, idxFlow.Val)
	if idxT == nil {
		idxT = mindscript.S{"id", "Any"}
	}

	ip := c.ip
	if ip != nil {
		recvT = ip.ResolveType(recvT, c.env)
		idxT = ip.ResolveType(idxT, c.env)
	}

	isId := func(t mindscript.S, name string) bool {
		return len(t) >= 2 && t[0].(string) == "id" && t[1].(string) == name
	}

	// Array indexing: arr[int] → Elem, non-int → MS-ARG-TYPE-MISMATCH.
	if elemT, ok := arrayElemType(recvT); ok {
		okInt := isId(idxT, "Int")
		if !okInt && ip != nil {
			okInt = ip.IsSubtype(idxT, mindscript.S{"id", "Int"}, c.env)
		}
		if !okInt {
			c.idx.addDiag(Diag{
				StartByte: 0,
				EndByte:   0,
				Code:      "MS-ARG-TYPE-MISMATCH",
				Message:   "array index must be Int",
			})
			return newFlowFromType(mindscript.S{"id", "Any"})
		}
		return newFlowFromType(elemT)
	}

	// Map indexing:
	// - string literal key → same as get (field or MS-MAP-MISSING-KEY).
	// - dynamic key → unknown field → Any.
	if len(recvT) > 0 {
		if tag, _ := recvT[0].(string); tag == "map" {
			if len(idxNode) >= 2 && idxNode[0].(string) == "str" {
				key, _ := idxNode[1].(string)
				if ft, ok := mapFieldType(recvT, key); ok {
					return newFlowFromType(ft)
				}
				c.idx.addDiag(Diag{
					StartByte: 0,
					EndByte:   0,
					Code:      "MS-MAP-MISSING-KEY",
					Message:   "missing key '" + key + "' on map/module",
				})
				return newFlowFromType(mindscript.S{"id", "Any"})
			}
			return newFlowFromType(mindscript.S{"id", "Any"})
		}
	}

	// Unknown receiver type.
	return newFlowFromType(mindscript.S{"id", "Any"})
}

func (c *foldCtx) foldCall(n mindscript.S) flow {
	if len(n) < 2 {
		return newFlowFromType(mindscript.S{"id", "Any"})
	}

	// Fold callee.
	calleeNode, _ := n[1].(mindscript.S)
	cf := c.fold(calleeNode)
	calleeT := typeOf(c.ip, c.env, cf.Val)
	if calleeT == nil {
		calleeT = mindscript.S{"id", "Any"}
	}

	// Fold args for side-effects and types.
	argTypes := make([]mindscript.S, 0, len(n)-2)
	for i := 2; i < len(n); i++ {
		if arg, ok := n[i].(mindscript.S); ok {
			af := c.fold(arg)
			t := typeOf(c.ip, c.env, af.Val)
			if t == nil {
				t = mindscript.S{"id", "Any"}
			}
			argTypes = append(argTypes, t)
		}
	}

	ip := c.ip
	if ip == nil {
		return newFlowFromType(mindscript.S{"id", "Any"})
	}

	resT := calleeT
	overflowReported := false

	for _, at := range argTypes {
		// Expect an arrow: ("binop","->", P, R)
		if len(resT) >= 4 {
			if tag, _ := resT[0].(string); tag == "binop" {
				if op, _ := resT[1].(string); op == "->" {
					pt, _ := resT[2].(mindscript.S)
					next, _ := resT[3].(mindscript.S)

					if !ip.IsSubtype(at, pt, c.env) {
						c.idx.addDiag(Diag{
							StartByte: 0,
							EndByte:   0,
							Code:      "MS-ARG-TYPE-MISMATCH",
							Message:   "argument type mismatch",
						})
					}

					resT = next
					continue
				}
			}
		}

		// Not an arrow anymore: extra args ⇒ overflow.
		if !overflowReported {
			c.idx.addDiag(Diag{
				StartByte: 0,
				EndByte:   0,
				Code:      "MS-ARG-OVERFLOW",
				Message:   "too many arguments",
			})
			overflowReported = true
		}
		resT = mindscript.S{"id", "Any"}
		break
	}

	return newFlowFromType(resT)
}

func (c *foldCtx) foldBinop(n mindscript.S) flow {
	if len(n) < 4 {
		return newFlowFromType(mindscript.S{"id", "Any"})
	}

	op, _ := n[1].(string)
	lhs, _ := n[2].(mindscript.S)
	rhs, _ := n[3].(mindscript.S)

	lf := c.fold(lhs)
	rf := c.fold(rhs)

	lt := typeOf(c.ip, c.env, lf.Val)
	rt := typeOf(c.ip, c.env, rf.Val)
	if lt == nil {
		lt = mindscript.S{"id", "Any"}
	}
	if rt == nil {
		rt = mindscript.S{"id", "Any"}
	}

	// Constant div/mod by zero based on raw RHS literal.
	if op == "/" || op == "%" {
		if len(rhs) >= 2 {
			if tag, _ := rhs[0].(string); tag == "int" {
				if v, ok := rhs[1].(int64); ok && v == 0 {
					c.idx.addDiag(Diag{
						StartByte: 0,
						EndByte:   0,
						Code:      "MS-DIV-BY-ZERO-CONST",
						Message:   "division or modulo by constant zero",
					})
				}
			}
		}
	}

	ip := c.ip
	if ip == nil {
		return newFlowFromType(mindscript.S{"id", "Any"})
	}

	isId := func(t mindscript.S, name string) bool {
		return len(t) >= 2 && t[0].(string) == "id" && t[1].(string) == name
	}

	switch op {
	case "+", "-", "*", "%", "/":
		// Arithmetic result typing.
		if isId(lt, "Int") && isId(rt, "Int") {
			// Int / Int is Int per spec.
			return newFlowFromType(mindscript.S{"id", "Int"})
		}
		if (isId(lt, "Int") && isId(rt, "Num")) ||
			(isId(lt, "Num") && isId(rt, "Int")) ||
			(isId(lt, "Num") && isId(rt, "Num")) {
			return newFlowFromType(mindscript.S{"id", "Num"})
		}
		return newFlowFromType(mindscript.S{"id", "Any"})

	case "&", "|", "^", "<<", ">>":
		if !(isId(lt, "Int") && isId(rt, "Int")) {
			c.idx.addDiag(Diag{
				StartByte: 0,
				EndByte:   0,
				Code:      "MS-BITWISE-NONINT",
				Message:   "bitwise operators require Int operands",
			})
			return newFlowFromType(mindscript.S{"id", "Any"})
		}
		return newFlowFromType(mindscript.S{"id", "Int"})
	}

	return newFlowFromType(mindscript.S{"id", "Any"})
}

func (c *foldCtx) foldUnop(n mindscript.S) flow {
	if len(n) < 3 {
		return newFlowFromType(mindscript.S{"id", "Any"})
	}
	op, _ := n[1].(string)
	expr, _ := n[2].(mindscript.S)
	tf := c.fold(expr)
	t := typeOf(c.ip, c.env, tf.Val)
	if t == nil {
		t = mindscript.S{"id", "Any"}
	}

	switch op {
	case "-":
		// Keep it simple; detailed arithmetic lives in binop.
		return newFlowFromType(t)
	case "!":
		return newFlowFromType(mindscript.S{"id", "Bool"})
	default:
		return newFlowFromType(mindscript.S{"id", "Any"})
	}
}

func (c *foldCtx) foldTypeExpr(n mindscript.S) flow {
	// ("type", typeAst)
	if len(n) < 2 {
		return newFlowFromType(mindscript.S{"id", "Type"})
	}
	typeAst, _ := n[1].(mindscript.S)

	// Construct a VTType value for this schema in the current env.
	// This relies on the mindscript runtime's type-construction helper.
	tv := mindscript.TypeValIn(typeAst, c.env)

	return flow{Val: tv}
}

// ("decl", name)
// Expression semantics: let x
// - Side effect: bind x = null in current env.
// - Value: null.
func (c *foldCtx) foldDecl(n mindscript.S) flow {
	if len(n) < 2 || c.env == nil {
		return newFlowFromType(mindscript.S{"id", "Null"})
	}

	name, _ := n[1].(string)
	if name == "" {
		return newFlowFromType(mindscript.S{"id", "Null"})
	}

	c.env.Define(name, newSymbolVal(VTSymbol{
		Type: mindscript.S{"id", "Null"},
	}))
	return newFlowFromType(mindscript.S{"id", "Null"})
}

// ("assign", lhs, rhs?)
func (c *foldCtx) foldAssign(n mindscript.S) flow {
	if len(n) < 2 {
		return newFlow(nil)
	}

	lhs, _ := n[1].(mindscript.S)

	invalidTarget := func(msg string, t mindscript.S) flow {
		if msg == "" {
			msg = "invalid assignment target"
		}
		c.idx.addDiag(Diag{
			StartByte: 0, // TODO: use spans
			EndByte:   0,
			Code:      "MS-INVALID-ASSIGN-TARGET",
			Message:   msg,
		})
		if t == nil {
			t = mindscript.S{"id", "Any"}
		}
		return newFlowFromType(t)
	}

	// Incomplete assignment: ("assign", lhs)
	if len(n) == 2 {
		// Parser shouldn't really give us this in valid code; just ignore.
		return newFlow(nil)
	}

	// We have an RHS.
	rhs, _ := n[2].(mindscript.S)
	rhsFlow := c.fold(rhs)
	rhsVal := rhsFlow.Val
	if (rhsVal == mindscript.Value{}) {
		rhsVal = newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Any"}})
	}
	ip := c.ip

	// 1) Declaration + init:
	// ("assign", ("decl", name), rhs)
	// let name = rhs
	if len(lhs) >= 2 && lhs[0].(string) == "decl" {
		name, _ := lhs[1].(string)
		if name != "" && c.env != nil {
			c.env.Define(name, rhsVal)
		}
		// Expression value is rhs.
		return flow{Val: rhsVal}
	}

	// 2) Simple assignment:
	// ("assign", ("id", name), rhs)
	if len(lhs) >= 2 && lhs[0].(string) == "id" {
		name, _ := lhs[1].(string)
		if name == "" || c.env == nil {
			return flow{Val: rhsVal}
		}

		// Must already exist.
		v, err := c.env.Get(name)
		if err != nil {
			c.idx.addDiag(Diag{
				StartByte: 0,
				EndByte:   0,
				Code:      "MS-UNKNOWN-NAME",
				Message:   "unknown name: " + name,
			})
			// Don't implicitly define; expression still has value rhs.
			return flow{Val: rhsVal}
		}

		// If it’s already an analysis symbol: merge/update the summary.
		if sym, ok := asSymbol(v); ok {
			newT := typeOf(ip, c.env, rhsVal)
			if newT == nil {
				newT = mindscript.S{"id", "Any"}
			}

			// IMPORTANT: Assignments overwrite the previous value's type.
			// NEVER use UnifyTypes here. LUB is ONLY for merging control-flow
			// joins (if/else arms, loop exits, return/break/continue),
			// not sequential assignments.
			sym.Type = newT

			if err := c.env.Set(name, newSymbolVal(sym)); err != nil {
				return invalidTarget(err.Error(), newT)
			}
			return flow{Val: newSymbolVal(sym)}
		}

		// Ambient/runtime or VTType: try to Set directly.
		if err := c.env.Set(name, rhsVal); err != nil {
			return invalidTarget(err.Error(), typeOf(ip, c.env, rhsVal))
		}
		return flow{Val: rhsVal}
	}

	// Destructuring support: darr / dobj (possibly nested).
	// We treat any pattern containing ("decl", ...) as let-style;
	// otherwise it's assignment-style (ids must already exist).
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

	// bind recursively applies pattern p to type t.
	// doc is only used for object-pattern missing-key Null bindings.
	var bind func(p mindscript.S, t mindscript.S, decl bool, doc string)

	bind = func(p mindscript.S, t mindscript.S, decl bool, doc string) {
		if t == nil {
			t = mindscript.S{"id", "Any"}
		}
		if len(p) == 0 {
			return
		}
		tag, _ := p[0].(string)

		switch tag {
		case "decl":
			// ("decl", name) or ("decl", subpattern)
			if len(p) < 2 {
				return
			}
			if name, ok := p[1].(string); ok && name != "" {
				c.env.Define(name, newSymbolVal(VTSymbol{Type: t, Doc: doc}))
				return
			}
			if sub, ok := p[1].(mindscript.S); ok {
				bind(sub, t, true, doc)
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
				c.env.Define(name, newSymbolVal(VTSymbol{Type: t, Doc: doc}))
				return
			}
			// assignment-style: must already exist
			v, err := c.env.Get(name)
			if err != nil {
				c.idx.addDiag(Diag{
					StartByte: 0, EndByte: 0,
					Code:    "MS-UNKNOWN-NAME",
					Message: "unknown name: " + name,
				})
				return
			}
			sym, ok := asSymbol(v)
			if !ok {
				sym = VTSymbol{}
			}
			sym.Type = t
			if doc != "" {
				sym.Doc = doc
			}
			if err := c.env.Set(name, newSymbolVal(sym)); err != nil {
				c.idx.addDiag(Diag{
					StartByte: 0, EndByte: 0,
					Code:    "MS-INVALID-ASSIGN-TARGET",
					Message: err.Error(),
				})
			}

		case "darr":
			// Array destructuring.
			elemT, ok := arrayElemType(t)
			if !ok || elemT == nil {
				elemT = mindscript.S{"id", "Any"}
			}
			// Too-short RHS only when RHS is a literal array we can see.
			pats := len(p) - 1
			if pats > 0 && len(rhs) > 0 {
				if rtag, _ := rhs[0].(string); rtag == "array" {
					if len(rhs)-1 < pats {
						c.idx.addDiag(Diag{
							StartByte: 0, EndByte: 0,
							Code:    "MS-INVALID-ASSIGN-TARGET",
							Message: "array destructuring RHS has too few elements",
						})
					}
				}
			}
			for i := 1; i < len(p); i++ {
				if sub, ok := p[i].(mindscript.S); ok {
					bind(sub, elemT, decl, "")
				}
			}

		case "dobj":
			// Object destructuring.
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
					// Missing key in let-pattern: bind as Null with doc.
					bind(
						sub,
						mindscript.S{"id", "Null"},
						true,
						"object pattern: missing key '"+key+"'",
					)
				} else {
					if ft == nil {
						ft = mindscript.S{"id", "Any"}
					}
					bind(sub, ft, decl, "")
				}
			}
		default:
			// get/idx/etc inside patterns: leave as side-effect-only.
			return
		}
	}

	if len(lhs) > 0 {
		if tag, _ := lhs[0].(string); tag == "darr" || tag == "dobj" {
			bind(lhs, typeOf(ip, c.env, rhsVal), hasDecl(lhs), "")
			// Value of destructuring expression is the RHS value.
			return flow{Val: rhsVal}
		}
	}

	// 3) Valid non-id targets: get / idx / destructuring.
	if len(lhs) > 0 {
		if tag, _ := lhs[0].(string); tag == "get" || tag == "idx" || tag == "darr" || tag == "dobj" {
			// For get/idx: side-effect only; expression value is rhs.
			// (darr/dobj handled above.)
			return flow{Val: rhsVal}
		}
	}

	// 4) Everything else is invalid as an assignment target.
	return invalidTarget("", typeOf(ip, c.env, rhsVal))
}

// ("block", ...)
func (c *foldCtx) foldBlock(n mindscript.S) flow {
	mode := blockPlain
	if c.topLevel {
		mode = blockTopLevel
	}
	return c.foldBlockWithMode(n, mode)
}

// foldBlockWithMode handles top-level, plain blocks, function bodies, and loop bodies.
// For normal fallthrough, the result value is the value of the *last* expression
// on a non-terminated path. Earlier statements are just steps.
func (c *foldCtx) foldBlockWithMode(n mindscript.S, mode blockMode) flow {
	if len(n) == 0 {
		return newFlowFromType(mindscript.S{"id", "Null"})
	}

	// Environment:
	// - Top-level: reuse RootEnv so lets are visible.
	// - Others: new child env for lexical scope.
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
			return newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Any"}})
		}
		u := ip.UnifyTypes(ta, tb, env)
		return newSymbolVal(VTSymbol{Type: u})
	}

	var funRet mindscript.Value
	var loopBrk mindscript.Value
	var loopCont mindscript.Value

	for i := 1; i < len(n); i++ {
		sub, ok := n[i].(mindscript.S)
		if !ok {
			continue
		}

		sf := child.fold(sub)

		// Merge control-flow signals.
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

		// Last non-terminating expression wins for fallthrough value.
		if !sf.Terminated && (sf.Val != mindscript.Value{}) {
			out.Val = sf.Val
			haveVal = true
		}

		// After termination, later statements are unreachable.
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
		out.Val = newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Null"}})
	}
	return out
}

func (c *foldCtx) foldIf(n mindscript.S) flow {
	// ("if", ("pair", cond, thenBlock)..., elseBlock?)
	if len(n) < 2 {
		return newFlowFromType(mindscript.S{"id", "Any"})
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
			return newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Any"}})
		}
		u := ip.UnifyTypes(ta, tb, env)
		return newSymbolVal(VTSymbol{Type: u})
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
			_ = c.fold(cond)
			tf := c.foldBlockWithMode(thenBlock, blockPlain)

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

		// Trailing bare block is else arm.
		if tag == "block" && i == len(n)-1 {
			hasElse = true
			ef := c.foldBlockWithMode(arm, blockPlain)

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

	// No explicit else ⇒ implicit null arm.
	if !hasElse {
		nullV := newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Null"}})
		armsVal = unifyVals(armsVal, nullV)
	}
	if (armsVal == mindscript.Value{}) {
		armsVal = newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Null"}})
	}

	out.Val = armsVal
	return out
}

func (c *foldCtx) foldWhile(n mindscript.S) flow {
	// ("while", cond, bodyBlock)
	if len(n) < 3 {
		return newFlowFromType(mindscript.S{"id", "Any"})
	}
	cond, _ := n[1].(mindscript.S)
	body, _ := n[2].(mindscript.S)
	_ = c.fold(cond)

	bf := c.foldBlockWithMode(body, blockLoopBody)

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
			return newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Any"}})
		}
		u := ip.UnifyTypes(ta, tb, env)
		return newSymbolVal(VTSymbol{Type: u})
	}

	var tv mindscript.Value
	if (bf.Val != mindscript.Value{}) {
		tv = unifyVals(tv, bf.Val)
	}
	if bf.HasBrk {
		tv = unifyVals(tv, bf.Brk)
	}
	// while-expression type always includes Null (loop may not run).
	tv = unifyVals(tv, newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Null"}}))
	if (tv == mindscript.Value{}) {
		tv = newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Null"}})
	}

	return flow{
		Val:    tv,
		Ret:    bf.Ret, // returns bubble out; break/continue are consumed
		HasRet: bf.HasRet,
	}
}

func (c *foldCtx) foldFor(n mindscript.S) flow {
	// ("for", target, iter, bodyBlock)
	if len(n) < 4 {
		return newFlowFromType(mindscript.S{"id", "Any"})
	}
	// Note: current skeleton ignores target element typing; iter folded for side-effects only.
	iter, _ := n[2].(mindscript.S)
	body, _ := n[3].(mindscript.S)
	_ = c.fold(iter)

	bf := c.foldBlockWithMode(body, blockLoopBody)

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
			return newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Any"}})
		}
		u := ip.UnifyTypes(ta, tb, env)
		return newSymbolVal(VTSymbol{Type: u})
	}

	var tv mindscript.Value
	if (bf.Val != mindscript.Value{}) {
		tv = unifyVals(tv, bf.Val)
	}
	if bf.HasBrk {
		tv = unifyVals(tv, bf.Brk)
	}
	tv = unifyVals(tv, newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Null"}}))
	if (tv == mindscript.Value{}) {
		tv = newSymbolVal(VTSymbol{Type: mindscript.S{"id", "Null"}})
	}

	return flow{
		Val:    tv,
		Ret:    bf.Ret,
		HasRet: bf.HasRet,
	}
}

func (c *foldCtx) foldFunLike(n mindscript.S) flow {
	if len(n) < 3 {
		return newFlowFromType(mindscript.S{"id", "Any"})
	}

	tag, _ := n[0].(string)
	isOracle := tag == "oracle"

	// Params: ("array", ("pair", ("id", name), typeS?)...)
	paramsNode, _ := n[1].(mindscript.S)
	var paramNames []string
	var paramTypes []mindscript.S
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
			if c.ip != nil {
				pt = c.ip.ResolveType(pt, c.env)
			}
			paramNames = append(paramNames, name)
			paramTypes = append(paramTypes, pt)
		}
	}

	// Declared return type (default Any).
	retT := mindscript.S{"id", "Any"}
	if rt, ok := n[2].(mindscript.S); ok {
		retT = rt
	}
	if c.ip != nil {
		retT = c.ip.ResolveType(retT, c.env)
	}

	// Oracle effective return: nullable unless already nullable or Any.
	effRet := retT
	if isOracle {
		isNullable := len(retT) >= 3 && retT[0] == "unop" && retT[1] == "?"
		isAny := len(retT) >= 2 && retT[0] == "id" && retT[1] == "Any"
		if !isNullable && !isAny {
			effRet = mindscript.S{"unop", "?", retT}
		}
	}

	// Build arrow type (right-assoc), with 0-arg sugar: Null -> R.
	funType := effRet
	if len(paramTypes) == 0 {
		funType = mindscript.S{
			"binop", "->",
			mindscript.S{"id", "Null"},
			effRet,
		}
	} else {
		for i := len(paramTypes) - 1; i >= 0; i-- {
			funType = mindscript.S{"binop", "->", paramTypes[i], funType}
		}
	}

	// For plain fun (with a real body block), check returns against effRet.
	if !isOracle && len(n) >= 4 {
		if body, ok := n[3].(mindscript.S); ok && len(body) > 0 && body[0] == "block" {
			funEnv := mindscript.NewEnv(c.env)
			for i := range paramNames {
				funEnv.Define(paramNames[i], newSymbolVal(VTSymbol{Type: paramTypes[i]}))
			}
			child := *c
			child.env = funEnv
			child.topLevel = false
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
					c.idx.addDiag(Diag{
						StartByte: 0, // TODO: attach to function span
						EndByte:   0,
						Code:      "MS-RET-TYPE-MISMATCH",
						Message:   "return type does not match function declaration",
					})
				}
			}
		}
	}

	return newFlowFromType(funType)
}

func (c *foldCtx) foldModule(n mindscript.S) flow {
	// TODO: module env, collect exports into map type, bind as VTSymbol.
	_ = n
	return newFlow(nil)
}
