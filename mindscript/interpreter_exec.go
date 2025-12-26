// interpreter_exec.go — PRIVATE: execution & call engine for MindScript.
//   - Parses source (via lexer/parser), compiles S-expr → bytecode (via emitter),
//     runs on the VM, and **bubbles unified hard errors (*Error) without formatting**.
//   - Implements function application, currying, and native-call scoping.
//   - No exported identifiers here. The public facade lives in interpreter.go.
//
// ──────────────────────────────────────────────────────────────────────────────
// MARKING PLAN (PRECISE CARETS)
// =============================
//
// We make runtime caret locations precise and predictable by aligning VM marks
// with parser spans.
//
// Invariants
// ----------
//  1. **1:1 Node ↔ Span (parser)** — the parser records exactly one Span per AST
//     node (every subexpression has a node and a span).
//  2. **1:1 Node ↔ Mark (emitter)** — the emitter records **exactly one** PCMark
//     for every AST node it emits code for. (Nodes that truly produce no code do
//     not get a mark unless they are direct failure targets.)
//  3. **Monotonic marks** — marks are appended in bytecode order with PC=here().
//  4. **No late parent marks** — once we place a child’s mark immediately before
//     a failure-prone instruction, we must not emit a parent/sibling mark before
//     that instruction executes.
//  5. **Correct PathBase** — chunks compiled for function bodies / module bodies
//     carry a SourceRef whose PathBase points at the body’s absolute AST path,
//     so marks inside map to the right spans.
//
// Placement Rules (authoritative)
// -------------------------------
//   - **Identifiers**: mark the identifier node **right before** opLoadGlobal.
//   - **Unary** (-, not): mark the operand **right before** the opcode.
//   - **Binary** (-,*,/,%, <,<=,>,>=, ==, !=): emit LHS, then RHS; mark the **RHS**
//     **right before** the arithmetic/compare opcode (or before the builtin CALL
//     for '+', which lowers to __plus).
//   - **Calls**: evaluate callee (no mark). For each argument:
//     1) emit arg code
//     2) mark the **arg node** **right before** `opCall 1`
//     3) emit `opCall 1`
//     For zero-arg call: mark the **callee** right before `opCall 0`.
//   - **Property** (`obj.name`): emit obj, mark the **property token** right before
//     `opGetProp`.
//   - **Index** (`obj[idx]`): emit obj then idx; mark the **idx node** right before
//     `opGetIdx`.
//   - **if / while gates**: mark the **tested condition node** right before
//     `opJumpIfFalse`.
//   - **Blocks**: never emit a parent mark between a child’s mark and its failing
//     instruction. No extra block-level mark is needed. **Important:** child
//     paths must use the **original AST child index** (do not compact after
//     skipping noopish children), to keep NodePath ↔ Span alignment.
//   - **Annotations**: attribute errors to the **subject** node (not the wrapper).
//     For `#(doc) (lhs = rhs)`, we mark the **LHS** before the assignment call.
//   - **for**: the iterator expression is marked once at the `__to_iter(iterExpr)`
//     call site. We do not duplicate a mark at the loop head.
//
// NOTE: These rules ensure that the VM’s “last mark with PC ≤ failingPC” picks
// the blameworthy child.
//
// Error policy (unchanged)
// ------------------------
//   - Soft errors → annotated-null Values.
//   - Hard errors → *Error {Kind, Msg, Src, Line, Col} bubbled up; formatting only
//     at the public API surface.
package mindscript

import (
	"fmt"
)

////////////////////////////////////////////////////////////////////////////////
//                          PRIVATE EXEC FACADE (to API)
////////////////////////////////////////////////////////////////////////////////

type execImpl struct{ ip *Interpreter }

func newExec(ip *Interpreter) execCore { return &execImpl{ip: ip} }

// evalSource parses + evaluates in the provided env (fresh or persistent).
// Returns Value on success; on hard failure returns a *Error with Src attached.
// No pretty printing here.
func (x *execImpl) evalSource(src string, env *Env) (Value, error) {
	ast, spans, err := ParseSExprWithSpans(src)
	if err != nil {
		if e, ok := err.(*Error); ok && e.Src == nil {
			e.Src = &SourceRef{Name: "<main>", Src: src, Spans: spans}
		}
		return Value{}, err
	}
	sr := &SourceRef{Name: "<main>", Src: src, Spans: spans}
	return x.ip.runTopWithSource(ast, env, false, sr)
}

// evalAST evaluates an AST in the provided env.
// No pretty printing here.
func (x *execImpl) evalAST(ast S, env *Env) (Value, error) {
	return x.ip.runTopWithSource(ast, env, false, nil)
}

func (x *execImpl) applyArgsScoped(fn Value, args []Value, callSite *Env) Value {
	return x.ip.applyArgsScoped(fn, args, callSite)
}

func (x *execImpl) funMeta(fn Value) (Callable, bool) {
	if fn.Tag != VTFun {
		return nil, false
	}
	return &funCallable{f: fn.Data.(*Fun), doc: fn.Annot}, true
}

////////////////////////////////////////////////////////////////////////////////
//                      CORE EXECUTION PLUMBING (PRIVATE)
////////////////////////////////////////////////////////////////////////////////

// runWithTrampoline centralizes fatal error handling for top-level runs.
// It converts panics and structured runtime errors into either an annotated-null
// Value (when uncaught==true) or a *Error with caret information.
func (ip *Interpreter) runWithTrampoline(
	sr *SourceRef,
	uncaught bool,
	run func() Value,
) (out Value, err error) {
	defer func() {
		if r := recover(); r != nil {
			switch sig := r.(type) {
			case *Error:
				if uncaught {
					out, err = annotNull(sig.Msg), nil
					return
				}
				if sig.Src == nil {
					sig.Src = sr
				}
				err = sig
				out = Value{}
			case rtErr:
				srcRef := sig.src
				if srcRef == nil {
					srcRef = sr
				}
				line, col := sig.line, sig.col
				if line <= 0 || col <= 0 {
					line, col = ip.sourcePosFromChunk(nil, srcRef, 0)
				}
				if uncaught {
					out, err = annotNull(sig.msg), nil
					return
				}
				err = &Error{Kind: DiagRuntime, Msg: sig.msg, Src: srcRef, Line: line, Col: col}
				out = Value{}
			case error:
				msg := sig.Error()
				if uncaught {
					out, err = annotNull(msg), nil
					return
				}
				line, col := ip.sourcePosFromChunk(nil, sr, 0)
				err = &Error{Kind: DiagRuntime, Msg: msg, Src: sr, Line: line, Col: col}
				out = Value{}
			default:
				msg := fmt.Sprintf("runtime panic: %v", r)
				if uncaught {
					out, err = annotNull(msg), nil
					return
				}
				line, col := ip.sourcePosFromChunk(nil, sr, 0)
				err = &Error{Kind: DiagRuntime, Msg: msg, Src: sr, Line: line, Col: col}
				out = Value{}
			}
		}
	}()

	out = run()
	return out, nil
}

func (ip *Interpreter) runTopWithSource(ast S, env *Env, uncaught bool, sr *SourceRef) (Value, error) {

	// Debug spans.
	if DebuggingMode {
		_ = VerifySpanIndexPostOrder(ast, sr, 40, nil)
	}

	return ip.runWithTrampoline(sr, uncaught, func() Value {
		ch := ip.jitTop(ast, sr)
		prev := ip.currentSrc
		ip.currentSrc = ch.Src
		res := ip.runChunk(ch, env, 0)
		ip.currentSrc = prev

		switch res.status {
		case vmOK, vmReturn:
			return res.value
		case vmRuntimeError:
			if uncaught {
				return res.value
			}
			line, col := ip.sourcePosFromChunk(ch, ch.Src, res.pc)
			msg := res.value.Annot
			if msg == "" {
				msg = "runtime error"
			}
			// Bubble as *Error; trampoline will convert to API-level (Value, error).
			panic(&Error{Kind: DiagRuntime, Msg: msg, Src: ch.Src, Line: line, Col: col})
		default:
			if uncaught {
				return errNull("unknown VM status")
			}
			line, col := ip.sourcePosFromChunk(ch, ch.Src, res.pc)
			panic(&Error{Kind: DiagRuntime, Msg: "unknown VM status", Src: ch.Src, Line: line, Col: col})
		}
	})
}

// Build a one-off top-level function body and ensure it is compiled.
func (ip *Interpreter) jitTop(ast S, sr *SourceRef) *Chunk {
	f := &Fun{ReturnType: S{"id", "Any"}, Body: ast, Src: sr}
	ip.ensureChunkWithSource(f, sr)
	return f.Chunk
}

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

////////////////////////////////////////////////////////////////////////////////
//                    CALL ENGINE: APPLY / CURRY / EXECUTE
////////////////////////////////////////////////////////////////////////////////

func (ip *Interpreter) applyArgsScoped(fn Value, args []Value, callSite *Env) Value {
	if fn.Tag != VTFun {
		fail("not a function")
	}
	f := fn.Data.(*Fun)

	if len(args) == 0 {
		switch len(f.Params) {
		case 0:
			return ip.execFunBodyScoped(fn, callSite)
		case 1:
			if ip.isType(Null, f.ParamTypes[0], f.Env) {
				return ip.applyOneScoped(fn, Null, callSite)
			}
			fail(fmt.Sprintf("arity mismatch: expected %d, got 0", len(f.Params)))
		default:
			fail(fmt.Sprintf("arity mismatch: expected %d, got 0", len(f.Params)))
		}
	}

	cur := fn
	for i := 0; i < len(args); i++ {
		cur = ip.applyOneScoped(cur, args[i], callSite)
		if i < len(args)-1 && cur.Tag != VTFun {
			fail("too many arguments")
		}
	}
	return cur
}

func (ip *Interpreter) applyOneScoped(fnVal Value, arg Value, callSite *Env) Value {
	if fnVal.Tag != VTFun {
		fail("not a function")
	}
	f := fnVal.Data.(*Fun)

	if len(f.Params) == 0 {
		res := ip.execFunBodyScoped(fnVal, callSite)
		if res.Tag != VTFun {
			fail("too many arguments")
		}
		return ip.applyOneScoped(res, arg, callSite)
	}

	paramName := f.Params[0]
	paramType := f.ParamTypes[0]
	if !ip.isType(arg, paramType, f.Env) {
		exp := FormatType(paramType)
		got := FormatType(ip.ValueToType(arg, f.Env))
		fail(fmt.Sprintf("type mismatch in parameter '%s': expected %s, got %s",
			paramName, exp, got))
	}

	parent := f.Env
	if f.NativeName != "" && callSite != nil {
		if f.Env == nil || f.Env == ip.Core {
			parent = callSite
		}
	}
	callEnv := NewEnv(parent)
	callEnv.Define(paramName, arg)

	if len(f.Params) > 1 {
		next := FunVal(&Fun{
			Params:     append([]string{}, f.Params[1:]...),
			ParamTypes: append([]S{}, f.ParamTypes[1:]...),
			ReturnType: f.ReturnType,
			Body:       f.Body,
			Env:        callEnv,
			HiddenNull: f.HiddenNull,
			Chunk:      f.Chunk,
			NativeName: f.NativeName,
			Src:        f.Src,
			IsOracle:   f.IsOracle,
			Examples:   f.Examples,
			Sig:        f.Sig, // preserve original declaration signature across currying
		})
		next.Annot = fnVal.Annot
		return next
	}

	execFun := &Fun{
		Params:     nil,
		ParamTypes: append([]S(nil), f.ParamTypes...),
		ReturnType: f.ReturnType,
		Body:       f.Body,
		Env:        callEnv,
		HiddenNull: f.HiddenNull,
		Chunk:      f.Chunk,
		NativeName: f.NativeName,
		IsOracle:   f.IsOracle,
		Examples:   f.Examples,
		Src:        f.Src,
		Sig:        f.Sig, // preserve for execution
	}
	execVal := FunVal(execFun)
	execVal.Annot = fnVal.Annot
	return ip.execFunBodyScoped(execVal, callSite)
}

func (ip *Interpreter) execFunBodyScoped(funVal Value, callSite *Env) Value {
	if funVal.Tag != VTFun {
		fail("not a function")
	}
	f := funVal.Data.(*Fun)

	if f.NativeName != "" {
		impl, ok := ip.native[f.NativeName]
		if !ok {
			fail(fmt.Sprintf("unknown native %q", f.NativeName))
		}
		scope := withScope(f.Env, callSite)
		prev := ip.currentSrc
		if f.Src != nil {
			ip.currentSrc = f.Src
		}
		res := impl(ip, &callCtx{argEnv: f.Env, scope: scope})
		ip.currentSrc = prev
		if !ip.isType(res, f.ReturnType, f.Env) {
			exp := FormatType(f.ReturnType)
			got := FormatType(ip.ValueToType(res, f.Env))
			fail(fmt.Sprintf("return type mismatch: expected %s, got %s", exp, got))
		}
		return res
	}

	if f.IsOracle {
		scope := withScope(f.Env, callSite)
		ctx := &callCtx{argEnv: f.Env, scope: scope}
		return ip.execOracle(funVal, ctx)
	}

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
			exp := FormatType(f.ReturnType)
			got := FormatType(ip.ValueToType(res.value, f.Env))
			line, col := ip.sourcePosFromChunk(f.Chunk, f.Src, res.pc)
			panicRt(fmt.Sprintf("return type mismatch: expected %s, got %s", exp, got), f.Src, line, col)
		}
		return res.value
	case vmRuntimeError:
		line, col := ip.sourcePosFromChunk(f.Chunk, f.Src, res.pc)
		panicRt(res.value.Annot, f.Src, line, col)
		return errNull("unreachable")
	default:
		return errNull("unknown VM status")
	}
}

////////////////////////////////////////////////////////////////////////////////
//                      SOURCE MAPPING (PC → (line, col))
////////////////////////////////////////////////////////////////////////////////

func (ip *Interpreter) sourcePosFromChunk(ch *Chunk, sr *SourceRef, pc int) (int, int) {
	// Single debug hook
	if DebuggingMode {
		dumpSourcePosDebug(ch, sr, pc)
	}

	src := ""
	if sr != nil {
		src = sr.Src
	}

	// Early fallback if we don't have enough info
	if ch == nil || sr == nil || sr.Spans == nil || len(ch.Marks) == 0 || src == "" {
		return 1, 1
	}

	// Pick the last mark with PC <= failing PC
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

	tryPath := func(p NodePath) (int, int, bool) {
		for cut := len(p); cut >= 0; cut-- {
			sub := p[:cut]
			if sp, ok := sr.Spans.Get(sub); ok {
				line, col := offsetToLineCol(src, sp.StartByte)
				return line, col, true
			}
		}
		return 1, 1, false
	}

	// Try the best mark's path
	if line, col, ok := tryPath(ch.Marks[i].Path); ok {
		return line, col
	}

	// Walk earlier marks backwards
	for k := i - 1; k >= 0; k-- {
		if line, col, ok := tryPath(ch.Marks[k].Path); ok {
			return line, col
		}
	}

	return 1, 1
}

// withScope returns override if non-nil (use the call-site env for effects),
// otherwise it returns parent (the function's closure env).
func withScope(parent, override *Env) *Env {
	if override != nil {
		return override
	}
	return parent
}

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

////////////////////////////////////////////////////////////////////////////////
//                 PRIVATE ADAPTERS: Callable / CallCtx impls
////////////////////////////////////////////////////////////////////////////////

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
	argEnv *Env
	scope  *Env
}

func (c *callCtx) Arg(name string) Value {
	if v, err := c.argEnv.Get(name); err == nil {
		return v
	}
	fail("missing argument: " + name)
	return Null
}

// NumArg returns the named argument as a Num Value:
//   - Int is promoted to Num(float64(int64))
//   - Num is returned as-is
func (c *callCtx) NumArg(name string) Value {
	v := c.Arg(name)
	if v.Tag == VTInt {
		return Num(float64(v.Data.(int64)))
	}
	if v.Tag == VTNum {
		return v
	}
	fail("expected number: " + name)
	return Null
}

func (c *callCtx) Env() *Env { return c.scope }

////////////////////////////////////////////////////////////////////////////////
//                             EMITTER (AST → BYTECODE)
////////////////////////////////////////////////////////////////////////////////

type emitter struct {
	ip        *Interpreter
	code      []uint32
	consts    []Value
	ctrlStack []ctrlCtx

	// Source mapping
	src       *SourceRef
	marks     []PCMark
	path      NodePath
	rootDepth int
}

type ctrlCtx struct {
	isLoop     bool
	breakJumps []int
	contJumps  []int
}

func newEmitter(ip *Interpreter, src *SourceRef) *emitter {
	e := &emitter{ip: ip, src: src}
	if src != nil && len(src.PathBase) > 0 {
		e.path = append(e.path, src.PathBase...)
	}
	// Record depth at chunk root; root "block" executes in provided Env,
	// nested blocks get their own Env via PushEnv/PopEnv.
	e.rootDepth = len(e.path)
	return e
}

func (e *emitter) inRootBlock() bool { return len(e.path) == e.rootDepth }

// ---------------------- mark helpers (centralized) ---------------------------

// Emit a mark for an absolute AST path immediately before a failure-prone opcode.
// NOTE: Marks MUST be appended immediately before the instruction that can fail
// because of that path. Do NOT emit any other mark until that instruction.
func (e *emitter) markHereFor(abs NodePath) {
	e.marks = append(e.marks, PCMark{PC: e.here(), Path: append(NodePath(nil), abs...)})
}

// Mark the current node (rare; for node-level blame).
func (e *emitter) markSelf() { e.markHereFor(append(NodePath(nil), e.path...)) }

// Mark a direct child index under the current node.
func (e *emitter) markChild(childIdx int) {
	e.markHereFor(append(append(NodePath(nil), e.path...), childIdx))
}

// Wrappers that enforce "mark immediately before opcode" ordering.
func (e *emitter) emitWithMarkChild(op opcode, childIdx int, imm uint32) {
	e.markChild(childIdx)
	e.emit(op, imm)
}
func (e *emitter) callWithMarkChild(argc int, childIdx int) {
	e.markChild(childIdx)
	e.emit(opCall, uint32(argc))
}

// ----------------------------------------------------------------------------

func equalConst(a, b Value) bool {
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
	case VTArray, VTMap, VTFun, VTType, VTModule, VTHandle:
		// For const-pool purposes you usually don't intern compound values;
		// fall back to pointer/identity if you really need it:
		return &a == &b
	default:
		return false
	}
}

func (e *emitter) k(v Value) uint32 {
	for i := range e.consts {
		if equalConst(e.consts[i], v) { // <-- use strict equality here
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

func (e *emitter) pushBlockCtx() { e.ctrlStack = append(e.ctrlStack, ctrlCtx{isLoop: false}) }
func (e *emitter) pushLoopCtx()  { e.ctrlStack = append(e.ctrlStack, ctrlCtx{isLoop: true}) }
func (e *emitter) popCtx() ctrlCtx {
	i := len(e.ctrlStack) - 1
	c := e.ctrlStack[i]
	e.ctrlStack = e.ctrlStack[:i]
	return c
}
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

// helpers for loops/blocks persisting "last" value
func (e *emitter) preloadAssignToLast(lastName string) {
	e.emit(opLoadGlobal, e.ks("__assign_set"))
	// Preload the lvalue pattern (("id", lastName)) as AST handle. No call yet.
	e.emit(opConst, e.k(HandleVal("ast", S{"id", lastName})))
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

// Child path scaffolding
func (e *emitter) withChild(childIdx int, f func()) {
	e.path = append(e.path, childIdx)
	f()
	e.path = e.path[:len(e.path)-1]
}

// Builtin call (no automatic marks; callers place marks per plan).
func (e *emitter) callBuiltin(name string, args ...S) {
	e.emit(opLoadGlobal, e.ks(name))
	for _, a := range args {
		e.emitExpr(a)
	}
	e.emit(opCall, uint32(len(args)))
}

// callBuiltinV calls a Core builtin with constant Value arguments.
// Use this only when *all* arguments are constants you can encode now.
func (e *emitter) callBuiltinV(name string, args ...Value) {
	e.emit(opLoadGlobal, e.ks(name))
	for _, v := range args {
		e.emit(opConst, e.k(v))
	}
	e.emit(opCall, uint32(len(args)))
}

func (e *emitter) emitMakeFun(params S, retT S, bodyCarrier S, isOracle bool, examples S, basePath NodePath) {
	namesArr := make([]Value, 0, max(0, len(params)-1))
	for i := 1; i < len(params); i++ {
		p := params[i].(S)
		if len(p) == 0 || p[0].(string) != "pair" {
			continue
		}
		namesArr = append(namesArr, Str(p[1].(S)[1].(string)))
	}
	if len(retT) == 0 {
		retT = S{"id", "Any"}
	}
	e.emit(opLoadGlobal, e.ks("__make_fun"))
	for _, v := range namesArr {
		e.emit(opConst, e.k(v))
	}
	e.emit(opMakeArr, uint32(len(namesArr)))

	// Build the param types at runtime via ("type", …) so they capture env.
	typeCount := 0
	for i := 1; i < len(params); i++ {
		p := params[i].(S)
		if len(p) == 0 || p[0].(string) != "pair" {
			continue
		}
		t := p[2].(S) // may be empty
		if len(t) == 0 {
			t = S{"id", "Any"}
		}
		e.emitExpr(S{"type", t})
		typeCount++
	}
	e.emit(opMakeArr, uint32(typeCount))

	// Return type (env-pinned) and BODY as AST handle
	e.emitExpr(S{"type", retT})
	e.emit(opConst, e.k(HandleVal("ast", bodyCarrier)))

	e.emit(opConst, e.k(Bool(isOracle)))
	e.emitExpr(examples)
	for _, idx := range basePath {
		e.emit(opConst, e.k(Int(int64(idx))))
	}
	e.emit(opMakeArr, uint32(len(basePath)))
	e.emit(opCall, 7)
}

func (e *emitter) emitFunBody(body S) {
	e.emitExpr(body)
	e.emit(opReturn, 0)
}

// Emit an expression node following the precise mark rules.
func (e *emitter) emitExpr(n S) {
	if len(n) == 0 {
		e.emit(opConst, e.k(Null))
		return
	}

	switch n[0].(string) {

	// ----- literals / ids -----
	case "int":
		e.emit(opConst, e.k(Int(n[1].(int64))))
	case "num":
		e.emit(opConst, e.k(Num(n[1].(float64))))
	case "str":
		e.emit(opConst, e.k(Str(n[1].(string))))
	case "bool":
		e.emit(opConst, e.k(Bool(n[1].(bool))))
	case "noop":
		// ignore
	case "null":
		e.emit(opConst, e.k(Null))

	case "id":
		// Identifier load can fail → mark the id right before opLoadGlobal.
		e.markSelf()
		e.emit(opLoadGlobal, e.ks(n[1].(string)))

	// ----- blocks -----
	case "block":
		// Root block (whole chunk) executes directly in the provided Env.
		// Nested blocks get their own lexical Env via PushEnv/PopEnv.
		isRoot := e.inRootBlock()
		if !isRoot {
			e.emit(opPushEnv, 0)
		}

		e.pushBlockCtx()
		emitted := 0
		for j := 1; j < len(n); j++ {
			child := n[j].(S)
			if isNoopish(child) {
				continue
			}
			if emitted > 0 {
				e.emit(opPop, 0)
			}
			origIdx := j - 1 // original AST child index
			e.withChild(origIdx, func() { e.emitExpr(child) })
			emitted++
		}
		if emitted == 0 {
			e.emit(opConst, e.k(Null))
		}
		exit := e.here()
		ctx := e.popCtx()
		for _, at := range ctx.breakJumps {
			e.patch(at, exit)
		}
		for _, at := range ctx.contJumps {
			e.patch(at, exit)
		}
		if !isRoot {
			// Pop the lexical Env for this block, leaving the block's
			// value on the stack (PopEnv only touches Env, not stack).
			e.emit(opPopEnv, 0)
		}

	// ----- flow: break / continue -----
	case "break":
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
		at := e.here()
		e.emit(opJump, 0)
		e.addBreakJump(at)
	case "continue":
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
		at := e.here()
		e.emit(opJump, 0)
		e.addContJump(at)

	// ----- unary -----
	case "unop":
		op := n[1].(string)
		if op == "?" {
			e.emit(opConst, e.k(errNull("postfix '?' invalid here")))
			return
		}
		e.withChild(1, func() { e.emitExpr(n[2].(S)) })
		// Mark operand right before opcode.
		switch op {
		case "not":
			e.emitWithMarkChild(opNot, 1, 0)
		case "-":
			e.emitWithMarkChild(opNeg, 1, 0)
		case "~":
			// bitwise not (unary)
			e.emitWithMarkChild(opBnot, 1, 0)
		default:
			e.emit(opConst, e.k(errNull("unknown unary op")))
		}

	// ----- binary -----
	case "binop":
		op := n[1].(string)
		if op == "and" || op == "or" {
			// Short-circuit: mark tested subexpr at the gate.
			e.withChild(1, func() { e.emitExpr(n[2].(S)) })
			e.markChild(1)
			jf := e.here()
			e.emit(opJumpIfFalse, 0)

			// unified AND/OR short-circuit
			pre := func() { e.withChild(2, func() { e.emitExpr(n[3].(S)) }) } // RHS
			post := func() { e.emit(opConst, e.k(Bool(false))) }              // const false
			if op == "or" {
				pre, post = func() { e.emit(opConst, e.k(Bool(true))) }, pre // const true, then RHS
			}
			pre()
			jend := e.here()
			e.emit(opJump, 0)
			lother := e.here()
			e.patch(jf, lother)
			post()
			lend := e.here()
			e.patch(jend, lend)
			return
		}
		a, b := n[2].(S), n[3].(S)
		switch op {
		case "==":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opEq, 2, 0)
		case "!=":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opNe, 2, 0)
		case "+":
			e.emit(opLoadGlobal, e.ks("__plus"))
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.callWithMarkChild(2, 2)
		case "-":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opSub, 2, 0)
		case "*":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opMul, 2, 0)
		case "/":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opDiv, 2, 0)
		case "%":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opMod, 2, 0)
		case "<":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opLt, 2, 0)
		case "<=":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opLe, 2, 0)
		case ">":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opGt, 2, 0)
		case ">=":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opGe, 2, 0)
		case "**":
			// exponentiation
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opPow, 2, 0)
		case "&":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opBand, 2, 0)
		case "|":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opBor, 2, 0)
		case "^":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opBxor, 2, 0)
		case "<<":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opShl, 2, 0)
		case ">>":
			e.withChild(1, func() { e.emitExpr(a) })
			e.withChild(2, func() { e.emitExpr(b) })
			e.emitWithMarkChild(opShr, 2, 0)
		default:
			e.emit(opConst, e.k(errNull("unsupported operator")))
		}

	// ----- assignment -----
	case "assign":
		lhs := n[1].(S)
		opName := "__assign_set"
		pat := lhs
		if len(lhs) > 0 && lhs[0].(string) == "let" {
			opName = "__assign_def"
			// Declarative assignment: ("let", P) on the LHS.
			pat = lhs[1].(S)
		}
		e.emit(opLoadGlobal, e.ks(opName))
		// Pass the **pattern** as a pure AST handle (no leading "let").
		e.emit(opConst, e.k(HandleVal("ast", pat)))
		e.withChild(1, func() { e.emitExpr(n[2].(S)) })
		// Attribute assignment target errors to LHS: mark child #0 right before the CALL.
		e.callWithMarkChild(2, 0)

	case "let":
		// Declaration-only:
		//   let P
		// lowers to:
		//   __declare_pattern(<AST pattern P>)
		e.emit(opLoadGlobal, e.ks("__declare_pattern"))
		e.emit(opConst, e.k(HandleVal("ast", n[1].(S))))
		e.markChild(0)
		e.emit(opCall, 1)

	// ----- arrays / maps -----
	case "array":
		emitted := 0
		for i := 1; i < len(n); i++ {
			child := n[i].(S)
			if isNoopish(child) {
				continue
			}
			e.withChild(i-1, func() { e.emitExpr(child) })
			emitted++
		}
		e.emit(opMakeArr, uint32(emitted))

	case "map":
		keys := []S{}
		vals := []S{}
		for i := 1; i < len(n); i++ {
			p := n[i].(S)
			if len(p) == 0 {
				continue
			}
			tag := p[0].(string)
			if tag != "pair" && tag != "pair!" {
				continue
			}
			keys = append(keys, p[1].(S))
			vals = append(vals, p[2].(S))
		}
		e.emit(opLoadGlobal, e.ks("__map_from"))
		for i := 0; i < len(keys); i++ {
			idx := i
			e.withChild(idx, func() { e.withChild(0, func() { e.emitExpr(keys[idx]) }) })
		}
		e.emit(opMakeArr, uint32(len(keys)))
		for i := 0; i < len(vals); i++ {
			idx := i
			e.withChild(idx, func() { e.withChild(1, func() { e.emitExpr(vals[idx]) }) })
		}
		e.emit(opMakeArr, uint32(len(vals)))
		e.emit(opCall, 2)

	// ----- property / index -----
	case "get":
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
		// Blame the property token (child #1) right before opGetProp.
		e.emitWithMarkChild(opGetProp, 1, e.ks(n[2].(S)[1].(string)))

	case "idx":
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
		e.withChild(1, func() { e.emitExpr(n[2].(S)) })
		// Blame the index expression (child #1) right before opGetIdx.
		e.emitWithMarkChild(opGetIdx, 1, 0)

	// ----- call -----
	case "call":
		// Evaluate callee once.
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })

		argc := 0
		if len(n) == 2 {
			// Zero-arg call: blame callee right before CALL 0.
			e.callWithMarkChild(0, 0)
			return
		}
		// Apply args one by one; skip noopish args; blame each emitted arg.
		for i := 2; i < len(n); i++ {
			arg := n[i].(S)
			if isNoopish(arg) {
				continue
			}
			argIdx := i - 1
			e.withChild(argIdx, func() { e.emitExpr(arg) })
			e.callWithMarkChild(1, argIdx)
			argc++
		}
		if argc == 0 {
			// No real arguments after skipping noops → call with 0 args.
			e.callWithMarkChild(0, 0)
		}
		return

	// ----- fun / oracle -----
	case "fun":
		// ("fun", params, ret, body) → body child index is 2
		absBase := append(append(NodePath(nil), e.path...), 2)
		e.emitMakeFun(n[1].(S), n[2].(S), n[3].(S), false, S{"null"}, absBase)
	case "oracle":
		e.withChild(2, func() {
			e.emitMakeFun(n[1].(S), n[2].(S), S{"oracle"}, true, n[3].(S), nil)
		})

	// ----- return -----
	case "return":
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
		e.markSelf()
		e.emit(opReturn, 0)

	// ----- if -----
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
			p := arms[i].(S) // ("pair", cond, thenBlock)
			// Emit condition; mark the condition node (pair child #0) at the gate.
			e.withChild(i, func() { e.withChild(0, func() { e.emitExpr(p[1].(S)) }) })
			condAbs := append(append(NodePath(nil), e.path...), i, 0)
			e.markHereFor(condAbs)
			jf := e.here()
			e.emit(opJumpIfFalse, 0)

			e.withChild(i, func() { e.withChild(1, func() { e.emitExpr(p[2].(S)) }) })
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

	// ----- while -----
	case "while":
		cond := n[1].(S)
		body := n[2].(S)

		lastName := fmt.Sprintf("$last_%d", e.here())
		// Declare $last_* as a simple identifier binding initialized to null.
		e.callBuiltinV("__assign_def",
			HandleVal("ast", S{"id", lastName}),
			Null,
		)
		e.emit(opPop, 0)

		head := e.here()
		e.withChild(0, func() { e.emitExpr(cond) })
		// Mark the condition node right before the gate.
		e.markChild(0)
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

	// ----- for -----
	case "for":
		target := n[1].(S)
		iterExpr := n[2].(S)
		body := n[3].(S)

		iterName := fmt.Sprintf("$iter_%d", e.here())
		// Declare the hidden iterator binding with an AST handle (not a Type).
		// We'll initialize it immediately with __to_iter(iterExpr) below.
		e.emit(opLoadGlobal, e.ks("__assign_def"))
		e.emit(opConst, e.k(HandleVal("ast", S{"id", iterName})))

		// __to_iter(iterExpr); mark the iterExpr (child #1) at this call site.
		e.emit(opLoadGlobal, e.ks("__to_iter"))
		e.withChild(1, func() { e.emitExpr(iterExpr) })
		e.callWithMarkChild(1, 1) // __to_iter(iterExpr)
		e.emit(opCall, 2)         // assign_def(<AST id>, iterator)
		e.emit(opPop, 0)

		tmpName := fmt.Sprintf("$tmp_%d", e.here())
		e.callBuiltinV("__assign_def", HandleVal("ast", S{"id", tmpName}), Null)
		e.emit(opPop, 0)

		lastName := fmt.Sprintf("$last_%d", e.here())
		e.callBuiltinV("__assign_def", HandleVal("ast", S{"id", lastName}), Null)
		e.emit(opPop, 0)

		head := e.here()

		// tmp = iter(Null)
		e.emit(opLoadGlobal, e.ks("__assign_set"))
		// lvalue as AST handle
		e.emit(opConst, e.k(HandleVal("ast", S{"id", tmpName})))
		e.emit(opLoadGlobal, e.ks(iterName))
		e.emit(opConst, e.k(Null))
		e.emit(opCall, 1) // iter(Null)
		e.emit(opCall, 2) // assign_set(Type(tmp), result)
		e.emit(opPop, 0)

		// gate: __iter_should_stop(tmp)
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

		// For `for` headers, the pattern is always treated as declarative.
		e.emit(opLoadGlobal, e.ks("__assign_def"))

		e.emit(opConst, e.k(HandleVal("ast", target)))
		e.emit(opLoadGlobal, e.ks(tmpName))
		e.emit(opCall, 2)
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

	// ----- type / module / annot -----
	case "type":
		// Lower to: __type_from_ast(<handle carrying AST>), pinning env at runtime.
		e.emit(opLoadGlobal, e.ks("__type_from_ast"))
		e.emit(opConst, e.k(HandleVal("type-ast", n[1].(S))))
		// Blame the subject type node (child #0) at the call site.
		e.markChild(0)
		e.emit(opCall, 1)

	// ----- type-only surface form seen in value position -----
	case "enum":
		// Users wrote Enum[...] as a *value*. Raise a compile-time hard error with a caret on Enum[...] itself.
		if e.src != nil && e.src.Spans != nil {
			if sp, ok := e.src.Spans.Get(e.path); ok {
				line, col := offsetToLineCol(e.src.Src, sp.StartByte)
				panicRt("Enum[...] is a type expression; wrap it with 'type' to obtain a runtime Type (e.g., type Enum[...]).", e.src, line, col)
			}
		}
		// Fallback if no spans are available (defensive):
		panicRt("Enum[...] is a type expression; wrap it with 'type' to obtain a runtime Type (e.g., type Enum[...]).", e.src, 1, 1)

	case "module":
		// Lower to: __make_module(nameExpr, <AST handle>, basePathArray)
		e.emit(opLoadGlobal, e.ks("__make_module"))
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
		// Pass the module body as a pure AST handle (no env pinning).
		e.emit(opConst, e.k(HandleVal("ast", n[2].(S))))
		absBase := append(append(NodePath(nil), e.path...), 1) // ("module", name, body) → body at child #1
		for _, idx := range absBase {
			e.emit(opConst, e.k(Int(int64(idx))))
		}
		e.emit(opMakeArr, uint32(len(absBase)))
		e.emit(opCall, 3)

	case "annot":
		text := n[1].(S)[1].(string)
		subj := n[2].(S)

		if isNoopish(subj) {
			return
		}

		// #(doc) (lhs = rhs)  ==>  lhs = #(doc) rhs
		if len(subj) > 0 && subj[0].(string) == "assign" {
			lhs := subj[1].(S)
			rhs := subj[2].(S)
			opName := "__assign_set"
			pat := lhs
			if len(lhs) > 0 && lhs[0].(string) == "let" {
				opName = "__assign_def"
				pat = lhs[1].(S)
			}
			e.emit(opLoadGlobal, e.ks(opName))
			// Bare pattern as AST handle
			e.emit(opConst, e.k(HandleVal("ast", pat)))
			e.emit(opLoadGlobal, e.ks("__annotate"))
			e.emit(opConst, e.k(Str(text)))
			e.withChild(1, func() { e.withChild(1, func() { e.emitExpr(rhs) }) })
			e.emit(opCall, 2) // __annotate
			// Attribute assignment errors to LHS (annot child #1 = assign, its child #0 = lhs).
			lhsAbs := append(append(NodePath(nil), e.path...), 1, 0)
			e.markHereFor(lhsAbs)
			e.emit(opCall, 2) // __assign_*
			return
		}

		// LVALUE-aware: #(doc) subj where subj ∈ {id,get,idx,let}
		if len(subj) > 0 {
			switch subj[0].(string) {
			case "id", "get", "idx", "let":
				opName := "__assign_set"
				pat := subj
				if subj[0].(string) == "let" {
					opName = "__assign_def"
					pat = subj[1].(S)
				}
				e.emit(opLoadGlobal, e.ks(opName))
				// Subject pattern (no leading "let") as AST handle
				e.emit(opConst, e.k(HandleVal("ast", pat)))
				e.emit(opLoadGlobal, e.ks("__annotate"))
				e.emit(opConst, e.k(Str(text)))
				e.withChild(1, func() { e.emitExpr(subj) }) // build annotated RHS
				e.emit(opCall, 2)
				// Attribute to subject itself.
				e.markChild(1)
				e.emit(opCall, 2)
				return
			}
		}

		// default: #(doc) expr  ==>  __annotate(doc, expr)
		e.emit(opLoadGlobal, e.ks("__annotate"))
		e.emit(opConst, e.k(Str(text)))
		e.withChild(1, func() { e.emitExpr(subj) })
		e.emit(opCall, 2)

	default:
		e.emit(opConst, e.k(errNull(fmt.Sprintf("unknown AST tag: %s", n[0].(string)))))
	}
}
