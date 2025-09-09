// interpreter_exec.go — PRIVATE: execution & call engine for MindScript.
// - Parses source (via lexer/parser), compiles S-expr → bytecode (via emitter),
//   runs on the VM, and **bubbles unified hard errors (*Error) without formatting**.
// - Implements function application, currying, and native-call scoping.
// - No exported identifiers here. The public facade lives in interpreter.go.
//
// Error policy (post-refactor):
//   • Soft errors → annotated-null Values (never returned as Go errors).
//   • Hard errors → *Error {Kind, Msg, Src, Line, Col} bubbled up; no pretty printing here.
//     Pretty printing happens only at the public API surface (see interpreter.go / errors.go).
//
// Concurrency & isolates (minimal, Lua-style):
//   • A single Interpreter instance is **not** re-entrant. For parallelism,
//     call (*Interpreter).Clone() and run the clone in another goroutine.
//   • This file keeps per-interpreter mutable state (e.g., currentSrc) confined
//     to the instance. Using clones ensures there are no data races.
//
// Emitter placement:
//   The emitter is defined here and obtained via newEmitter(ip, sr).

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
		// Attach SourceRef if missing; do not format.
		if e, ok := err.(*Error); ok && e.Src == nil {
			e.Src = &SourceRef{Name: "<main>", Src: src, Spans: spans}
		}
		return Value{}, err
	}
	return x.ip.runTopWithSource(ast, env, false, &SourceRef{Name: "<main>", Src: src, Spans: spans})
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

// runTopWithSource compiles+executes AST with error surfacing.
// If uncaught is true, runtime failures become annotated-null Values.
// Otherwise, hard failures bubble as *Error (no formatting).
func (ip *Interpreter) runTopWithSource(ast S, env *Env, uncaught bool, sr *SourceRef) (out Value, err error) {
	defer func() {
		if r := recover(); r != nil {
			switch sig := r.(type) {
			case returnSig:
				out, err = sig.v, nil
			case *Error:
				// Preserve the exact diagnostic kind & attach a SourceRef if missing.
				if uncaught {
					out, err = annotNull(sig.Msg), nil // SOFT path still honored
					return
				}
				if sig.Src == nil {
					sig.Src = sr
				}
				out = Value{} // no value on hard error
				err = sig     // bubble as-is (Lex/Parse/Runtime/Incomplete)
				return

			case rtErr:
				// rtErr carries msg and optional precise (src, line, col).
				// If position is missing, map PC → (line, col) using current chunk/source.
				srcRef := sig.src
				if srcRef == nil {
					srcRef = sr
				}
				line, col := sig.line, sig.col
				if line <= 0 || col <= 0 {
					// Best-effort mapping from current ip.currentSrc.
					line, col = ip.sourcePosFromChunk(nil, srcRef, 0)
				}
				if uncaught {
					out, err = errNull(sig.msg), nil // SOFT: annotated-null flows out
					return
				}
				err = &Error{
					Kind: DiagRuntime,
					Msg:  sig.msg,
					Src:  srcRef,
					Line: line,
					Col:  col,
				}
				out = Value{}

			case error:
				// Unknown panic error → treat as runtime hard error.
				if uncaught {
					out, err = annotNull(sig.Error()), nil // SOFT
					return
				}
				line, col := ip.sourcePosFromChunk(nil, sr, 0)
				err = &Error{
					Kind: DiagRuntime,
					Msg:  sig.Error(),
					Src:  sr,
					Line: line,
					Col:  col,
				}
				out = Value{}

			default:
				// Non-error panic payload.
				if uncaught {
					out, err = annotNull(fmt.Sprintf("runtime panic: %v", r)), nil // SOFT
					return
				}
				line, col := ip.sourcePosFromChunk(nil, sr, 0)
				err = &Error{
					Kind: DiagRuntime,
					Msg:  fmt.Sprintf("runtime panic: %v", r),
					Src:  sr,
					Line: line,
					Col:  col,
				}
				out = Value{}
			}
		}
	}()

	ch := ip.jitTop(ast, sr)
	prev := ip.currentSrc
	ip.currentSrc = ch.Src
	res := ip.runChunk(ch, env, 0)
	ip.currentSrc = prev

	switch res.status {
	case vmOK, vmReturn:
		return res.value, nil

	case vmRuntimeError:
		// Value contains the message in Annot. Hard error unless uncaught.
		if uncaught {
			return res.value, nil // SOFT: annotated-null flows out
		}
		line, col := ip.sourcePosFromChunk(ch, ch.Src, res.pc)
		msg := res.value.Annot
		if msg == "" {
			msg = "runtime error"
		}
		return Value{}, &Error{
			Kind: DiagRuntime,
			Msg:  msg,
			Src:  ch.Src,
			Line: line,
			Col:  col,
		}

	default:
		if uncaught {
			return errNull("unknown VM status"), nil // SOFT
		}
		line, col := ip.sourcePosFromChunk(ch, ch.Src, res.pc)
		return Value{}, &Error{
			Kind: DiagRuntime,
			Msg:  "unknown VM status",
			Src:  ch.Src,
			Line: line,
			Col:  col,
		}
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

func (ip *Interpreter) ensureChunkWithSource(f *Fun, sr *SourceRef) {
	// Native functions and oracles are not JIT-compiled here.
	if f.Chunk != nil || f.NativeName != "" || f.IsOracle {
		return
	}
	em := newEmitter(ip, sr) // private emitter
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

	// Zero-arg application.
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
		// If more args left but the intermediate result isn't a function → too many args.
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

	// Already saturated → execute, then keep applying to the result (currying chains).
	if len(f.Params) == 0 {
		res := ip.execFunBodyScoped(fnVal, callSite)
		if res.Tag != VTFun {
			fail("too many arguments")
		}
		return ip.applyOneScoped(res, arg, callSite)
	}

	// Type check against the next parameter.
	paramName := f.Params[0]
	paramType := f.ParamTypes[0]
	if !ip.isType(arg, paramType, f.Env) {
		fail(fmt.Sprintf("type mismatch in parameter '%s'", paramName))
	}

	// Bind argument into a fresh call env.
	parent := f.Env
	// For natives, if we're being called from a site with a concrete scope, prefer that
	// as the parent for argument bindings when the closure env is nil or Core.
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
			IsOracle:   f.IsOracle,
			Examples:   append([]Value(nil), f.Examples...),
		})
	}

	// Last arg supplied → execute.
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

	// Oracles are handled elsewhere (private oracle impl lives in ops or a separate file).
	if f.IsOracle {
		scope := withScope(f.Env, callSite)          // where effects should land
		ctx := &callCtx{argEnv: f.Env, scope: scope} // access to bound args + scope
		return ip.execOracle(funVal, ctx)
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
			// Map the mismatch to the return expression location.
			line, col := ip.sourcePosFromChunk(f.Chunk, f.Src, res.pc)
			panicRt("return type mismatch", f.Src, line, col)
		}
		return res.value
	case vmRuntimeError:
		// Map PC→(line,col) against the callee's own chunk/source and bubble up
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
	src := ""
	if sr != nil {
		src = sr.Src
	}
	if ch == nil || sr == nil || sr.Spans == nil || len(ch.Marks) == 0 || src == "" {
		return 1, 1
	}

	// Last mark with PC <= pc
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

	// 1) Use THIS mark’s path; if missing, climb its ancestors.
	p := ch.Marks[i].Path
	for cut := len(p); cut >= 0; cut-- {
		if span, ok := sr.Spans.Get(p[:cut]); ok {
			return offsetToLineCol(src, span.StartByte)
		}
	}

	// 2) Then walk earlier marks; for each, try their ancestors too.
	for k := i - 1; k >= 0; k-- {
		q := ch.Marks[k].Path
		for cut := len(q); cut >= 0; cut-- {
			if span, ok := sr.Spans.Get(q[:cut]); ok {
				return offsetToLineCol(src, span.StartByte)
			}
		}
	}
	return 1, 1
}

func (ip *Interpreter) fallbackSrc(sr *SourceRef, ast S) string {
	if sr != nil && sr.Src != "" {
		return sr.Src
	}
	if ast != nil {
		return FormatSExpr(ast)
	}
	return ""
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

////////////////////////////////////////////////////////////////////////////////
//                                 HELPERS
////////////////////////////////////////////////////////////////////////////////

func withScope(parent, override *Env) *Env {
	if override != nil {
		return override
	}
	return parent
}

////////////////////////////////////////////////////////////////////////////////
//                             EMITTER (AST → BYTECODE)
////////////////////////////////////////////////////////////////////////////////

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

type ctrlCtx struct {
	isLoop     bool
	breakJumps []int
	contJumps  []int
}

func newEmitter(ip *Interpreter, src *SourceRef) *emitter {
	e := &emitter{ip: ip, src: src}
	if src != nil && len(src.PathBase) > 0 {
		// Start marks at the absolute path of this sub-tree
		e.path = append(e.path, src.PathBase...)
	}
	return e
}

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

func (e *emitter) emitMakeFun(params S, retT S, bodyCarrier S, isOracle bool, examples S, basePath NodePath) {
	namesArr := make([]Value, 0, max(0, len(params)-1))
	typesArr := make([]Value, 0, max(0, len(params)-1))
	for i := 1; i < len(params); i++ {
		p := params[i].(S)
		namesArr = append(namesArr, Str(p[1].(S)[1].(string)))
		t := p[2].(S)
		if len(t) == 0 {
			t = S{"id", "Any"}
		}
		typesArr = append(typesArr, TypeVal(t))
	}
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
	e.emit(opConst, e.k(TypeVal(bodyCarrier)))
	e.emit(opConst, e.k(Bool(isOracle)))
	e.emitExpr(examples)
	// basePath: [Int]
	for _, idx := range basePath {
		e.emit(opConst, e.k(Int(int64(idx))))
	}
	e.emit(opMakeArr, uint32(len(basePath)))
	e.emit(opCall, 7)
}

// Entry: emit whole function body and return.
func (e *emitter) emitFunBody(body S) {
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
	case "noop":
		e.emit(opConst, e.k(Null))
	case "null":
		e.emit(opConst, e.k(Null))

	case "id":
		e.emit(opLoadGlobal, e.ks(n[1].(string)))

	case "block":
		e.pushBlockCtx()

		// Skip noopish children entirely. Leave the last non-noop value on the stack;
		// if all children are noopish, push plain Null. Guarantees callers always get a value.
		emitted := 0
		nAll := len(n) - 1
		for i := 1; i <= nAll; i++ {
			child := n[i].(S)
			if isNoopish(child) {
				continue
			}
			if emitted > 0 {
				e.emit(opPop, 0)
			}
			idx := i - 1 // preserve original child index for source marks
			e.withChild(idx, func() { e.emitExpr(child) })
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

	case "break":
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
		at := e.here()
		e.emit(opJump, 0)
		e.addBreakJump(at)
		return

	case "continue":
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
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
		e.withChild(1, func() { e.emitExpr(n[2].(S)); e.mark() })
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
			e.withChild(1, func() { e.emitExpr(n[2].(S)); e.mark() })
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
				jf := e.here() // Jump if LHS is false → evaluate RHS
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
			e.emitBinaryOpAB(a, b, opEq)
		case "!=":
			e.emitBinaryOpAB(a, b, opNe)
		case "+":
			e.emitBinaryBuiltinAB("__plus", a, b)
		case "-":
			e.emitBinaryOpAB(a, b, opSub)
		case "*":
			e.emitBinaryOpAB(a, b, opMul)
		case "/":
			e.emitBinaryOpAB(a, b, opDiv)
		case "%":
			e.emitBinaryOpAB(a, b, opMod)
		case "<":
			e.emitBinaryOpAB(a, b, opLt)
		case "<=":
			e.emitBinaryOpAB(a, b, opLe)
		case ">":
			e.emitBinaryOpAB(a, b, opGt)
		case ">=":
			e.emitBinaryOpAB(a, b, opGe)
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
		e.withChild(1, func() { e.emitExpr(n[2].(S)) })
		// Attribute assignment errors (e.g., bad target/index) to the LHS.
		e.withChild(0, func() {
			e.mark()
		})
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
			// child i-1 is the ("pair", key, val)
			e.withChild(i-1, func() { // visit ("pair", key, val)
				e.withChild(0, func() { // key path inside the pair
					e.emitExpr(keys[i].(S))
				})
			})
		}
		e.emit(opMakeArr, uint32(len(keys)-1))
		for i := 1; i < len(vals); i++ {
			e.withChild(i-1, func() {
				e.withChild(1, func() { // value path inside the pair
					e.emitExpr(vals[i].(S))
				})
			})
		}
		e.emit(opMakeArr, uint32(len(vals)-1))
		e.emit(opCall, 2)

	case "get":
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
		e.withChild(1, func() { e.mark() })
		e.emit(opGetProp, e.ks(n[2].(S)[1].(string)))

	case "idx":
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
		e.withChild(1, func() { e.emitExpr(n[2].(S)) })
		e.withChild(1, func() { e.mark() })
		e.emit(opGetIdx, 0)

	case "call":
		// 1) Evaluate callee once; it stays on the stack for the first application.
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })

		argc := len(n) - 2
		if argc == 0 {
			// Zero-arg call: keep a single call-site mark (status quo).
			e.mark() // maps to the whole call node path
			e.emit(opCall, 0)
			return
		}

		// 2) Apply arguments one-by-one, marking each CALL at that argument’s path.
		for i := 2; i < len(n); i++ {
			argIdx := i - 1
			e.withChild(argIdx, func() {
				e.emitExpr(n[i].(S)) // push arg i
				e.mark()             // << mark tied to *this argument’s* NodePath
				e.emit(opCall, 1)    // apply exactly one argument
			})
		}
		return

	case "fun":
		// Absolute path to the BODY inside the Type carrier:
		// ("fun", params, ret, ("type", body)) → child 2 is the carrier, child 0 is the body
		absBase := append(append(NodePath(nil), e.path...), 2, 0)
		e.emitMakeFun(
			n[1].(S),  // params
			n[2].(S),  // ret (may be empty)
			n[3].(S),  // body carrier (type AST)
			false,     // isOracle
			S{"null"}, // examples
			absBase,
		)
	case "oracle":
		e.withChild(2, func() {
			// oracles don't JIT a body chunk here → no body base path
			e.emitMakeFun(
				n[1].(S),
				n[2].(S),
				S{"oracle"},
				true,
				n[3].(S),
				nil,
			)
		})

	case "return":
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
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
				e.withChild(0, func() { e.emitExpr(p[1].(S)); e.mark() }) // cond
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
		e.emit(opPop, 0)

		head := e.here()
		// Mark at the loop condition for precise boolean type errors.
		e.withChild(0, func() {
			e.emitExpr(cond)
			e.mark()
		})
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
		// Define iterator variable: __assign_def(Type(decl iterName), __to_iter(iterExpr))
		e.emit(opLoadGlobal, e.ks("__assign_def"))
		e.emitExpr(S{"type", S{"decl", iterName}})

		// Build value: __to_iter(iterExpr), attributing marks to child #1 (iterExpr)
		e.emit(opLoadGlobal, e.ks("__to_iter"))
		e.withChild(1, func() { e.emitExpr(iterExpr) })
		e.mark()          // mark the __to_iter call site
		e.emit(opCall, 1) // __to_iter(iterExpr)
		e.emit(opCall, 2) // assign_def(TypeDecl, iterator)
		e.emit(opPop, 0)  // discard __assign_def return value

		tmpName := fmt.Sprintf("$tmp_%d", e.here())
		e.callBuiltin("__assign_def", S{"type", S{"decl", tmpName}}, S{"null"})
		e.emit(opPop, 0) // discard __assign_def return value

		lastName := fmt.Sprintf("$last_%d", e.here())
		e.callBuiltin("__assign_def", S{"type", S{"decl", lastName}}, S{"null"})
		e.emit(opPop, 0) // discard __assign_def return value

		head := e.here()

		e.emit(opLoadGlobal, e.ks("__assign_set"))
		e.emit(opConst, e.k(TypeVal(S{"id", tmpName})))
		e.emit(opLoadGlobal, e.ks(iterName))
		e.emit(opConst, e.k(Null))
		e.withChild(1, func() { e.mark() })
		e.emit(opCall, 1)
		e.emit(opCall, 2)
		e.emit(opPop, 0) // discard __assign_set return value

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
		// Attribute the body emission to child #2
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

	case "module":
		// Lower to: __make_module(nameValue, Type(bodyAst), basePathArray)
		e.emit(opLoadGlobal, e.ks("__make_module"))
		e.withChild(0, func() { e.emitExpr(n[1].(S)) })
		e.emit(opConst, e.k(TypeVal(n[2].(S))))
		// Absolute path to the BODY inside the Type carrier:
		// ("module", name, ("type", body)) → child 1 is the carrier, child 0 is the body
		absBase := append(append(NodePath(nil), e.path...), 1, 0)
		for _, idx := range absBase {
			e.emit(opConst, e.k(Int(int64(idx))))
		}
		e.emit(opMakeArr, uint32(len(absBase)))
		e.mark()
		e.emit(opCall, 3)

	case "annot":
		text := n[1].(S)[1].(string)
		subj := n[2].(S)

		// Lone PRE annotation over a blank line is a no-op: produce Null, no side effects.
		if isNoopish(subj) {
			e.emit(opConst, e.k(Null))
			return
		}

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
			e.withChild(1, func() { // go into ("assign", lhs, rhs)
				e.withChild(1, func() { // child #1 inside assign = rhs
					e.emitExpr(rhs)
				})
			})
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
		e.emit(opLoadGlobal, e.ks("__annotate"))
		e.emit(opConst, e.k(Str(text)))
		e.withChild(1, func() { // child #1 of ("annot", text, subj)
			e.emitExpr(subj)
		})
		e.emit(opCall, 2)

	default:
		e.emit(opConst, e.k(errNull(fmt.Sprintf("unknown AST tag: %s", n[0].(string)))))
	}
}

func (e *emitter) emitBinaryOpAB(a, b S, op opcode) {
	e.withChild(1, func() { e.emitExpr(a) })
	e.withChild(2, func() { e.emitExpr(b); e.mark() })
	e.emit(op, 0)
}
func (e *emitter) emitBinaryBuiltinAB(name string, a, b S) {
	e.emit(opLoadGlobal, e.ks(name))
	e.withChild(1, func() { e.emitExpr(a) })
	e.withChild(2, func() { e.emitExpr(b) })
	e.emit(opCall, 2)
}

// Private panic signaling & null helpers are defined in interpreter_ops.go:
//   - type returnSig struct{ v Value }
//   - type rtErr struct{ msg string; src *SourceRef; line, col int }
//   - func fail(msg string)
//   - func panicRt(msg string, src *SourceRef, line, col int)
//   - func errNull(msg string) Value
//   - func annotNull(msg string) Value
//   - func withAnnot(v Value, ann string) Value
// These are shared by exec/ops files within the package.
