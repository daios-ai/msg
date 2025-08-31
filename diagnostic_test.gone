package mindscript

import (
	"fmt"
	"strings"
	"testing"
)

// --- Utilities ---------------------------------------------------------------

// Parse "RUNTIME ERROR at L:C:" from an error string (best-effort).
func parseAtLC(err error) (line, col int, ok bool) {
	s := err.Error()
	needle := " at "
	i := strings.Index(s, needle)
	if i < 0 {
		return 0, 0, false
	}
	sub := s[i+len(needle):]
	var l, c int
	n, _ := fmt.Sscanf(sub, "%d:%d:", &l, &c)
	return l, c, n == 2
}

// --- 1) Do function chunks have absolute marks that resolve? -----------------

func Test_MarksResolve_AgainstFunctionSpans(t *testing.T) {
	ip := NewInterpreter()

	defSrc := `let f = fun() do 1 / 0 end`
	if _, err := ip.EvalPersistentSource(defSrc); err != nil {
		t.Fatalf("def error: %v", err)
	}
	v, err := ip.Global.Get("f")
	if err != nil {
		t.Fatalf("get f: %v", err)
	}
	if v.Tag != VTFun {
		t.Fatalf("f is not a function")
	}
	f := v.Data.(*Fun)

	if f.Src == nil || f.Src.Src == "" {
		t.Fatalf("f.Src missing/empty")
	}
	if f.Src.Spans == nil {
		t.Fatalf("f.Src.Spans is nil (ParseSExprWithSpans not wired into Fun.Src?)")
	}
	if len(f.Src.PathBase) == 0 {
		t.Fatalf("f.Src.PathBase is empty (emitter/__make_fun didn’t supply base path)")
	}

	ip.ensureChunkWithSource(f, f.Src)
	if f.Chunk == nil || len(f.Chunk.Marks) == 0 {
		t.Fatalf("f.Chunk marks missing (no PC marks recorded)")
	}

	// At least one mark should resolve through the SpanIndex (absolute NodePath).
	resolves := false
	for _, mk := range f.Chunk.Marks {
		if _, ok := f.Src.Spans.Get(mk.Path); ok {
			resolves = true
			break
		}
	}
	if !resolves {
		t.Fatalf("No function-chunk mark path resolved in f.Src.Spans (PathBase/absolute marks likely not wired)")
	}
}

// --- 2) Can we find the exact mark/span for the failing operator inside f? ---

func Test_FindBinOpMarkSpan_ForOneDivZero(t *testing.T) {
	ip := NewInterpreter()

	defSrc := `let f = fun() do 1 / 0 end`
	if _, err := ip.EvalPersistentSource(defSrc); err != nil {
		t.Fatalf("def error: %v", err)
	}
	v, _ := ip.Global.Get("f")
	f := v.Data.(*Fun)

	ip.ensureChunkWithSource(f, f.Src)
	if f.Src.Spans == nil {
		t.Fatalf("f.Src.Spans nil")
	}

	// Walk marks from the end and find one whose span text contains "/ 0".
	found := false
	var gotLine, gotCol, wantLine, wantCol int
	for i := len(f.Chunk.Marks) - 1; i >= 0; i-- {
		mk := f.Chunk.Marks[i]
		if sp, ok := f.Src.Spans.Get(mk.Path); ok {
			frag := f.Src.Src[sp.StartByte:sp.EndByte]
			if strings.Contains(frag, "/ 0") || strings.Contains(frag, "1 / 0") {
				found = true
				gotLine, gotCol = offsetToLineCol(f.Src.Src, sp.StartByte)
				start := strings.Index(defSrc, "1 / 0")
				wantLine, wantCol = offsetToLineCol(defSrc, start)
				break
			}
		}
	}
	if !found {
		t.Fatalf("Could not find any mark whose span contains '/ 0' (emitter mark->path mismatch?)")
	}
	if gotLine != wantLine || gotCol != wantCol {
		t.Fatalf("binop span mismatch: got %d:%d, want %d:%d", gotLine, gotCol, wantLine, wantCol)
	}
}

// --- 3) Does vmResult.pc map to an inner (non-1:1) location for f’s body? ----

func Test_VM_PC_Maps_InsideFunction(t *testing.T) {
	ip := NewInterpreter()

	defSrc := `let f = fun() do 1 / 0 end`
	if _, err := ip.EvalPersistentSource(defSrc); err != nil {
		t.Fatalf("def error: %v", err)
	}
	v, _ := ip.Global.Get("f")
	f := v.Data.(*Fun)

	ip.ensureChunkWithSource(f, f.Src)
	res := ip.runChunk(f.Chunk, f.Env, 0)
	if res.status != vmRuntimeError {
		t.Fatalf("expected vmRuntimeError, got %v", res.status)
	}
	line, col := ip.sourcePosFromChunk(f.Chunk, f.Src, res.pc)

	// Expect line/col to land at the "1 / 0" operator start (not 1:1).
	start := strings.Index(defSrc, "1 / 0")
	expL, expC := offsetToLineCol(defSrc, start)
	if line != expL || col != expC {
		t.Fatalf("vm pc map mismatch: got %d:%d, want %d:%d", line, col, expL, expC)
	}
}

// --- 4) Does execFunBodyScoped rethrow a structured rtErr with src/line/col? --

func Test_ExecRethrow_StructuredRtErr(t *testing.T) {
	ip := NewInterpreter()

	defSrc := `let f = fun() do 1 / 0 end`
	if _, err := ip.EvalPersistentSource(defSrc); err != nil {
		t.Fatalf("def error: %v", err)
	}
	v, _ := ip.Global.Get("f")
	fn := v

	defer func() {
		r := recover()
		if r == nil {
			t.Fatalf("expected panic(rtErr) from execFunBodyScoped")
		}
		switch e := r.(type) {
		case rtErr:
			if e.msg == "" || e.src == nil || e.src.Src == "" || e.line <= 0 || e.col <= 0 {
				t.Fatalf("rtErr missing fields: %#v", e)
			}
		case *rtErr:
			if e.msg == "" || e.src == nil || e.src.Src == "" || e.line <= 0 || e.col <= 0 {
				t.Fatalf("*rtErr missing fields: %#v", e)
			}
		default:
			t.Fatalf("panic type was not rtErr: %#v", r)
		}
	}()

	// This should panic out of execFunBodyScoped when it tries to execute f.
	_ = ip.applyArgsScoped(fn, nil, nil)
	t.Fatalf("unreachable")
}

// --- 5) End-to-end: top-level error should use inner function source ---------

func Test_E2E_InnerCaret_UsesFunctionSource_DivZero(t *testing.T) {
	ip := NewInterpreter()

	defSrc := `let f = fun() do 1 / 0 end`
	if _, err := ip.EvalPersistentSource(defSrc); err != nil {
		t.Fatalf("def error: %v", err)
	}
	_, err := ip.EvalPersistentSource("f()")
	if err == nil {
		t.Fatalf("expected runtime error")
	}

	gotL, gotC, ok := parseAtLC(err)
	if !ok {
		t.Fatalf("could not parse L:C from error: %v", err)
	}
	start := strings.Index(defSrc, "1 / 0")
	expL, expC := offsetToLineCol(defSrc, start)
	if gotL != expL || gotC < expC {
		t.Fatalf("outer error mapped to call-site or wrong pos: got %d:%d, want %d:>=%d", gotL, gotC, expL, expC)
	}
	// Snippet should contain the function definition text, not only "f()".
	if strings.Contains(err.Error(), "f()") && !strings.Contains(err.Error(), "let f = fun()") {
		t.Fatalf("snippet appears to be call-site instead of inner function:\n%s", err)
	}
}

func Test_E2E_InnerCaret_UsesFunctionSource_IndexOOB(t *testing.T) {
	ip := NewInterpreter()

	defSrc := `let f = fun() do [][0] end`
	if _, err := ip.EvalPersistentSource(defSrc); err != nil {
		t.Fatalf("def error: %v", err)
	}
	_, err := ip.EvalPersistentSource("1 + f()")
	if err == nil {
		t.Fatalf("expected runtime error")
	}

	gotL, gotC, ok := parseAtLC(err)
	if !ok {
		t.Fatalf("could not parse L:C from error: %v", err)
	}
	start := strings.Index(defSrc, "[]")
	expL, expC := offsetToLineCol(defSrc, start)
	if gotL != expL || gotC < expC {
		t.Fatalf("outer error mapped to call-site or wrong pos: got %d:%d, want %d:>=%d", gotL, gotC, expL, expC)
	}
	if strings.Contains(err.Error(), "1 + f()") && !strings.Contains(err.Error(), "let f = fun()") {
		t.Fatalf("snippet appears to be call-site instead of inner function:\n%s", err)
	}
}

func Test_E2E_InnerCaret_UsesNestedFunctionSource(t *testing.T) {
	ip := NewInterpreter()

	defSrc := "let f = fun() do\n  let g = fun() do 1/0 end\n  g()\nend"
	if _, err := ip.EvalPersistentSource(defSrc); err != nil {
		t.Fatalf("def error: %v", err)
	}
	_, err := ip.EvalPersistentSource("f()")
	if err == nil {
		t.Fatalf("expected runtime error")
	}

	gotL, gotC, ok := parseAtLC(err)
	if !ok {
		t.Fatalf("could not parse L:C from error: %v", err)
	}
	start := strings.Index(defSrc, "1/0")
	expL, expC := offsetToLineCol(defSrc, start)
	if gotL != expL || gotC < expC {
		t.Fatalf("outer error mapped to call-site or wrong pos: got %d:%d, want %d:>=%d", gotL, gotC, expL, expC)
	}
	if strings.Contains(err.Error(), "f()") && !strings.Contains(err.Error(), "let g = fun()") {
		t.Fatalf("snippet appears to be call-site instead of inner nested function:\n%s", err)
	}
}

// ----------------- helpers to walk S-expression AST -----------------

// getChildren returns child nodes as (childIndex, childNode).
func getChildren(n S) [][2]any {
	out := make([][2]any, 0, max(0, len(n)-1))
	for i := 1; i < len(n); i++ {
		if child, ok := n[i].(S); ok {
			out = append(out, [2]any{i - 1, child}) // child index is i-1 (see NodePath doc)
		}
	}
	return out
}

func findFirstPath(root S, pred func(S) bool) (NodePath, bool) {
	var dfs func(n S, path NodePath) (NodePath, bool)
	dfs = func(n S, path NodePath) (NodePath, bool) {
		if len(n) > 0 {
			if tag, _ := n[0].(string); pred != nil && tag != "" && pred(n) {
				return append(NodePath(nil), path...), true
			}
		}
		for _, ch := range getChildren(n) {
			idx := ch[0].(int)
			child := ch[1].(S)
			if p, ok := dfs(child, append(append(NodePath(nil), path...), idx)); ok {
				return p, true
			}
		}
		return nil, false
	}
	return dfs(root, nil)
}

func subtreeHasBinopDiv(n S) bool {
	found := false
	var dfs func(S)
	dfs = func(x S) {
		if found || len(x) == 0 {
			return
		}
		if x[0].(string) == "binop" && len(x) >= 2 && x[1].(string) == "/" {
			found = true
			return
		}
		for _, ch := range getChildren(x) {
			dfs(ch[1].(S))
			if found {
				return
			}
		}
	}
	dfs(n)
	return found
}

func findBinopDivPath(root S) (NodePath, bool) {
	return findFirstPath(root, func(n S) bool {
		return len(n) > 1 && n[0].(string) == "binop" && n[1].(string) == "/"
	})
}

// ----------------- diagnostics -----------------

// 1) Spans: do we have entries for inner fun, its body, and "/" under it?
func Test_Spans_For_InnerFun_And_Binop(t *testing.T) {
	src := "let f = fun() do\n  let g = fun() do 1/0 end\n  g()\nend"

	ast, spans, err := ParseSExprWithSpans(src)
	if err != nil {
		t.Fatalf("ParseSExprWithSpans error: %v", err)
	}

	// Find the inner ("fun", ...) whose subtree contains the "/" binop
	funPath, ok := findFirstPath(ast, func(n S) bool {
		if len(n) == 0 || n[0].(string) != "fun" {
			return false
		}
		// Child 2 (NodePath index) is the body by design ("fun", params, ret, body)
		if len(n) < 4 {
			return false
		}
		body := n[3].(S)
		return subtreeHasBinopDiv(body)
	})
	if !ok {
		t.Fatalf("could not locate inner fun path with '/' in body")
	}

	// Its body child is funPath + [2]
	bodyPath := append(append(NodePath(nil), funPath...), 2)

	// Binop path under the inner fun
	binopPath, ok := findBinopDivPath(ast)
	if !ok {
		t.Fatalf("could not locate '/' binop path")
	}

	// Assert spans exist and substrings look right
	if sp, ok := spans.Get(funPath); !ok {
		t.Fatalf("no span for inner fun path %v", funPath)
	} else {
		frag := src[sp.StartByte:sp.EndByte]
		if !strings.Contains(frag, "fun()") {
			t.Fatalf("funPath span does not include 'fun()': %q", frag)
		}
	}

	if sp, ok := spans.Get(bodyPath); !ok {
		t.Fatalf("no span for inner fun BODY path %v (off-by-one child?)", bodyPath)
	} else {
		frag := src[sp.StartByte:sp.EndByte]
		if !strings.Contains(frag, "1/0") {
			t.Fatalf("bodyPath span does not include '1/0': %q", frag)
		}
	}

	if sp, ok := spans.Get(binopPath); !ok {
		t.Fatalf("no span for '/' binop path %v", binopPath)
	} else {
		frag := src[sp.StartByte:sp.EndByte]
		if !strings.Contains(frag, "1/0") && !strings.Contains(frag, "/ 0") {
			t.Fatalf("binop span unexpected: %q", frag)
		}
	}
}

// 2) Runtime-built inner function g: does Src.PathBase equal the BODY path?
func Test_InnerFunc_PathBase_Equals_BodyPath(t *testing.T) {
	// ret() returns g (the inner function) without calling it.
	def := "let ret = fun() do\n  let g = fun() do 1/0 end\n  g\nend"

	ip := NewInterpreter()
	if _, err := ip.EvalPersistentSource(def); err != nil {
		t.Fatalf("def error: %v", err)
	}

	// Parse same src to compute expected fun/body paths
	ast, spans, err := ParseSExprWithSpans(def)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	funPath, ok := findFirstPath(ast, func(n S) bool {
		if len(n) == 0 || n[0].(string) != "fun" {
			return false
		}
		// We want the INNER fun (g). Look for the one whose body has "/".
		if len(n) < 4 {
			return false
		}
		return subtreeHasBinopDiv(n[3].(S))
	})
	if !ok {
		t.Fatalf("no inner fun in AST")
	}
	bodyPath := append(append(NodePath(nil), funPath...), 2)

	// Sanity: spans has bodyPath
	if _, ok := spans.Get(bodyPath); !ok {
		t.Fatalf("spans missing inner body path %v", bodyPath)
	}

	// Build inner g by calling ret()
	val, err := ip.EvalPersistentSource("ret()")
	if err != nil {
		t.Fatalf("calling ret() failed: %v", err)
	}
	if val.Tag != VTFun {
		t.Fatalf("ret() did not return a function; got %v", val.Tag)
	}
	g := val.Data.(*Fun)

	if g.Src == nil {
		t.Fatalf("g.Src nil")
	}
	if len(g.Src.PathBase) == 0 {
		t.Fatalf("g.Src.PathBase empty (base path not attached by __make_fun?)")
	}

	// Compare exactly to expected body path (not the fun node path)
	if strings.TrimSpace(g.Src.Src) != strings.TrimSpace(def) {
		t.Fatalf("g.Src.Src mismatch; expected it to be the defining source text")
	}
	if len(g.Src.PathBase) != len(bodyPath) {
		t.Fatalf("g.Src.PathBase len=%d != bodyPath len=%d; got %v want %v",
			len(g.Src.PathBase), len(bodyPath), g.Src.PathBase, bodyPath)
	}
	for i := range bodyPath {
		if g.Src.PathBase[i] != bodyPath[i] {
			t.Fatalf("g.Src.PathBase differs at index %d: got %v want %v",
				i, g.Src.PathBase, bodyPath)
		}
	}
}

// 3) JIT marks under g: does the VM map PC to the binop span using g.Src?
func Test_InnerFunc_VM_PC_Maps_To_Binop_In_Body(t *testing.T) {
	def := "let ret = fun() do\n  let g = fun() do 1/0 end\n  g\nend"

	ip := NewInterpreter()
	if _, err := ip.EvalPersistentSource(def); err != nil {
		t.Fatalf("def error: %v", err)
	}
	v, err := ip.EvalPersistentSource("ret()")
	if err != nil {
		t.Fatalf("ret() error: %v", err)
	}
	if v.Tag != VTFun {
		t.Fatalf("ret() did not return a function")
	}
	g := v.Data.(*Fun)

	// Ensure chunk & spans are set
	ip.ensureChunkWithSource(g, g.Src)
	if g.Src == nil || g.Src.Spans == nil {
		t.Fatalf("g.Src or g.Src.Spans nil")
	}
	if len(g.Chunk.Marks) == 0 {
		t.Fatalf("g.Chunk.Marks empty")
	}

	// Force executing g to produce the VM failure and get a PC.
	res := ip.runChunk(g.Chunk, g.Env, 0)
	if res.status != vmRuntimeError {
		t.Fatalf("expected vmRuntimeError, got %v", res.status)
	}

	line, col := ip.sourcePosFromChunk(g.Chunk, g.Src, res.pc)

	// Compute expected pos of "1/0" in def
	start := strings.Index(def, "1/0")
	expL, expC := offsetToLineCol(def, start)
	if line != expL || col < expC {
		t.Fatalf("inner VM PC map mismatch: got %d:%d, want %d:>=%d", line, col, expL, expC)
	}
}
