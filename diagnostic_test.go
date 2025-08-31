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
