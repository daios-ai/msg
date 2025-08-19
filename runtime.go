// runtime.go
//
// This file implements the standard runtime/builtins *against the stable engine
// surface* defined in interpreter.go. It avoids reaching into the engine's
// internals (no ip.eval, no direct stack/VM knowledge). The only remaining
// intentional touch-point is for `spawn`, where we temporarily copy a function's
// closure to a snapshot Env to preserve isolation semantics. See the TODO there.

package mindscript

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"math"
	"math/rand"
	"net"
	"os"
	"strings"
	"time"
)

// annotate a core builtin function value with a docstring
func setBuiltinDoc(ip *Interpreter, name, doc string) {
	if v, err := ip.Core.Get(name); err == nil {
		ip.Core.Define(name, withAnnot(v, doc))
	}
}

// NewInterpreterWithBuiltins initializes a fresh interpreter with:
//   - Core: shared env for built-ins
//   - Global: user env chained to Core
//   - Native registry + module cache initialized
//   - Standard built-ins registered (type, IO, concurrency, net, JSON, etc.)
func NewInterpreterWithBuiltins() *Interpreter {
	ip := NewInterpreter()

	// Recreate Core/Global for a clean runtime env.
	ip.Core = NewEnv(nil)
	ip.Global = NewEnv(ip.Core)

	// registries — set up BEFORE installing engine core helpers
	ip.native = map[string]NativeImpl{}
	ip.modules = map[string]*moduleRec{}

	// Re-install engine core helpers (__assign_set, __plus, __map_from, __resolve_type, …)
	ip.initCore()

	// Standard library
	registerStandardBuiltins(ip)
	registerConcurrencyBuiltins(ip)
	registerIOBuiltins(ip)
	registerIntrospectionBuiltins(ip)
	registerUtilityBuiltins(ip)
	registerNetBuiltins(ip)

	return ip
}

func NewRuntime() *Interpreter { return NewInterpreterWithBuiltins() }

// Public convenience if consumers already created an Interpreter.
func RegisterBuiltins(ip *Interpreter) {
	if ip == nil {
		return
	}
	if ip.Core == nil {
		ip.Core = NewEnv(nil)
	}
	if ip.native == nil {
		ip.native = map[string]NativeImpl{}
	}
	if ip.modules == nil {
		ip.modules = map[string]*moduleRec{}
	}
	registerStandardBuiltins(ip)
	registerConcurrencyBuiltins(ip)
	registerIOBuiltins(ip)
	registerIntrospectionBuiltins(ip)
	registerUtilityBuiltins(ip)
	registerNetBuiltins(ip)
}

// ---- standard built-ins ----------------------------------------------------

func registerStandardBuiltins(ip *Interpreter) {
	// typeOf(x: Any) -> Type
	ip.RegisterNative(
		"typeOf",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"id", "Type"},
		func(ip *Interpreter, ctx CallCtx) Value {
			x := ctx.MustArg("x")
			return TypeVal(ip.ValueToType(x, ctx.Env()))
		},
	)
	setBuiltinDoc(ip, "typeOf", "Return the dynamic Type of x.\nReturns a Type value usable with isType/isSubtype.")

	// isType(x: Any, T: Type) -> Bool
	ip.RegisterNative(
		"isType",
		[]ParamSpec{
			{Name: "x", Type: S{"id", "Any"}},
			{Name: "T", Type: S{"id", "Type"}},
		},
		S{"id", "Bool"},
		func(ip *Interpreter, ctx CallCtx) Value {
			x := ctx.MustArg("x")
			tv := ctx.MustArg("T")
			if tv.Tag != VTType {
				fail("isType expects a Type as second argument")
			}
			return Bool(ip.IsType(x, tv.Data.(S), ctx.Env()))
		},
	)
	setBuiltinDoc(ip, "isType", "Check whether value x conforms to type T.\nSecond argument must be a Type (e.g. type Int).")

	// isSubtype(A: Type, B: Type) -> Bool
	ip.RegisterNative(
		"isSubtype",
		[]ParamSpec{
			{Name: "A", Type: S{"id", "Type"}},
			{Name: "B", Type: S{"id", "Type"}},
		},
		S{"id", "Bool"},
		func(ip *Interpreter, ctx CallCtx) Value {
			Av := ctx.MustArg("A")
			Bv := ctx.MustArg("B")
			if Av.Tag != VTType || Bv.Tag != VTType {
				fail("isSubtype expects Types as both arguments")
			}
			return Bool(ip.IsSubtype(Av.Data.(S), Bv.Data.(S), ctx.Env()))
		},
	)
	setBuiltinDoc(ip, "isSubtype", "True if type A is a subtype of type B (Python-like structural semantics).")

	// import(path: Str) -> Module
	ip.RegisterNative(
		"import",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("path")
			if pv.Tag != VTStr {
				return annotNull("import expects a string path")
			}
			importer := ""
			if n := len(ip.loadStack); n > 0 {
				importer = ip.loadStack[n-1]
			}
			mod, err := ip.importModule(pv.Data.(string), importer)
			if err != nil {
				return annotNull(err.Error())
			}
			return mod
		},
	)
	setBuiltinDoc(ip, "import", "Load a module by URL or file path (auto-detected).\nSearch order for files: importer dir → CWD → MINDSCRIPT_PATH; default extension added if missing.")

	// importCode(name: Str, src: Str) -> Module
	ip.RegisterNative(
		"importCode",
		[]ParamSpec{
			{Name: "name", Type: S{"id", "Str"}},
			{Name: "src", Type: S{"id", "Str"}},
		},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			nv := ctx.MustArg("name")
			sv := ctx.MustArg("src")
			if nv.Tag != VTStr || sv.Tag != VTStr {
				return annotNull("importCode expects (name: Str, src: Str)")
			}
			name := nv.Data.(string)
			src := sv.Data.(string)

			// Parse
			ast, perr := ParseSExpr(src)
			if perr != nil {
				return annotNull(fmt.Sprintf("parse error in mem:%s: %v", name, perr))
			}

			// Evaluate in isolated env parented to Core (uncaught to treat rtErr as failure)
			modEnv := NewEnv(ip.Core)
			var rterr error
			var evalRes Value
			func() {
				defer func() {
					if r := recover(); r != nil {
						switch sig := r.(type) {
						case rtErr:
							rterr = fmt.Errorf("runtime error in mem:%s: %s", name, sig.msg)
						default:
							rterr = fmt.Errorf("runtime panic in mem:%s: %v", name, r)
						}
					}
				}()
				if len(ast) > 0 {
					evalRes = ip.EvalASTUncaught(ast, modEnv, true)
				}
			}()
			if rterr == nil && evalRes.Tag == VTNull && evalRes.Annot != "" {
				rterr = fmt.Errorf("runtime error in mem:%s: %s", name, evalRes.Annot)
			}
			if rterr != nil {
				fail(rterr.Error())
			}

			exports := make(map[string]Value, len(modEnv.table))
			for k, v := range modEnv.table {
				exports[k] = v
			}
			return Value{Tag: VTModule, Data: &Module{Name: "mem:" + name, Exports: exports}}
		},
	)
	setBuiltinDoc(ip, "importCode",
		"Evaluate a string as a module and return it as a Module value.\n"+
			"Note: This does not register into the module cache; subsequent import(name) will not refer to this.")

}

// --- Opaque handle (Lua-like userdata) + concrete boxed types ---

type Handle struct {
	Kind string
	Data any
}

func HandleVal(kind string, data any) Value {
	return Value{Tag: VTHandle, Data: &Handle{Kind: kind, Data: data}}
}

func asHandle(v Value, want string) *Handle {
	if v.Tag != VTHandle {
		fail("expected handle")
	}
	h := v.Data.(*Handle)
	if want != "" && h.Kind != want {
		fail("wrong handle kind")
	}
	return h
}

// Channel box (shared by all channel builtins)
type chanBox struct {
	ch chan Value
}

// File handle (shared by all file I/O builtins)
type fileH struct {
	f    *os.File
	rb   *bufio.Reader
	wb   *bufio.Writer
	hasR bool
	hasW bool
}

// Sockets
type netConnH struct {
	c  net.Conn
	rb *bufio.Reader
	wb *bufio.Writer
}

type netListenerH struct {
	ln net.Listener
}

// --- Deep copy & snapshot for isolated worlds --------------------------------

func cloneValue(v Value) Value {
	switch v.Tag {
	case VTNull, VTBool, VTInt, VTNum, VTStr, VTType, VTFun: // VTFun env handled separately for spawn
		return v
	case VTArray:
		xs := v.Data.([]Value)
		cp := make([]Value, len(xs))
		for i := range xs {
			cp[i] = cloneValue(xs[i])
		}
		return Arr(cp)
	case VTMap:
		m := v.Data.(map[string]Value)
		cp := make(map[string]Value, len(m))
		for k, vv := range m {
			cp[k] = cloneValue(vv)
		}
		return Map(cp)
	default:
		// Userdata/modules are NOT copied; processes should not capture them.
		return v
	}
}

func snapshotEnv(e *Env) *Env {
	// Flatten chain into one level (shadowing by nearer scopes wins).
	flat := map[string]Value{}
	for cur := e; cur != nil; cur = cur.parent {
		for k, v := range cur.table {
			if _, exists := flat[k]; !exists {
				flat[k] = cloneValue(v)
			}
		}
	}
	cp := NewEnv(nil)
	for k, v := range flat {
		cp.Define(k, v)
	}
	return cp
}

// --- Concurrency primitives -------------------------------------------------

type procState struct {
	done   chan struct{}
	result Value
	cancel chan struct{} // cooperative
}

func registerConcurrencyBuiltins(ip *Interpreter) {
	// spawn(f: Any->Any) -> Any (proc handle)
	ip.RegisterNative(
		"spawn",
		[]ParamSpec{{Name: "f", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			fv := ctx.MustArg("f")
			if fv.Tag != VTFun {
				fail("spawn expects a function")
			}

			// TODO(engine): provide engine API to rebind a callable's closure env
			// e.g., CloneCallableWithEnv(fn, snapshotEnv). For now, copy *Fun.
			fn := fv.Data.(*Fun)

			// Snapshot closure env for isolation
			snap := snapshotEnv(fn.Env)
			// Clone function onto the snapshot
			work := &Fun{
				Params:     append([]string{}, fn.Params...),
				ParamTypes: append([]S{}, fn.ParamTypes...),
				ReturnType: fn.ReturnType,
				Body:       fn.Body,
				Env:        snap,
				HiddenNull: fn.HiddenNull,
			}

			pr := &procState{done: make(chan struct{}), cancel: make(chan struct{})}
			go func() {
				defer func() {
					if r := recover(); r != nil {
						switch sig := r.(type) {
						case returnSig:
							pr.result = sig.v
						case rtErr:
							pr.result = errNull(sig.msg)
						default:
							pr.result = errNull(fmt.Sprintf("runtime panic: %v", r))
						}
					}
					close(pr.done)
				}()
				pr.result = ip.execFunBody(work)
			}()
			return HandleVal("proc", pr)
		},
	)
	setBuiltinDoc(ip, "spawn", "Run a function in a new process with an isolated snapshot of its environment.\nPass a fully-curried function; returns a proc handle for join/cancel.")

	ip.RegisterNative(
		"join",
		[]ParamSpec{{Name: "p", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("p")
			pr := asHandle(pv, "proc").Data.(*procState)
			<-pr.done
			return pr.result
		},
	)
	setBuiltinDoc(ip, "join", "Wait for a proc to finish and return its result.\nIf the proc failed, returns an annotated null describing the error.")

	ip.RegisterNative(
		"cancel",
		[]ParamSpec{{Name: "p", Type: S{"id", "Any"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("p")
			pr := asHandle(pv, "proc").Data.(*procState)
			select {
			case <-pr.cancel:
			default:
				close(pr.cancel)
			}
			return Null
		},
	)
	setBuiltinDoc(ip, "cancel", "Request cooperative cancellation of a proc.\nBest-effort; user code may opt-in to observe cancellation.")

	// Channels (untyped)
	ip.RegisterNative("chan", nil, S{"id", "Any"}, func(ip *Interpreter, ctx CallCtx) Value {
		return HandleVal("chan", &chanBox{ch: make(chan Value)})
	})
	setBuiltinDoc(ip, "chan", "Create a new unbuffered channel.\nUse chanSend/chanRecv/chanClose to communicate.")

	ip.RegisterNative(
		"chanSend",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}, {Name: "x", Type: S{"id", "Any"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			cb := asHandle(ctx.MustArg("c"), "chan").Data.(*chanBox)
			x := ctx.MustArg("x")
			cb.ch <- x
			return Null
		},
	)
	setBuiltinDoc(ip, "chanSend", "Send a value on channel c (blocks until a receiver is ready).")

	ip.RegisterNative(
		"chanRecv",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			cb := asHandle(ctx.MustArg("c"), "chan").Data.(*chanBox)
			v, ok := <-cb.ch
			if !ok {
				return annotNull("channel closed")
			}
			return v
		},
	)
	setBuiltinDoc(ip, "chanRecv", "Receive a value from channel c (blocks until a sender is ready).\nReturns annotated null \"channel closed\" after close.")

	ip.RegisterNative(
		"chanClose",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			cb := asHandle(ctx.MustArg("c"), "chan").Data.(*chanBox)
			close(cb.ch)
			return Null
		},
	)
	setBuiltinDoc(ip, "chanClose", "Close channel c.\nFurther receives yield annotated-null; sending after close is an error.")

}

// --- I/O primitives (file & network) ----------------------------------------

func registerIOBuiltins(ip *Interpreter) {
	openFile := func(path, mode string) (*fileH, error) {
		var flag int
		switch mode {
		case "r":
			flag = os.O_RDONLY
		case "w":
			flag = os.O_WRONLY | os.O_CREATE | os.O_TRUNC
		case "a":
			flag = os.O_WRONLY | os.O_CREATE | os.O_APPEND
		case "rw":
			flag = os.O_RDWR | os.O_CREATE
		default:
			return nil, fmt.Errorf("invalid mode %q", mode)
		}
		f, err := os.OpenFile(path, flag, 0o644)
		if err != nil {
			return nil, err
		}
		h := &fileH{f: f}
		if strings.Contains(mode, "r") {
			h.rb = bufio.NewReader(f)
			h.hasR = true
		}
		if strings.Contains(mode, "w") || mode == "a" || mode == "rw" {
			h.wb = bufio.NewWriter(f)
			h.hasW = true
		}
		return h, nil
	}

	// helpers to fetch reader/writer across file or net handles
	getReader := func(hv Value) (*bufio.Reader, string) {
		h := asHandle(hv, "")
		switch h.Kind {
		case "file":
			fh := h.Data.(*fileH)
			if !fh.hasR {
				fail("not readable")
			}
			return fh.rb, "file"
		case "net":
			nh := h.Data.(*netConnH)
			return nh.rb, "net"
		default:
			fail("unsupported handle for read")
			return nil, ""
		}
	}
	getWriter := func(hv Value) (*bufio.Writer, string) {
		h := asHandle(hv, "")
		switch h.Kind {
		case "file":
			fh := h.Data.(*fileH)
			if !fh.hasW {
				fail("not writable")
			}
			return fh.wb, "file"
		case "net":
			nh := h.Data.(*netConnH)
			return nh.wb, "net"
		default:
			fail("unsupported handle for write")
			return nil, ""
		}
	}

	ip.RegisterNative(
		"open",
		[]ParamSpec{
			{Name: "path", Type: S{"id", "Str"}},
			{Name: "mode", Type: S{"enum", S{"str", "r"}, S{"str", "w"}, S{"str", "a"}, S{"str", "rw"}}},
		},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("path")
			mv := ctx.MustArg("mode")
			if pv.Tag != VTStr {
				fail("open expects path: Str")
			}
			if mv.Tag != VTStr {
				fail("open expects mode: Str")
			}
			h, err := openFile(pv.Data.(string), mv.Data.(string))
			if err != nil {
				fail(err.Error())
			}
			return HandleVal("file", h)
		},
	)
	setBuiltinDoc(ip, "open", "Open a file at path with mode \"r\" | \"w\" | \"a\" | \"rw\".\nReturns a file handle for read/write/flush/close.")

	ip.RegisterNative(
		"close",
		[]ParamSpec{{Name: "h", Type: S{"id", "Any"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			h := asHandle(ctx.MustArg("h"), "")
			switch h.Kind {
			case "file":
				fh := h.Data.(*fileH)
				if fh.wb != nil {
					_ = fh.wb.Flush()
				}
				_ = fh.f.Close()
			case "net":
				nh := h.Data.(*netConnH)
				if nh.wb != nil {
					_ = nh.wb.Flush()
				}
				_ = nh.c.Close()
			default:
				fail("unsupported handle for close")
			}
			return Null
		},
	)
	setBuiltinDoc(ip, "close", "Close a file or network handle.\nFlushes buffered output first.")

	ip.RegisterNative(
		"readAll",
		[]ParamSpec{{Name: "h", Type: S{"id", "Any"}}},
		S{"id", "Str"},
		func(ip *Interpreter, ctx CallCtx) Value {
			rb, _ := getReader(ctx.MustArg("h"))
			b, err := io.ReadAll(rb)
			if err != nil {
				fail(err.Error())
			}
			return Str(string(b))
		},
	)
	setBuiltinDoc(ip, "readAll", "Read all remaining bytes from a file or network handle and return as Str.")

	ip.RegisterNative(
		"readN",
		[]ParamSpec{{Name: "h", Type: S{"id", "Any"}}, {Name: "n", Type: S{"id", "Int"}}},
		S{"id", "Str"},
		func(ip *Interpreter, ctx CallCtx) Value {
			rb, _ := getReader(ctx.MustArg("h"))
			nv := ctx.MustArg("n")
			n := int(nv.Data.(int64))
			buf := make([]byte, n)
			k, err := io.ReadFull(rb, buf)
			if err != nil && err != io.EOF && err != io.ErrUnexpectedEOF {
				fail(err.Error())
			}
			return Str(string(buf[:k]))
		},
	)
	setBuiltinDoc(ip, "readN", "Read up to n bytes from a file or network handle and return as Str.")

	ip.RegisterNative(
		"readLine",
		[]ParamSpec{{Name: "h", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			rb, _ := getReader(ctx.MustArg("h"))
			s, err := rb.ReadString('\n')
			if errors.Is(err, io.EOF) && s == "" {
				return Null
			}
			if err != nil && !errors.Is(err, io.EOF) {
				fail(err.Error())
			}
			return Str(strings.TrimRight(s, "\n"))
		},
	)
	setBuiltinDoc(ip, "readLine", "Read one line (without trailing newline) from a file or network handle.\nReturns null at EOF.")

	ip.RegisterNative(
		"write",
		[]ParamSpec{{Name: "h", Type: S{"id", "Any"}}, {Name: "s", Type: S{"id", "Str"}}},
		S{"id", "Int"},
		func(ip *Interpreter, ctx CallCtx) Value {
			wb, _ := getWriter(ctx.MustArg("h"))
			sv := ctx.MustArg("s")
			n, err := wb.WriteString(sv.Data.(string))
			if err != nil {
				fail(err.Error())
			}
			return Int(int64(n))
		},
	)
	setBuiltinDoc(ip, "write", "Write string s to a file or network handle; returns number of bytes written (Int).")

	ip.RegisterNative(
		"flush",
		[]ParamSpec{{Name: "h", Type: S{"id", "Any"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			wb, _ := getWriter(ctx.MustArg("h"))
			if err := wb.Flush(); err != nil {
				fail(err.Error())
			}
			return Null
		},
	)
	setBuiltinDoc(ip, "flush", "Flush buffered output for a file or network handle.")

	ip.RegisterNative(
		"readFile",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"id", "Str"},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("path")
			b, err := os.ReadFile(pv.Data.(string))
			if err != nil {
				fail(err.Error())
			}
			return Str(string(b))
		},
	)
	setBuiltinDoc(ip, "readFile", "Read the whole file at path into a Str.")

	ip.RegisterNative(
		"writeFile",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}, {Name: "data", Type: S{"id", "Str"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("path")
			dv := ctx.MustArg("data")
			if err := os.WriteFile(pv.Data.(string), []byte(dv.Data.(string)), 0o644); err != nil {
				fail(err.Error())
			}
			return Null
		},
	)
	setBuiltinDoc(ip, "writeFile", "Write Str data to file at path (overwrites).")

	ip.RegisterNative(
		"listDir",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"array", S{"id", "Str"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("path")
			ents, err := os.ReadDir(pv.Data.(string))
			if err != nil {
				fail(err.Error())
			}
			out := make([]Value, 0, len(ents))
			for _, e := range ents {
				out = append(out, Str(e.Name()))
			}
			return Arr(out)
		},
	)
	setBuiltinDoc(ip, "listDir", "List directory entries as [Str] of names.")

	// Bind std streams with doc
	ip.Core.Define("stdin", withAnnot(HandleVal("file", &fileH{f: os.Stdin, rb: bufio.NewReader(os.Stdin), hasR: true}), "Standard input stream handle."))
	ip.Core.Define("stdout", withAnnot(HandleVal("file", &fileH{f: os.Stdout, wb: bufio.NewWriter(os.Stdout), hasW: true}), "Standard output stream handle."))
	ip.Core.Define("stderr", withAnnot(HandleVal("file", &fileH{f: os.Stderr, wb: bufio.NewWriter(os.Stderr), hasW: true}), "Standard error stream handle."))

}

// --- Introspection & docs ----------------------------------------------------

func registerIntrospectionBuiltins(ip *Interpreter) {
	ip.RegisterNative(
		"funInfo",
		[]ParamSpec{{Name: "f", Type: S{"id", "Any"}}},
		S{"map",
			S{"pair!", S{"str", "params"}, S{"array", S{"map",
				S{"pair!", S{"str", "name"}, S{"id", "Str"}},
				S{"pair!", S{"str", "type"}, S{"id", "Type"}},
			}}},
			S{"pair!", S{"str", "return"}, S{"id", "Type"}},
			S{"pair", S{"str", "doc"}, S{"id", "Str"}},
		},
		func(ip *Interpreter, ctx CallCtx) Value {
			fv := ctx.MustArg("f")
			callable, ok := ip.FunMeta(fv)
			if !ok {
				fail("funInfo expects a function")
			}
			ps := callable.ParamSpecs()
			params := make([]Value, 0, len(ps))
			for _, p := range ps {
				params = append(params, Map(map[string]Value{
					"name": Str(p.Name),
					"type": TypeVal(ip.ResolveType(p.Type, callable.ClosureEnv())),
				}))
			}
			doc := callable.Doc()
			if doc == "" && fv.Annot != "" {
				doc = fv.Annot
			}
			return Map(map[string]Value{
				"params": Arr(params),
				"return": TypeVal(ip.ResolveType(callable.ReturnType(), callable.ClosureEnv())),
				"doc":    Str(doc),
			})
		},
	)
	setBuiltinDoc(ip, "funInfo", "Return metadata for function f as a map: params (name & type), return type, and docstring.")

	ip.RegisterNative(
		"funType",
		[]ParamSpec{{Name: "f", Type: S{"id", "Any"}}},
		S{"id", "Type"},
		func(ip *Interpreter, ctx CallCtx) Value {
			fv := ctx.MustArg("f")
			return TypeVal(ip.ValueToType(fv, ctx.Env()))
		},
	)
	setBuiltinDoc(ip, "funType", "Return the Type of function f (chains of A->B->C).")

	ip.RegisterNative(
		"typeEquals",
		[]ParamSpec{{Name: "a", Type: S{"id", "Type"}}, {Name: "b", Type: S{"id", "Type"}}},
		S{"id", "Bool"},
		func(ip *Interpreter, ctx CallCtx) Value {
			av := ctx.MustArg("a")
			bv := ctx.MustArg("b")
			return Bool(equalS(av.Data.(S), bv.Data.(S)))
		},
	)
	setBuiltinDoc(ip, "typeEquals", "Structural equality check between two fully-resolved Types.")

	ip.RegisterNative(
		"typeFields",
		[]ParamSpec{{Name: "t", Type: S{"id", "Type"}}},
		S{"array", S{"map",
			S{"pair!", S{"str", "name"}, S{"id", "Str"}},
			S{"pair!", S{"str", "type"}, S{"id", "Type"}},
			S{"pair!", S{"str", "required"}, S{"id", "Bool"}},
		}},
		func(ip *Interpreter, ctx CallCtx) Value {
			tv := ctx.MustArg("t")
			t := ip.ResolveType(tv.Data.(S), ctx.Env())
			if len(t) == 0 || t[0].(string) != "map" {
				return Arr(nil)
			}
			fs := mapTypeFields(t)
			out := make([]Value, 0, len(fs))
			for k, fi := range fs {
				out = append(out, Map(map[string]Value{
					"name":     Str(k),
					"type":     TypeVal(fi.typ),
					"required": Bool(fi.required),
				}))
			}
			return Arr(out)
		},
	)
	setBuiltinDoc(ip, "typeFields", "For a map Type, return [{name, type, required}] describing its fields.")

	ip.RegisterNative(
		"arrayElemType",
		[]ParamSpec{{Name: "t", Type: S{"id", "Type"}}},
		S{"unop", "?", S{"id", "Type"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			tv := ctx.MustArg("t")
			t := ip.ResolveType(tv.Data.(S), ctx.Env())
			if len(t) == 2 && t[0].(string) == "array" {
				return TypeVal(t[1].(S))
			}
			return Null
		},
	)
	setBuiltinDoc(ip, "arrayElemType", "If t is [T], return T as a Type; otherwise null.")

	ip.RegisterNative(
		"isNullable",
		[]ParamSpec{{Name: "t", Type: S{"id", "Type"}}},
		S{"id", "Bool"},
		func(ip *Interpreter, ctx CallCtx) Value {
			tv := ctx.MustArg("t")
			t := ip.ResolveType(tv.Data.(S), ctx.Env())
			return Bool(len(t) >= 3 && t[0].(string) == "unop" && t[1].(string) == "?")
		},
	)
	setBuiltinDoc(ip, "isNullable", "Return true if t is nullable (T?).")

	ip.RegisterNative(
		"baseType",
		[]ParamSpec{{Name: "t", Type: S{"id", "Type"}}},
		S{"id", "Type"},
		func(ip *Interpreter, ctx CallCtx) Value {
			tv := ctx.MustArg("t")
			bt, _ := deopt(ip.ResolveType(tv.Data.(S), ctx.Env()))
			return TypeVal(bt)
		},
	)
	setBuiltinDoc(ip, "baseType", "Strip nullable from T? and return base T.")

	// docs
	ip.RegisterNative(
		"doc",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			x := ctx.MustArg("x")
			if x.Annot == "" {
				return Null
			}
			ln := strings.SplitN(x.Annot, "\n", 2)[0]
			return Str(ln)
		},
	)
	setBuiltinDoc(ip, "doc", "Return the first line of x's docstring (annotation) or null if absent.")

	ip.RegisterNative(
		"help",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			x := ctx.MustArg("x")
			if x.Annot == "" {
				return Null
			}
			return Str(x.Annot)
		},
	)
	setBuiltinDoc(ip, "help", "Return the full docstring (annotation) attached to x or null if absent.")

}

// --- Utilities: time, rand, json --------------------------------------------

func registerUtilityBuiltins(ip *Interpreter) {
	ip.RegisterNative("nowMillis", nil, S{"id", "Int"}, func(ip *Interpreter, ctx CallCtx) Value {
		return Int(time.Now().UnixMilli())
	})
	setBuiltinDoc(ip, "nowMillis", "Current Unix time in milliseconds (Int).")

	ip.RegisterNative(
		"sleep",
		[]ParamSpec{{Name: "ms", Type: S{"id", "Int"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			ms := ctx.MustArg("ms")
			time.Sleep(time.Duration(ms.Data.(int64)) * time.Millisecond)
			return Null
		},
	)
	setBuiltinDoc(ip, "sleep", "Sleep for ms milliseconds; returns null.")

	var rng = rand.New(rand.NewSource(time.Now().UnixNano()))
	ip.RegisterNative(
		"seedRand",
		[]ParamSpec{{Name: "n", Type: S{"id", "Int"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			n := ctx.MustArg("n")
			rng.Seed(n.Data.(int64))
			return Null
		},
	)
	setBuiltinDoc(ip, "seedRand", "Seed the pseudo-random generator deterministically.")

	ip.RegisterNative("randInt",
		[]ParamSpec{{Name: "n", Type: S{"id", "Int"}}},
		S{"id", "Int"},
		func(ip *Interpreter, ctx CallCtx) Value {
			n := ctx.MustArg("n")
			return Int(int64(rng.Intn(int(n.Data.(int64)))))
		},
	)
	setBuiltinDoc(ip, "randInt", "Return a random Int in [0, n).")

	ip.RegisterNative("randFloat", nil, S{"id", "Num"}, func(ip *Interpreter, ctx CallCtx) Value {
		return Num(rng.Float64())
	})
	setBuiltinDoc(ip, "randFloat", "Return a random Num in [0.0, 1.0).")

	// JSON
	ip.RegisterNative(
		"jsonParse",
		[]ParamSpec{{Name: "s", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			sv := ctx.MustArg("s")
			var x any
			if err := json.Unmarshal([]byte(sv.Data.(string)), &x); err != nil {
				fail(err.Error())
			}
			return goJSONToValue(x)
		},
	)
	setBuiltinDoc(ip, "jsonParse", "Parse a JSON string into MindScript values (null/bool/num/str/array/map).")

	ip.RegisterNative(
		"jsonStringify",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"id", "Str"},
		func(ip *Interpreter, ctx CallCtx) Value {
			xv := ctx.MustArg("x")
			b, err := json.Marshal(valueToGoJSON(xv))
			if err != nil {
				fail(err.Error())
			}
			return Str(string(b))
		},
	)
	setBuiltinDoc(ip, "jsonStringify", "Serialize a MindScript value to a compact JSON string.")

}

func goJSONToValue(x any) Value {
	switch v := x.(type) {
	case nil:
		return Null
	case bool:
		return Bool(v)
	case float64:
		// JSON numbers are float64; cast to Int if integral
		if math.Trunc(v) == v {
			return Int(int64(v))
		}
		return Num(v)
	case string:
		return Str(v)
	case []any:
		out := make([]Value, len(v))
		for i := range v {
			out[i] = goJSONToValue(v[i])
		}
		return Arr(out)
	case map[string]any:
		m := make(map[string]Value, len(v))
		for k, vv := range v {
			m[k] = goJSONToValue(vv)
		}
		return Map(m)
	default:
		return annotNull("unsupported JSON value")
	}
}

func valueToGoJSON(v Value) any {
	switch v.Tag {
	case VTNull:
		return nil
	case VTBool:
		return v.Data.(bool)
	case VTInt:
		return v.Data.(int64)
	case VTNum:
		return v.Data.(float64)
	case VTStr:
		return v.Data.(string)
	case VTArray:
		xs := v.Data.([]Value)
		out := make([]any, len(xs))
		for i := range xs {
			out[i] = valueToGoJSON(xs[i])
		}
		return out
	case VTMap:
		m := v.Data.(map[string]Value)
		out := make(map[string]any, len(m))
		for k, vv := range m {
			out[k] = valueToGoJSON(vv)
		}
		return out
	default:
		return fmt.Sprintf("<%v>", v.Tag)
	}
}

// --- Networking --------------------------------------------------------------

func registerNetBuiltins(ip *Interpreter) {
	// netConnect("host\:port") -> "net" handle
	ip.RegisterNative(
		"netConnect",
		[]ParamSpec{{Name: "addr", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			av := ctx.MustArg("addr")
			conn, err := net.Dial("tcp", av.Data.(string))
			if err != nil {
				fail(err.Error())
			}
			return HandleVal("net", &netConnH{
				c:  conn,
				rb: bufio.NewReader(conn),
				wb: bufio.NewWriter(conn),
			})
		},
	)
	setBuiltinDoc(ip, "netConnect", "Open a TCP connection to addr \"host:port\"; returns a network handle usable with read*/write/flush/close.")

	// netListen("host:port") -> "listener" handle
	ip.RegisterNative(
		"netListen",
		[]ParamSpec{{Name: "addr", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			av := ctx.MustArg("addr")
			ln, err := net.Listen("tcp", av.Data.(string))
			if err != nil {
				fail(err.Error())
			}
			return HandleVal("listener", &netListenerH{ln: ln})
		},
	)
	setBuiltinDoc(ip, "netListen", "Listen on a TCP address \"host:port\"; returns a listener handle for netAccept.")

	// netAccept(listener) -> "net" handle
	ip.RegisterNative(
		"netAccept",
		[]ParamSpec{{Name: "l", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			ln := asHandle(ctx.MustArg("l"), "listener").Data.(*netListenerH).ln
			conn, err := ln.Accept()
			if err != nil {
				fail(err.Error())
			}
			return HandleVal("net", &netConnH{
				c:  conn,
				rb: bufio.NewReader(conn),
				wb: bufio.NewWriter(conn),
			})
		},
	)
	setBuiltinDoc(ip, "netAccept", "Accept one TCP connection from a listener; returns a network handle.")

}

// -----------------------------------------------------------------------------
// Back-compat shims for existing code (runtime.go / vm.go expect these names)
// -----------------------------------------------------------------------------

// Old name used by runtime.go
func (ip *Interpreter) execFunBody(f *Fun) Value {
	return ip.execFunBodyScoped(f, nil)
}
