// === FILE: builtin_concurrency.go ===
package mindscript

import (
	"fmt"
	"time"
)

// Concurrency primitives for MindScript.
//
// Design notes (MindScript idioms):
// • Success should NEVER be signaled by returning Null. Null is reserved for
//   soft errors (with an annotation message). If an op can fail, its return
//   type is nullable (T?); otherwise it should return a concrete non-null type.
// • Each spawned process runs in its own interpreter isolate (Clone). The
//   spawned function’s closure is deep-snapshotted into that isolate. When the
//   process ends, its environments die with it. Writing into Core is impossible,
//   and Global in each isolate is sealed from mutating Core (see NewInterpreter).
// • VTType values are rebound when cloning (like VTFun): any pinned Env is
//   deep-snapshotted into the target isolate so type resolution is local.

type procState struct {
	done   chan struct{}
	result Value
	cancel chan struct{} // cooperative (best effort)
}

type chanBox struct {
	ch chan Value
}

func safeClose(ch chan Value) {
	// Close is idempotent via recover guard.
	defer func() { _ = recover() }()
	close(ch)
}

// ---------- deep cloning for isolates ----------

type cloneCtx struct {
	seenEnv map[*Env]*Env
	seenFun map[*Fun]*Fun
}

func deepSnapshotEnvInto(ctx *cloneCtx, src *Env, newParent *Env) *Env {
	if src == nil {
		return newParent
	}
	if ctx.seenEnv == nil {
		ctx.seenEnv = make(map[*Env]*Env)
	}
	if dst, ok := ctx.seenEnv[src]; ok {
		return dst
	}
	parent := deepSnapshotEnvInto(ctx, src.parent, newParent)
	dst := NewEnv(parent)
	dst.sealParentWrites = src.sealParentWrites
	ctx.seenEnv[src] = dst
	for k, v := range src.table {
		dst.table[k] = deepCloneValue(ctx, v, newParent)
	}
	return dst
}

func deepCloneValue(ctx *cloneCtx, v Value, targetCore *Env) Value {
	switch v.Tag {
	case VTNull, VTBool, VTInt, VTNum, VTStr:
		return v

	case VTType:
		// Rebind like VTFun: carry AST, snapshot env (if any) into target isolate.
		tv := v.Data.(*TypeValue)
		var env *Env
		if tv.Env != nil {
			env = deepSnapshotEnvInto(ctx, tv.Env, targetCore)
		}
		out := Value{Tag: VTType, Data: &TypeValue{Ast: tv.Ast, Env: env}}
		out.Annot = v.Annot
		return out

	case VTArray:
		src := v.Data.(*ArrayObject).Elems
		cp := make([]Value, len(src))
		for i := range src {
			cp[i] = deepCloneValue(ctx, src[i], targetCore)
		}
		return Arr(cp)

	case VTMap:
		src := v.Data.(*MapObject)
		dst := &MapObject{
			Entries: make(map[string]Value, len(src.Entries)),
			KeyAnn:  make(map[string]string, len(src.KeyAnn)),
			Keys:    make([]string, 0, len(src.Keys)),
		}
		for _, k := range src.Keys {
			dst.Keys = append(dst.Keys, k)
			if ann, ok := src.KeyAnn[k]; ok {
				dst.KeyAnn[k] = ann
			}
			dst.Entries[k] = deepCloneValue(ctx, src.Entries[k], targetCore)
		}
		return Value{Tag: VTMap, Data: dst}

	case VTModule:
		// Snapshot module as a plain map of exports.
		return deepCloneValue(ctx, AsMapValue(v), targetCore)

	case VTFun:
		f := v.Data.(*Fun)
		if ctx.seenFun == nil {
			ctx.seenFun = make(map[*Fun]*Fun)
		}
		if memo, ok := ctx.seenFun[f]; ok {
			nv := FunVal(memo)
			nv.Annot = v.Annot
			return nv
		}
		var newEnv *Env
		if f.NativeName != "" {
			// Natives should rebind to the child's Core so their effects land
			// in the child isolate (call-site scoping still applies).
			newEnv = targetCore
		} else {
			newEnv = deepSnapshotEnvInto(ctx, f.Env, targetCore)
		}
		nf := &Fun{
			Params:     append([]string(nil), f.Params...),
			ParamTypes: append([]S(nil), f.ParamTypes...),
			ReturnType: f.ReturnType,
			Body:       f.Body,
			Env:        newEnv,
			HiddenNull: f.HiddenNull,
			Chunk:      f.Chunk,
			NativeName: f.NativeName,
			IsOracle:   f.IsOracle,
			Examples:   deepCloneValue(ctx, f.Examples, targetCore),
			Src:        f.Src,
		}
		ctx.seenFun[f] = nf
		nv := FunVal(nf)
		nv.Annot = v.Annot
		return nv

	case VTHandle:
		// Share host handles (e.g., channels, proc handles) across isolates.
		return v

	default:
		return v
	}
}

// ---------------------------------------------------------------------------

func registerConcurrencyBuiltins(ip *Interpreter) {
	// procSpawn(f) -> proc handle
	ip.RegisterNative(
		"procSpawn",
		[]ParamSpec{{Name: "f", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			fv := ctx.MustArg("f")
			if fv.Tag != VTFun {
				fail("procSpawn expects a function")
			}
			orig := fv.Data.(*Fun)
			child := ip.Clone()

			cc := &cloneCtx{}
			snap := deepSnapshotEnvInto(cc, orig.Env, child.Core)

			work := &Fun{
				Params:     append([]string{}, orig.Params...),
				ParamTypes: append([]S{}, orig.ParamTypes...),
				ReturnType: orig.ReturnType,
				Body:       orig.Body,
				// IMPORTANT: natives rebind to child's Core; user funcs use deep snapshot.
				Env: func() *Env {
					if orig.NativeName != "" {
						return child.Core
					}
					return snap
				}(),
				HiddenNull: orig.HiddenNull,
				Chunk:      orig.Chunk,
				NativeName: orig.NativeName,
				IsOracle:   orig.IsOracle,
				Examples:   deepCloneValue(cc, orig.Examples, child.Core),
				Src:        orig.Src,
			}
			execVal := FunVal(work)
			execVal.Annot = fv.Annot

			pr := &procState{done: make(chan struct{}), cancel: make(chan struct{})}
			go func() {
				defer func() {
					if r := recover(); r != nil {
						switch sig := r.(type) {
						case returnSig:
							pr.result = sig.v
						case rtErr:
							pr.result = errNull(sig.msg)
						case error:
							pr.result = errNull(sig.Error())
						default:
							pr.result = errNull(fmt.Sprintf("runtime panic: %v", r))
						}
					}
					close(pr.done)
				}()
				// cooperative cancel hook can be observed by user code via procCancelled
				pr.result = child.Call0(execVal)
			}()
			return HandleVal("proc", pr)
		},
	)
	setBuiltinDoc(ip, "procSpawn", `Run a function concurrently in an isolated process (clone of the current interpreter).

The function's closure environment is deep-snapshotted into the child isolate.
Results are retrieved via procJoin. Cancellation is cooperative; see procCancel / procCancelled.

Params:
	f: Fun

Returns:
	Any   # proc handle (opaque)`)

	// procJoin(p) -> Any
	ip.RegisterNative(
		"procJoin",
		[]ParamSpec{{Name: "p", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			pr := asHandle(ctx.MustArg("p"), "proc").Data.(*procState)
			<-pr.done
			return pr.result
		},
	)
	setBuiltinDoc(ip, "procJoin", `Wait for a process to finish and return its result.

Params:
	p: Any   # proc handle

Returns:
	Any`)

	// procCancel(p) -> Bool | Null(err)
	ip.RegisterNative(
		"procCancel",
		[]ParamSpec{{Name: "p", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Bool"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			pr := asHandle(ctx.MustArg("p"), "proc").Data.(*procState)
			// Close once; if already closed, signal soft error.
			closed := false
			func() {
				defer func() {
					if r := recover(); r != nil {
						closed = true
					}
				}()
				close(pr.cancel)
			}()
			if closed {
				return annotNull("already cancelled")
			}
			return Bool(true)
		},
	)
	setBuiltinDoc(ip, "procCancel", `Request cooperative cancellation of a process (best effort).

See also: procCancelled.

Params:
	p: Any   # proc handle

Returns:
	Bool?   # true if cancellation was newly requested; Null(err) if already cancelled`)

	// procCancelled(p) -> Bool
	ip.RegisterNative(
		"procCancelled",
		[]ParamSpec{{Name: "p", Type: S{"id", "Any"}}},
		S{"id", "Bool"},
		func(_ *Interpreter, ctx CallCtx) Value {
			pr := asHandle(ctx.MustArg("p"), "proc").Data.(*procState)
			select {
			case <-pr.cancel:
				return Bool(true)
			default:
				return Bool(false)
			}
		},
	)
	setBuiltinDoc(ip, "procCancelled", `Check whether cancellation was requested for a process.

See also: procCancel.

Params:
	p: Any   # proc handle

Returns:
	Bool`)

	// procJoinAll(ps:[proc]) -> [Any]
	ip.RegisterNative(
		"procJoinAll",
		[]ParamSpec{{Name: "ps", Type: S{"array", S{"id", "Any"}}}},
		S{"array", S{"id", "Any"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			ps := ctx.MustArg("ps").Data.(*ArrayObject).Elems
			out := make([]Value, len(ps))
			for i, p := range ps {
				pr := asHandle(p, "proc").Data.(*procState)
				<-pr.done
				out[i] = pr.result
			}
			return Arr(out)
		},
	)
	setBuiltinDoc(ip, "procJoinAll", `Wait for all processes to finish and return their results in order.

Params:
	ps: [Any]   # list of proc handles

Returns:
	[Any]`)

	// procJoinAny(ps:[proc]) -> { index:Int, value:Any }?
	ip.RegisterNative(
		"procJoinAny",
		[]ParamSpec{{Name: "ps", Type: S{"array", S{"id", "Any"}}}},
		S{"unop", "?", S{"map"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			ps := ctx.MustArg("ps").Data.(*ArrayObject).Elems
			if len(ps) == 0 {
				return errNull("procJoinAny: empty list")
			}
			type res struct {
				i int
				v Value
			}
			ch := make(chan res, len(ps))
			for i, p := range ps {
				pr := asHandle(p, "proc").Data.(*procState)
				go func(i int, pr *procState) {
					<-pr.done
					ch <- res{i, pr.result}
				}(i, pr)
			}
			r := <-ch
			mo := &MapObject{
				Entries: map[string]Value{"index": Int(int64(r.i)), "value": r.v},
				KeyAnn:  map[string]string{},
				Keys:    []string{"index", "value"},
			}
			return Value{Tag: VTMap, Data: mo}
		},
	)
	setBuiltinDoc(ip, "procJoinAny", `Wait for any process to finish; return its index and value.

Params:
	ps: [Any]   # list of proc handles (non-empty)

Returns:
	{ index: Int, value: Any }?

Errors:
	annotated Null when ps is empty`)

	// ------------------ Channels ------------------

	ip.RegisterNative(
		"chanOpen",
		[]ParamSpec{{Name: "cap", Type: S{"unop", "?", S{"id", "Int"}}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			capacity := int64(0)
			if v, ok := ctx.Arg("cap"); ok && v.Tag == VTInt {
				capacity = v.Data.(int64)
				if capacity < 0 {
					fail("chanOpen: cap must be >= 0")
				}
			}
			return HandleVal("chan", &chanBox{ch: make(chan Value, int(capacity))})
		},
	)
	setBuiltinDoc(ip, "chanOpen", `Create a new channel for Values.

When cap > 0, the channel is buffered.

Params:
	cap: Int?   # capacity (default 0)

Returns:
	Any   # channel handle (opaque)`)

	// chanSend(c, x) -> Bool?  (true on success; Null("channel closed") on error)
	ip.RegisterNative(
		"chanSend",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}, {Name: "x", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Bool"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			cb := asHandle(ctx.MustArg("c"), "chan").Data.(*chanBox)
			x := ctx.MustArg("x")
			ok := true
			func() {
				defer func() {
					if r := recover(); r != nil {
						ok = false
					}
				}()
				cb.ch <- x // blocking send; panic if closed → caught above
			}()
			if !ok {
				return annotNull("channel closed")
			}
			return Bool(true)
		},
	)
	setBuiltinDoc(ip, "chanSend", `Send a value on a channel (blocking).

Params:
	c: Any   # channel handle
	x: Any

Returns:
	Bool?   # true on success; Null("channel closed") if the channel is closed`)

	// chanRecv(c) -> Any?  (Null("channel closed") if closed and empty)
	ip.RegisterNative(
		"chanRecv",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			cb := asHandle(ctx.MustArg("c"), "chan").Data.(*chanBox)
			v, ok := <-cb.ch
			if !ok {
				return annotNull("channel closed")
			}
			return v
		},
	)
	setBuiltinDoc(ip, "chanRecv", `Receive a value from a channel (blocking).

Params:
	c: Any   # channel handle

Returns:
	Any

Errors:
	annotated Null with message "channel closed" if the channel is closed and empty`)

	// chanTrySend(c, x) -> Bool
	ip.RegisterNative(
		"chanTrySend",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}, {Name: "x", Type: S{"id", "Any"}}},
		S{"id", "Bool"},
		func(_ *Interpreter, ctx CallCtx) Value {
			cb := asHandle(ctx.MustArg("c"), "chan").Data.(*chanBox)
			x := ctx.MustArg("x")
			sent := false
			func() {
				defer func() {
					if r := recover(); r != nil { // send on closed channel
						sent = false
					}
				}()
				select {
				case cb.ch <- x:
					sent = true // sent immediately
				default:
					sent = false // would block
				}
			}()
			return Bool(sent)
		},
	)
	setBuiltinDoc(ip, "chanTrySend", `Attempt a non-blocking send on a channel.

Params:
	c: Any   # channel handle
	x: Any

Returns:
	Bool   # true if sent, false if would block`)

	// chanTryRecv(c) -> { ok:Bool, value:Any }
	ip.RegisterNative(
		"chanTryRecv",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}},
		S{"map"},
		func(_ *Interpreter, ctx CallCtx) Value {
			cb := asHandle(ctx.MustArg("c"), "chan").Data.(*chanBox)
			out := &MapObject{
				Entries: map[string]Value{},
				KeyAnn:  map[string]string{},
				Keys:    []string{"ok", "value"},
			}
			select {
			case v, ok := <-cb.ch:
				if !ok {
					// Non-blocking completed; communicate closed via annotated Null.
					out.Entries["ok"] = Bool(true)
					out.Entries["value"] = annotNull("channel closed")
				} else {
					out.Entries["ok"] = Bool(true)
					out.Entries["value"] = v
				}
			default:
				out.Entries["ok"] = Bool(false)
				out.Entries["value"] = Null
			}
			return Value{Tag: VTMap, Data: out}
		},
	)
	setBuiltinDoc(ip, "chanTryRecv", `Attempt a non-blocking receive from a channel.

If a value is immediately available, returns {ok:true, value:V}.
If the channel is closed and empty, returns {ok:true, value: <annotated null "channel closed">}.
If no value is available and the channel is open, returns {ok:false, value:null}.

Params:
	c: Any   # channel handle

Returns:
	{ ok: Bool, value: Any }`)

	// chanClose(c) -> Bool?  (true on first close; Null("channel already closed") otherwise)
	ip.RegisterNative(
		"chanClose",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Bool"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			cb := asHandle(ctx.MustArg("c"), "chan").Data.(*chanBox)
			first := true
			func() {
				defer func() {
					if r := recover(); r != nil {
						first = false
					}
				}()
				close(cb.ch)
			}()
			if !first {
				return annotNull("channel already closed")
			}
			return Bool(true)
		},
	)
	setBuiltinDoc(ip, "chanClose", `Close a channel (idempotent).

On first close returns true; on subsequent closes returns Null("channel already closed").

Params:
	c: Any   # channel handle

Returns:
	Bool?`)

	// ------------------ Timers ------------------

	ip.RegisterNative(
		"timerAfter",
		[]ParamSpec{{Name: "ms", Type: S{"id", "Int"}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			ms := ctx.MustArg("ms").Data.(int64)
			if ms < 0 {
				fail("timerAfter: ms must be >= 0")
			}
			cb := &chanBox{ch: make(chan Value, 1)}
			go func() {
				<-time.After(time.Duration(ms) * time.Millisecond)
				// Send tick; ignore if consumer already closed.
				func() { defer func() { _ = recover() }(); cb.ch <- Int(time.Now().UnixMilli()) }()
				// Close; ignore if already closed by consumer.
				func() { defer func() { _ = recover() }(); close(cb.ch) }()
			}()
			return HandleVal("chan", cb)
		},
	)
	setBuiltinDoc(ip, "timerAfter", `Create a channel that emits one tick after a delay, then closes.

The tick payload is the Unix timestamp in milliseconds (Int).

Params:
	ms: Int   # delay in milliseconds (>= 0)

Returns:
	Any   # channel handle

Errors:
	annotated Null if ms < 0`)

	ip.RegisterNative(
		"ticker",
		[]ParamSpec{{Name: "ms", Type: S{"id", "Int"}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			ms := ctx.MustArg("ms").Data.(int64)
			if ms <= 0 {
				fail("ticker: ms must be > 0")
			}
			cb := &chanBox{ch: make(chan Value, 1)}
			go func() {
				tk := time.NewTicker(time.Duration(ms) * time.Millisecond)
				defer tk.Stop()
				for t := range tk.C {
					sent := true
					func() {
						defer func() {
							if r := recover(); r != nil {
								sent = false // channel likely closed by consumer
							}
						}()
						cb.ch <- Int(t.UnixMilli())
					}()
					if !sent {
						return
					}
				}
			}()
			return HandleVal("chan", cb)
		},
	)
	setBuiltinDoc(ip, "ticker", `Create a channel that emits periodic ticks until closed by the consumer.

The tick payload is the Unix timestamp in milliseconds (Int).
Call chanClose on the returned channel to stop the ticker goroutine.

Params:
	ms: Int   # period in milliseconds (> 0)

Returns:
	Any   # channel handle

Errors:
	annotated Null if ms <= 0`)
}
