// === FILE: builtin_concurrency.go ===
package mindscript

import (
	"fmt"
	"time"
)

// --- Concurrency primitives -------------------------------------------------

type procState struct {
	done   chan struct{}
	result Value
	cancel chan struct{} // cooperative (best effort)
}

type chanBox struct {
	ch chan Value
}

func safeSend(ch chan Value, v Value) (ok bool) {
	defer func() { _ = recover() }()
	ch <- v
	return true
}

func safeClose(ch chan Value) {
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
	case VTNull, VTBool, VTInt, VTNum, VTStr, VTType:
		return v

	case VTArray:
		src := v.Data.([]Value)
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
			dst.KeyAnn[k] = src.KeyAnn[k]
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
			newEnv = targetCore // natives rebind to child's Core
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
			Examples:   append([]Value(nil), f.Examples...),
			Src:        f.Src,
		}
		ctx.seenFun[f] = nf
		nv := FunVal(nf)
		nv.Annot = v.Annot
		return nv

	case VTHandle:
		// IMPORTANT: allow sharing host handles (e.g., channels, proc handles)
		// across isolates so processes can communicate. They’re Go-level
		// concurrency objects and safe to share.
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
				Env:        snap,
				HiddenNull: orig.HiddenNull,
				Chunk:      orig.Chunk,
				NativeName: orig.NativeName,
				IsOracle:   orig.IsOracle,
				Examples:   append([]Value(nil), orig.Examples...),
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
				// cooperative cancel hook could be polled by user code via a builtin
				pr.result = child.Call0(execVal)
			}()
			return HandleVal("proc", pr)
		},
	)
	setBuiltinDoc(ip, "procSpawn", `Run a function concurrently in an isolated process. Use procJoin/procCancel/procJoinAll/procJoinAny.`)

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
	setBuiltinDoc(ip, "procJoin", `Wait for a process to finish and return its result.`)

	// procCancel(p) -> Null (best-effort)
	ip.RegisterNative(
		"procCancel",
		[]ParamSpec{{Name: "p", Type: S{"id", "Any"}}},
		S{"id", "Null"},
		func(_ *Interpreter, ctx CallCtx) Value {
			pr := asHandle(ctx.MustArg("p"), "proc").Data.(*procState)
			select {
			case <-pr.cancel:
			default:
				close(pr.cancel)
			}
			return Null
		},
	)
	setBuiltinDoc(ip, "procCancel", `Request cooperative cancellation of a process (best effort).`)

	// procJoinAll(ps:[proc]) -> [Any]
	ip.RegisterNative(
		"procJoinAll",
		[]ParamSpec{{Name: "ps", Type: S{"array", S{"id", "Any"}}}},
		S{"array", S{"id", "Any"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			ps := ctx.MustArg("ps").Data.([]Value)
			out := make([]Value, len(ps))
			for i, p := range ps {
				pr := asHandle(p, "proc").Data.(*procState)
				<-pr.done
				out[i] = pr.result
			}
			return Arr(out)
		},
	)
	setBuiltinDoc(ip, "procJoinAll", `Wait for all processes to finish and return their results in order.`)

	// procJoinAny(ps:[proc]) -> { index:Int, value:Any }
	ip.RegisterNative(
		"procJoinAny",
		[]ParamSpec{{Name: "ps", Type: S{"array", S{"id", "Any"}}}},
		S{"map"},
		func(_ *Interpreter, ctx CallCtx) Value {
			ps := ctx.MustArg("ps").Data.([]Value)
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
	setBuiltinDoc(ip, "procJoinAny", `Wait for any process to finish; return its index and value.`)

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
	setBuiltinDoc(ip, "chanOpen", `Create a new channel (buffered when cap>0).`)

	ip.RegisterNative(
		"chanSend",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}, {Name: "x", Type: S{"id", "Any"}}},
		S{"id", "Null"},
		func(_ *Interpreter, ctx CallCtx) Value {
			cb := asHandle(ctx.MustArg("c"), "chan").Data.(*chanBox)
			cb.ch <- ctx.MustArg("x")
			return Null
		},
	)
	setBuiltinDoc(ip, "chanSend", `Send a value on a channel (blocking).`)

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
	setBuiltinDoc(ip, "chanRecv", `Receive a value from a channel (blocking).`)

	ip.RegisterNative(
		"chanTrySend",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}, {Name: "x", Type: S{"id", "Any"}}},
		S{"id", "Bool"},
		func(_ *Interpreter, ctx CallCtx) Value {
			cb := asHandle(ctx.MustArg("c"), "chan").Data.(*chanBox)
			x := ctx.MustArg("x")
			select {
			case cb.ch <- x:
				return Bool(true)
			default:
				return Bool(false)
			}
		},
	)
	setBuiltinDoc(ip, "chanTrySend", `Attempt a non-blocking send on a channel.`)

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
	setBuiltinDoc(ip, "chanTryRecv", `Attempt a non-blocking receive from a channel.`)

	ip.RegisterNative(
		"chanClose",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}},
		S{"id", "Null"},
		func(_ *Interpreter, ctx CallCtx) Value {
			cb := asHandle(ctx.MustArg("c"), "chan").Data.(*chanBox)
			safeClose(cb.ch)
			return Null
		},
	)
	setBuiltinDoc(ip, "chanClose", `Close a channel (idempotent).`)

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
				if safeSend(cb.ch, Int(time.Now().UnixMilli())) {
					safeClose(cb.ch)
				}
			}()
			return HandleVal("chan", cb)
		},
	)
	setBuiltinDoc(ip, "timerAfter", `Emit one tick after a delay, then close.`)

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
					if !safeSend(cb.ch, Int(t.UnixMilli())) {
						return
					}
				}
			}()
			return HandleVal("chan", cb)
		},
	)
	setBuiltinDoc(ip, "ticker", `Emit periodic ticks on a channel until closed.`)
}
