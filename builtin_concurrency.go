// === FILE: std_sys.go ===
package mindscript

import (
	"fmt"
	"time"
)

// --- Concurrency primitives -------------------------------------------------

type procState struct {
	done   chan struct{}
	result Value
	cancel chan struct{} // cooperative
}

// Channel box (shared by all channel builtins)
type chanBox struct {
	ch chan Value
}

// --- Deep copy & snapshot for isolated worlds --------------------------------

func cloneValue(v Value) Value {
	switch v.Tag {
	case VTNull, VTBool, VTInt, VTNum, VTStr, VTType, VTFun:
		return v
	case VTArray:
		xs := v.Data.([]Value)
		cp := make([]Value, len(xs))
		for i := range xs {
			cp[i] = cloneValue(xs[i])
		}
		return Arr(cp)
	case VTMap:
		mo := v.Data.(*MapObject)
		// Deep-copy entries
		entries := make(map[string]Value, len(mo.Entries))
		for k, vv := range mo.Entries {
			entries[k] = cloneValue(vv)
		}
		// Preserve insertion order and per-key annotations
		keys := make([]string, len(mo.Keys))
		copy(keys, mo.Keys)
		keyAnn := make(map[string]string, len(mo.KeyAnn))
		for k, ann := range mo.KeyAnn {
			keyAnn[k] = ann
		}
		return Value{
			Tag: VTMap,
			Data: &MapObject{
				Entries: entries,
				KeyAnn:  keyAnn,
				Keys:    keys,
			},
		}
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

// safeSend attempts to send v to ch; it returns false if ch is closed.
func safeSend(ch chan Value, v Value) (ok bool) {
	defer func() {
		if r := recover(); r != nil {
			ok = false
		}
	}()
	ch <- v
	return true
}

// safeClose closes ch, ignoring double-close panics.
func safeClose(ch chan Value) {
	defer func() { _ = recover() }()
	close(ch)
}

func registerConcurrencyBuiltins(ip *Interpreter) {
	// spawn(f: Any->Any) -> Any (proc handle)
	ip.RegisterNative(
		"procSpawn",
		[]ParamSpec{{Name: "f", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			fv := ctx.MustArg("f")
			// Contractual: must be a function (hard error)
			if fv.Tag != VTFun {
				fail("procSpawn expects a function")
			}
			orig := fv.Data.(*Fun)

			// Snapshot the closure env to isolate the spawned task.
			snap := snapshotEnv(orig.Env)

			// Clone the function to run in the snapshot.
			work := &Fun{
				Params:     append([]string{}, orig.Params...),
				ParamTypes: append([]S{}, orig.ParamTypes...),
				ReturnType: orig.ReturnType,
				Body:       orig.Body,
				Env:        snap,
				HiddenNull: orig.HiddenNull,
				Chunk:      orig.Chunk,      // ok to reuse compiled chunk
				NativeName: orig.NativeName, // in case someone passes a native
				IsOracle:   orig.IsOracle,
				Examples:   append([]Value(nil), orig.Examples...),
			}
			// Wrap as a Value and preserve the original annotation (#-doc).
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
							// Runtime failures inside the spawned proc become soft errors.
							pr.result = errNull(sig.msg)
						default:
							// Unknown panic → soft error result
							pr.result = errNull(fmt.Sprintf("runtime panic: %v", r))
						}
					}
					close(pr.done)
				}()

				// Use the public API (no internal executor calls).
				pr.result = ip.Call0(execVal) // arity/type mismatches surface as hard errors
			}()

			return HandleVal("proc", pr)
		},
	)
	setBuiltinDoc(ip, "procSpawn", `Run a function in a new lightweight process.

The function runs concurrently in an isolated snapshot of its closure
environment (variables are deep-copied where applicable). Pass a fully-applied
function (no missing parameters). If the function returns an annotated null,
joining the process yields that error annotation.

Params:
  f: Any — a function to execute (must be zero-arity after partial application)

Returns:
  Opaque process handle (use with procJoin/procCancel)

Examples:
  # Simple worker
  let worker = fun() do
    40 + 2
  end
  let p = procSpawn(worker)
  procJoin(p)           ## => 42

  ## Partial application first, then spawn
  let add = fun(a: Int, b: Int) -> Int do a + b end
  let add1 = add(1)     # now zero-arity
  let p2 = procSpawn(add1)
  procJoin(p2)          ## => 1 + b (b must be bound inside add1's closure)

  ## Propagate failure as annotated null via join
  let boom = fun() do error("boom") end
  let p3 = procSpawn(boom)
  procJoin(p3)          ## => null annotated with "boom"

Notes:
  • Use procJoin(proc) to retrieve the result (or annotated error).
  • Use procCancel(proc) to request cooperative cancellation (best effort).
  • The spawned function sees a snapshot of its original closure env.`)

	ip.RegisterNative(
		"procJoin",
		[]ParamSpec{{Name: "p", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("p")
			pr := asHandle(pv, "proc").Data.(*procState) // wrong kind → hard error via asHandle
			<-pr.done
			return pr.result
		},
	)
	setBuiltinDoc(ip, "procJoin", `Wait for a process to finish and return its result.

Params:
  p: proc — a handle returned by procSpawn

Returns:
  Any — the function's result, or an annotated null if the process failed.`)

	ip.RegisterNative(
		"procCancel",
		[]ParamSpec{{Name: "p", Type: S{"id", "Any"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("p")
			pr := asHandle(pv, "proc").Data.(*procState) // wrong kind → hard error via asHandle
			select {
			case <-pr.cancel:
			default:
				close(pr.cancel)
			}
			return Null
		},
	)
	setBuiltinDoc(ip, "procCancel", `Request cooperative cancellation of a process.

Cancellation is best-effort: user code/libraries may choose to observe the
request and stop early.

Params:
  p: proc — a handle returned by procSpawn

Returns:
  Null`)

	// Channels (untyped)

	// chanOpen(cap: Int?) -> Any (chan handle)
	ip.RegisterNative(
		"chanOpen",
		[]ParamSpec{{Name: "cap", Type: S{"unop", "?", S{"id", "Int"}}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
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
	setBuiltinDoc(ip, "chanOpen", `Create a new channel.

When cap is 0 or omitted, returns an unbuffered channel.
When cap > 0, returns a buffered channel with the given capacity.

Params:
  cap: Int? — buffer size (default 0, unbuffered)

Returns:
  chan handle (opaque)

See also:
  chanSend, chanRecv, chanTrySend, chanTryRecv, chanClose`)

	ip.RegisterNative(
		"chanSend",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}, {Name: "x", Type: S{"id", "Any"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			cb := asHandle(ctx.MustArg("c"), "chan").Data.(*chanBox) // wrong kind → hard error
			x := ctx.MustArg("x")
			// Sending to a closed channel is a misuse → hard error (propagates as panic).
			cb.ch <- x
			return Null
		},
	)
	setBuiltinDoc(ip, "chanSend", `Send a value on a channel.

Blocks until a receiver is ready (or until buffer has free space for buffered channels).

Params:
  c: chan — channel handle
  x: Any  — value to send

Returns:
  Null`)

	ip.RegisterNative(
		"chanRecv",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			cb := asHandle(ctx.MustArg("c"), "chan").Data.(*chanBox) // wrong kind → hard error
			v, ok := <-cb.ch
			if !ok {
				// Soft error: channel has been closed.
				return annotNull("channel closed")
			}
			return v
		},
	)
	setBuiltinDoc(ip, "chanRecv", `Receive a value from a channel.

Blocks until a sender is ready (or until buffer holds an item). After chanClose(c),
further receives return an annotated null with the message "channel closed".

Params:
  c: chan — channel handle

Returns:
  Any — the received value, or annotated null after close`)

	// chanTrySend(c,x) -> Bool
	ip.RegisterNative(
		"chanTrySend",
		[]ParamSpec{
			{Name: "c", Type: S{"id", "Any"}},
			{Name: "x", Type: S{"id", "Any"}},
		},
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
	setBuiltinDoc(ip, "chanTrySend", `Attempt a non-blocking send on a channel.

Params:
  c: chan — channel handle
  x: Any  — value to send

Returns:
  Bool — true if the value was sent; false if it would block (buffer full / no receiver).
Notes:
  • Sending on a closed channel is a hard error (misuse).`)

	// chanTryRecv(c) -> { ok: Bool, value: Any }
	ip.RegisterNative(
		"chanTryRecv",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}},
		S{"map"}, // { ok: Bool, value: Any }
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
					// Closed: ok=true (a state change), value carries the reason as annotated null.
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

Params:
  c: chan — channel handle

Returns:
  { ok: Bool, value: Any }
  • ok=true  → value is a received item, or an annotated null ("channel closed") if the channel is closed.
  • ok=false → no item available (would block); value is null.
Notes:
  • Use chanRecv for blocking receives.`)

	ip.RegisterNative(
		"chanClose",
		[]ParamSpec{{Name: "c", Type: S{"id", "Any"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			cb := asHandle(ctx.MustArg("c"), "chan").Data.(*chanBox) // wrong kind → hard error
			// Double-close is misuse in Go and will panic → hard error.
			close(cb.ch)
			return Null
		},
	)
	setBuiltinDoc(ip, "chanClose", `Close a channel.

After closing:
  • chanRecv returns an annotated null ("channel closed")
  • Sending on a closed channel is an error

Params:
  c: chan — channel handle

Returns:
  Null`)

	// --- Timers --------------------------------------------------------------

	// timerAfter(ms: Int) -> Any (chan handle)
	// Sends the current time (millis since epoch) once, then closes the channel.
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
	setBuiltinDoc(ip, "timerAfter", `Pause for a duration and emit one tick.

After ms milliseconds, a single Int (current time in millis) is sent on the returned
channel, then the channel is closed.

Params:
  ms: Int — milliseconds to wait

Returns:
  chan — handle (opaque)`)

	// ticker(ms: Int) -> Any (chan handle)
	// Emits current time millis every period until the channel is closed by the caller.
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
						// Channel was closed by user: stop ticking.
						return
					}
				}
			}()
			return HandleVal("chan", cb)
		},
	)
	setBuiltinDoc(ip, "ticker", `Emit periodic ticks.

Returns a channel that receives the current time in milliseconds every ms.
Calling chanClose(c) on the returned channel stops the ticker.

Params:
  ms: Int — period in milliseconds (must be > 0)

Returns:
  chan — handle (opaque)

Notes:
  • On close, one in-flight tick may be dropped.`)
}
