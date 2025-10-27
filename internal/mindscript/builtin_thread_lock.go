// === FILE: builtin_thread_lock.go ===
package mindscript

import (
	"fmt"
	"runtime"
)

//
// Single-thread / owner affinity builtins
//
// Overview:
//   - ownerWrap(m: Any, pinOSThread?: Bool) -> Any
//       Creates a dedicated interpreter isolate + owner goroutine processing
//       requests serially. If pinOSThread==true, the goroutine calls
//       runtime.LockOSThread() so all work runs on a single OS thread.
//       Returns an opaque Handle("owner").
//
//   - ownerRun(o: Any, f: Fun) -> Any
//       Deep-snapshots f into the owner's isolate and invokes f(m) there.
//
//   - ownerCall(o: Any, f: Fun, args: [Any]) -> Any
//       Deep-snapshots f and args into the owner's isolate and calls Apply(f,args).
//
//   - ownerGet(o: Any, key: Str) -> Any
//   - ownerSet(o: Any, key: Str, v: Any) -> Any
//       Property access/mutation of the wrapped value m when it's a map/module.
//
//   - ownerClose(o: Any) -> Bool?
//       Closes the request queue; returns true on first close,
//       Null("already closed") otherwise.
//
// Semantics:
//   • Success never returns bare Null.
//   • Operational failures use annotated Null (errNull/annotNull).
//   • Panics in owner-executed code are caught and surfaced as annotated Null.
//   • The wrapped value 'm' lives only in the owner isolate.
//   • All owner ops are non-reentrant and strictly ordered.
//

// ownerReq is a single RPC into the owner goroutine.
type ownerReq struct {
	run func() Value
	ret chan Value
}

// owner holds the dedicated isolate and the serialized request queue.
type owner struct {
	ip   *Interpreter // dedicated isolate
	m    Value        // wrapped value (lives in owner's isolate)
	reqs chan ownerReq
	done chan struct{}
}

// startOwner creates a new owner isolate and goroutine.
// If pin==true, the goroutine is pinned to a single OS thread.
func startOwner(src *Interpreter, m Value, pin bool) *owner {
	child := src.Clone()

	// Snapshot m into the child's Core (same strategy as builtin_concurrency.go).
	cc := &cloneCtx{}
	mSnap := deepCloneValue(cc, m, child.Core)

	o := &owner{
		ip:   child,
		m:    mSnap,
		reqs: make(chan ownerReq),
		done: make(chan struct{}),
	}

	go func() {
		if pin {
			runtime.LockOSThread()
			defer runtime.UnlockOSThread()
		}
		defer close(o.done)

		for req := range o.reqs {
			var out Value
			func() {
				defer func() {
					if r := recover(); r != nil {
						switch e := r.(type) {
						case rtErr:
							out = errNull(e.msg)
						case error:
							out = errNull(e.Error())
						default:
							out = errNull(fmt.Sprintf("panic: %v", r))
						}
					}
				}()
				out = req.run()
			}()
			// Deliver response; if receiver went away, don't panic.
			func() { defer func() { _ = recover() }(); req.ret <- out }()
		}
	}()

	return o
}

// enqueueOwner serializes a unit of work onto the owner; returns annotated Null
// if the owner has been closed.
func enqueueOwner(o *owner, run func() Value) Value {
	ret := make(chan Value, 1)
	req := ownerReq{run: run, ret: ret}

	// Sending on a closed channel panics; guard with recover.
	sent := true
	func() {
		defer func() {
			if r := recover(); r != nil {
				sent = false
			}
		}()
		o.reqs <- req
	}()
	if !sent {
		return annotNull("owner closed")
	}
	return <-ret
}

// registerSingleThreadBuiltins installs the owner/affinity builtins into 'target'.
// Call this during runtime seeding (similar to registerConcurrencyBuiltins).
func registerThreadLockBuiltins(ip *Interpreter, target *Env) {
	// ownerWrap(m: Any, pinOSThread?: Bool) -> Any
	ip.RegisterRuntimeBuiltin(
		target,
		"ownerWrap",
		[]ParamSpec{
			{Name: "m", Type: S{"id", "Any"}},
			{Name: "pinOSThread", Type: S{"unop", "?", S{"id", "Bool"}}},
		},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			m := ctx.Arg("m")
			pin := false
			if v := ctx.Arg("pinOSThread"); v.Tag == VTBool {
				pin = v.Data.(bool)
			}
			o := startOwner(ip, m, pin)
			return HandleVal("owner", o)
		},
	)
	setBuiltinDoc(target, "ownerWrap", `Create a single-thread owner wrapper for value 'm'.

All operations submitted to the owner run sequentially on one goroutine.
If pinOSThread is true, the goroutine is pinned to a single OS thread.

Params:
  m: Any
  pinOSThread: Bool?   # default false

Returns:
  Any   # owner handle (opaque)`)

	// ownerRun(o: Any, f: Fun) -> Any
	ip.RegisterRuntimeBuiltin(
		target,
		"ownerRun",
		[]ParamSpec{
			{Name: "o", Type: S{"id", "Any"}},
			{Name: "f", Type: S{"id", "Any"}},
		},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			o := asHandle(ctx.Arg("o"), "owner").Data.(*owner)
			fv := ctx.Arg("f")
			if fv.Tag != VTFun {
				return errNull("ownerRun: f must be a function")
			}

			// Deep-snapshot f into owner's isolate.
			cc := &cloneCtx{}
			fSnap := deepCloneValue(cc, fv, o.ip.Core)

			return enqueueOwner(o, func() Value {
				// Call fSnap(m) inside the owner.
				return o.ip.Apply(fSnap, []Value{o.m})
			})
		},
	)
	setBuiltinDoc(target, "ownerRun", `Run function f(m) inside the owner's single thread.

Params:
  o: Any   # owner handle
  f: Fun

Returns:
  Any`)

	// ownerCall(o: Any, f: Fun, args: [Any]) -> Any
	ip.RegisterRuntimeBuiltin(
		target,
		"ownerCall",
		[]ParamSpec{
			{Name: "o", Type: S{"id", "Any"}},
			{Name: "f", Type: S{"id", "Any"}},
			{Name: "args", Type: S{"array", S{"id", "Any"}}},
		},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			o := asHandle(ctx.Arg("o"), "owner").Data.(*owner)
			fv := ctx.Arg("f")
			if fv.Tag != VTFun {
				return errNull("ownerCall: f must be a function")
			}
			args := ctx.Arg("args").Data.(*ArrayObject).Elems

			cc := &cloneCtx{}
			fSnap := deepCloneValue(cc, fv, o.ip.Core)
			aSnap := make([]Value, len(args))
			for i := range args {
				aSnap[i] = deepCloneValue(cc, args[i], o.ip.Core)
			}

			return enqueueOwner(o, func() Value {
				return o.ip.Apply(fSnap, aSnap)
			})
		},
	)
	setBuiltinDoc(target, "ownerCall", `Call f(...args) inside the owner's single thread.

Params:
  o: Any     # owner handle
  f: Fun
  args: [Any]

Returns:
  Any`)

	// ownerGet(o: Any, key: Str) -> Any
	ip.RegisterRuntimeBuiltin(
		target,
		"ownerGet",
		[]ParamSpec{
			{Name: "o", Type: S{"id", "Any"}},
			{Name: "key", Type: S{"id", "Str"}},
		},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			o := asHandle(ctx.Arg("o"), "owner").Data.(*owner)
			key := ctx.Arg("key").Data.(string)

			return enqueueOwner(o, func() Value {
				mv := AsMapValue(o.m)
				if mv.Tag != VTMap {
					return errNull("ownerGet: wrapped value is not a map/module")
				}
				mo := mv.Data.(*MapObject)
				if v, ok := mo.Entries[key]; ok {
					return v
				}
				return annotNull(fmt.Sprintf("missing key '%s'", key))
			})
		},
	)
	setBuiltinDoc(target, "ownerGet", `Get a property from the wrapped value (map/module) inside the owner.

Params:
  o: Any    # owner handle
  key: Str

Returns:
  Any   # annotated Null if missing key`)

	// ownerSet(o: Any, key: Str, v: Any) -> Any
	ip.RegisterRuntimeBuiltin(
		target,
		"ownerSet",
		[]ParamSpec{
			{Name: "o", Type: S{"id", "Any"}},
			{Name: "key", Type: S{"id", "Str"}},
			{Name: "v", Type: S{"id", "Any"}},
		},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			o := asHandle(ctx.Arg("o"), "owner").Data.(*owner)
			key := ctx.Arg("key").Data.(string)
			v := ctx.Arg("v")

			// Snapshot v to owner's isolate.
			cc := &cloneCtx{}
			vSnap := deepCloneValue(cc, v, o.ip.Core)

			return enqueueOwner(o, func() Value {
				mv := AsMapValue(o.m)
				if mv.Tag != VTMap {
					return errNull("ownerSet: wrapped value is not a map/module")
				}
				mo := mv.Data.(*MapObject)
				if _, ok := mo.Entries[key]; !ok {
					mo.Keys = append(mo.Keys, key)
				}
				mo.Entries[key] = vSnap
				return vSnap
			})
		},
	)
	setBuiltinDoc(target, "ownerSet", `Set a property on the wrapped value (map/module) inside the owner.

Params:
  o: Any
  key: Str
  v: Any

Returns:
  Any`)

	// ownerClose(o: Any) -> Bool?
	ip.RegisterRuntimeBuiltin(
		target,
		"ownerClose",
		[]ParamSpec{{Name: "o", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Bool"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			o := asHandle(ctx.Arg("o"), "owner").Data.(*owner)
			first := true
			func() {
				defer func() {
					if r := recover(); r != nil { // close of closed channel
						first = false
					}
				}()
				close(o.reqs)
			}()
			if !first {
				return annotNull("already closed")
			}
			<-o.done
			return Bool(true)
		},
	)
	setBuiltinDoc(target, "ownerClose", `Close the owner's request queue (idempotent).

On first close returns true; on subsequent closes returns Null("already closed").

Params:
  o: Any

Returns:
  Bool?`)
}
