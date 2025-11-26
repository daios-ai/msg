// === FILE: builtin_actor.go ===
package mindscript

import (
	"fmt"
	"runtime"
)

//
// Single-thread / actor (owner-affinity) builtins
//
// Overview:
//   - actorStart(m: Any, pinOSThread?: Bool) -> Handle.actor
//       Creates a dedicated interpreter isolate + actor goroutine processing
//       requests serially. If pinOSThread==true, the goroutine calls
//       runtime.LockOSThread() so all work runs on a single OS thread.
//       Returns an opaque Handle("actor").
//
//   - actorRun(a: Handle.actor, f: Fun|Any) -> Any
//       Deep-snapshots f into the actor's isolate and invokes f(m) there.
//
//   - actorCall(a: Handle.actor, f: Fun|Any, args: [Any]) -> Any
//       Deep-snapshots f and args into the actor's isolate and calls Apply(f,args).
//
//   - actorGet(a: Handle.actor, key: Str) -> Any
//   - actorSet(a: Handle.actor, key: Str, v: Any) -> Any
//       Property access/mutation of the wrapped value m when it's a map/module.
//
//   - actorClose(a: Handle.actor) -> Bool?
//       Closes the request queue; returns true on first close,
//       Null("already closed") otherwise.
//
// Semantics:
//   • Success never returns bare Null.
//   • Operational failures use annotated Null (errNull/annotNull).
//   • Panics in actor-executed code are caught and surfaced as annotated Null.
//   • The wrapped value 'm' lives only in the actor's isolate.
//   • All actor ops are non-reentrant and strictly ordered.
//

// actorReq is a single RPC into the actor goroutine.
type actorReq struct {
	run func() Value
	ret chan Value
}

// actor holds the dedicated isolate and the serialized request queue.
type actor struct {
	ip   *Interpreter // dedicated isolate
	m    Value        // wrapped value (lives in actor's isolate)
	reqs chan actorReq
	done chan struct{}
}

// startActor creates a new actor isolate and goroutine.
// If pin==true, the goroutine is pinned to a single OS thread.
func startActor(src *Interpreter, m Value, pin bool) *actor {
	child := src.Clone()

	// Snapshot m into the child's Core (same strategy as builtin_concurrency.go).
	cc := &cloneCtx{}
	mSnap := deepCloneValue(cc, m, child.Core)

	a := &actor{
		ip:   child,
		m:    mSnap,
		reqs: make(chan actorReq),
		done: make(chan struct{}),
	}

	go func() {
		if pin {
			runtime.LockOSThread()
			defer runtime.UnlockOSThread()
		}
		defer close(a.done)

		for req := range a.reqs {
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

	return a
}

// enqueueActor serializes a unit of work onto the actor; returns annotated Null
// if the actor has been closed.
func enqueueActor(a *actor, run func() Value) Value {
	ret := make(chan Value, 1)
	req := actorReq{run: run, ret: ret}

	// Sending on a closed channel panics; guard with recover.
	sent := true
	func() {
		defer func() {
			if r := recover(); r != nil {
				sent = false
			}
		}()
		a.reqs <- req
	}()
	if !sent {
		return annotNull("actor closed")
	}
	return <-ret
}

// registerActorBuiltins installs the actor/affinity builtins into 'target'.
// Call this during runtime seeding (similar to registerConcurrencyBuiltins).
func registerActorBuiltins(ip *Interpreter, target *Env) {
	// actorStart(m: Any, pinOSThread?: Bool) -> Handle.actor
	ip.RegisterRuntimeBuiltin(
		target,
		"actorStart",
		[]ParamSpec{
			{Name: "m", Type: S{"id", "Any"}},
			{Name: "pinOSThread", Type: S{"unop", "?", S{"id", "Bool"}}},
		},
		S{"get", S{"id", "Handle"}, S{"str", "actor"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			m := ctx.Arg("m")
			pin := false
			if v := ctx.Arg("pinOSThread"); v.Tag == VTBool {
				pin = v.Data.(bool)
			}
			a := startActor(ip, m, pin)
			return HandleVal("actor", a)
		},
	)
	setBuiltinDoc(target, "actorStart", `Create a single-thread actor wrapper for value 'm'.

All operations submitted to the actor run sequentially on one goroutine.
If pinOSThread is true, the goroutine is pinned to a single OS thread.

Params:
  m: Any
  pinOSThread: Bool?   # default false

Returns:
  Handle.actor   # opaque actor handle`)

	// actorRun(a: Handle.actor, f: Fun|Any) -> Any
	ip.RegisterRuntimeBuiltin(
		target,
		"actorRun",
		[]ParamSpec{
			{Name: "a", Type: S{"get", S{"id", "Handle"}, S{"str", "actor"}}},
			{Name: "f", Type: S{"id", "Any"}},
		},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			a := asHandle(ctx.Arg("a"), "actor").Data.(*actor)
			fv := ctx.Arg("f")
			if fv.Tag != VTFun {
				return errNull("actorRun: f must be a function")
			}

			// Deep-snapshot f into actor's isolate.
			cc := &cloneCtx{}
			fSnap := deepCloneValue(cc, fv, a.ip.Core)

			return enqueueActor(a, func() Value {
				// Call fSnap(m) inside the actor.
				return a.ip.Apply(fSnap, []Value{a.m})
			})
		},
	)
	setBuiltinDoc(target, "actorRun", `Run function f(m) inside the actor's single thread.

Params:
  a: Handle.actor
  f: Fun

Returns:
  Any`)

	// actorCall(a: Handle.actor, f: Fun|Any, args: [Any]) -> Any
	ip.RegisterRuntimeBuiltin(
		target,
		"actorCall",
		[]ParamSpec{
			{Name: "a", Type: S{"get", S{"id", "Handle"}, S{"str", "actor"}}},
			{Name: "f", Type: S{"id", "Any"}},
			{Name: "args", Type: S{"array", S{"id", "Any"}}},
		},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			a := asHandle(ctx.Arg("a"), "actor").Data.(*actor)
			fv := ctx.Arg("f")
			if fv.Tag != VTFun {
				return errNull("actorCall: f must be a function")
			}
			args := ctx.Arg("args").Data.(*ArrayObject).Elems

			cc := &cloneCtx{}
			fSnap := deepCloneValue(cc, fv, a.ip.Core)
			aSnap := make([]Value, len(args))
			for i := range args {
				aSnap[i] = deepCloneValue(cc, args[i], a.ip.Core)
			}

			return enqueueActor(a, func() Value {
				return a.ip.Apply(fSnap, aSnap)
			})
		},
	)
	setBuiltinDoc(target, "actorCall", `Call f(...args) inside the actor's single thread.

Params:
  a: Handle.actor
  f: Fun
  args: [Any]

Returns:
  Any`)

	// actorGet(a: Handle.actor, key: Str) -> Any
	ip.RegisterRuntimeBuiltin(
		target,
		"actorGet",
		[]ParamSpec{
			{Name: "a", Type: S{"get", S{"id", "Handle"}, S{"str", "actor"}}},
			{Name: "key", Type: S{"id", "Str"}},
		},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			a := asHandle(ctx.Arg("a"), "actor").Data.(*actor)
			key := ctx.Arg("key").Data.(string)

			return enqueueActor(a, func() Value {
				mv := AsMapValue(a.m)
				if mv.Tag != VTMap {
					return errNull("actorGet: wrapped value is not a map/module")
				}
				mo := mv.Data.(*MapObject)
				if v, ok := mo.Entries[key]; ok {
					return v
				}
				return annotNull(fmt.Sprintf("missing key '%s'", key))
			})
		},
	)
	setBuiltinDoc(target, "actorGet", `Get a property from the wrapped value (map/module) inside the actor.

Params:
  a: Handle.actor
  key: Str

Returns:
  Any   # annotated Null if missing key`)

	// actorSet(a: Handle.actor, key: Str, v: Any) -> Any
	ip.RegisterRuntimeBuiltin(
		target,
		"actorSet",
		[]ParamSpec{
			{Name: "a", Type: S{"get", S{"id", "Handle"}, S{"str", "actor"}}},
			{Name: "key", Type: S{"id", "Str"}},
			{Name: "v", Type: S{"id", "Any"}},
		},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			a := asHandle(ctx.Arg("a"), "actor").Data.(*actor)
			key := ctx.Arg("key").Data.(string)
			v := ctx.Arg("v")

			// Snapshot v to actor's isolate.
			cc := &cloneCtx{}
			vSnap := deepCloneValue(cc, v, a.ip.Core)

			return enqueueActor(a, func() Value {
				mv := AsMapValue(a.m)
				if mv.Tag != VTMap {
					return errNull("actorSet: wrapped value is not a map/module")
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
	setBuiltinDoc(target, "actorSet", `Set a property on the wrapped value (map/module) inside the actor.

Params:
  a: Handle.actor
  key: Str
  v: Any

Returns:
  Any`)

	// actorClose(a: Handle.actor) -> Bool?
	ip.RegisterRuntimeBuiltin(
		target,
		"actorClose",
		[]ParamSpec{{Name: "a", Type: S{"get", S{"id", "Handle"}, S{"str", "actor"}}}},
		S{"unop", "?", S{"id", "Bool"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			a := asHandle(ctx.Arg("a"), "actor").Data.(*actor)
			first := true
			func() {
				defer func() {
					if r := recover(); r != nil { // close of closed channel
						first = false
					}
				}()
				close(a.reqs)
			}()
			if !first {
				return annotNull("already closed")
			}
			<-a.done
			return Bool(true)
		},
	)
	setBuiltinDoc(target, "actorClose", `Close the actor's request queue (idempotent).

On first close returns true; on subsequent closes returns Null("already closed").

Params:
  a: Handle.actor

Returns:
  Bool?`)
}
