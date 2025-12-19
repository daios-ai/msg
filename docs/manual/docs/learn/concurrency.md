# Concurrency

Concurrency in MindScript is designed for scripts that need to overlap work, limit latency, and keep mutable state disciplined. The runtime offers three building blocks that compose cleanly: processes (`procSpawn` / `procJoin`), channels (`chanOpen` / `chanSend` / `chanRecv`), and actors (`actorStart` / `actorCall`). Each tool exists to solve a specific problem.

The most important design decision is that MindScript does not share ordinary data structures across concurrent execution. Arrays and objects are mutable in a single thread, but when you spawn concurrent work the runtime runs it in an isolated interpreter. The closure you provide is snapshotted into that isolate, and the work produces a result that is snapshotted back to the parent. This avoids data races by construction. If you need coordination, you do it explicitly with handles: process handles, channel handles, actor handles, file handles, and network handles.

This chapter explains how the model works, why it is shaped this way, and how to write correct concurrent programs that still feel like scripts.

---

## The concurrency contract: isolates, snapshots, and handles

A spawned computation runs in a fresh interpreter isolate. You can think of it as “the same program, but with its own heap.” The function you spawn is deep-snapshotted into that isolate along with the values it closes over. In practice this means that arrays and objects you captured become independent copies in the child. Mutating them in the child does not affect the parent, and mutating them in the parent does not affect the child.

This has two consequences that matter when you write real programs. First, it eliminates the most common concurrency failure mode in scripting languages: accidental shared mutation. Second, it forces you to be intentional about communication. If you want to exchange values between concurrent computations, you send them explicitly through channels, or you return them as process results, or you store them inside an actor and interact with that actor through actor operations.

Handles are the one exception: they are opaque references managed by the runtime. Some handle kinds are specifically meant to be shared across isolates, such as channels and process handles. When you pass a handle into a spawned function, you are not copying an in-memory data structure; you are passing a reference to an external resource. That is precisely the point: handles are how concurrent parts of the program coordinate.

---

## Processes: `procSpawn` and `procJoin`

A process in MindScript is a concurrent evaluation of a function in an isolate. You create a process with `procSpawn(f)` and you wait for it with `procJoin(p)`.

The function you spawn should be callable with no arguments. In MindScript that can be written either as `fun() do ... end` or as `fun(_: Null) do ... end`, since a single-parameter function whose parameter type is `Null` can also be called with no arguments.

```mindscript
let p = procSpawn(fun() do
    40 + 2
end)

procJoin(p)  # 42
```

### What happens on failure

A spawned process can fail in two ways. It can return a normal “soft failure” value such as `null  # <reason>`, which is just data. Or it can panic. Panics inside a spawned process are caught and converted into an annotated `null` result. This is intentional: it means a process always “joins” to a value, and you decide how to handle it.

```mindscript
let p = procSpawn(fun() do
    panic("boom")
end)

let v = procJoin(p)
if v == null then
    println("process failed: " + (noteGet(v) or "<no details>"))
end
```

This pattern keeps concurrent code composable. The parent thread can join, inspect the annotation, and decide whether to retry, skip, or stop.

### Joining many: `procJoinAll` and `procJoinAny`

When you have multiple independent tasks, joining them one at a time is verbose and often less efficient. The runtime provides two helpers.

`procJoinAll(ps)` waits for all processes in the given array and returns an array of their results in the same order.

`procJoinAny(ps)` waits until any process completes and returns `{ index: Int, value: Any }`. If the list is empty, it returns `null` with an annotation.

A simple parallel map illustrates `procJoinAll`. The example below performs a CPU-bound transform of many inputs at once and then collects results.

```mindscript
let work = fun(x: Int) -> Int do
    let n = x
    let i = 0
    while i < 500_000 do
        n = (n * 1664525 + 1013904223) % 2147483647
        i = i + 1
    end
    n
end

let xs = [1, 2, 3, 4, 5, 6, 7, 8]
let ps = []

for x in xs do
    push(ps, procSpawn(fun() do work(x) end))
end

let ys = procJoinAll(ps)
ys
```

Notice what you do not need to think about. There is no locking around `xs`, and there is no danger that one task mutates another task’s intermediate state, because each spawned function runs in its own isolate with its own copied values.

---

## Cancellation: cooperative stop signals

Cancellation in MindScript is cooperative. The parent can request cancellation with `procCancel(p)`, and code running in the process can observe that request with `procCancelled(p)` if it has access to its own handle.

In practice, cancellation is most useful when you combine processes with timeouts or with “first result wins” patterns. Cancellation is also idempotent in the sense that the first cancel attempt returns `true`, and subsequent attempts return `null` with an annotation indicating that it was already cancelled.

A common pattern is a slow operation with a deadline. You start the operation and a timer, wait for whichever finishes first, and cancel the other.

---

## Timers and timeouts: `timerAfter` and `ticker`

Concurrency becomes more useful when you can bound waiting. MindScript provides timers as channels.

`timerAfter(ms)` returns a channel handle that will deliver a single tick after `ms` milliseconds. The tick is an integer timestamp (Unix milliseconds). After sending the tick, the channel closes.

`ticker(ms)` returns a channel handle that delivers ticks repeatedly until you close the channel from the receiving side.

Because timers are channels, you can integrate them with the same receive loop you would write for a network stream, and you can use them with process orchestration.

### Implementing a timeout for a process

The code below runs a task and returns `null  # <timeout>` if it does not finish within the deadline. It uses `procJoinAny` to wait for either the task or the timer.

```mindscript
let withTimeout = fun(ms: Int, f: Null -> Any) -> Any? do
    let pTask = procSpawn(f)
    let pTimer = procSpawn(fun() do
        let c = timerAfter(ms)
        chanRecv(c)
    end)

    let r = procJoinAny([pTask, pTimer])
    if r == null then
        null  # <internal error>
    elif r.index == 0 then
        procCancel(pTimer)
        r.value
    else
        procCancel(pTask)
        null  # <timeout>
    end
end

withTimeout(200, fun() do
    sleep(500)
    "done"
end)
```

The example uses `chanRecv` to wait for the timer tick; since `timerAfter` closes after one tick, this cannot leak an infinite stream.

### Periodic work with `ticker`

A ticker is appropriate when you want to do periodic polling or sampling without blocking the rest of the program.

```mindscript
let t = ticker(250)

let i = 0
while i < 5 do
    let ts = chanRecv(t)
    if ts == null then
        break(null)
    end
    println("tick at " + str(ts))
    i = i + 1
end

chanClose(t)
```

Closing a ticker’s channel stops the ticking. This is a deliberate resource-management step, like closing a file.

---

## Channels: explicit communication between concurrent parts

Channels are the primitive for communicating values between concurrent computations. A channel is a handle of kind `Handle.chan`. You create one with `chanOpen(cap)`.

A capacity of `0` creates an unbuffered channel, where sends and receives rendezvous. A positive capacity creates a buffered channel, where sends can proceed until the buffer is full.

A channel’s operations follow a disciplined failure convention: “closed” is not a panic in the sending direction, but it is still not a normal value. In several operations, a closed channel is represented as `null` annotated with a message. This makes it visible and explicit.

### Send and receive

`chanSend(c, x)` blocks until the value is accepted, returning `true` on success and returning `null` with an annotation if the channel is closed.

`chanRecv(c)` blocks until a value is available and returns that value. If the channel is closed and empty, it returns `null` annotated with a message indicating the channel is closed.

This means you should treat `null` from `chanRecv` as “stop,” but if you care about whether it is a normal stop or an abnormal close, you look at `noteGet`.

### Non-blocking operations

Sometimes blocking is wrong: you want to attempt an operation and continue if it would block. `chanTrySend(c, x)` returns a boolean indicating whether the value was sent.

`chanTryRecv(c)` returns a map `{ ok: Bool, value: Any }`. When `ok` is `false`, no value was available right now and the channel is still open; the `value` is `null`. When `ok` is `true`, `value` contains a received value, which may itself be an annotated `null` indicating that the channel is closed and empty.

This shape makes it possible to write polling loops without turning control flow into exceptions.

### A producer/consumer pipeline

The example below runs a producer in one process and a consumer in another, using a channel to carry work items. The producer closes the channel to signal completion.

```mindscript
let c = chanOpen(8)

let producer = procSpawn(fun() do
    let i = 0
    while i < 10 do
        if chanSend(c, i) == null then
            return (null  # <channel closed early>)
        end
        i = i + 1
    end
    chanClose(c)
    null
end)

let consumer = procSpawn(fun() do
    let sum = 0
    while true do
        let v = chanRecv(c)
        if v == null then
            break(sum)
        end
        sum = sum + v
    end
end)

procJoin(producer)
procJoin(consumer)  # 45
```

This style is explicit and predictable. Values move through one boundary, and completion is signaled through channel closure. If the channel closes unexpectedly, the annotation on the `null` makes the reason inspectable.

---

## Actors: serialized access to shared mutable state

Processes and channels are good when tasks are independent or communicate through message passing. Actors solve a different problem: you have mutable state that must be accessed safely from multiple places, and you want a single owner of that state.

An actor is a dedicated isolate with an internal state value `m`. Calls into the actor are serialized, so there are no data races on `m`. You start an actor with `actorStart(m, pinOSThread?)`. You interact with it using `actorRun`, `actorCall`, `actorGet`, and `actorSet`.

The key is that actor operations deep-snapshot values into the actor isolate, run there, and then deep-snapshot results back. This preserves the same “no shared mutable data” guarantee, while still giving you a single logical state.

### A safe counter service

This example creates a counter actor and increments it from multiple processes. The increments are serialized inside the actor, so the final count is deterministic.

```mindscript
let counter = actorStart({ value: 0 })

let inc = fun(m: {}) -> Int do
    m.value = m.value + 1
    m.value
end

let ps = []
let i = 0
while i < 20 do
    push(ps, procSpawn(fun() do
        actorRun(counter, inc)
    end))
    i = i + 1
end

procJoinAll(ps)

actorGet(counter, "value")  # 20
actorClose(counter)
```

The important point is not that the counter increments. The important point is that you never need locks, and you never need to reason about aliasing of mutable objects. The actor isolate is the owner, and every interaction is a serialized message into that owner.

### Actor calls with explicit arguments

When you want to call a function with arguments inside the actor, use `actorCall(a, f, args)`.

```mindscript
let store = actorStart({})

let put = fun(m: {}, k: Str, v: Any) -> Bool do
    m.(k) = v
    true
end

actorCall(store, put, ["answer", 42])
actorGet(store, "answer")  # 42
actorClose(store)
```

Actor getters and setters (`actorGet` and `actorSet`) are specialized helpers for the common case where `m` is a map or a module. They are convenient, but `actorRun` and `actorCall` are the general mechanism.

---

## Choosing the right tool

If you need parallelism for independent work, processes are the simplest option: spawn, join, and treat failures as values. If you need explicit communication, add channels and model your program as message passing. If you need a single authoritative mutable state, use an actor so you get serialized access without locks.

The model is intentionally strict: ordinary arrays and objects are not shared across concurrent execution. That constraint is what makes MindScript concurrency reliable in scripts, where correctness and predictability matter more than squeezing every last microsecond out of shared-memory parallelism.
