# Concurrency

When you want to execute more than one task at the same time, you can use concurrency. MindScript's concurrency model is based on processes that run in cloned interpreter isolates (no shared memory) and communicate via message passing. 

The runtime offers three building blocks that compose cleanly: 

* **processes**, which can be spawned and synchronized;
* **channels**, used for communication;
* and **actors**, i.e. single-threaded workers that own state.

Each tool exists to solve a specific problem.


### The concurrency contract

A spawned computation runs in a fresh interpreter isolate. You can think of it as "the same program, but with its own heap." The function you spawn is deep-snapshotted into that isolate along with the values it closes over. In practice this means that arrays and objects you captured become independent copies in the child. Mutating them in the child does not affect the parent, and vice versa.

This has two consequences that matter when you write real programs. First, it eliminates the most common concurrency failure mode in scripting languages: accidental shared mutation. Second, it forces you to be intentional about communication. If you want to exchange values between concurrent computations, you send them explicitly through channels, or you return them as process results, or you store them inside an actor and interact with that actor through actor operations.

Handles are the one exception: they are opaque references managed by the runtime. Some handle kinds are specifically meant to be shared across isolates, such as channels and process handles. When you pass a handle into a spawned function, you are not copying an in-memory data structure; you are passing a reference to an external resource. 

---

## Processes

A process in MindScript is a concurrent evaluation of a function in an isolate. You create a process with `procSpawn(f)` which runs a function `f` of type `fun(Null) -> Any`, and you wait for it with `procJoin(p)` where `p` is the process handle.

```mindscript-repl
==> let p = procSpawn(fun() do
...     40 + 2
... end)
<handle: proc>

==> procJoin(p)
42
```

### What happens on failure

When a spawned process fails, whether it is by returning an error or by a panic, will always return an error when joined, because panics inside a spawned process are caught and converted into an error  result annotated with the runtime error message. This is intentional: it means a process always "joins" to a value, and you decide how to handle it.

```mindscript-repl
==> let p = procSpawn(fun() do
...     panic("boom")
... end)
<handle: proc>

==> let v = procJoin(p)
null # boom
```

This pattern keeps concurrent code composable. The parent thread can join, inspect the annotation, and decide whether to retry, skip, or stop.

### Waiting for one or all to finish

When you have multiple independent tasks, joining them one at a time is verbose and often less efficient. The runtime provides two helpers, `procJoinAll(ps)` and `procJoinAny(ps)` where `ps` is an array of process handles `[Handle.proc]`:

- `procJoinAll(ps)` waits for all processes in the given array and returns an array of their results in the same order;
- `procJoinAny(ps)` waits until any process completes and returns `{ index: Int, value: Any }`. If the list is empty, it returns an error.

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
```

Then, we wait for one or all of them to finish with

```mindscript-repl
==> procJoinAny(ps)
{index: 6, value: 1774372447}

==> procJoinAll(ps)
[
	1890744469,
	1871349132,
	1851953795,
	1832558458,
	1813163121,
	1793767784,
	1774372447,
	1754977110
]
```

Notice what you do not need to think about. There is no locking around `xs`, and there is no danger that one task mutates another task’s intermediate state, because each spawned function runs in its own isolate with its own copied values.

---

## Channels

Channels are the primitive for communicating values between concurrent computations. A channel is a handle of kind `Handle.chan`. You create one with `chanOpen(cap)`, where `cap` is the capacity.

```mindscript-repl
==> let channel = chanOpen(10)
<handle: chan>
```

A capacity of `0` creates an unbuffered channel, where sends and receives rendezvous. A positive capacity creates a buffered channel, where sends can proceed until the buffer is full.

### Send and receive

There are two main functions for communication: `chanSend(c, x)` sends a value `x` through a channel `c` and `chanRecv(c)` retrieves a value from it. Both are *blocking*, that is, they will wait until the value is accepted.

In the next example, pay attention to when the message is received:

```mindscript-repl
==> let c = chanOpen()
<handle: chan>

==> let reader = procSpawn(fun() do
...     println("Received: " + chanRecv(c))
... end)
<handle: proc>
 
==> let writer = procSpawn(fun() do
...     chanSend(c, "Hi")
... end)
<handle: proc>
Received: Hi
```

When a channel is closed receiving from/sending to it yields an error.

```mindscript-repl
==> let ch = chanOpen()
<handle: chan>

==> chanClose(ch)
true
 
==> let r = chanRecv(ch)
null # channel closed

==> let s = chanSend(ch, "hi")
null # channel closed
```

### Non-blocking operations

Sometimes blocking is wrong: you want to attempt an operation and continue if it would block. `chanTrySend(c, x)` returns a boolean indicating whether the value was sent. `chanTryRecv(c)` returns a map `{ ok: Bool, value: Any }` which will be equal to:

- `{ ok: false, value: null }` when no value was available right now;
- `{ ok: true, value: v }` when a value `v` was available, which could be `null` if the channel is closed.

This shape makes it possible to write polling loops without turning control flow into exceptions.

In the example below we'll try to read before and after a value has been sent:

```mindscript-repl
==> let ch = chanOpen(1)
<handle: chan>

==> let r = chanTryRecv(ch)
{ok: false, value: null}

==> let s = chanTrySend(ch, "hi")
true

==> let r = chanTryRecv(ch)
{ok: true, value: "hi"}
```

### A producer/consumer pipeline

The example below runs a producer in one process and a consumer in another, using a channel to carry work items. The producer closes the channel to signal completion.

```mindscript
let c = chanOpen(8)

# Write 0, 1, ..., 9 to a channel
let producer = procSpawn(fun() do
    let i = 0
    while i < 10 do
        if chanSend(c, i) == null then
            return null  # channel closed early
        end
        i = i + 1
    end
    chanClose(c)
    null
end)

# Consume numbers from a channel and add them
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
procJoin(consumer)  # Returns 45.
```

This style is explicit and predictable. Values move through one boundary, and completion is signaled through channel closure. If the channel closes unexpectedly, the annotation on the `null` makes the reason inspectable.

---

## Timers and timeouts

MindScript provides timers as channels:

- `timerAfter(ms)` returns a channel handle that will deliver a single tick after `ms` milliseconds. The tick is an integer timestamp (Unix milliseconds). After sending the tick, the channel closes.
- `ticker(ms)` returns a channel handle that delivers ticks repeatedly until you close the channel from the receiving side.

Because timers are channels, you can integrate them with the same receive loop you would write for a network stream, and you can use them with process orchestration.

### Implementing a timeout for a process

The code below runs a task and returns `null # timeout` if it does not finish within the deadline. It uses `procJoinAny` to wait for either the task or the timer.

```mindscript
let withTimeout = fun(ms: Int, f: Null -> Any) -> Any? do
    let pTask = procSpawn(f)
    let pTimer = procSpawn(fun() do
        let c = timerAfter(ms)
        chanRecv(c)
    end)

    let r = procJoinAny([pTask, pTimer])
    if r == null then
        null  # internal error
    elif r.index == 0 then
        r.value
    else
        procCancel(pTask)
        null  # timeout
    end
end

withTimeout(200, fun() do
    sleep(500)
    "done"
end)
```

The example uses `chanRecv` to wait for the timer tick; since `timerAfter` closes after one tick, this cannot leak an infinite stream.

!!! note
    MindScript does not provide primitives to forcefully terminate a running process. If this is required, it must be implemented cooperatively, e.g. with a termination signal channel.

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

## Actors

Processes and channels are good when tasks are independent or communicate through message passing. But sometime you have a mutable state that must be accessed safely from multiple places, and you want a single owner of that state. This is what actors are for.

Intuitively, an actor is a wrapper around one piece of mutable state that guarantees that "only one thing touches it at a time". You create an actor with `actorStart(m, pinOSThread?)` which creates a dedicated isolate with an internal state value `m`. Optionally, you can pin this actor to a single thread by setting `pinOSThread` to `true`.

You can then interact with this actor using `actorRun`, `actorCall`, `actorGet`, and `actorSet`. Calls into the actor are serialized, so there are no data races races on the content `m`. Let's briefly review what these functions do:

* **Getter/setter**: `actorGet(actor, field)` and `actorSet(actor, field, value)`: these are getter and setter functions respectively, and they assume that the wrapped value is either a map or a module.
* **Function call**: `actorRun(actor, f)` and `actorCall(actor, f, [arg1, ..., argN])`: these functions allow running a function `f` on the wrapped value `m`. The difference is that `actorRun` calls `f(m)` and `actorCall` calls `f(m, arg1, ..., argN)`, i.e. the latter is suitable for function calls that require more arguments.

### A safe counter service

This example creates a counter actor and increments it from multiple processes. The increments are serialized inside the actor, so the final count is deterministic. Without an actor, this could lead to data races.

```mindscript
# Actor with a value
let counter = actorStart({ value: 0 }, false)

# Increment value by one.
let inc = fun(m: {value: Int}) -> Int do
    m.value = m.value + 1
end

# Create 10 concurrent workers who increment the value.

let ps = []
for i in range(0, 10) do
    let p = procSpawn(fun() do
        actorRun(counter, inc)
    end)
    push(ps, p)
end
```

If we now synchronize all workers, we get:

```mindscript-repl
==> procJoinAll(ps)
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

which is the list of values returned by each worker. To retrieve the final value of the counter, we can do

```mindscript-repl
==> actorGet(counter, "value")
10
```

Notice how we achieved this serialization *without using locks*.

Finally, we should close the actor.

```mindscript-repl
==> actorClose(counter)
true
```

### Actor calls with explicit arguments

When you want to call a function with arguments inside the actor, use `actorCall(a, f, args)`.

```mindscript
let store = actorStart({})

let put = fun(m: {}, k: Str, v: Any) -> Bool do
    m[k] = v
    true
end

actorCall(store, put, ["answer", 42])
actorGet(store, "answer")  # 42
actorClose(store)
```

Actor getters and setters (`actorGet` and `actorSet`) are specialized helpers for the common case where `m` is a map or a module. They are convenient, but `actorRun` and `actorCall` are the general mechanism.

