# Networking

!!! warning
    This page is under construction.

MindScript's networking primitives allow performing the tasks that you'd expect: fetching data from an API, posting results to a service, and speaking a simple TCP protocol. 

The runtime exposes network connections (sockets) as `Handle.net` values. The same I/O operations you already use for [files](/learn/io) also work for sockets. That is, we can use the already familiar functions `readN`, `readAll`, `readLine`, `write`, `flush`, and `close`. 

---

## TCP Connections

HTTP covers most integration work (see below), but raw TCP is still useful for quick tools and for talking to services that use a custom line protocol.

MindScript provides `netConnect(addr)` to create an outbound connection. To set up a listener, use `netListen(addr)`; once it is ready, you can accept an inbound connection with `netAccept(listener)`.

Once you obtain a `Handle.net` connection handle from either `netConnect` or `netAccept`, you can use the same I/O primitives as files to write/read from it.

```mindscript
let conn = netConnect("example.com:80")
if conn == null then
    let reason = noteGet(conn)
    reason = if reason == null then "no details" else reason end
    println("connection failed: " + reason)
else
    # use conn here with readN, write, etc.
    ...

    close(conn)
end
```

### A tiny TCP client and echo server

In the following example we want to implement a simple TCP client that connects to an echo server on port `9000` of localhost. The client will write "ping", read the reply, and print the result.

```mindscript
# Tiny TCP client.

let c = netConnect("127.0.0.1:9000")
if c == null then
    let reason = noteGet(c)
    reason = if reason == null then "no details" else reason end
    println("connect failed: " + reason)
    null
else
    write(c, "ping\n")
    flush(c)

    let line = readLine(c)
    close(c)

    if line == null then
        null  # no reply
    else
        println("Got reply: " + line)
    end
end
```

The flush is usually unnecessary for sockets because writes go directly to the OS, but using `flush` keeps the example consistent with the buffered I/O model and avoids surprises if a handle is layered through a buffered writer.

Now on to the server. Our tiny server accepts a connection and echoes each incoming line. This example handles one connection and then exits. Handling many connections is a concurrency topic, but the protocol logic is the same.

```mindscript
# Tiny echo server.

let l = netListen("127.0.0.1:9000")
if l == null then
    let reason = noteGet(l)
    reason = if reason == null then "no details" else reason end
    println("listen failed: " + reason)
else
    let c = netAccept(l)
    close(l)

    if c == null then
        let reason = noteGet(c)
        reason = if reason == null then "no details" else reason end
        println("accept failed: " + (noteGet(c) or "<no details>"))
    else
        while true do
            let line = readLine(c)
            if line == null then
                break
            end
            write(c, line + "\n")
            flush(c)
        end
        close(c)
    end
end
```

This shows the essential server discipline: acquire resources, handle boundary failures, loop until EOF, then close.

---

## HTTP

TCP is fairly low level and requires manual socket handling. MindScript also provides a high-level HTTP client. In practice, most modern programs will use the HTTP client because it covers the common "call an API" use case.

The function `http(req)` performs an HTTP request and returns a response object. The request `req` is a map where `url` is a required field and everything else is optional. The response is also a map and includes fields such as status code, headers, and the full response body as a string. The function returns an error when it cannot produce a meaningful response (for example, due to a network error or timeout).

A minimal GET looks like this:

```mindscript
let r = http({url: "https://www.daios.ai"})
if r == null then
    let reason = str(noteGet(r))
    println("request failed: " + reason)
else
    r.status
end
```

A non-null response is not automatically a "success" because HTTP represents many application-level failures as normal responses with status codes. It is good practice to check `r.status` explicitly. In the next example we'll use the "POST" method for the request.

```mindscript
let r = http({url: "https://api.example.com/items", method: "post"})
if r == null then
    r
elif r.status < 200 or r.status >= 300 then
    null  # http status not ok
else
    jsonParse(r.body)
end
```

Notice how here we assume the endpoint returns a JSON string, and `jsonParse` is used to parse it.

### The request map

HTTP requests are usually parameterized by method, headers, body, and timeout. These can be provided in the request objects using the fields `method` (as we've already seen), `headers`, `body`, and `timeout` respectively.

To illustrate this, we'll write a request to an endpoint for posting a new user record along with their role.

```mindscript
let payload = jsonStringify({name: "Ada", role: "engineer"})
if payload == null then
    payload
else
    let r = http({
        url: "https://api.example.com/users",
        method: "POST",
        headers: {
            "content-type": "application/json",
            "accept": "application/json",
        },
        body: payload,
        timeoutMs: 15_000
    })

    if r == null then
        r
    elif r.status != 201 then
        null  # create failed
    else
        jsonRepair(r.body)
    end
end
```

Headers are represented as a map of strings to strings. This is an intentional constraint: it avoids ambiguity and makes requests deterministic. On responses, header values are also strings; if a header has multiple values, they are joined into a single string, which is typically good enough for script-level work.

The timeout is specified in milliseconds. Timeouts are not “exceptions”; they are ordinary failures and so they produce `null` with a note. This is important for robust automation: timeouts should be handled explicitly, often by retrying.

### A retry loop with backoff

A careful script should expect transient failures and retry a small number of times. Since failures are represented as values, a retry loop is ordinary computation.

```mindscript
let fetchJson = fun(url: Str) -> Any? do
    let attempt = 0
    let r = null

    while attempt < 5 and r == null do
        r = http({url: url, timeoutMs: 5_000})
        if r == null then
            attempt = attempt + 1
            sleep(200 * (attempt * attempt))
        end
    end

    if r == null then
        null  # <http failed after retries>
    elif r.status < 200 or r.status >= 300 then
        null  # <http status not ok>
    else
        jsonRepair(r.body)
    end
end
```

The backoff is quadratic purely because it is simple and adequate for many scripts. More elaborate strategies belong in a dedicated library, but the key idea is the same: treat network unreliability as normal.

---

## HTTP streaming: large downloads and early resource release

Buffered HTTP is convenient but sometimes too expensive. If a response body is large, buffering it into memory can be slow or even impossible. MindScript therefore offers `httpStream(req)`, which returns a response object where the body is exposed as a readable `Handle.net` named `bodyH`.

The design mirrors Unix streaming: you pull bytes from the handle in chunks and process them as you go. This makes it possible to download large files, compute checksums, or decompress streams without building huge intermediate strings.

A streaming download to a file looks like this:

```mindscript
let url = "https://example.com/big.bin"

let r = httpStream({url: url, timeoutMs: 30_000})
if r == null then
    println("download start failed: " + (noteGet(r) or "<no details>"))
    null
else
    if r.status < 200 or r.status >= 300 then
        close(r.bodyH)
        null  # <http status not ok>
    else
        let out = open("big.bin", "w")
        if out == null then
            close(r.bodyH)
            out
        else
            while true do
                let chunk = readN(r.bodyH, 64_000)
                if chunk == null then
                    close(r.bodyH)
                    close(out)
                    return (null  # <read failed>)
                end
                if len(chunk) == 0 then
                    break(null)  # EOF
                end

                let n = write(out, chunk)
                if n == null then
                    close(r.bodyH)
                    close(out)
                    return (null  # <write failed>)
                end
            end

            close(r.bodyH)
            close(out)
            null
        end
    end
end
```

Two details here are worth keeping as habits.

First, the response handle must be closed even on error. Closing early is not just cleanup; it allows the underlying HTTP transport to release the connection, which matters in scripts that run many requests.

Second, the loop uses `readN` and checks `len(chunk) == 0` to detect EOF. This is the standard streaming idiom in MindScript.

### Streaming uploads with `bodyH`

Sometimes the large data is on the request side rather than the response side. The HTTP functions accept `bodyH` in the request. When provided, it must be a readable file or network handle. This lets you send a file without reading it into memory.

```mindscript
let f = open("payload.bin", "r")
if f == null then
    f
else
    let r = http({
        url: "https://api.example.com/upload",
        method: "POST",
        headers: {"content-type": "application/octet-stream"},
        bodyH: f,
        timeoutMs: 60_000
    })

    close(f)

    if r == null then
        r
    elif r.status < 200 or r.status >= 300 then
        null  # <upload failed>
    else
        r.body
    end
end
```

The explicit `close(f)` is important. A readable handle is still an OS resource, and the upload will finish sooner than a long-running script might otherwise exit.


---

## Diagnosing failures responsibly

Networking failures are often outside your control. What you can control is how clearly your script reports them and how safely it continues.

When a networking primitive returns `null`, printing `noteGet(value)` is usually enough for logs. When an operation panics, that is usually a contract mistake: for example, passing a non-string header value or giving a malformed request shape. In those cases you should fix the program rather than “handle the error.” If you truly need to treat a hard failure as data—say, you are writing a tool that validates request objects—wrap the operation in `try(fun() do ... end)` and propagate the resulting `{ok, value}`.

---

## Summary

MindScript networking is deliberately built around one core idea: network connections are streams, and streams are handled through the same I/O primitives everywhere. Use `http` for ordinary API calls, and treat non-2xx statuses as normal boundary outcomes that you must check explicitly. Use `httpStream` when buffering a body is too expensive, and close streaming handles promptly to release resources. Use `netConnect` and `netListen` when you need raw TCP, and reuse the same `readLine`/`readN`/`write` patterns you already know.
