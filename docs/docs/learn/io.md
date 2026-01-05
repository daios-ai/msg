# Input and Output

Input/output is how a MindScript program communicates with its environment: terminals, pipelines, files, and (through other libraries) networks and processes. The language provides **handles** that represent open streams such as standard input, standard output, open files, and sockets.

---

## The standard I/O streams

Every MindScript program starts with three predefined handles, `STDIN`, `STDOUT`, and `STDERR`, of type `<handle: file>`. These correspond to the conventional Unix streams.

```mindscript-repl
==> STDIN
<handle: file> # Readable handle for the process standard input.

==> STDOUT
<handle: file> # Writable handle for the process standard output.

==> STDERR
<handle: file> # Writable handle for the process standard error.
```

The standard printing functions `print`, `println`, `printf` all write to `STDOUT`. For instance, the following three all print `Hello, world!` followed by a newline.

```mindscript
print("Hello, world!\n")

println("Hello, world!")

printf("%s, %s!\n", ["Hello", "world"])
```

---

## Writing output

The most fundamental output operation is `write(h, s: Str) -> Int?`. It writes the bytes of the string `s` to the handle `h`. On success it returns the number of bytes written; on failure it returns an error (`null`). The word "bytes" matters: `write` does not interpret the string, and it does not insert separators or newlines. If you want a newline, you must include `"\n"` yourself.

```mindscript
write(STDOUT, "hello")
write(STDOUT, "\n")
```

Output is buffered. Buffering improves performance and reduces system calls, but it also means you might not see output immediately. When you need output to appear right away (typically for progress messages) call `flush(h) -> Bool?`. A good mental model is that `write` appends to a buffer and `flush` pushes that buffer to the operating system.

```mindscript
write(STDERR, "loading...\n")
flush(STDERR)
```

When you need formatted output, use `sprintf(fmt: Str, args: [Any]) -> Str?` to build a string or `printf(fmt: Str, args: [Any]) -> Str?` to write formatted output to stdout. The `printf` function writes through the same buffered stdout writer as `write(STDOUT, ...)` and flushes, which helps keep output ordering stable when you mix the two.

```mindscript
printf("count=%d\n", [42])
```

If the format string and the arguments do not match, `sprintf` and `printf` will return an error.

!!! note
    The `sprintf` and `printf` functions are based on Go's `fmt.Printf`, which hsa a similar syntax as `printf` from the C standard library.

---

## Reading input

Streams can be consumed in different ways, so MindScript provides three basic reading operations.

### Reading the whole stream: `readAll`

The function `readAll(h) -> Str?` reads from a handle until end-of-file and returns the entire contents as one string. If reading fails, it returns `null`. This is the simplest choice when the input is small enough to fit comfortably in memory, and it is common in “filter” programs that read from stdin, transform, and write to stdout.

```mindscript
let input = readAll(STDIN)
if input == null then
    let reason = noteGet(input)
    reason = if reason == null then "no details" else reason end
    write(STDERR, "read failed: " + reason + "\n")
    flush(STDERR)
    null
else
    let out = replace("\\s+", " ", strip(input))
    write(STDOUT, out + "\n")
    flush(STDOUT)
    out
end
```

Notice the structure: the I/O boundary is checked immediately, the transformation is ordinary computation, and output is written explicitly.

### Reading line by line: `readLine`

The function `readLine(h) -> Str?` returns the next line of text without its trailing newline (`"\n"` or `"\r\n"`). When the stream is at end-of-file, it returns `null`. This design makes line-processing loops direct: read a line, stop when the read returns `null`, otherwise process and continue.

```mindscript
while true do
    let line = readLine(STDIN)
    if line == null then
        break(null)
    end

    write(STDOUT, strip(line) + "\n")
end

flush(STDOUT)
```

It is important to distinguish an empty line from end-of-file. An empty line is the string `""`, which is a valid value that `readLine` may return. End-of-file is `null`. If you care about distinguishing “normal EOF” from “a read failure,” check the annotation.

### Reading fixed-size chunks: `readN`

The function `readN(h, n: Int) -> Str?` reads up to `n` bytes and returns a string containing exactly what it read. If reading fails, it returns `null`. End-of-file is detected when `readN` returns a string of length zero, so the typical loop checks `len(chunk) == 0`.

```mindscript
let bufSize = 64_000

while true do
    let chunk = readN(STDIN, bufSize)
    if chunk == null then
        let reason = noteGet(chunk)
        reason = if reason == null then "no reason" else reason end
        write(STDERR, "read failed: " + reason + "\n")
        flush(STDERR)
        break(null)
    end
    if len(chunk) == 0 then
        break(null)  # EOF
    end
    write(STDOUT, chunk)
end

flush(STDOUT)
```

This pattern is the backbone of streaming tasks: copying, hashing, compression, and large transfers all reduce to reading chunks until EOF, processing each chunk, and writing results.

---

## Files

MindScript supports whole-file helpers for simple scripts and explicit file handles for streaming.

The simplests interface is `readFile(path: Str) -> Str?`, which reads an entire file as one string, and `writeFile(path: Str, data: Str) -> Int?` to overwrite a file with the given data. As with other I/O, failures return errors.

```mindscript
let s = readFile("input.txt")
if s == null then
    let reason = noteGet(s)
    reason = if reason == null then "no details" else reason end
    write(STDERR, "cannot read input.txt: " + reason + "\n")
    flush(STDERR)
else
    let out = toUpper(s)
    let n = writeFile("output.txt", out)
    if n == null then
        let reason = noteGet(n)
        reason = if reason == null then "no details" else reason end
        write(STDERR, "cannot write output.txt: " + reason + "\n")
        flush(STDERR)
    else
        n
    end
end
```

When you need incremental processing, you open a file and work with a handle. The function `open(path: Str, mode: "r"|"w"|"a"|"rw") -> Handle.file?` returns a file handle. Once you have a handle, you use the same `readAll`, `readLine`, `readN`, `write`, `flush`, and `close` operations you already learned. Closing is important: it releases operating-system resources, and for writable handles it flushes buffered output.

```mindscript
let f = open("big.bin", "r")
if f != null then
    let chunk = readN(f, 64_000)
    close(f)
    chunk
end
```

A file copy example shows the full discipline of stream programming. You acquire resources, loop reading and writing until EOF, and ensure that both handles are closed on every exit path. In this example, end-of-file is detected by `len(chunk) == 0` and is not treated as an error.

```mindscript
let copyFile = fun(srcPath: Str, dstPath: Str) -> Bool? do
    let src = open(srcPath, "r")
    if src == null then
        return null  # cannot open source
    end

    let dst = open(dstPath, "w")
    if dst == null then
        close(src)
        return null  # cannot open destination
    end

    while true do
        let chunk = readN(src, 64_000)
        if chunk == null then
            close(src)
            close(dst)
            return null  # read error
        end
        if len(chunk) == 0 then
            break(null)  # EOF
        end

        let n = write(dst, chunk)
        if n == null then
            close(src)
            close(dst)
            return null  # write error
        end
    end

    close(src)
    close(dst)
    true
end
```

---

## Directories and paths

Scripts often need to build paths portably and work with directory trees. The function `pathJoin(parts: [Str]) -> Str` joins path components using the platform’s conventions, which keeps scripts portable across operating systems.

```mindscript
let p = pathJoin(["out", "result.txt"])
```

To create a directory (and any missing parents), use `mkdir(path) -> Bool?`. To list a directory, use `dirList(path) -> [Str]?`, which returns the names within the directory (or `null` on failure).

```mindscript
if mkdir("out") == null then
    write(STDERR, "cannot create out/\n")
    flush(STDERR)
    null
else
    let xs = dirList(".")
    if xs == null then
        write(STDERR, "cannot list directory\n")
        flush(STDERR)
        null
    else
        for name in xs do
            write(STDOUT, name + "\n")
        end
        flush(STDOUT)
        xs
    end
end
```

Other OS helpers exist, such as `stat` for reading a file status,  `rename` and `remove` to rename and remove a file, and `cwd` and `chdir` to get or change the current working directory.

---

## Bytes and text

Values of type `Str` are byte containers and thus work well in I/O operations (file contents, stream chunks, HTTP bodies, compressed data, and cryptographic digests). However, text-oriented functions like `print` and others (e.g. `toUpper`, `strip`, `split`, `substr`, etc.) interpret strings as Unicode text.

When a string contains arbitrary bytes, it may not be printable. In that case, encode it first with `hexEncode` or `base64Encode`. For example, to compute and print a SHA-256 digest as hex:

```mindscript
let b = readFile("payload.bin")
if b == null then
    b
else
    write(STDOUT, hexEncode(sha256(b)) + "\n")
    flush(STDOUT)
end
```
