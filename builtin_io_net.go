// std_io_net.go — I/O & networking builtins with hard/soft error policy.
package mindscript

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"strings"
	"time"
)

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

// --- OS primitives ----------------------------------------

func registerOsBuiltins(ip *Interpreter) {

	ip.RegisterNative(
		"osEnv",
		[]ParamSpec{{Name: "name", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			n := ctx.MustArg("name").Data.(string)
			if v, ok := os.LookupEnv(n); ok {
				return Str(v)
			}
			return Null
		},
	)
	setBuiltinDoc(ip, "osEnv", `Read an environment variable.

Params:
  name: Str
Returns:
  Str? (null if unset)`)
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
				// Contractual misuse: file not opened for reading.
				fail("not readable")
			}
			return fh.rb, "file"
		case "net":
			nh := h.Data.(*netConnH)
			return nh.rb, "net"
		default:
			// Contractual: wrong handle kind.
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
				// Contractual misuse: file not opened for writing.
				fail("not writable")
			}
			return fh.wb, "file"
		case "net":
			nh := h.Data.(*netConnH)
			return nh.wb, "net"
		default:
			// Contractual: wrong handle kind.
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
				fail("open expects path: Str") // contractual
			}
			if mv.Tag != VTStr {
				fail("open expects mode: Str") // contractual
			}
			h, err := openFile(pv.Data.(string), mv.Data.(string))
			if err != nil {
				// soft (e.g., permission denied / not found)
				return annotNull(err.Error())
			}
			return HandleVal("file", h)
		},
	)
	setBuiltinDoc(ip, "open", `Open a file and return a handle.

Modes:
  "r"  — read-only
  "w"  — write (truncate or create)
  "a"  — append (create if needed)
  "rw" — read/write (create if needed)

Params:
  path: Str
  mode: Str ("r" | "w" | "a" | "rw")

Returns:
  file handle usable with read*/write/flush/close,
  or null (annotated) on I/O failure.`)

	ip.RegisterNative(
		"close",
		[]ParamSpec{{Name: "h", Type: S{"id", "Any"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			h := asHandle(ctx.MustArg("h"), "")
			var errMsg string
			switch h.Kind {
			case "file":
				fh := h.Data.(*fileH)
				if fh.wb != nil {
					if err := fh.wb.Flush(); err != nil && errMsg == "" {
						errMsg = err.Error()
					}
				}
				if err := fh.f.Close(); err != nil && errMsg == "" {
					errMsg = err.Error()
				}
			case "net":
				nh := h.Data.(*netConnH)
				if nh.wb != nil {
					if err := nh.wb.Flush(); err != nil && errMsg == "" {
						errMsg = err.Error()
					}
				}
				if err := nh.c.Close(); err != nil && errMsg == "" {
					errMsg = err.Error()
				}
			default:
				fail("unsupported handle for close") // contractual
			}
			if errMsg != "" {
				return annotNull(errMsg) // soft
			}
			return Null
		},
	)
	setBuiltinDoc(ip, "close", `Close a file or network handle.

Flushes buffered output (if any) before closing.
Returns null (annotated) on I/O failure; hard-errors on misuse.`)

	ip.RegisterNative(
		"readAll",
		[]ParamSpec{{Name: "h", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			rb, _ := getReader(ctx.MustArg("h"))
			b, err := io.ReadAll(rb)
			if err != nil {
				return annotNull(err.Error()) // soft
			}
			return Str(string(b))
		},
	)
	setBuiltinDoc(ip, "readAll", `Read all remaining bytes from a handle.

Blocks until EOF and returns the data as Str.
Returns null (annotated) on I/O error.`)

	ip.RegisterNative(
		"readN",
		[]ParamSpec{{Name: "h", Type: S{"id", "Any"}}, {Name: "n", Type: S{"id", "Int"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			rb, _ := getReader(ctx.MustArg("h"))
			nv := ctx.MustArg("n")
			n := int(nv.Data.(int64))
			if n < 0 {
				fail("readN expects n >= 0") // contractual
			}
			buf := make([]byte, n)
			k, err := io.ReadFull(rb, buf)
			if err != nil && err != io.EOF && err != io.ErrUnexpectedEOF {
				return annotNull(err.Error()) // soft
			}
			return Str(string(buf[:k]))
		},
	)
	setBuiltinDoc(ip, "readN", `Read up to n bytes from a handle.

May return fewer than n bytes at EOF. Returns data as Str.
Hard-error if n < 0. Returns null (annotated) on I/O error.`)

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
				return annotNull(err.Error()) // soft
			}
			return Str(strings.TrimRight(s, "\n"))
		},
	)
	setBuiltinDoc(ip, "readLine", `Read one line from a handle (without the trailing newline).

Returns null at EOF. Returns null (annotated) on I/O error.`)

	ip.RegisterNative(
		"write",
		[]ParamSpec{{Name: "h", Type: S{"id", "Any"}}, {Name: "s", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"id", "Int"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			wb, _ := getWriter(ctx.MustArg("h"))
			sv := ctx.MustArg("s")
			n, err := wb.WriteString(sv.Data.(string))
			if err != nil {
				return annotNull(err.Error()) // soft
			}
			return Int(int64(n))
		},
	)
	setBuiltinDoc(ip, "write", `Write a string to a file or network handle.

Returns the number of bytes written as Int, or null (annotated) on I/O error.
Output is buffered; call flush to ensure delivery.`)

	ip.RegisterNative(
		"flush",
		[]ParamSpec{{Name: "h", Type: S{"id", "Any"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			wb, _ := getWriter(ctx.MustArg("h"))
			if err := wb.Flush(); err != nil {
				return annotNull(err.Error()) // soft
			}
			return Null
		},
	)
	setBuiltinDoc(ip, "flush", `Flush buffered output for a handle.

Ensures written data is visible to readers/peers.
Returns null (annotated) on I/O error.`)

	ip.RegisterNative(
		"readFile",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("path")
			b, err := os.ReadFile(pv.Data.(string))
			if err != nil {
				return annotNull(err.Error()) // soft
			}
			return Str(string(b))
		},
	)
	setBuiltinDoc(ip, "readFile", `Read an entire file into a string.

Params:
  path: Str

Returns:
  Str, or null (annotated) on I/O error.`)

	ip.RegisterNative(
		"writeFile",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}, {Name: "data", Type: S{"id", "Str"}}},
		S{"id", "Null"},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("path")
			dv := ctx.MustArg("data")
			if err := os.WriteFile(pv.Data.(string), []byte(dv.Data.(string)), 0o644); err != nil {
				return annotNull(err.Error()) // soft
			}
			return Null
		},
	)
	setBuiltinDoc(ip, "writeFile", `Write a string to a file (overwriting if it exists).

Creates the file if necessary with mode 0644.
Returns null (annotated) on I/O error.`)

	ip.RegisterNative(
		"dirList",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("path")
			ents, err := os.ReadDir(pv.Data.(string))
			if err != nil {
				return annotNull(err.Error()) // soft
			}
			out := make([]Value, 0, len(ents))
			for _, e := range ents {
				out = append(out, Str(e.Name()))
			}
			return Arr(out)
		},
	)
	setBuiltinDoc(ip, "dirList", `List directory entries as an array of names.

Params:
  path: Str

Returns:
  [Str], or null (annotated) on I/O error (e.g., permission denied).`)

	// Bind std streams with doc
	ip.Core.Define("stdin",
		withAnnot(
			HandleVal("file", &fileH{f: os.Stdin, rb: bufio.NewReader(os.Stdin), hasR: true}),
			`Standard input stream handle.

Readable; use readLine/readN/readAll.`))

	ip.Core.Define("stdout",
		withAnnot(
			HandleVal("file", &fileH{f: os.Stdout, wb: bufio.NewWriter(os.Stdout), hasW: true}),
			`Standard output stream handle.

Writable; use write/flush.`))

	ip.Core.Define("stderr",
		withAnnot(
			HandleVal("file", &fileH{f: os.Stderr, wb: bufio.NewWriter(os.Stderr), hasW: true}),
			`Standard error stream handle.

Writable; use write/flush.`))

	// sprintf(fmt: Str, args: [Any]) -> Str
	ip.RegisterNative(
		"sprintf",
		[]ParamSpec{{Name: "fmt", Type: S{"id", "Str"}}, {Name: "args", Type: S{"array", S{"id", "Any"}}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			f := ctx.MustArg("fmt").Data.(string)
			as := ctx.MustArg("args").Data.([]Value)
			goArgs := make([]any, len(as))
			for i := range as {
				goArgs[i] = fmtArgFromValue(as[i])
			}
			return Str(fmt.Sprintf(f, goArgs...))
		},
	)
	setBuiltinDoc(ip, "sprintf", `Format a string with printf-style verbs.

Supports Go-style verbs like %s, %v, %d, %f, etc.
Args are passed as an array: sprintf("%s = %v", ["x", 42])

Params:
  fmt:  Str
  args: [Any]

Returns:
  Str`)

	// printf(fmt: Str, args: [Any]) -> Any  (returns printed string, or null on write error)
	ip.RegisterNative(
		"printf",
		[]ParamSpec{{Name: "fmt", Type: S{"id", "Str"}}, {Name: "args", Type: S{"array", S{"id", "Any"}}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			f := ctx.MustArg("fmt").Data.(string)
			as := ctx.MustArg("args").Data.([]Value)
			goArgs := make([]any, len(as))
			for i := range as {
				goArgs[i] = fmtArgFromValue(as[i])
			}
			ret := fmt.Sprintf(f, goArgs...)
			if _, err := fmt.Print(ret); err != nil {
				return annotNull(err.Error()) // soft
			}
			return Str(ret)
		},
	)
	setBuiltinDoc(ip, "printf", `Print a formatted string to standard output.

Convenience over sprintf + stdout. Caller controls newlines:
  printf("%s = %v\n", ["x", 42])

Params:
  fmt:  Str
  args: [Any]

Returns:
  Str (what was printed), or null (annotated) on write error.`)
}

// --- Networking --------------------------------------------------------------

func registerNetBuiltins(ip *Interpreter) {
	// netConnect("host:port") -> "net" handle
	ip.RegisterNative(
		"netConnect",
		[]ParamSpec{{Name: "addr", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			av := ctx.MustArg("addr")
			conn, err := net.Dial("tcp", av.Data.(string))
			if err != nil {
				return annotNull(err.Error()) // soft
			}
			return HandleVal("net", &netConnH{
				c:  conn,
				rb: bufio.NewReader(conn),
				wb: bufio.NewWriter(conn),
			})
		},
	)
	setBuiltinDoc(ip, "netConnect", `Open a TCP connection to "host:port".

Returns a network handle usable with read*/write/flush/close,
or null (annotated) on network error.`)

	// netListen("host:port") -> "listener" handle
	ip.RegisterNative(
		"netListen",
		[]ParamSpec{{Name: "addr", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			av := ctx.MustArg("addr")
			ln, err := net.Listen("tcp", av.Data.(string))
			if err != nil {
				return annotNull(err.Error()) // soft
			}
			return HandleVal("listener", &netListenerH{ln: ln})
		},
	)
	setBuiltinDoc(ip, "netListen", `Listen on a TCP address "host:port".

Returns a listener handle for netAccept. Use close(listener) to stop listening.
Returns null (annotated) on bind/listen error.`)

	// netAccept(listener) -> "net" handle
	ip.RegisterNative(
		"netAccept",
		[]ParamSpec{{Name: "l", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			ln := asHandle(ctx.MustArg("l"), "listener").Data.(*netListenerH).ln
			conn, err := ln.Accept()
			if err != nil {
				return annotNull(err.Error()) // soft
			}
			return HandleVal("net", &netConnH{
				c:  conn,
				rb: bufio.NewReader(conn),
				wb: bufio.NewWriter(conn),
			})
		},
	)
	setBuiltinDoc(ip, "netAccept", `Accept one TCP connection from a listener.

Blocks until a client connects. Returns a network handle,
or null (annotated) on accept error.`)

	ip.RegisterNative(
		"http",
		[]ParamSpec{{Name: "req", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			rv := ctx.MustArg("req")
			if rv.Tag != VTMap {
				fail("http expects a request map") // contractual
			}
			mo := rv.Data.(*MapObject)

			// url (required)
			uv, ok := mo.Entries["url"]
			if !ok || uv.Tag != VTStr {
				fail("http req.url must be a string") // contractual
			}
			url := uv.Data.(string)

			// method (optional, default GET)
			method := "GET"
			if mv, ok := mo.Entries["method"]; ok {
				if mv.Tag != VTStr {
					fail("http req.method must be a string") // contractual
				}
				if m := strings.TrimSpace(strings.ToUpper(mv.Data.(string))); m != "" {
					method = m
				}
			}

			// body (optional)
			var bodyReader io.Reader
			if bv, ok := mo.Entries["body"]; ok {
				if bv.Tag != VTStr {
					fail("http req.body must be a string") // contractual
				}
				bodyReader = strings.NewReader(bv.Data.(string))
			}

			req, err := http.NewRequest(method, url, bodyReader)
			if err != nil {
				return annotNull(err.Error()) // soft (bad URL, etc.)
			}

			// headers (optional)
			if hv, ok := mo.Entries["headers"]; ok {
				if hv.Tag != VTMap {
					fail("http req.headers must be a map") // contractual
				}
				hm := hv.Data.(*MapObject).Entries
				for k, vv := range hm {
					if vv.Tag != VTStr {
						fail("http header values must be strings") // contractual
					}
					req.Header.Set(k, vv.Data.(string))
				}
			}

			client := &http.Client{Timeout: 30 * time.Second}
			resp, err := client.Do(req)
			if err != nil {
				return annotNull(err.Error()) // soft (network timeout, DNS, etc.)
			}
			defer resp.Body.Close()

			b, err := io.ReadAll(resp.Body)
			if err != nil {
				return annotNull(err.Error()) // soft
			}

			// headers out (join multi-values with ", ")
			hdrs := make(map[string]Value, len(resp.Header))
			for k, vs := range resp.Header {
				hdrs[k] = Str(strings.Join(vs, ", "))
			}

			return Map(map[string]Value{
				"status":  Int(int64(resp.StatusCode)),
				"headers": Map(hdrs),
				"body":    Str(string(b)),
			})
		},
	)
	setBuiltinDoc(ip, "http", `Make an HTTP request.

Input:
  req: {
    url:     Str       (required)
    method:  Str?      (default "GET")
    headers: {Str:Str}?
    body:    Str?      (for POST/PUT/PATCH, etc.)
  }

Output:
  { status: Int, headers: {Str:Str}, body: Str }

Notes:
  • Uses a 30s timeout.
  • Contract mistakes (wrong types/shapes) are hard errors.
  • Network/parse failures return null (annotated).`)
}

// --- Formatting --------------------------------------------------------------

func fmtArgFromValue(v Value) any {
	switch v.Tag {
	case VTStr:
		// Let %s work naturally and avoid extra quotes that Value.String() would add.
		return v.Data.(string)
	case VTInt:
		return v.Data.(int64)
	case VTNum:
		return v.Data.(float64)
	case VTBool:
		return v.Data.(bool)
	case VTNull:
		// Friendlier than nil/%!s(<nil>): prints "null" with %s and "null" with %v too.
		return "null"
	default:
		// For arrays/maps/functions/types/handles/modules, defer to Value.String().
		return v
	}
}
