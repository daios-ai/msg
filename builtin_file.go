// os_io_builtins.go
//
// This file provides:
//   • OS primitives: osEnv, osSetEnv, stat, mkdir, rename, remove, cwd, chdir, tempDir
//   • File & network I/O: open/close/read*/write/flush/readFile/writeFile/dirList
//   • Formatted strings: sprintf/printf
//   • Standard I/O handles (constants): STDIN, STDOUT, STDERR
//
// Conventions:
//   - camelCase exported names
//   - docstring-style comments via setBuiltinDoc
//   - hard errors with fail(...); soft I/O failures return annotated null
//   - tabs for indentation

package mindscript

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"strings"
)

// File handle (shared by all file I/O builtins)
type fileH struct {
	f     *os.File
	rb    *bufio.Reader
	wb    *bufio.Writer
	hasR  bool
	hasW  bool
	isStd bool // true for STDIN/STDOUT/STDERR (we never Close() these)
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

	ip.RegisterNative(
		"osSetEnv",
		[]ParamSpec{
			{Name: "name", Type: S{"id", "Str"}},
			{Name: "value", Type: S{"unop", "?", S{"id", "Str"}}}, // Str? (null = unset)
		},
		S{"unop", "?", S{"id", "Bool"}}, // Bool?
		func(_ *Interpreter, ctx CallCtx) Value {
			name := ctx.MustArg("name").Data.(string)
			v, ok := ctx.Arg("value")
			if ok && v.Tag == VTStr {
				if err := os.Setenv(name, v.Data.(string)); err != nil {
					return annotNull(err.Error())
				}
				return Bool(true)
			}
			// Null or omitted -> unset
			if err := os.Unsetenv(name); err != nil {
				return annotNull(err.Error())
			}
			return Bool(true)
		},
	)
	setBuiltinDoc(ip, "osSetEnv", `Set or unset an environment variable.

If value is null (or omitted), the variable is unset.

Params:
	name:  Str
	value: Str?  # null → unset

Returns:
	Bool? (annotated null on OS error)`)

	// stat(path) -> { isDir!:Bool, size!:Int, modTimeMillis!:Int, mode!:Int }?
	ip.RegisterNative(
		"stat",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"unop", "?", S{
			"map",
			S{"pair!", S{"str", "isDir"}, S{"id", "Bool"}},
			S{"pair!", S{"str", "size"}, S{"id", "Int"}},
			S{"pair!", S{"str", "modTimeMillis"}, S{"id", "Int"}},
			S{"pair!", S{"str", "mode"}, S{"id", "Int"}},
		}},
		func(_ *Interpreter, ctx CallCtx) Value {
			p := ctx.MustArg("path").Data.(string)
			info, err := os.Stat(p)
			if err != nil {
				return annotNull(err.Error())
			}
			mo := &MapObject{
				Entries: map[string]Value{
					"isDir":         Bool(info.IsDir()),
					"size":          Int(info.Size()),
					"modTimeMillis": Int(info.ModTime().UnixNano() / 1e6),
					"mode":          Int(int64(info.Mode())),
				},
				KeyAnn: map[string]string{},
				Keys:   []string{"isDir", "size", "modTimeMillis", "mode"},
			}
			return Value{Tag: VTMap, Data: mo}
		},
	)
	setBuiltinDoc(ip, "stat", `File status (like ls -l metadata).

Params:
	path: Str

Returns:
	{ isDir!: Bool, size!: Int, modTimeMillis!: Int, mode!: Int }?,
	or annotated null on error (e.g., not found).`)

	ip.RegisterNative(
		"mkdir",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"id", "Bool"}}, // Bool?
		func(_ *Interpreter, ctx CallCtx) Value {
			p := ctx.MustArg("path").Data.(string)
			// Create intermediate directories for practicality.
			if err := os.MkdirAll(p, 0o755); err != nil {
				return annotNull(err.Error())
			}
			return Bool(true)
		},
	)
	setBuiltinDoc(ip, "mkdir", `Create a directory (creating parents as needed).

Params:
	path: Str

Returns:
	Bool? (annotated null on error)`)

	ip.RegisterNative(
		"rename",
		[]ParamSpec{
			{Name: "old", Type: S{"id", "Str"}},
			{Name: "new", Type: S{"id", "Str"}},
		},
		S{"unop", "?", S{"id", "Bool"}}, // Bool?
		func(_ *Interpreter, ctx CallCtx) Value {
			oldP := ctx.MustArg("old").Data.(string)
			newP := ctx.MustArg("new").Data.(string)
			if err := os.Rename(oldP, newP); err != nil {
				return annotNull(err.Error())
			}
			return Bool(true)
		},
	)
	setBuiltinDoc(ip, "rename", `Rename (move) a file or directory.

Params:
	old: Str
	new: Str

Returns:
	Bool? (annotated null on error)`)

	ip.RegisterNative(
		"remove",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"id", "Bool"}}, // Bool?
		func(_ *Interpreter, ctx CallCtx) Value {
			p := ctx.MustArg("path").Data.(string)
			// Be conservative: don't remove recursively.
			if err := os.Remove(p); err != nil {
				return annotNull(err.Error())
			}
			return Bool(true)
		},
	)
	setBuiltinDoc(ip, "remove", `Delete a file or an empty directory.

Params:
	path: Str

Returns:
	Bool? (annotated null on error). Note: fails for non-empty directories.`)

	ip.RegisterNative(
		"cwd",
		[]ParamSpec{},
		S{"unop", "?", S{"id", "Str"}},
		func(_ *Interpreter, _ CallCtx) Value {
			if wd, err := os.Getwd(); err == nil {
				return Str(wd)
			} else {
				return annotNull(err.Error())
			}
		},
	)
	setBuiltinDoc(ip, "cwd", `Get the current working directory.

Returns:
	Str? (annotated null on error)`)

	ip.RegisterNative(
		"chdir",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"id", "Bool"}}, // Bool?
		func(_ *Interpreter, ctx CallCtx) Value {
			p := ctx.MustArg("path").Data.(string)
			if err := os.Chdir(p); err != nil {
				return annotNull(err.Error())
			}
			return Bool(true)
		},
	)
	setBuiltinDoc(ip, "chdir", `Change the current working directory.

Params:
	path: Str

Returns:
	Bool? (annotated null on error)`)

	ip.RegisterNative(
		"tempDir",
		[]ParamSpec{},
		S{"id", "Str"},
		func(_ *Interpreter, _ CallCtx) Value {
			return Str(os.TempDir())
		},
	)
	setBuiltinDoc(ip, "tempDir", `Return the system temporary directory.

Returns:
	Str`)
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
			flag = os.O_RDWR | os.O_CREATE // read/write, no truncate
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
		// write-capable modes
		if mode == "w" || mode == "a" || mode == "rw" {
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
				fail("not readable")
			}
			return fh.rb, "file"
		case "net":
			nh := h.Data.(*netConnH)
			return nh.rb, "net"
		default:
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
				fail("not writable")
			}
			return fh.wb, "file"
		case "net":
			nh := h.Data.(*netConnH)
			return nh.wb, "net"
		default:
			fail("unsupported handle for write")
			return nil, ""
		}
	}

	// ---------- Standard I/O handles (constants)

	stdinH := &fileH{
		f:     os.Stdin,
		rb:    bufio.NewReader(os.Stdin),
		wb:    nil,
		hasR:  true,
		hasW:  false,
		isStd: true,
	}
	stdoutH := &fileH{
		f:     os.Stdout,
		rb:    nil,
		wb:    bufio.NewWriter(os.Stdout),
		hasR:  false,
		hasW:  true,
		isStd: true,
	}
	stderrH := &fileH{
		f:     os.Stderr,
		rb:    nil,
		wb:    bufio.NewWriter(os.Stderr),
		hasR:  false,
		hasW:  true,
		isStd: true,
	}

	ip.Core.Define("STDIN", HandleVal("file", stdinH))
	ip.Core.Define("STDOUT", HandleVal("file", stdoutH))
	ip.Core.Define("STDERR", HandleVal("file", stderrH))

	setBuiltinDoc(ip, "STDIN", `Readable handle for the process standard input.`)
	setBuiltinDoc(ip, "STDOUT", `Writable handle for the process standard output.`)
	setBuiltinDoc(ip, "STDERR", `Writable handle for the process standard error.`)

	// ---------- File & stream I/O

	ip.RegisterNative(
		"open",
		[]ParamSpec{
			{Name: "path", Type: S{"id", "Str"}},
			{Name: "mode", Type: S{"enum", S{"str", "r"}, S{"str", "w"}, S{"str", "a"}, S{"str", "rw"}}},
		},
		S{"unop", "?", S{"id", "Any"}}, // Any?
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("path")
			mv := ctx.MustArg("mode")
			if pv.Tag != VTStr {
				fail("open expects path: Str")
			}
			if mv.Tag != VTStr {
				fail("open expects mode: Str")
			}
			h, err := openFile(pv.Data.(string), mv.Data.(string))
			if err != nil {
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
	"rw" — read/write (create if needed; does not truncate)

Params:
	path: Str
	mode: Str ("r" | "w" | "a" | "rw")

Returns:
	file handle usable with read*/write/flush/close,
	or null (annotated) on I/O failure.`)

	// close(h: Any) -> Bool?
	ip.RegisterNative(
		"close",
		[]ParamSpec{{Name: "h", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Bool"}}, // Bool?
		func(ip *Interpreter, ctx CallCtx) Value {
			h := asHandle(ctx.MustArg("h"), "")
			var errMsg string
			switch h.Kind {
			case "file":
				fh := h.Data.(*fileH)
				// Always flush if writable
				if fh.wb != nil {
					if err := fh.wb.Flush(); err != nil && errMsg == "" {
						errMsg = err.Error()
					}
				}
				// Never Close() STDIN/OUT/ERR
				if !fh.isStd {
					if err := fh.f.Close(); err != nil && errMsg == "" {
						errMsg = err.Error()
					}
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

			case "listener":
				ln := h.Data.(*netListenerH)
				if err := ln.ln.Close(); err != nil && errMsg == "" {
					errMsg = err.Error()
				}

			default:
				fail("unsupported handle for close")
			}
			if errMsg != "" {
				return annotNull(errMsg)
			}
			return Bool(true)
		},
	)
	setBuiltinDoc(ip, "close", `Close a file, network connection, or listener handle.

Flushes buffered output (if any) before closing.
Never closes STDIN/STDOUT/STDERR; they are only flushed.
Returns annotated null on I/O failure; hard-errors on misuse.

Returns:
	Bool?`)

	ip.RegisterNative(
		"readAll",
		[]ParamSpec{{Name: "h", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			rb, _ := getReader(ctx.MustArg("h"))
			b, err := io.ReadAll(rb)
			if err != nil {
				return annotNull(err.Error())
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
				fail("readN expects n >= 0")
			}
			buf := make([]byte, n)
			k, err := io.ReadFull(rb, buf)
			if err != nil && err != io.EOF && err != io.ErrUnexpectedEOF {
				return annotNull(err.Error())
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
				return annotNull(err.Error())
			}
			// Trim trailing newline(s) (handle CRLF and LF)
			return Str(strings.TrimRight(s, "\r\n"))
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
				return annotNull(err.Error())
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
		S{"unop", "?", S{"id", "Bool"}}, // Bool?
		func(ip *Interpreter, ctx CallCtx) Value {
			wb, _ := getWriter(ctx.MustArg("h"))
			if err := wb.Flush(); err != nil {
				return annotNull(err.Error())
			}
			return Bool(true)
		},
	)
	setBuiltinDoc(ip, "flush", `Flush buffered output for a handle.

Ensures written data is visible to readers/peers.
Returns annotated null on I/O error.

Returns:
	Bool?`)

	ip.RegisterNative(
		"readFile",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("path")
			b, err := os.ReadFile(pv.Data.(string))
			if err != nil {
				return annotNull(err.Error())
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
		S{"unop", "?", S{"id", "Int"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("path")
			dv := ctx.MustArg("data")
			data := dv.Data.(string)
			if err := os.WriteFile(pv.Data.(string), []byte(data), 0o644); err != nil {
				return annotNull(err.Error())
			}
			return Int(int64(len(data)))
		},
	)
	setBuiltinDoc(ip, "writeFile", `Write a string to a file (overwriting if it exists).

Creates the file if necessary with mode 0644.
Returns the number of bytes written as Int, or null (annotated) on I/O error.`)

	ip.RegisterNative(
		"dirList",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"array", S{"id", "Str"}}},
		func(_ *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("path")
			ents, err := os.ReadDir(pv.Data.(string))
			if err != nil {
				return annotNull(err.Error())
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

	// sprintf(fmt: Str, args: [Any]) -> Str?
	ip.RegisterNative(
		"sprintf",
		[]ParamSpec{
			{Name: "fmt", Type: S{"id", "Str"}},
			{Name: "args", Type: S{"array", S{"id", "Any"}}},
		},
		S{"unop", "?", S{"id", "Str"}}, // Str?
		func(_ *Interpreter, ctx CallCtx) Value {
			f := ctx.MustArg("fmt").Data.(string)
			as := ctx.MustArg("args").Data.(*ArrayObject).Elems
			goArgs := make([]any, len(as))
			for i := range as {
				goArgs[i] = fmtArgFromValue(as[i])
			}
			out := fmt.Sprintf(f, goArgs...)
			// Simple, low-false-positive heuristic: Go emits patterns like "%!(EXPLAINER=...)".
			if strings.Contains(out, "%!(") {
				return annotNull("sprintf: format/arg mismatch")
			}
			return Str(out)
		},
	)
	setBuiltinDoc(ip, "sprintf", `Format a string with printf-style verbs.

Supports Go-style verbs like %s, %v, %d, %f, etc.
Args are passed as an array: sprintf("%s = %v", ["x", 42])

Params:
	fmt:  Str
	args: [Any]

Returns:
	Str? — the formatted string, or null (annotated) on format/arg mismatch.`)

	// printf(fmt: Str, args: [Any]) -> Str?
	// NOTE: Writes through the same buffered STDOUT handle used by write/flush to avoid interleaving.
	ip.RegisterNative(
		"printf",
		[]ParamSpec{
			{Name: "fmt", Type: S{"id", "Str"}},
			{Name: "args", Type: S{"array", S{"id", "Any"}}},
		},
		S{"unop", "?", S{"id", "Str"}}, // Str?
		func(_ *Interpreter, ctx CallCtx) Value {
			f := ctx.MustArg("fmt").Data.(string)
			as := ctx.MustArg("args").Data.(*ArrayObject).Elems
			goArgs := make([]any, len(as))
			for i := range as {
				goArgs[i] = fmtArgFromValue(as[i])
			}
			out := fmt.Sprintf(f, goArgs...)
			if strings.Contains(out, "%!(") {
				return annotNull("printf: format/arg mismatch")
			}
			// Write via the same buffered writer as STDOUT to keep ordering sane.
			if stdoutH.wb == nil {
				return annotNull("printf: stdout not writable")
			}
			if _, err := stdoutH.wb.WriteString(out); err != nil {
				return annotNull("printf: " + err.Error())
			}
			if err := stdoutH.wb.Flush(); err != nil {
				return annotNull("printf: " + err.Error())
			}
			return Str(out)
		},
	)
	setBuiltinDoc(ip, "printf", `Print a formatted string to standard output.

Writes via STDOUT's buffered writer to preserve order with write(STDOUT,...).
Caller controls newlines:
	printf("%s = %v\n", ["x", 42])

Params:
	fmt:  Str
	args: [Any]

Returns:
	Str? — the printed string, or null (annotated) on write error.`)
}

// --- Formatting --------------------------------------------------------------

func fmtArgFromValue(v Value) any {
	switch v.Tag {
	case VTStr:
		return v.Data.(string)
	case VTInt:
		return v.Data.(int64)
	case VTNum:
		return v.Data.(float64)
	case VTBool:
		return v.Data.(bool)
	case VTNull:
		return "null"
	default:
		// For arrays/maps/functions/types/handles/modules, defer to Value.String().
		return v
	}
}
