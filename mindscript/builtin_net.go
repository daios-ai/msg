// net_builtins.go
//
// Builtins surfaced:
//   • netConnect(addr: Str) -> Any
//   • netListen(addr: Str)  -> Any
//   • netAccept(l: Any)     -> Any
//   • http(req: {...})      -> { ... }?
//   • httpStream(req: {...}) -> { ... }?
//
// Notes:
//   - http(req) returns the whole body as Str (as before), but now also fills
//     statusText/url/proto/durationMs (end-to-end).
//   - httpStream(req) returns headers + a *streaming* body handle under `bodyH`.
//     The handle is Kind "net" so readN/readAll/close work unchanged.
//   - Uploads: req.body (Str) or req.bodyH (handle) for streaming uploads.
//   - Timeouts: req.timeoutMs (default 30000).
//   - Errors: contract mistakes hard-fail; network/IO return annotated null.

package mindscript

import (
	"bufio"
	"io"
	"net"
	"net/http"
	"strings"
	"time"
)

// These handle types are shared with I/O code:
//
// type netConnH struct {
// 	c  net.Conn
// 	rb *bufio.Reader
// 	wb *bufio.Writer
// }
//
// type netListenerH struct {
// 	ln net.Listener
// }

// readOnlyConn adapts an io.ReadCloser (HTTP response body) to net.Conn,
// enough for our buffered reads + close. Writes are not supported.
type readOnlyConn struct {
	rc io.ReadCloser
}

func (c *readOnlyConn) Read(b []byte) (int, error)         { return c.rc.Read(b) }
func (c *readOnlyConn) Write(_ []byte) (int, error)        { return 0, io.ErrClosedPipe }
func (c *readOnlyConn) Close() error                       { return c.rc.Close() }
func (c *readOnlyConn) LocalAddr() net.Addr                { return staticAddr("http-body") }
func (c *readOnlyConn) RemoteAddr() net.Addr               { return staticAddr("http-body") }
func (c *readOnlyConn) SetDeadline(_ time.Time) error      { return nil }
func (c *readOnlyConn) SetReadDeadline(_ time.Time) error  { return nil }
func (c *readOnlyConn) SetWriteDeadline(_ time.Time) error { return nil }

type staticAddr string

func (a staticAddr) Network() string { return string(a) }
func (a staticAddr) String() string  { return string(a) }

// helper: build headers map {Str: Str} (multi-values joined with ", ")
func headersToMap(h http.Header) Value {
	hm := make(map[string]Value, len(h))
	for k, vs := range h {
		hm[k] = Str(strings.Join(vs, ", "))
	}
	return Map(hm)
}

func registerNetBuiltins(ip *Interpreter, target *Env) {
	// netConnect("host:port") -> "net" handle
	ip.RegisterRuntimeBuiltin(
		target,
		"netConnect",
		[]ParamSpec{{Name: "addr", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"get", S{"id", "Handle"}, S{"str", "net"}}},
		func(ip *Interpreter, ctx CallCtx) Value {
			av := ctx.Arg("addr")
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
	setBuiltinDoc(target, "netConnect", `Open a TCP connection to "host:port".

Returns:
	Handle.net — usable with read*/write/flush/close,
	or null (annotated) on network error.`)

	// netListen("host:port") -> "listener" handle
	ip.RegisterRuntimeBuiltin(
		target,
		"netListen",
		[]ParamSpec{{Name: "addr", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			av := ctx.Arg("addr")
			ln, err := net.Listen("tcp", av.Data.(string))
			if err != nil {
				return annotNull(err.Error()) // soft
			}
			return HandleVal("listener", &netListenerH{ln: ln})
		},
	)
	setBuiltinDoc(target, "netListen", `Listen on a TCP address "host:port".

Returns:
	Handle.listener — pass to netAccept; close(listener) to stop;
	null (annotated) on bind/listen error.`)

	// netAccept(listener) -> "net" handle
	ip.RegisterRuntimeBuiltin(
		target,
		"netAccept",
		[]ParamSpec{{Name: "l", Type: S{"get", S{"id", "Handle"}, S{"str", "listener"}}}},
		S{"unop", "?", S{"get", S{"id", "Handle"}, S{"str", "net"}}}, func(ip *Interpreter, ctx CallCtx) Value {
			ln := asHandle(ctx.Arg("l"), "listener").Data.(*netListenerH).ln
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
	setBuiltinDoc(target, "netAccept", `Accept one TCP connection from a listener.

Blocks until a client connects.

Returns:
	Handle.net? — network handle, or null (annotated) on accept error.`)

	// HTTP input and output types.
	var httpReqT = S{
		"map",
		S{"pair!", S{"str", "url"}, S{"id", "Str"}},
		S{"pair", S{"str", "method"}, S{"id", "Str"}},
		S{"pair", S{"str", "headers"}, S{"map"}},
		S{"pair", S{"str", "body"}, S{"id", "Str"}},  // optional text body
		S{"pair", S{"str", "bodyH"}, S{"id", "Any"}}, // optional upload handle
		S{"pair", S{"str", "timeoutMs"}, S{"id", "Int"}},
	}

	var httpRespT = S{
		"unop", "?", S{
			"map",
			S{"pair!", S{"str", "status"}, S{"id", "Int"}},
			S{"pair", S{"str", "statusText"}, S{"id", "Str"}},
			S{"pair!", S{"str", "headers"}, S{"map"}},
			S{"pair", S{"str", "url"}, S{"id", "Str"}},
			S{"pair", S{"str", "proto"}, S{"id", "Str"}},
			S{"pair", S{"str", "durationMs"}, S{"id", "Int"}},
			S{"pair", S{"str", "body"}, S{"id", "Str"}}, // for http()
			S{"pair", S{"str", "bodyH"}, S{"get", S{"id", "Handle"}, S{"str", "net"}}},
		},
	}

	// -----------------------------------------
	// http: buffered body (as text)
	// -----------------------------------------
	ip.RegisterRuntimeBuiltin(
		target,
		"http",
		[]ParamSpec{{Name: "req", Type: httpReqT}},
		httpRespT,
		func(_ *Interpreter, ctx CallCtx) Value {
			rv := ctx.Arg("req")
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

			// body or bodyH (prefer handle if both provided)
			var bodyReader io.Reader
			if hv, ok := mo.Entries["bodyH"]; ok && hv.Tag != VTNull {
				// Accept file/net handle as upload source (read side).
				h := asHandle(hv, "")
				switch h.Kind {
				case "file":
					bodyReader = h.Data.(*fileH).rb
				case "net":
					bodyReader = h.Data.(*netConnH).rb
				default:
					fail("http req.bodyH must be a readable handle (file/net)")
				}
			} else if bv, ok := mo.Entries["body"]; ok {
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

			// timeoutMs (optional)
			timeout := 30 * time.Second
			if tv, ok := mo.Entries["timeoutMs"]; ok {
				if tv.Tag != VTInt {
					fail("http req.timeoutMs must be an Int (milliseconds)")
				}
				timeout = time.Duration(tv.Data.(int64)) * time.Millisecond
			}

			start := time.Now()
			client := &http.Client{Timeout: timeout}
			resp, err := client.Do(req)
			if err != nil {
				return annotNull(err.Error()) // soft (network timeout, DNS, etc.)
			}
			defer resp.Body.Close()

			b, err := io.ReadAll(resp.Body)
			if err != nil {
				return annotNull(err.Error()) // soft
			}
			durMs := int64(time.Since(start) / time.Millisecond)

			out := map[string]Value{
				"status":     Int(int64(resp.StatusCode)),
				"statusText": Str(resp.Status),
				"headers":    headersToMap(resp.Header),
				"body":       Str(string(b)),
				"url":        Str(resp.Request.URL.String()),
				"proto":      Str(resp.Proto),
				"durationMs": Int(durMs),
			}
			return Map(out)
		},
	)
	setBuiltinDoc(target, "http", `Make an HTTP request (buffered).

Input:
	req: {
		url!:       Str
		method:     Str?         # default "GET"
		headers:    {}?
		body:       Str?         # text body
		bodyH:      Any          # readable handle (file/net) for upload; preferred over body
		timeoutMs:  Int?         # default 30000
	}

Output:
	{
		status!:     Int
		statusText:  Str
		headers!:    {}          # multi-values joined by ", "
		body!:       Str         # full response body (text/binary-as-text)
		url:         Str         # final URL after redirects
		proto:       Str         # "HTTP/1.1", "HTTP/2.0"
		durationMs:  Int         # end-to-end (including body read)
	}?  # annotated null on network/IO error`)

	// -----------------------------------------
	// httpStream: streaming body (as a "net" handle)
	// -----------------------------------------
	ip.RegisterRuntimeBuiltin(
		target,
		"httpStream",
		[]ParamSpec{{Name: "req", Type: httpReqT}},
		httpRespT,
		func(_ *Interpreter, ctx CallCtx) Value {
			rv := ctx.Arg("req")
			if rv.Tag != VTMap {
				fail("httpStream expects a request map") // contractual
			}
			mo := rv.Data.(*MapObject)

			// url (required)
			uv, ok := mo.Entries["url"]
			if !ok || uv.Tag != VTStr {
				fail("httpStream req.url must be a string") // contractual
			}
			url := uv.Data.(string)

			// method (optional, default GET)
			method := "GET"
			if mv, ok := mo.Entries["method"]; ok {
				if mv.Tag != VTStr {
					fail("httpStream req.method must be a string")
				}
				if m := strings.TrimSpace(strings.ToUpper(mv.Data.(string))); m != "" {
					method = m
				}
			}

			// body or bodyH (prefer handle if both provided)
			var bodyReader io.Reader
			if hv, ok := mo.Entries["bodyH"]; ok && hv.Tag != VTNull {
				h := asHandle(hv, "")
				switch h.Kind {
				case "file":
					bodyReader = h.Data.(*fileH).rb
				case "net":
					bodyReader = h.Data.(*netConnH).rb
				default:
					fail("httpStream req.bodyH must be a readable handle (file/net)")
				}
			} else if bv, ok := mo.Entries["body"]; ok {
				if bv.Tag != VTStr {
					fail("httpStream req.body must be a string")
				}
				bodyReader = strings.NewReader(bv.Data.(string))
			}

			req, err := http.NewRequest(method, url, bodyReader)
			if err != nil {
				return annotNull(err.Error())
			}

			// headers (optional)
			if hv, ok := mo.Entries["headers"]; ok {
				if hv.Tag != VTMap {
					fail("httpStream req.headers must be a map")
				}
				hm := hv.Data.(*MapObject).Entries
				for k, vv := range hm {
					if vv.Tag != VTStr {
						fail("http header values must be strings")
					}
					req.Header.Set(k, vv.Data.(string))
				}
			}

			// timeoutMs (optional)
			timeout := 30 * time.Second
			if tv, ok := mo.Entries["timeoutMs"]; ok {
				if tv.Tag != VTInt {
					fail("httpStream req.timeoutMs must be an Int (milliseconds)")
				}
				timeout = time.Duration(tv.Data.(int64)) * time.Millisecond
			}

			start := time.Now()
			client := &http.Client{Timeout: timeout}
			resp, err := client.Do(req)
			if err != nil {
				return annotNull(err.Error())
			}
			// Wrap the response body in a readOnlyConn so it looks like a "net" handle.
			ro := &readOnlyConn{rc: resp.Body}
			h := &netConnH{
				c:  ro,
				rb: bufio.NewReader(ro),
				// wb is nil (read-only)
			}
			durMs := int64(time.Since(start) / time.Millisecond) // until headers received

			out := map[string]Value{
				"status":     Int(int64(resp.StatusCode)),
				"statusText": Str(resp.Status),
				"headers":    headersToMap(resp.Header),
				"bodyH":      HandleVal("net", h), // works with readN/readAll/close
				"url":        Str(resp.Request.URL.String()),
				"proto":      Str(resp.Proto),
				"durationMs": Int(durMs),
			}
			return Map(out)
		},
	)
	setBuiltinDoc(target, "httpStream", `Make an HTTP request (streaming).

Input:
	req: {
		url!:       Str
		method:     Str?         # default "GET"
		headers:    {}?
		body:       Str?         # text body
		bodyH:      Any          # readable handle (file/net) for upload; preferred over body
		timeoutMs:  Int?         # default 30000
	}

Output:
	{
		status!:     Int
		statusText:  Str
		headers!:    {Str: Str}
		bodyH!:      Handle.net   # readable handle (Kind "net"); use readN/readAll/close
		url:         Str
		proto:       Str
		durationMs:  Int          # time until headers (body not read)
	}?  # annotated null on network/IO error

Notes:
	• The returned bodyH is *read-only*. Writing to it will fail.
	• Use close(bodyH) when done to release the connection early.`)
}
