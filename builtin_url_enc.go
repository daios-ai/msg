// encoding_url_builtins.go
//
// Builtins surfaced:
//  1. base64Encode(x: Str) -> Str
//  2. base64Decode(s: Str) -> Str?              // soft error (annotated null) on invalid input
//  3. hexEncode(x: Str)    -> Str
//  4. hexDecode(s: Str)    -> Str?              // soft error (annotated null) on invalid input
//  5. urlParse(s: Str) -> { scheme:Str, host:Str, port:Int?, path:Str, query:{}, fragment:Str? }
//  6. urlBuild(u: { scheme:Str, host:Str, port:Int?, path:Str?, query:{}, fragment:Str? }) -> Str
//  7. urlQueryParse(s: Str) -> {}               // map Str -> [Str]
//  8. urlQueryString(q: {}) -> Str              // accepts map Str -> Str|[Str]
//
// Conventions:
//   - Functions are camelCase; docs are docstring-style (first line, blank, details).
//   - Tabs for indentation.
//   - Param/return types use public S-expr type forms (open-world maps as S{"map"}).
//   - Contract mistakes hard-fail via fail(...); data/parse errors return annotated null.
//
// Notes on "query" maps:
//   - The type system uses open-world maps ({}). Docs specify value semantics as Str -> [Str].
//   - urlQueryString accepts either Str or [Str] values for convenience; anything else hard-fails.
package mindscript

import (
	"encoding/base64"
	"encoding/hex"
	"net"
	"net/url"
	"strconv"
	"strings"
)

func registerEncodingURLBuiltins(ip *Interpreter) {
	annNull := func(msg string) Value {
		v := Null
		v.Annot = msg
		return v
	}

	// base64Encode(x: Str) -> Str
	ip.RegisterNative(
		"base64Encode",
		[]ParamSpec{{Name: "x", Type: S{"id", "Str"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			x := ctx.MustArg("x")
			if x.Tag != VTStr {
				fail("base64Encode: x must be Str")
			}
			out := base64.StdEncoding.EncodeToString([]byte(x.Data.(string)))
			return Str(out)
		},
	)
	setBuiltinDoc(ip, "base64Encode", `Base64-encode bytes from a string.

Params:
	x: Str — input bytes (string may contain arbitrary bytes)

Returns:
	Str — standard Base64 with '=' padding (RFC 4648).`)

	// base64Decode(s: Str) -> Str?
	ip.RegisterNative(
		"base64Decode",
		[]ParamSpec{{Name: "s", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"id", "Str"}}, // Str?
		func(_ *Interpreter, ctx CallCtx) Value {
			s := ctx.MustArg("s")
			if s.Tag != VTStr {
				fail("base64Decode: s must be Str")
			}
			b, err := base64.StdEncoding.DecodeString(s.Data.(string))
			if err != nil {
				return annNull("invalid base64: " + err.Error())
			}
			return Str(string(b))
		},
	)
	setBuiltinDoc(ip, "base64Decode", `Decode a standard Base64 string.

Params:
	s: Str — standard Base64 with '=' padding

Returns:
	Str? — decoded bytes as Str, or null (annotated) on invalid input.`)

	// hexEncode(x: Str) -> Str
	ip.RegisterNative(
		"hexEncode",
		[]ParamSpec{{Name: "x", Type: S{"id", "Str"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			x := ctx.MustArg("x")
			if x.Tag != VTStr {
				fail("hexEncode: x must be Str")
			}
			out := hex.EncodeToString([]byte(x.Data.(string)))
			return Str(out)
		},
	)
	setBuiltinDoc(ip, "hexEncode", `Hex-encode bytes to a lowercase hexadecimal string.

Params:
	x: Str — input bytes

Returns:
	Str — lowercase hex (two chars per byte).`)

	// hexDecode(s: Str) -> Str?
	ip.RegisterNative(
		"hexDecode",
		[]ParamSpec{{Name: "s", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"id", "Str"}}, // Str?
		func(_ *Interpreter, ctx CallCtx) Value {
			s := ctx.MustArg("s")
			if s.Tag != VTStr {
				fail("hexDecode: s must be Str")
			}
			in := s.Data.(string)
			b, err := hex.DecodeString(in)
			if err != nil {
				return annNull("invalid hex: " + err.Error())
			}
			return Str(string(b))
		},
	)
	setBuiltinDoc(ip, "hexDecode", `Decode a hexadecimal string.

Params:
	s: Str — hex string (case-insensitive), even length

Returns:
	Str? — decoded bytes as Str, or null (annotated) on invalid input.`)

	// urlParse(s: Str) -> { scheme, host, port?, path, query:{}, fragment? }?
	ip.RegisterNative(
		"urlParse",
		[]ParamSpec{{Name: "s", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"map"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			sv := ctx.MustArg("s")
			if sv.Tag != VTStr {
				fail("urlParse: s must be Str")
			}
			raw := sv.Data.(string)
			u, err := url.Parse(raw)
			if err != nil {
				return annNull("invalid URL: " + err.Error())
			}

			// Build query map Str -> [Str]
			q := u.Query() // url.Values (map[string][]string)
			qOut := &MapObject{Entries: map[string]Value{}, KeyAnn: map[string]string{}, Keys: []string{}}
			// stable key order
			keys := make([]string, 0, len(q))
			for k := range q {
				keys = append(keys, k)
			}
			if len(keys) > 1 {
				sortStrings(keys)
			}
			for _, k := range keys {
				arr := make([]Value, 0, len(q[k]))
				for _, s := range q[k] {
					arr = append(arr, Str(s))
				}
				qOut.Entries[k] = Arr(arr)
				qOut.Keys = append(qOut.Keys, k)
			}

			out := &MapObject{
				Entries: map[string]Value{},
				KeyAnn:  map[string]string{},
				Keys:    []string{},
			}
			put := func(k string, v Value) { out.Entries[k] = v; out.Keys = append(out.Keys, k) }

			put("scheme", Str(u.Scheme))
			host := u.Hostname()
			put("host", Str(host))
			if p := u.Port(); p != "" {
				if n, err := strconv.Atoi(p); err == nil {
					put("port", Int(int64(n)))
				}
			}
			// Path: prefer RawPath if set; else Path.
			path := u.EscapedPath()
			put("path", Str(path))
			put("query", Value{Tag: VTMap, Data: qOut})
			if u.Fragment != "" {
				put("fragment", Str(u.Fragment))
			}
			return Value{Tag: VTMap, Data: out}
		},
	)
	setBuiltinDoc(ip, "urlParse", `Parse a URL into components.

Params:
	s: Str — URL string

Returns:
	{
		scheme!:   Str,
		host!:     Str,
		port:      Int?,         # present if URL has an explicit numeric port
		path!:     Str,          # escaped path
		query!:    {},           # map Str -> [Str]
		fragment:  Str?
	}?

Notes:
	• Query values preserve multiplicity as arrays.
	• IPv6 hosts are returned without brackets in 'host'.`)

	// urlBuild(u: { scheme, host, port?, path?, query:{}, fragment? }) -> Str
	ip.RegisterNative(
		"urlBuild",
		[]ParamSpec{{Name: "u", Type: S{"map"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			uv := ctx.MustArg("u")
			if uv.Tag != VTMap {
				fail("urlBuild: u must be {}")
			}
			m := uv.Data.(*MapObject).Entries

			getStr := func(key string, required bool) string {
				v, ok := m[key]
				if !ok {
					if required {
						fail("urlBuild: missing required field: " + key)
					}
					return ""
				}
				if v.Tag != VTStr {
					fail("urlBuild: " + key + " must be Str")
				}
				return v.Data.(string)
			}
			getPort := func() (int, bool) {
				v, ok := m["port"]
				if !ok || v.Tag == VTNull {
					return 0, false
				}
				if v.Tag != VTInt {
					fail("urlBuild: port must be Int")
				}
				return int(v.Data.(int64)), true
			}
			buildQuery := func() string {
				v, ok := m["query"]
				if !ok || v.Tag == VTNull {
					return ""
				}
				if v.Tag != VTMap {
					fail("urlBuild: query must be {}")
				}
				mv := v.Data.(*MapObject)
				qs := url.Values{}
				for _, k := range mv.Keys {
					val := mv.Entries[k]
					switch val.Tag {
					case VTStr:
						qs.Add(k, val.Data.(string))
					case VTArray:
						for _, it := range val.Data.([]Value) {
							if it.Tag != VTStr {
								fail("urlBuild: query values must be Str or [Str]")
							}
							qs.Add(k, it.Data.(string))
						}
					default:
						fail("urlBuild: query values must be Str or [Str]")
					}
				}
				if enc := qs.Encode(); enc != "" {
					return "?" + enc
				}
				return ""
			}

			scheme := getStr("scheme", true)
			host := getStr("host", true)
			path := getStr("path", false)
			port, hasPort := getPort()
			frag := getStr("fragment", false)

			u := url.URL{Scheme: scheme}
			if hasPort {
				u.Host = net.JoinHostPort(host, strconv.Itoa(port))
			} else {
				// If host already includes brackets/port, respect it as-is.
				u.Host = host
			}
			// Path is expected to be an escaped path already; set RawPath/Path to preserve it.
			if path != "" {
				// url.URL expects Path unescaped; attempt to unescape once.
				if unesc, err := url.PathUnescape(path); err == nil {
					u.Path = unesc
					u.RawPath = path
				} else {
					// Fallback: set Path only.
					u.Path = path
				}
			}
			out := u.Scheme + "://"
			out += u.Host
			if u.RawPath != "" {
				out += u.RawPath
			} else if u.Path != "" {
				out += u.EscapedPath()
			}
			out += buildQuery()
			if frag != "" {
				out += "#" + url.PathEscape(frag)
			}
			return Str(out)
		},
	)
	setBuiltinDoc(ip, "urlBuild", `Build a URL string from components.

Params:
	u: {
		scheme!:  Str,
		host!:    Str,           # hostname only; port via 'port'
		port:     Int?,
		path:     Str?,          # escaped path (e.g. "/a%20b")
		query:    {},            # map Str -> Str|[Str]
		fragment: Str?
	}

Returns:
	Str — URL string.

Notes:
	• Use 'port' to add a port; IPv6 hosts are handled via proper bracket formatting.
	• 'query' values accept Str or [Str].`)

	// urlQueryParse(s: Str) -> {}?
	ip.RegisterNative(
		"urlQueryParse",
		[]ParamSpec{{Name: "s", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"map"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			sv := ctx.MustArg("s")
			if sv.Tag != VTStr {
				fail("urlQueryParse: s must be Str")
			}
			raw := strings.TrimPrefix(sv.Data.(string), "?")
			vals, err := url.ParseQuery(raw)
			if err != nil {
				return annNull("invalid query: " + err.Error())
			}
			out := &MapObject{Entries: map[string]Value{}, KeyAnn: map[string]string{}, Keys: []string{}}
			keys := make([]string, 0, len(vals))
			for k := range vals {
				keys = append(keys, k)
			}
			if len(keys) > 1 {
				sortStrings(keys)
			}
			for _, k := range keys {
				arr := make([]Value, 0, len(vals[k]))
				for _, s := range vals[k] {
					arr = append(arr, Str(s))
				}
				out.Entries[k] = Arr(arr)
				out.Keys = append(out.Keys, k)
			}
			return Value{Tag: VTMap, Data: out}
		},
	)
	setBuiltinDoc(ip, "urlQueryParse", `Parse a URL query string into a map.

Params:
	s: Str — query string with or without the leading '?'

Returns:
	{}? — map Str -> [Str], or null on invalid input.

Notes:
	• Percent-decoding is applied to keys and values.`)

	// urlQueryString(q: {}) -> Str
	ip.RegisterNative(
		"urlQueryString",
		[]ParamSpec{{Name: "q", Type: S{"map"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			qv := ctx.MustArg("q")
			if qv.Tag != VTMap {
				fail("urlQueryString: q must be {}")
			}
			mv := qv.Data.(*MapObject)
			qs := url.Values{}
			for _, k := range mv.Keys {
				val := mv.Entries[k]
				switch val.Tag {
				case VTStr:
					qs.Add(k, val.Data.(string))
				case VTArray:
					for _, it := range val.Data.([]Value) {
						if it.Tag != VTStr {
							fail("urlQueryString: values must be Str or [Str]")
						}
						qs.Add(k, it.Data.(string))
					}
				default:
					fail("urlQueryString: values must be Str or [Str]")
				}
			}
			return Str(qs.Encode())
		},
	)
	setBuiltinDoc(ip, "urlQueryString", `Serialize a query map to 'application/x-www-form-urlencoded'.

Params:
	q: {} — map Str -> Str|[Str]

Returns:
	Str — percent-encoded query string (without leading '?').`)
}

// Small local sort helper to avoid importing "sort" at top-level of the file if undesired.
func sortStrings(xs []string) {
	// Simple insertion sort; tiny inputs typical for query maps.
	for i := 1; i < len(xs); i++ {
		j := i
		for j > 0 && xs[j-1] > xs[j] {
			xs[j-1], xs[j] = xs[j], xs[j-1]
			j--
		}
	}
}
