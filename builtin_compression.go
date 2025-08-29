// builtin_compression.go
//
// Builtins surfaced:
//  1. gzipCompress(data: Str) -> Str
//  2. gzipDecompress(data: Str) -> Str?
//
// Conventions:
//   - Functions are camelCase; docs are docstring-style.
//   - Uses public API only; hard errors via fail(...).
//   - Tabs for indentation.
package mindscript

import (
	"bytes"
	"compress/gzip"
	"io"
)

func registerCompressionBuiltins(ip *Interpreter) {
	// gzipCompress(data: Str) -> Str
	// Compress data using gzip (default level).
	//
	// Params:
	// 	data: Str — input bytes (may contain arbitrary bytes)
	//
	// Returns:
	// 	Str — gzip-compressed bytes
	//
	// Notes:
	// 	• Output is binary; treat as bytes stored in Str.
	// 	• Implements RFC 1952 via Go's compress/gzip.
	ip.RegisterNative(
		"gzipCompress",
		[]ParamSpec{{Name: "data", Type: S{"id", "Str"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			arg := ctx.MustArg("data")
			if arg.Tag != VTStr {
				fail("gzipCompress: data must be Str")
			}
			src := []byte(arg.Data.(string))

			var buf bytes.Buffer
			zw, err := gzip.NewWriterLevel(&buf, gzip.DefaultCompression)
			if err != nil {
				fail("gzipCompress: init: " + err.Error())
			}
			if _, err := zw.Write(src); err != nil {
				_ = zw.Close()
				fail("gzipCompress: write: " + err.Error())
			}
			if err := zw.Close(); err != nil {
				fail("gzipCompress: close: " + err.Error())
			}
			return Str(buf.String())
		},
	)

	setBuiltinDoc(ip, "gzipCompress", `Compress data using gzip (default level).

Params:
	data: Str — input bytes (may contain arbitrary bytes)

Returns:
	Str — gzip-compressed bytes

Notes:
	• Output is binary; treat as bytes stored in Str.
	• Compatible with standard gzip tools and RFC 1952.`)

	// gzipDecompress(data: Str) -> Str?
	// Decompress a gzip payload.
	//
	// Params:
	// 	data: Str — gzip-compressed bytes
	//
	// Returns:
	// 	Str? — decompressed bytes, or null (annotated) on format/IO error
	//
	// Notes:
	// 	• Returns annotated null on invalid gzip headers, checksum mismatch, truncated input, etc.
	ip.RegisterNative(
		"gzipDecompress",
		[]ParamSpec{{Name: "data", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"id", "Str"}}, // Str?
		func(_ *Interpreter, ctx CallCtx) Value {
			arg := ctx.MustArg("data")
			if arg.Tag != VTStr {
				fail("gzipDecompress: data must be Str")
			}
			src := []byte(arg.Data.(string))

			gr, err := gzip.NewReader(bytes.NewReader(src))
			if err != nil {
				return Value{Tag: VTNull, Annot: "gzip: " + err.Error()}
			}
			defer gr.Close()

			out, err := io.ReadAll(gr)
			if err != nil {
				return Value{Tag: VTNull, Annot: "gzip: " + err.Error()}
			}
			return Str(string(out))
		},
	)

	setBuiltinDoc(ip, "gzipDecompress", `Decompress a gzip payload.

Params:
	data: Str — gzip-compressed bytes

Returns:
	Str? — decompressed bytes, or null (annotated) on error

Notes:
	• Returns annotated null on invalid gzip headers, checksum mismatch, truncated input, etc.
	• Output is binary; treat as bytes stored in Str.`)
}
