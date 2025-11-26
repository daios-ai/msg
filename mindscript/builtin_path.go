// path_builtins.go
//
// Builtins surfaced:
//  1. pathJoin(parts: [Str]) -> Str
//  2. pathBase(path: Str) -> Str
//  3. pathDir(path: Str) -> Str
//  4. pathExt(path: Str) -> Str
//  5. pathClean(path: Str) -> Str
//
// Conventions:
//   - Functions are camelCase; docs are docstring-style (first line, blank, details).
//   - Uses public API only; hard errors via fail(...).
//   - Tabs for indentation.
package mindscript

import (
	"fmt"
	"path/filepath"
)

func registerPathBuiltins(ip *Interpreter, target *Env) {
	// pathJoin(parts: [Str]) -> Str
	// Join path elements using the OS-specific separator.
	ip.RegisterRuntimeBuiltin(
		target,
		"pathJoin",
		[]ParamSpec{{Name: "parts", Type: S{"array", S{"id", "Str"}}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			pv := ctx.Arg("parts")
			if pv.Tag != VTArray {
				fail("pathJoin: parts must be [Str]")
			}
			items := pv.Data.(*ArrayObject).Elems
			elems := make([]string, 0, len(items))
			for i, v := range items {
				if v.Tag != VTStr {
					fail(fmt.Sprintf("pathJoin: parts[%d] must be Str", i))
				}
				elems = append(elems, v.Data.(string))
			}
			return Str(filepath.Join(elems...))
		},
	)
	setBuiltinDoc(target, "pathJoin", `Join path elements using the OS-specific separator.

Params:
	parts: [Str] — path fragments

Returns:
	Str — joined path

Notes:
	• Mirrors Go's filepath.Join semantics (cleans the result, removes empty segments).
	• Works cross-platform (Windows/POSIX).`)

	// pathBase(path: Str) -> Str
	// Return the last element of path. For empty string, returns ".".
	ip.RegisterRuntimeBuiltin(
		target,
		"pathBase",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			p := mustStrArg("pathBase", ctx, "path")
			return Str(filepath.Base(p))
		},
	)
	setBuiltinDoc(target, "pathBase", `Return the last element of a path (OS-specific).

Params:
	path: Str

Returns:
	Str — base name

Notes:
	• Mirrors Go's filepath.Base semantics ("" → ".", strips trailing separators).`)

	// pathDir(path: Str) -> Str
	// Return all but the last element of path, typically the parent directory.
	ip.RegisterRuntimeBuiltin(
		target,
		"pathDir",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			p := mustStrArg("pathDir", ctx, "path")
			return Str(filepath.Dir(p))
		},
	)
	setBuiltinDoc(target, "pathDir", `Return all but the last element of a path.

Params:
	path: Str

Returns:
	Str — directory component

Notes:
	• Mirrors Go's filepath.Dir semantics ("" → ".", root stays root).`)

	// pathExt(path: Str) -> Str
	// Return the file name extension including the leading dot, or "" if none.
	ip.RegisterRuntimeBuiltin(
		target,
		"pathExt",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			p := mustStrArg("pathExt", ctx, "path")
			return Str(filepath.Ext(p))
		},
	)
	setBuiltinDoc(target, "pathExt", `Return the file extension (including the leading dot), or "" if none.

Params:
	path: Str

Returns:
	Str — extension

Notes:
	• Mirrors Go's filepath.Ext semantics (".bashrc" → ".bashrc", "archive.tar.gz" → ".gz").`)

	// pathClean(path: Str) -> Str
	// Clean up a path by applying lexical simplifications.
	ip.RegisterRuntimeBuiltin(
		target,
		"pathClean",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			p := mustStrArg("pathClean", ctx, "path")
			return Str(filepath.Clean(p))
		},
	)
	setBuiltinDoc(target, "pathClean", `Clean a path by applying lexical simplifications.

Params:
	path: Str

Returns:
	Str — cleaned path

Notes:
	• Removes redundant separators and up-levels like "." and ".." where possible.
	• Mirrors Go's filepath.Clean semantics; no I/O performed.
	• Cross-platform behavior handled by filepath.`)
}

// mustStrArg is a tiny helper for native implementations in this file.
// It fails with a consistent message if the named argument is not a Str.
func mustStrArg(fn string, ctx CallCtx, name string) string {
	v := ctx.Arg(name)
	if v.Tag != VTStr {
		fail(fmt.Sprintf("%s: %s must be Str", fn, name))
	}
	return v.Data.(string)
}
