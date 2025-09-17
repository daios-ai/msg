// ast_introspection_builtins.go
//
// Builtins surfaced:
//  1. noteGet(x: Any) -> Str?
//  2. noteSet(text: Str, value: Any) -> Any
//  3. astParse(src: Str) -> []                 // error on lex/parse failure
//  4. astEval(ast: []) -> Any                  // Decode → Validate → Eval; errors on validation/eval failures
//  5. astFormat(ast: []) -> Str                // Decode → Validate → Format; soft error on invalid
//  6. astValidate(ast: []) -> [ {path!:Str, code!:Str, message!:Str, got!:Str, expect!:Str} ]
//  7. reflect(val: Any) -> []                  // constructor code; soft error on unreflectable values
//  8. reify(rt: []) -> Any?                    // Decode → Validate → EvalPersistent; soft errors on decode/validate/eval
//
// Conventions:
//   - Functions are camelCase; docs are docstring-style (first line, blank, details).
//   - Uses public API only; hard errors via fail(...).
//   - No panic shield: unexpected panics bubble up (by request).
//   - Tabs for indentation.
//   - All errors are null with an error message as an annotation except when there is a critical error (e.g. runtime error)
package mindscript

import (
	"strconv"
)

// ---- internal helpers ------------------------------------------------------

// softFirstValidationError converts IxValidateS's VTArray of error maps into
// a single annotated-null encapsulating ONLY the first error (for callers that
// want a single soft error: astEval/astFormat/reify).
func softFirstValidationError(errs Value) Value {
	if errs.Tag != VTArray {
		return annotNull(`Validation error at /: invalid validator output (E_INTERNAL)`)
	}
	items := errs.Data.(*ArrayObject).Elems
	if len(items) == 0 {
		return annotNull(`Validation error at /: (empty) (E_INTERNAL)`)
	}
	first := items[0]
	if first.Tag != VTMap {
		return annotNull(`Validation error at /: invalid error entry (E_INTERNAL)`)
	}
	m := first.Data.(*MapObject).Entries
	get := func(k string) string {
		if v, ok := m[k]; ok && v.Tag == VTStr {
			return v.Data.(string)
		}
		return ""
	}
	path := get("path")
	code := get("code")
	msg := get("message")
	got := get("got")
	exp := get("expect")
	detail := `(` + strconv.Quote(code) +
		`, got:` + strconv.Quote(got) +
		`, expect:` + strconv.Quote(exp) + `)`
	return annotNull(`Validation error at ` + path + `: ` + msg + ` ` + detail)
}

// valShapeErrors builds the **array-of-error-objects** shape for astValidate()
// when the packed runtime-S cannot be decoded.
func valShapeErrors(detail string) Value {
	return Arr([]Value{
		Map(map[string]Value{
			"path":    Str("/"),
			"code":    Str("E_RUNTIME_SHAPE"),
			"message": Str("invalid runtime-S"),
			"got":     Str(detail),
			"expect":  Str("[]"),
		}),
	})
}

// decodeAndValidate: [] → S → IxValidateS.
// Returns (S, Null) on success; otherwise (nil, annotated-null) with the first error.
// Used by astEval/astFormat/reify (not by astValidate).
func decodeAndValidate(av Value) (S, Value) {
	if av.Tag != VTArray {
		return nil, annotNull("expected []")
	}
	sexpr, err := IxFromS(av)
	if err != nil {
		return nil, annotNull(err.Error())
	}
	if verrs := IxValidateS(sexpr); verrs.Tag == VTArray && len(verrs.Data.(*ArrayObject).Elems) > 0 {
		return nil, softFirstValidationError(verrs)
	}
	return sexpr, Null
}

// ---- registration -----------------------------------------------------------

func registerIntrospectionBuiltins(ip *Interpreter) {
	// noteGet(x: Any) -> Str?
	ip.RegisterNative(
		"noteGet",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			v := ctx.MustArg("x")
			if v.Annot == "" {
				return Null
			}
			return Str(v.Annot)
		},
	)
	setBuiltinDoc(ip, "noteGet", `Get the annotation attached to a value, if any.

Params:
	x: Any — any runtime value
Returns:
	Str? — the annotation string, or null if not present
Notes:
	• See also: noteSet(text, value) to attach annotations programmatically.`)

	// noteSet(text: Str, value: Any) -> Any
	ip.RegisterNative(
		"noteSet",
		[]ParamSpec{
			{Name: "text", Type: S{"id", "Str"}},
			{Name: "value", Type: S{"id", "Any"}},
		},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			text := ctx.MustArg("text").Data.(string)
			v := ctx.MustArg("value")
			v.Annot = text
			return v
		},
	)
	setBuiltinDoc(ip, "noteSet", `Attach or replace an annotation on a value.

Params:
	text: Str  — annotation text to attach
	value: Any — value to annotate
Returns:
	Any — the same value with its annotation set to text
Notes:
	• This does not deep-copy arrays/maps; underlying data is shared.
	• Annotations never affect equality but are rendered by printers.`)

	// astParse(src: Str) -> []
	ip.RegisterNative(
		"astParse",
		[]ParamSpec{{Name: "src", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"array"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			arg := ctx.MustArg("src")
			if arg.Tag != VTStr {
				fail("astParse: src must be Str")
			}
			src := arg.Data.(string)
			ast, err := ParseSExpr(src)
			if err != nil {
				return annotNull(err.Error())
			}
			return IxToS(ast)
		},
	)
	setBuiltinDoc(ip, "astParse", `Parse source code into runtime-S (VTArray).

Params:
	src: Str — MindScript source
Returns:
	[]? — a runtime-S AST (["tag", ...])
Notes:
	• Encoding is stable and round-trippable with astEval / reify.`)

	// astEval(ast: []) -> Any
	ip.RegisterNative(
		"astEval",
		[]ParamSpec{{Name: "ast", Type: S{"array"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			av := ctx.MustArg("ast")
			sexpr, soft := decodeAndValidate(av)
			if sexpr == nil {
				return soft
			}
			res, err := ip.EvalAST(sexpr, ctx.Env())
			if err != nil {
				return annotNull(err.Error())
			}
			return res
		},
	)
	setBuiltinDoc(ip, "astEval", `Evaluate a runtime-S AST in the caller's scope.

Params:
	ast: [] — runtime-S AST (["tag", ...])
Returns:
	Any — evaluation result (or error)`)

	// astFormat(ast: []) -> Str
	ip.RegisterNative(
		"astFormat",
		[]ParamSpec{{Name: "ast", Type: S{"array"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			av := ctx.MustArg("ast")
			sexpr, soft := decodeAndValidate(av)
			if sexpr == nil {
				return soft
			}
			return Str(FormatSExpr(sexpr))
		},
	)
	setBuiltinDoc(ip, "astFormat", `Format a runtime-S AST into stable source.

Params:
	ast: [] — runtime-S AST
Returns:
	Str? — formatted source (or error)`)

	// astValidate(ast: []) -> [ {path!:Str, code!:Str, message!:Str, got!:Str, expect!:Str} ]
	// Always returns an array (possibly empty). Never returns annotated-null.
	ip.RegisterNative(
		"astValidate",
		[]ParamSpec{{Name: "ast", Type: S{"array"}}},
		S{"array"},
		func(_ *Interpreter, ctx CallCtx) Value {
			av := ctx.MustArg("ast")
			// If not an array or cannot decode: return a single standardized error object.
			if av.Tag != VTArray {
				return valShapeErrors("expected []")
			}
			sexpr, err := IxFromS(av)
			if err != nil {
				return valShapeErrors(err.Error())
			}
			// IxValidateS already returns VTArray of error maps; pass it through.
			// Empty array => success.
			return IxValidateS(sexpr)
		},
	)
	setBuiltinDoc(ip, "astValidate", `Validate a runtime-S AST.

Params:
  ast: [] — runtime-S AST
Returns:
  [ {path!:Str, code!:Str, message!:Str, got!:Str, expect!:Str} ]
    • Empty array on success.
    • On failure, one or more error objects. (Shape/decode errors produce one entry with code "E_RUNTIME_SHAPE".)`)

	// reflect(val: Any) -> []
	ip.RegisterNative(
		"reflect",
		[]ParamSpec{{Name: "val", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"array"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			return IxReflect(ctx.MustArg("val"))
		},
	)
	setBuiltinDoc(ip, "reflect", `Reflect a value into constructor code (runtime-S).

Params:
	val: Any — value to reflect
Returns:
	[]? — constructor code (runtime-S)
Soft errors:
	• Opaque handles/unknowns → annotated-null`)

	// reify(rt: []) -> Any
	ip.RegisterNative(
		"reify",
		[]ParamSpec{{Name: "rt", Type: S{"array"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			av := ctx.MustArg("rt")
			sexpr, soft := decodeAndValidate(av)
			if sexpr == nil {
				return soft
			}
			val, err := ip.EvalPersistent(sexpr)
			if err != nil {
				return annotNull(err.Error())
			}
			return val
		},
	)
	setBuiltinDoc(ip, "reify", `Decode and evaluate constructor code (runtime-S).

Params:
	rt: [] — constructor code produced by reflect(...) or astParse(...)
Returns:
	Any — constructed value`)
}
