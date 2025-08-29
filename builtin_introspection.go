// ast_introspection_builtins.go
//
// Builtins surfaced:
//  1. astParse(src: Str) -> []
//  2. astEval(ast: []) -> Any
//  3. reflect(val: Any) -> []           // constructor code
//  4. reify(rt: []) -> Any              // decode + evaluate (persistent)
//
// Conventions:
//   - Functions are camelCase; docs are docstring-style (first line, blank, details).
//   - Uses public API only; hard errors via fail(...).
//   - Tabs for indentation.
package mindscript

func registerIntrospectionBuiltins(ip *Interpreter) {
	// note(x: Any) -> Str?
	// Return the annotation (note) attached to a value, if any.
	ip.RegisterNative(
		"noteGet",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Str"}}, // Str?
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

	ip.RegisterNative(
		"bindings",
		[]ParamSpec{{Name: "localOnly", Type: S{"unop", "?", S{"id", "Bool"}}}},
		S{"map"}, // open-world object: {}
		func(_ *Interpreter, ctx CallCtx) Value {
			// localOnly == true → only current frame; else merged view (inner shadows outer)
			localOnly := false
			if v, ok := ctx.Arg("localOnly"); ok && v.Tag == VTBool {
				localOnly = v.Data.(bool)
			}

			out := &MapObject{
				Entries: map[string]Value{},
				KeyAnn:  map[string]string{},
				Keys:    []string{},
			}
			for e := ctx.Env(); e != nil; e = e.parent {
				for k, v := range e.table {
					if _, seen := out.Entries[k]; !seen {
						out.Entries[k] = v
						out.Keys = append(out.Keys, k)
					}
				}
				if localOnly {
					break
				}
			}
			return Value{Tag: VTMap, Data: out}
		},
	)

	setBuiltinDoc(ip, "bindings", `Inspect visible variable bindings as a map.

Returns a map of variable names to values from the current environment. When
localOnly is true, only the current frame's bindings are returned. Otherwise a
merged view of all visible frames is returned, where inner frames shadow outer
frames.

Params:
	localOnly: Bool? — if true, include only the current frame; if false/null, include all visible frames

Returns:
	{} — object map of name → value (shallow copy). Order is inner-to-outer
	     first-seen, suitable for iteration.

Notes:
	• This is read-only: mutating the returned map does not change the environment.
	• Useful for testing (detecting state leaks), debugging, and introspection.
`)

	// astParse(src: Str) -> []
	// Parse MindScript source into a runtime-S ([]) AST.
	// On syntax/lex errors, returns a *soft* error as annotated-null runtime-S:
	//   ["annot", ["str", <msg>], ["null"]]
	ip.RegisterNative(
		"astParse",
		[]ParamSpec{{Name: "src", Type: S{"id", "Str"}}},
		S{"array"},
		func(_ *Interpreter, ctx CallCtx) Value {
			arg := ctx.MustArg("src")
			if arg.Tag != VTStr {
				fail("astParse: src must be Str")
			}
			src := arg.Data.(string)

			ast, err := ParseSExpr(src)
			if err != nil {
				// Soft error: annotated-null runtime-S.
				return Arr([]Value{
					Str("annot"),
					Arr([]Value{Str("str"), Str(err.Error())}),
					Arr([]Value{Str("null")}),
				})
			}
			// Encode to runtime-S (IxToS soft-fails internally on unknown atoms).
			return IxToS(ast)
		},
	)
	setBuiltinDoc(ip, "astParse", `Parse source code into runtime-S (VTArray).

Parses a MindScript source string and returns its AST encoded as a runtime
S-expression (a VTArray whose first element is a tag string).

Params:
	src: Str — MindScript source

Returns:
	[] — a runtime-S AST
	• On lex/parse error this returns an annotated-null runtime-S:
	  ["annot", ["str", <message>], ["null"]]

Notes:
	• The encoding is stable and round-trippable with astEval / reify.`)

	// astEval(ast: []) -> Any
	// Evaluate a runtime-S ([]) AST in the *caller’s environment*.
	// Hard-fails if the AST is malformed or evaluation errors.
	ip.RegisterNative(
		"astEval",
		[]ParamSpec{{Name: "ast", Type: S{"array"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			av := ctx.MustArg("ast")
			if av.Tag != VTArray {
				fail("astEval: ast must be []")
			}
			sexpr, err := IxFromS(av) // strict validation
			if err != nil {
				fail("astEval: invalid AST: " + err.Error())
			}
			// Evaluate in caller scope so effects land where the user expects.
			res, err := ip.EvalAST(sexpr, ctx.Env())
			if err != nil {
				fail(err.Error())
			}
			return res
		},
	)
	setBuiltinDoc(ip, "astEval", `Evaluate a runtime-S AST in the caller's scope.

Validates and evaluates a packed AST (the ["tag", …] encoding) in the
*current* environment.

Params:
	ast: [] — runtime-S AST (see astParse)

Returns:
	Any — the evaluation result

Notes:
	• Effects (let/assignment) occur in the caller's scope.
	• Malformed AST or runtime failures are hard errors; wrap with try(...) to capture.`)

	// reflect(val: Any) -> []
	// Produce *constructor code* (runtime-S) that rebuilds the value when reified.
	// Never throws; unreflectable cases return annotated-null runtime-S.
	ip.RegisterNative(
		"reflect",
		[]ParamSpec{{Name: "val", Type: S{"id", "Any"}}},
		S{"array"},
		func(_ *Interpreter, ctx CallCtx) Value {
			v := ctx.MustArg("val")
			// IxReflect implements soft-encode semantics internally.
			return IxReflect(v)
		},
	)
	setBuiltinDoc(ip, "reflect", `Reflect a value into constructor code (runtime-S).

Returns a program AST (as runtime-S) that, when reified, reconstructs the
given value as closely as possible.

Params:
	val: Any — value to reflect

Returns:
	[] — constructor code (runtime-S)
	• Scalars/arrays/maps → literal nodes
	• Types (VTType) → ["type", <typeAst>]
	• User/oracle functions → ["fun"/"oracle", params, type, body]
	• Native functions → ["id", nativeName] (resolved by host env)
	• Modules → ["module", ["str", name], ("pair", ["id", export], <ctor>)*]
	• Opaque handles/unknowns → annotated-null runtime-S (soft error)

Notes:
	• This is *constructor code*, not a snapshot; closures do not capture lexical env.`)

	// reify(rt: []) -> Any
	// Decode a runtime-S program and evaluate it (persistent/global).
	// Hard-fails on malformed input, module capsules (no installer here),
	// any handle constructs, or runtime evaluation errors.
	ip.RegisterNative(
		"reify",
		[]ParamSpec{{Name: "rt", Type: S{"array"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			rt := ctx.MustArg("rt")
			if rt.Tag != VTArray {
				fail("reify: rt must be []")
			}
			val, err := ip.IxReify(rt) // decode + EvalPersistent
			if err != nil {
				fail(err.Error())
			}
			return val
		},
	)
	setBuiltinDoc(ip, "reify", `Decode and evaluate constructor code (runtime-S).

Validates a runtime-S program and evaluates it in the host (persistent/global)
environment, equivalent to EvalPersistent.

Params:
	rt: [] — constructor code produced by reflect(...) or astParse(...)

Returns:
	Any — the constructed value

Hard errors:
	• Malformed runtime-S input
	• Module capsules (installer not provided here)
	• Handle constructs
	• Runtime evaluation failures (use try(...) to capture)`)
}
