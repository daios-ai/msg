// ast_introspection_builtins.go
//
// Builtins surfaced:
//  1. noteGet(x: Any) -> Str?
//  2. noteSet(text: Str, value: Any) -> Any
//  3. bindings(localOnly: Bool?) -> {}
//  4. astParse(src: Str) -> []
//  5. astEval(ast: []) -> Any
//  6. reflect(val: Any) -> []           // constructor code
//  7. reify(rt: []) -> Any              // decode + evaluate (persistent)
//
// Conventions:
//   - Functions are camelCase; docs are docstring-style (first line, blank, details).
//   - Uses public API only; hard errors via fail(...).
//   - Tabs for indentation.
package mindscript

import "sort"

func registerIntrospectionBuiltins(ip *Interpreter) {
	// noteGet(x: Any) -> Str?
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

	// noteSet(text: Str, value: Any) -> Any
	// Attach/replace the annotation on a value and return the annotated value.
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

	// bindings(localOnly: Bool?) -> {}
	// Deterministic order: within each frame keys are sorted ascending; frames
	// are processed inner → outer. First-seen wins (shadowing preserved).
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
			seen := map[string]struct{}{}

			for e := ctx.Env(); e != nil; e = e.parent {
				// Collect frame keys not yet seen, then sort ascending for stable order.
				frameKeys := make([]string, 0, len(e.table))
				for k := range e.table {
					if _, already := seen[k]; !already {
						frameKeys = append(frameKeys, k)
					}
				}
				sort.Strings(frameKeys)
				for _, k := range frameKeys {
					// First-seen wins (inner shadows outer).
					out.Entries[k] = e.table[k]
					out.Keys = append(out.Keys, k)
					seen[k] = struct{}{}
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

Ordering:
	• Within each frame, keys are sorted ascending.
	• Frames are processed inner-to-outer; the first-seen binding for a name wins.

Params:
	localOnly: Bool? — if true, include only the current frame; if false/null, include all visible frames

Returns:
	{} — object map of name → value (shallow copy). Order is stable and suitable for iteration.

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
