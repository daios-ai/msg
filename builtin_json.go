// === FILE: builtin_json.go ===
package mindscript

import (
	"encoding/json"
	"fmt"
	"math"
	"strconv"
	"strings"
)

// --- Utilities: time, rand, json --------------------------------------------

func registerJsonBuiltins(ip *Interpreter) {
	ip.RegisterNative(
		"jsonParse",
		[]ParamSpec{{Name: "s", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			sv := ctx.MustArg("s")
			var x any
			if err := json.Unmarshal([]byte(sv.Data.(string)), &x); err != nil {
				// Soft error: invalid JSON text
				return annotNull("invalid JSON: " + err.Error())
			}
			return goJSONToValue(x)
		},
	)
	setBuiltinDoc(ip, "jsonParse", `Parse a JSON string into MindScript values.

Mapping rules:
  • null/bool/number/string map to Null/Bool/Int|Num/Str
  • arrays map to [Any]
  • objects map to {Str: Any}
  • integral JSON numbers become Int; other numbers become Num

Params:
  s: Str — JSON text

Returns:
  Any`)

	// jsonStringify(x: Any) -> Str
	// jsonStringify(x: Any) -> Str?
	ip.RegisterNative(
		"jsonStringify",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			xv := ctx.MustArg("x")
			gv, err := valueToGoJSON(xv)
			if err != nil {
				return annotNull("json stringify: " + err.Error())
			}
			b, err := json.Marshal(gv)
			if err != nil {
				// Soft error: value cannot be represented in JSON (e.g., NaN/Inf)
				return annotNull("json stringify: " + err.Error())
			}
			return Str(string(b))
		},
	)

	setBuiltinDoc(ip, "jsonStringify", `Serialize a value to a compact JSON string.

Arrays and maps are emitted as JSON arrays/objects. Object key order is not
guaranteed.

Params:
  x: Any

Returns:
  Str`)

	// typeToJSONSchema(t: Type) -> Any
	ip.RegisterNative(
		"typeToJSONSchema",
		[]ParamSpec{{Name: "t", Type: S{"id", "Type"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			tv := ctx.MustArg("t") // VTType value
			js := ip.TypeValueToJSONSchema(tv, ctx.Env())
			return goJSONToValue(js)
		},
	)
	setBuiltinDoc(ip, "typeToJSONSchema", `Convert a MindScript Type to a JSON Schema object.

Params:
  t: Type

Returns:
  Any — JSON Schema as a map/array structure (use jsonStringify to serialize)`)

	// jsonSchemaToType(schema: Any) -> Type?   (soft-error on unsupported schema)
	ip.RegisterNative(
		"jsonSchemaToType",
		[]ParamSpec{{Name: "schema", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Type"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			gv, err := valueToGoJSON(ctx.MustArg("schema"))
			if err != nil {
				return annotNull("json schema: " + err.Error())
			}
			doc, ok := gv.(map[string]any)
			if !ok {
				// Not a JSON object: widen gracefully (soft behavior rather than hard failure)
				return TypeVal(S{"id", "Any"})
			}

			// Convert the root to a Type value (keeps top-level description in Annot).
			tv := ip.JSONSchemaToTypeValue(doc)

			// Import $defs/definitions into the current environment as aliases.
			importDefs := func(defs map[string]any) {
				for name, defRaw := range defs {
					if defObj, ok := defRaw.(map[string]any); ok {
						s := ip.schemaNodeToMSType(defObj, doc, map[string]bool{})
						ctx.Env().Define(name, TypeVal(s))
					}
				}
			}
			if dm, ok := doc["$defs"].(map[string]any); ok {
				importDefs(dm)
			}
			if dm, ok := doc["definitions"].(map[string]any); ok {
				importDefs(dm)
			}

			return tv
		},
	)
	setBuiltinDoc(ip, "jsonSchemaToType", `Convert a JSON Schema object to a MindScript Type.

Notes:
  • Same-document $ref and common keywords are handled.
  • Unsupported constructs widen to Any.
  • "$defs"/"definitions" are imported into the current scope as type aliases.

Params:
  schema: Any — JSON object (e.g., from jsonParse)

Returns:
  Type?`)

	// typeStringToJSONSchema(src: Str) -> Any
	ip.RegisterNative(
		"typeStringToJSONSchema",
		[]ParamSpec{{Name: "src", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			src := ctx.MustArg("src").Data.(string)
			s, err := TypeStringToS(src)
			if err != nil {
				// Soft error: parse failure
				return annotNull(err.Error())
			}
			// Convert **value-centrically** to ensure annotations propagate.
			js := ip.TypeValueToJSONSchema(TypeVal(s), ctx.Env())
			return goJSONToValue(js)
		},
	)
	setBuiltinDoc(ip, "typeStringToJSONSchema", `Parse a MindScript type string and convert it to JSON Schema.

Params:
  src: Str — a single type expression (annotations map to "description")

Returns:
  Any — JSON Schema object`)

	// jsonSchemaStringToType(src: Str) -> Type
	ip.RegisterNative(
		"jsonSchemaStringToType",
		[]ParamSpec{{Name: "src", Type: S{"id", "Str"}}},
		S{"id", "Type"},
		func(ip *Interpreter, ctx CallCtx) Value {
			src := ctx.MustArg("src").Data.(string)
			doc, err := JSONSchemaStringToObject(src)
			if err != nil {
				// Soft error: invalid JSON schema text
				return annotNull(err.Error())
			}

			// Convert to a **Type value**.
			tv := ip.JSONSchemaToTypeValue(doc)

			// Import $defs/definitions into env (same logic as above).
			importDefs := func(defs map[string]any) {
				for name, defRaw := range defs {
					if defObj, ok := defRaw.(map[string]any); ok {
						s := ip.schemaNodeToMSType(defObj, doc, map[string]bool{})
						ctx.Env().Define(name, TypeVal(s))
					}
				}
			}
			if dm, ok := doc["$defs"].(map[string]any); ok {
				importDefs(dm)
			}
			if dm, ok := doc["definitions"].(map[string]any); ok {
				importDefs(dm)
			}

			return tv
		},
	)
	setBuiltinDoc(ip, "jsonSchemaStringToType", `Parse a JSON Schema string and convert it to a MindScript Type.

Params:
  src: Str — JSON text

Returns:
  Type`)

}

// valueToGoJSON converts a MindScript Value into a Go JSON-able value.
func valueToGoJSON(v Value) (any, error) {
	switch v.Tag {
	case VTNull:
		return nil, nil
	case VTBool:
		return v.Data.(bool), nil
	case VTInt:
		return v.Data.(int64), nil
	case VTNum:
		return v.Data.(float64), nil
	case VTStr:
		return v.Data.(string), nil
	case VTArray:
		xs := v.Data.(*ArrayObject).Elems
		out := make([]any, len(xs))
		for i := range xs {
			el, err := valueToGoJSON(xs[i])
			if err != nil {
				return nil, err
			}
			out[i] = el
		}
		return out, nil
	case VTMap:
		mo := v.Data.(*MapObject)
		out := make(map[string]any, len(mo.Entries))
		// Note: Go's json encoder doesn't preserve map insertion order.
		for _, k := range mo.Keys {
			ev, err := valueToGoJSON(mo.Entries[k])
			if err != nil {
				return nil, err
			}
			out[k] = ev
		}
		return out, nil
	default:
		// Functions, types, handles, modules, etc. are not JSON-serializable here.
		return nil, fmt.Errorf("unsupported value tag %v", v.Tag)
	}
}

// goJSONToValue converts a decoded JSON value into a MindScript Value.
// Handles json.Number (preferred when Decoder.UseNumber() is set) and fallbacks.
func goJSONToValue(x any) Value {
	switch v := x.(type) {
	case nil:
		return Null

	case bool:
		return Bool(v)

	case json.Number:
		// Distinguish Int vs Num using the textual form.
		s := v.String()
		if !strings.ContainsAny(s, ".eE") {
			if i, err := strconv.ParseInt(s, 10, 64); err == nil {
				return Int(i)
			}
			// fall through to float if it doesn't fit in int64
		}
		if f, err := strconv.ParseFloat(s, 64); err == nil {
			if math.IsNaN(f) || math.IsInf(f, 0) {
				return annotNull("json number out of range")
			}
			return Num(f)
		}
		return annotNull("json number parse error")

	case float64:
		// Path when UseNumber() wasn't used.
		if math.IsNaN(v) || math.IsInf(v, 0) {
			return annotNull("json number out of range")
		}
		if v == math.Trunc(v) {
			return Int(int64(v))
		}
		return Num(v)

	case string:
		return Str(v)

	case []any:
		out := make([]Value, len(v))
		for i := range v {
			out[i] = goJSONToValue(v[i])
		}
		return Arr(out)

	case map[string]any:
		entries := make(map[string]Value, len(v))
		keys := make([]string, 0, len(v))
		for k, vv := range v {
			entries[k] = goJSONToValue(vv)
			keys = append(keys, k)
		}
		mo := &MapObject{
			Entries: entries,
			KeyAnn:  map[string]string{},
			Keys:    keys, // insertion order from range is unspecified; fine for open-world maps
		}
		return Value{Tag: VTMap, Data: mo}

	default:
		// Shouldn’t happen with encoding/json, but keep a clear failure mode.
		return annotNull("unsupported JSON value")
	}
}
