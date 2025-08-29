package mindscript

// func registerOracleBuiltins(ip *Interpreter) {

// 	// isOracle(f: Any) -> Bool
// 	ip.RegisterNative(
// 		"isOracle",
// 		[]ParamSpec{{Name: "f", Type: S{"id", "Any"}}},
// 		S{"id", "Bool"},
// 		func(ip *Interpreter, ctx CallCtx) Value {
// 			f := ctx.MustArg("f")
// 			if f.Tag != VTFun {
// 				return Bool(false)
// 			}
// 			return Bool(f.Data.(*Fun).IsOracle)
// 		},
// 	)

// 	// oracleInfo(f: Any) -> { isOracle: Bool, prompt?: Str, examples?: [[Any,Any]], params: [Type], return: Type, note?: Str }
// 	ip.RegisterNative(
// 		"oracleInfo",
// 		[]ParamSpec{{Name: "f", Type: S{"id", "Any"}}},
// 		S{"id", "Any"},
// 		func(ip *Interpreter, ctx CallCtx) Value {
// 			fv := ctx.MustArg("f")
// 			if fv.Tag != VTFun {
// 				return annotNull("not a function")
// 			}
// 			fn := fv.Data.(*Fun)
// 			m := map[string]Value{
// 				"isOracle": Bool(fn.IsOracle),
// 				"params":   Arr(typesToValues(fn.ParamTypes)),
// 				"return":   TypeVal(fn.ReturnType),
// 			}
// 			if fn.IsOracle {
// 				if fn.Prompt != "" {
// 					m["prompt"] = Str(fn.Prompt)
// 				}
// 				if fv.Annot != "" {
// 					m["note"] = Str(fv.Annot)
// 				}
// 				if len(fn.Examples) > 0 {
// 					ex := make([]Value, 0, len(fn.Examples))
// 					for _, p := range fn.Examples {
// 						ex = append(ex, Arr([]Value{cloneValue(p[0]), cloneValue(p[1])}))
// 					}
// 					m["examples"] = Arr(ex)
// 				}
// 			}
// 			return Map(m)
// 		},
// 	)

// 	// oracleAddExample(f: Any, input: Any, output: Any) -> Null
// 	ip.RegisterNative(
// 		"oracleAddExample",
// 		[]ParamSpec{{"f", S{"id", "Any"}}, {"input", S{"id", "Any"}}, {"output", S{"id", "Any"}}},
// 		S{"id", "Null"},
// 		func(ip *Interpreter, ctx CallCtx) Value {
// 			fv := ctx.MustArg("f")
// 			if fv.Tag != VTFun || !fv.Data.(*Fun).IsOracle {
// 				return annotNull("not an oracle")
// 			}
// 			fn := fv.Data.(*Fun)
// 			in := cloneValue(ctx.MustArg("input"))
// 			out := cloneValue(ctx.MustArg("output"))
// 			// validate example conforms to signature
// 			if !validateOracleExample(ip, ctx.Env(), fn, in, out) {
// 				return annotNull("example does not conform to oracle signature")
// 			}
// 			fn.Examples = append(fn.Examples, [2]Value{in, out})
// 			return Null
// 		},
// 	)

// 	// oracleSetExamples(f: Any, ex: [[Any,Any]]) -> Null
// 	ip.RegisterNative(
// 		"oracleSetExamples",
// 		[]ParamSpec{{"f", S{"id", "Any"}}, {"examples", S{"array", S{"array", S{"id", "Any"}}}}},
// 		S{"id", "Null"},
// 		func(ip *Interpreter, ctx CallCtx) Value {
// 			fv := ctx.MustArg("f")
// 			if fv.Tag != VTFun || !fv.Data.(*Fun).IsOracle {
// 				return annotNull("not an oracle")
// 			}
// 			fn := fv.Data.(*Fun)
// 			xs := ctx.MustArg("examples").Data.([]Value)
// 			tmp := make([][2]Value, 0, len(xs))
// 			for _, pair := range xs {
// 				arr := pair.Data.([]Value)
// 				if len(arr) != 2 {
// 					return annotNull("each example must be a pair [input, output]")
// 				}
// 				in, out := cloneValue(arr[0]), cloneValue(arr[1])
// 				if !validateOracleExample(ip, ctx.Env(), fn, in, out) {
// 					return annotNull("example does not conform to oracle signature")
// 				}
// 				tmp = append(tmp, [2]Value{in, out})
// 			}
// 			fn.Examples = tmp
// 			return Null
// 		},
// 	)

// 	// oracleClearExamples(f: Any) -> Null
// 	ip.RegisterNative(
// 		"oracleClearExamples",
// 		[]ParamSpec{{"f", S{"id", "Any"}}},
// 		S{"id", "Null"},
// 		func(ip *Interpreter, ctx CallCtx) Value {
// 			fv := ctx.MustArg("f")
// 			if fv.Tag != VTFun || !fv.Data.(*Fun).IsOracle {
// 				return annotNull("not an oracle")
// 			}
// 			fv.Data.(*Fun).Examples = nil
// 			return Null
// 		},
// 	)

// 	// oracleGetPrompt/SetPrompt(f: Any, prompt?: Str)  -> Str|Null
// 	ip.RegisterNative(
// 		"oracleGetPrompt",
// 		[]ParamSpec{{"f", S{"id", "Any"}}},
// 		S{"unop", "?", S{"id", "Str"}},
// 		func(ip *Interpreter, ctx CallCtx) Value {
// 			fv := ctx.MustArg("f")
// 			if fv.Tag != VTFun || !fv.Data.(*Fun).IsOracle {
// 				return Null
// 			}
// 			p := fv.Data.(*Fun).Prompt
// 			if p == "" {
// 				return Null
// 			}
// 			return Str(p)
// 		},
// 	)
// 	ip.RegisterNative(
// 		"oracleSetPrompt",
// 		[]ParamSpec{{"f", S{"id", "Any"}}, {"prompt", S{"id", "Str"}}},
// 		S{"id", "Null"},
// 		func(ip *Interpreter, ctx CallCtx) Value {
// 			fv := ctx.MustArg("f")
// 			if fv.Tag != VTFun || !fv.Data.(*Fun).IsOracle {
// 				return annotNull("not an oracle")
// 			}
// 			fv.Data.(*Fun).Prompt = ctx.MustArg("prompt").Data.(string)
// 			return Null
// 		},
// 	)

// 	// --- Global backend controls (interpreter-wide) ---

// 	// oracleProviders() -> [Str]
// 	ip.RegisterNative("oracleProviders", nil, S{"array", S{"id", "Str"}}, func(ip *Interpreter, ctx CallCtx) Value {
// 		names := make([]Value, 0, len(oracleProviders))
// 		for name := range oracleProviders {
// 			names = append(names, Str(name))
// 		}
// 		return Arr(names)
// 	})

// 	// oracleSetProvider(name: Str) -> Null
// 	ip.RegisterNative(
// 		"oracleSetProvider",
// 		[]ParamSpec{{"name", S{"id", "Str"}}},
// 		S{"id", "Null"},
// 		func(ip *Interpreter, ctx CallCtx) Value {
// 			name := ctx.MustArg("name").Data.(string)
// 			p := oracleProviders[name]
// 			if p == nil {
// 				return annotNull("unknown provider: " + name)
// 			}
// 			ip.OracleProv = p
// 			return Null
// 		},
// 	)

// 	// oracleGetProvider() -> Str?
// 	ip.RegisterNative("oracleGetProvider", nil, S{"unop", "?", S{"id", "Str"}}, func(ip *Interpreter, ctx CallCtx) Value {
// 		if ip.OracleProv == nil {
// 			return Null
// 		}
// 		return Str(ip.OracleProv.Name())
// 	})

// 	// oracleSetModel(model: Str) / oracleGetModel() -> Str?
// 	ip.RegisterNative(
// 		"oracleSetModel",
// 		[]ParamSpec{{"model", S{"id", "Str"}}},
// 		S{"id", "Null"},
// 		func(ip *Interpreter, ctx CallCtx) Value {
// 			ip.OracleModel = ctx.MustArg("model").Data.(string)
// 			return Null
// 		},
// 	)
// 	ip.RegisterNative("oracleGetModel", nil, S{"unop", "?", S{"id", "Str"}}, func(ip *Interpreter, ctx CallCtx) Value {
// 		if ip.OracleModel == "" {
// 			return Null
// 		}
// 		return Str(ip.OracleModel)
// 	})

// 	// Optional tunables: temperature/topP/maxTokens/stop
// 	ip.RegisterNative(
// 		"oracleSetTemperature",
// 		[]ParamSpec{{"t", S{"unop", "?", S{"id", "Num"}}}},
// 		S{"id", "Null"},
// 		func(ip *Interpreter, ctx CallCtx) Value {
// 			v := ctx.MustArg("t")
// 			if v.Tag == VTNull {
// 				ip.OracleTemperature = nil
// 				return Null
// 			}
// 			f := v.Data.(float64)
// 			ip.OracleTemperature = &f
// 			return Null
// 		},
// 	)
// 	ip.RegisterNative("oracleGetTemperature", nil, S{"unop", "?", S{"id", "Num"}}, func(ip *Interpreter, ctx CallCtx) Value {
// 		if ip.OracleTemperature == nil {
// 			return Null
// 		}
// 		return Num(*ip.OracleTemperature)
// 	})

// 	ip.RegisterNative(
// 		"oracleSetTopP",
// 		[]ParamSpec{{"p", S{"unop", "?", S{"id", "Num"}}}},
// 		S{"id", "Null"},
// 		func(ip *Interpreter, ctx CallCtx) Value {
// 			v := ctx.MustArg("p")
// 			if v.Tag == VTNull {
// 				ip.OracleTopP = nil
// 				return Null
// 			}
// 			f := v.Data.(float64)
// 			ip.OracleTopP = &f
// 			return Null
// 		},
// 	)
// 	ip.RegisterNative("oracleGetTopP", nil, S{"unop", "?", S{"id", "Num"}}, func(ip *Interpreter, ctx CallCtx) Value {
// 		if ip.OracleTopP == nil {
// 			return Null
// 		}
// 		return Num(*ip.OracleTopP)
// 	})

// 	ip.RegisterNative(
// 		"oracleSetMaxTokens",
// 		[]ParamSpec{{"n", S{"unop", "?", S{"id", "Int"}}}},
// 		S{"id", "Null"},
// 		func(ip *Interpreter, ctx CallCtx) Value {
// 			v := ctx.MustArg("n")
// 			if v.Tag == VTNull {
// 				ip.OracleMaxTokens = nil
// 				return Null
// 			}
// 			n := int(v.Data.(int64))
// 			ip.OracleMaxTokens = &n
// 			return Null
// 		},
// 	)
// 	ip.RegisterNative("oracleGetMaxTokens", nil, S{"unop", "?", S{"id", "Int"}}, func(ip *Interpreter, ctx CallCtx) Value {
// 		if ip.OracleMaxTokens == nil {
// 			return Null
// 		}
// 		return Int(int64(*ip.OracleMaxTokens))
// 	})

// 	ip.RegisterNative(
// 		"oracleSetStop",
// 		[]ParamSpec{{"stops", S{"array", S{"id", "Str"}}}},
// 		S{"id", "Null"},
// 		func(ip *Interpreter, ctx CallCtx) Value {
// 			arr := ctx.MustArg("stops").Data.([]Value)
// 			out := make([]string, len(arr))
// 			for i := range arr {
// 				out[i] = arr[i].Data.(string)
// 			}
// 			ip.OracleStop = out
// 			return Null
// 		},
// 	)
// 	ip.RegisterNative("oracleGetStop", nil, S{"array", S{"id", "Str"}}, func(ip *Interpreter, ctx CallCtx) Value {
// 		xs := make([]Value, len(ip.OracleStop))
// 		for i := range ip.OracleStop {
// 			xs[i] = Str(ip.OracleStop[i])
// 		}
// 		return Arr(xs)
// 	})
// }

// func typesToValues(ts []S) []Value {
// 	out := make([]Value, len(ts))
// 	for i := range ts {
// 		out[i] = TypeVal(ts[i])
// 	}
// 	return out
// }

// func validateOracleExample(ip *Interpreter, env *Env, fn *Fun, in Value, out Value) bool {
// 	// For single-arg or zero-arg oracles the input example is that arg (or {}).
// 	// For multi-arg, input example must be an object with named fields matching params.
// 	switch len(fn.ParamTypes) {
// 	case 0:
// 		// allow {} input or Null (we accept anything; nothing to type-check here)
// 	case 1:
// 		if !ip.isType(in, fn.ParamTypes[0], env) {
// 			return false
// 		}
// 	default:
// 		if in.Tag != VTMap {
// 			return false
// 		}
// 		mo := in.Data.(*MapObject)
// 		for i := range fn.ParamTypes {
// 			name := fn.Params[i]
// 			if name == "" {
// 				name = fmt.Sprintf("arg%d", i)
// 			}
// 			v, ok := mo.Entries[name]
// 			if !ok || !ip.isType(v, fn.ParamTypes[i], env) {
// 				return false
// 			}
// 		}
// 	}
// 	return ip.isType(out, fn.ReturnType, env)
// }
