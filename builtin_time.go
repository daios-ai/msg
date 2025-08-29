// time_builtins.go
//
// Builtins surfaced:
//  1. nowMillis() -> Int
//  2. nowNanos() -> Int
//  3. sleep(ms: Int) -> Null
//  4. dateNow() -> { year, month, day, hour, minute, second, millisecond }
//  5. timeFormatRFC3339(millis: Int) -> Str
//  6. timeParseRFC3339(s: Str) -> Int?
//
// Conventions:
//   - Functions are camelCase; docs are docstring-style (first line, blank, details).
//   - Uses public API only; hard errors via fail(...); soft errors via annotated nulls.
//   - Tabs for indentation.
package mindscript

import (
	"time"
)

func registerTimeBuiltins(ip *Interpreter) {
	// nowMillis() -> Int
	// Current wall-clock time in milliseconds since the Unix epoch.
	ip.RegisterNative("nowMillis", nil, S{"id", "Int"}, func(_ *Interpreter, _ CallCtx) Value {
		return Int(time.Now().UnixMilli())
	})
	setBuiltinDoc(ip, "nowMillis", `Current wall-clock time in milliseconds since the Unix epoch.

Returns:
	Int`)

	// nowNanos() -> Int
	// Current wall-clock time in nanoseconds since the Unix epoch.
	ip.RegisterNative("nowNanos", nil, S{"id", "Int"}, func(_ *Interpreter, _ CallCtx) Value {
		return Int(time.Now().UnixNano())
	})
	setBuiltinDoc(ip, "nowNanos", `Current wall-clock time in nanoseconds since the Unix epoch.

Returns:
	Int`)

	// sleep(ms: Int) -> Null
	// Pause execution for a number of milliseconds.
	ip.RegisterNative(
		"sleep",
		[]ParamSpec{{Name: "ms", Type: S{"id", "Int"}}},
		S{"id", "Null"},
		func(_ *Interpreter, ctx CallCtx) Value {
			ms := ctx.MustArg("ms").Data.(int64)
			time.Sleep(time.Duration(ms) * time.Millisecond)
			return Null
		},
	)
	setBuiltinDoc(ip, "sleep", `Pause execution for a number of milliseconds.

Params:
	ms: Int — milliseconds to sleep

Returns:
	Null`)

	// dateNow() -> { year, month, day, hour, minute, second, millisecond }
	// Current local date/time components.
	ip.RegisterNative(
		"dateNow",
		nil,
		S{"map"}, // open-world object: {}
		func(_ *Interpreter, _ CallCtx) Value {
			now := time.Now()
			mo := &MapObject{
				Entries: map[string]Value{
					"year":        Int(int64(now.Year())),
					"month":       Int(int64(int(now.Month()))), // 1–12
					"day":         Int(int64(now.Day())),
					"hour":        Int(int64(now.Hour())),
					"minute":      Int(int64(now.Minute())),
					"second":      Int(int64(now.Second())),
					"millisecond": Int(int64(now.Nanosecond() / int(time.Millisecond))),
				},
				KeyAnn: map[string]string{},
				Keys: []string{
					"year", "month", "day",
					"hour", "minute", "second", "millisecond",
				},
			}
			return Value{Tag: VTMap, Data: mo}
		},
	)
	setBuiltinDoc(ip, "dateNow", `Current local date/time components.

Fields:
	year, month(1–12), day(1–31),
	hour(0–23), minute(0–59), second(0–59),
	millisecond(0–999)

Returns:
	{Str: Any} — a map with the fields above`)

	// timeFormatRFC3339(millis: Int) -> Str
	// Format a Unix-epoch timestamp (milliseconds) as RFC 3339 (UTC).
	ip.RegisterNative(
		"timeFormatRFC3339",
		[]ParamSpec{{Name: "millis", Type: S{"id", "Int"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			ms := ctx.MustArg("millis").Data.(int64)
			t := time.Unix(0, ms*int64(time.Millisecond)).UTC()
			// RFC3339Nano emits fractional seconds only when needed.
			return Str(t.Format(time.RFC3339Nano))
		},
	)
	setBuiltinDoc(ip, "timeFormatRFC3339", `Format a Unix-epoch timestamp (milliseconds) as RFC 3339 (UTC).

Params:
	millis: Int — milliseconds since the Unix epoch

Returns:
	Str

Notes:
	• Uses UTC and emits fractional seconds only when needed (RFC3339Nano).`)

	// timeParseRFC3339(s: Str) -> Int?
	// Parse an RFC 3339 timestamp into milliseconds since the Unix epoch.
	// Accepts both second-precision and fractional (nano) variants.
	ip.RegisterNative(
		"timeParseRFC3339",
		[]ParamSpec{{Name: "s", Type: S{"id", "Str"}}},
		S{"unop", "?", S{"id", "Int"}}, // Int?
		func(_ *Interpreter, ctx CallCtx) Value {
			s := ctx.MustArg("s").Data.(string)
			if t, err := time.Parse(time.RFC3339Nano, s); err == nil {
				return Int(t.UnixNano() / 1e6)
			}
			if t, err := time.Parse(time.RFC3339, s); err == nil {
				return Int(t.UnixNano() / 1e6)
			} else {
				return annotNull("invalid RFC3339 time: " + err.Error())
			}
		},
	)
	setBuiltinDoc(ip, "timeParseRFC3339", `Parse an RFC 3339 timestamp into milliseconds since the Unix epoch.

Accepts both second-precision (RFC3339) and fractional (RFC3339Nano) forms.

Params:
	s: Str — RFC 3339 timestamp (e.g., "2024-06-18T12:34:56Z")

Returns:
	Int? — milliseconds since the Unix epoch, or null (annotated) on parse error`)
}
