package mindscript

import (
	"math"
	"testing"
	"time"
)

func Test_Builtin_Time_nowMillis_Reasonable(t *testing.T) {
	ip, _ := NewInterpreter()

	before := time.Now().UnixMilli()
	v := evalWithIP(t, ip, `nowMillis()`)
	after := time.Now().UnixMilli()

	if v.Tag != VTInt {
		t.Fatalf("nowMillis should return Int, got %#v", v)
	}
	ms := v.Data.(int64)

	// Allow a generous window for scheduling jitter.
	if ms < before-2000 || ms > after+2000 {
		t.Fatalf("nowMillis out of expected range: got %d, want between [%d, %d] ±2s", ms, before, after)
	}
}

func Test_Builtin_Time_nowNanos_Reasonable(t *testing.T) {
	ip, _ := NewInterpreter()

	before := time.Now().UnixNano()
	v := evalWithIP(t, ip, `nowNanos()`)
	after := time.Now().UnixNano()

	if v.Tag != VTInt {
		t.Fatalf("nowNanos should return Int, got %#v", v)
	}
	ns := v.Data.(int64)

	// Allow a generous window for scheduling jitter.
	if ns < before-5_000_000_000 || ns > after+5_000_000_000 {
		t.Fatalf("nowNanos out of expected range: got %d, want between [%d, %d] ±5s", ns, before, after)
	}
}

func Test_Builtin_Time_nowNanos_ConsistentWithMillis(t *testing.T) {
	ip, _ := NewInterpreter()

	vms := evalWithIP(t, ip, `nowMillis()`)
	vns := evalWithIP(t, ip, `nowNanos()`)

	if vms.Tag != VTInt || vns.Tag != VTInt {
		t.Fatalf("expected Int results, got ms=%#v ns=%#v", vms, vns)
	}
	ms := float64(vms.Data.(int64))
	ns := float64(vns.Data.(int64))

	diffMs := math.Abs(ns/1e6 - ms)
	if diffMs > 5000 { // 5s tolerance
		t.Fatalf("nowNanos and nowMillis differ too much: %.2f ms", diffMs)
	}
}

func Test_Builtin_Time_sleep_DelaysAtLeast(t *testing.T) {
	ip, _ := NewInterpreter()

	start := time.Now()
	_ = evalWithIP(t, ip, `sleep(50)`) // 50 ms
	elapsed := time.Since(start)

	if elapsed < 45*time.Millisecond {
		t.Fatalf("sleep(50) elapsed %v, want at least ~45ms", elapsed)
	}
}

func Test_Builtin_Time_dateNow_ShapeAndRanges(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `dateNow()`)
	m := mustMap(t, v)

	yearV, ok := mget(m, "year")
	if !ok || yearV.Tag != VTInt {
		t.Fatalf("year missing/int")
	}
	monthV, ok := mget(m, "month")
	if !ok || monthV.Tag != VTInt {
		t.Fatalf("month missing/int")
	}
	dayV, ok := mget(m, "day")
	if !ok || dayV.Tag != VTInt {
		t.Fatalf("day missing/int")
	}
	hourV, ok := mget(m, "hour")
	if !ok || hourV.Tag != VTInt {
		t.Fatalf("hour missing/int")
	}
	minV, ok := mget(m, "minute")
	if !ok || minV.Tag != VTInt {
		t.Fatalf("minute missing/int")
	}
	secV, ok := mget(m, "second")
	if !ok || secV.Tag != VTInt {
		t.Fatalf("second missing/int")
	}
	msV, ok := mget(m, "millisecond")
	if !ok || msV.Tag != VTInt {
		t.Fatalf("millisecond missing/int")
	}

	year := yearV.Data.(int64)
	month := monthV.Data.(int64)
	day := dayV.Data.(int64)
	hour := hourV.Data.(int64)
	min := minV.Data.(int64)
	sec := secV.Data.(int64)
	ms := msV.Data.(int64)

	if year < 2000 || year > 3000 {
		t.Fatalf("unexpected year: %d", year)
	}
	if month < 1 || month > 12 {
		t.Fatalf("unexpected month: %d", month)
	}
	if day < 1 || day > 31 {
		t.Fatalf("unexpected day: %d", day)
	}
	if hour < 0 || hour > 23 {
		t.Fatalf("unexpected hour: %d", hour)
	}
	if min < 0 || min > 59 {
		t.Fatalf("unexpected minute: %d", min)
	}
	if sec < 0 || sec > 59 {
		t.Fatalf("unexpected second: %d", sec)
	}
	if ms < 0 || ms > 999 {
		t.Fatalf("unexpected millisecond: %d", ms)
	}
}

func Test_Builtin_Time_timeFormatRFC3339_KnownEpoch(t *testing.T) {
	ip, _ := NewInterpreter()

	// 2021-01-01T00:00:00Z in milliseconds
	v := evalWithIP(t, ip, `timeFormatRFC3339(1609459200000)`)
	if v.Tag != VTStr {
		t.Fatalf("timeFormatRFC3339 should return Str, got %#v", v)
	}
	got := v.Data.(string)
	want := "2021-01-01T00:00:00Z"
	if got != want {
		t.Fatalf("unexpected RFC3339: got %q, want %q", got, want)
	}
}

func Test_Builtin_Time_timeFormatParse_Roundtrip(t *testing.T) {
	ip, _ := NewInterpreter()

	// Take a timestamp with a non-zero millisecond to verify the fraction survives.
	msNow := time.Now().UnixMilli()
	code := `
		let ms = ` + Int(msNow).String() + `
		let s = timeFormatRFC3339(ms)
		timeParseRFC3339(s)
	`
	v := evalWithIP(t, ip, code)
	if v.Tag != VTInt {
		t.Fatalf("roundtrip should return Int, got %#v", v)
	}
	ms2 := v.Data.(int64)
	if ms2 != msNow {
		t.Fatalf("roundtrip mismatch: got %d, want %d", ms2, msNow)
	}
}

func Test_Builtin_Time_timeParseRFC3339_Fractional(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `timeParseRFC3339("2021-01-01T00:00:00.123Z")`)
	if v.Tag != VTInt {
		t.Fatalf("expected Int, got %#v", v)
	}
	if v.Data.(int64) != 1609459200123 {
		t.Fatalf("unexpected millis: got %d, want 1609459200123", v.Data.(int64))
	}
}

func Test_Builtin_Time_timeParseRFC3339_Invalid(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `timeParseRFC3339("not-a-time")`)
	wantAnnotatedContains(t, v, "invalid rfc3339")
}
