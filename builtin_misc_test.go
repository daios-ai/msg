// === FILE: builtin_misc_test.go ===
package mindscript

import (
	"fmt"
	"math"
	"os"
	"os/exec"
	"strings"
	"testing"
)

// approx float helper
func approx(a, b, eps float64) bool {
	if a == b {
		return true
	}
	d := a - b
	if d < 0 {
		d = -d
	}
	return d <= eps
}

func Test_Builtin_Misc_rand_seed_and_randInt_deterministic(t *testing.T) {
	ip, _ := NewRuntime()

	// Produce a pair after a specific seed.
	pair := func(seed int) (int64, int64) {
		v := evalWithIP(t, ip, `
			do
				seedRand(`+strconvI(seed)+`)
				let a = randInt(100)
				let b = randInt(1000)
				[a, b]
			end
		`)
		xs := v.Data.([]Value)
		return xs[0].Data.(int64), xs[1].Data.(int64)
	}

	a1, b1 := pair(123)
	a2, b2 := pair(123)
	if a1 != a2 || b1 != b2 {
		t.Fatalf("determinism failed for seed 123: (%d,%d) vs (%d,%d)", a1, b1, a2, b2)
	}

	c1, d1 := pair(321)
	// With a different seed, very likely different pair.
	if a1 == c1 && b1 == d1 {
		t.Fatalf("different seed produced identical pair: (%d,%d)", c1, d1)
	}
}

func Test_Builtin_Misc_randFloat_range(t *testing.T) {
	ip, _ := NewRuntime()
	v := evalWithIP(t, ip, `randFloat(null)`)
	if v.Tag != VTNum {
		t.Fatalf("randFloat should return Num, got %#v", v)
	}
	f := v.Data.(float64)
	if !(f >= 0.0 && f < 1.0) {
		t.Fatalf("randFloat out of range: %v", f)
	}
}

func Test_Builtin_Misc_str_basic_and_fallback(t *testing.T) {
	ip, _ := NewRuntime()

	// Scalars
	if s := evalWithIP(t, ip, `str(42)`); s.Tag != VTStr || s.Data.(string) != "42" {
		t.Fatalf("str(42) => '42', got %#v", s)
	}
	if s := evalWithIP(t, ip, `str(true)`); s.Tag != VTStr || s.Data.(string) != "true" {
		t.Fatalf("str(true) => 'true', got %#v", s)
	}
	if s := evalWithIP(t, ip, `str(null)`); s.Tag != VTStr || s.Data.(string) != "null" {
		t.Fatalf("str(null) => 'null', got %#v", s)
	}

	// Arrays/maps now use the pretty-printer (not JSON)
	if s := evalWithIP(t, ip, `str([1, 2])`); s.Tag != VTStr || s.Data.(string) != "[ 1, 2 ]" {
		t.Fatalf(`str([1,2]) => "[ 1, 2 ]", got %#v`, s)
	}
	s := evalWithIP(t, ip, `str({a:1, b:2})`)
	if s.Tag != VTStr {
		t.Fatalf(`str({a:1,b:2}) should return Str, got %#v`, s)
	}
	got := s.Data.(string)
	if !(strings.HasPrefix(got, "{") && strings.Contains(got, "a: 1") && strings.Contains(got, "b: 2") && strings.HasSuffix(got, "}")) {
		t.Fatalf(`str({a:1,b:2}) => pretty map string, got %q`, got)
	}
}

func Test_Builtin_Misc_int_num_bool_len(t *testing.T) {
	ip, _ := NewRuntime()

	// int
	if v := evalWithIP(t, ip, `int(123)`); v.Tag != VTInt || v.Data.(int64) != 123 {
		t.Fatalf("int(123) => 123, got %#v", v)
	}
	if v := evalWithIP(t, ip, `int(2.9)`); v.Tag != VTInt || v.Data.(int64) != 2 {
		t.Fatalf("int(2.9) => 2, got %#v", v)
	}
	if v := evalWithIP(t, ip, `int("42")`); v.Tag != VTInt || v.Data.(int64) != 42 {
		t.Fatalf(`int("42") => 42, got %#v`, v)
	}
	if v := evalWithIP(t, ip, `int("nope")`); v.Tag != VTNull {
		t.Fatalf(`int("nope") => null, got %#v`, v)
	}

	// num
	if v := evalWithIP(t, ip, `num(2)`); v.Tag != VTNum || v.Data.(float64) != 2.0 {
		t.Fatalf("num(2) => 2.0, got %#v", v)
	}
	if v := evalWithIP(t, ip, `num("1.5")`); v.Tag != VTNum || !approx(v.Data.(float64), 1.5, 1e-12) {
		t.Fatalf(`num("1.5") => 1.5, got %#v`, v)
	}
	if v := evalWithIP(t, ip, `num("nope")`); v.Tag != VTNull {
		t.Fatalf(`num("nope") => null, got %#v`, v)
	}

	// bool truthiness — use a block (no ';') to define f then use it.
	vfun := evalWithIP(t, ip, `
		do
			let f = fun() do 0 end
			bool(f)
		end
	`)
	if vfun.Tag != VTBool || vfun.Data.(bool) != true {
		t.Fatalf("bool(fun) => true, got %#v", vfun)
	}

	// falsey cases
	if v := evalWithIP(t, ip, `bool(null)`); v.Tag != VTBool || v.Data.(bool) != false {
		t.Fatalf("bool(null) => false, got %#v", v)
	}
	if v := evalWithIP(t, ip, `bool(0)`); v.Tag != VTBool || v.Data.(bool) != false {
		t.Fatalf("bool(0) => false, got %#v", v)
	}
	if v := evalWithIP(t, ip, `bool("")`); v.Tag != VTBool || v.Data.(bool) != false {
		t.Fatalf(`bool("") => false, got %#v`, v)
	}
	if v := evalWithIP(t, ip, `bool([])`); v.Tag != VTBool || v.Data.(bool) != false {
		t.Fatalf("bool([]) => false, got %#v", v)
	}
	if v := evalWithIP(t, ip, `bool({})`); v.Tag != VTBool || v.Data.(bool) != false {
		t.Fatalf("bool({}) => false, got %#v", v)
	}

	// len
	if v := evalWithIP(t, ip, `len([10,20,30])`); v.Tag != VTInt || v.Data.(int64) != 3 {
		t.Fatalf("len([..]) => 3, got %#v", v)
	}
	if v := evalWithIP(t, ip, `len({a:1, b:2})`); v.Tag != VTInt || v.Data.(int64) != 2 {
		t.Fatalf("len({a,b}) => 2, got %#v", v)
	}
	if v := evalWithIP(t, ip, `len("😀")`); v.Tag != VTInt || v.Data.(int64) != 1 {
		t.Fatalf(`len("😀") => 1 (runes), got %#v`, v)
	}
	if v := evalWithIP(t, ip, `len(42)`); v.Tag != VTNull {
		t.Fatalf("len(42) => null, got %#v", v)
	}
}

func Test_Builtin_Misc_math_constants_and_funcs(t *testing.T) {
	ip, _ := NewRuntime()

	// constants
	if v := evalWithIP(t, ip, `PI`); v.Tag != VTNum || !approx(v.Data.(float64), math.Pi, 1e-15) {
		t.Fatalf("PI mismatch, got %#v", v)
	}
	if v := evalWithIP(t, ip, `E`); v.Tag != VTNum || !approx(v.Data.(float64), math.E, 1e-15) {
		t.Fatalf("E mismatch, got %#v", v)
	}

	// functions (pass Num args explicitly)
	if v := evalWithIP(t, ip, `sin(PI/2.0)`); v.Tag != VTNum || !approx(v.Data.(float64), 1.0, 1e-12) {
		t.Fatalf("sin(PI/2) ≈ 1.0, got %#v", v)
	}
	if v := evalWithIP(t, ip, `cos(0.0)`); v.Tag != VTNum || !approx(v.Data.(float64), 1.0, 1e-12) {
		t.Fatalf("cos(0) ≈ 1.0, got %#v", v)
	}
	if v := evalWithIP(t, ip, `tan(0.0)`); v.Tag != VTNum || !approx(v.Data.(float64), 0.0, 1e-12) {
		t.Fatalf("tan(0) ≈ 0.0, got %#v", v)
	}
	if v := evalWithIP(t, ip, `sqrt(9.0)`); v.Tag != VTNum || !approx(v.Data.(float64), 3.0, 1e-12) {
		t.Fatalf("sqrt(9) ≈ 3.0, got %#v", v)
	}
	if v := evalWithIP(t, ip, `exp(1.0)`); v.Tag != VTNum || !approx(v.Data.(float64), math.E, 1e-12) {
		t.Fatalf("exp(1) ≈ e, got %#v", v)
	}
	if v := evalWithIP(t, ip, `pow(2.0, 10.0)`); v.Tag != VTNum || !approx(v.Data.(float64), 1024.0, 1e-12) {
		t.Fatalf("pow(2,10) ≈ 1024, got %#v", v)
	}
}

func Test_Builtin_Misc_exit(t *testing.T) {
	// Run a child `go test` process that invokes exit(7) and verify exit code.
	if os.Getenv("MS_EXIT_CHILD") == "1" {
		ip, _ := NewRuntime()
		_, _ = ip.EvalSource(`exit(7)`) // terminates the process with code 7
		t.Fatal("unreachable")          // should never be reached
		return
	}

	cmd := exec.Command(os.Args[0], "-test.run", "Test_Builtin_Misc_exit")
	cmd.Env = append(os.Environ(), "MS_EXIT_CHILD=1")
	err := cmd.Run()

	// We expect an exit error with code 7.
	if ee, ok := err.(*exec.ExitError); ok {
		if ee.ExitCode() != 7 {
			t.Fatalf("child exited with code %d, want 7", ee.ExitCode())
		}
	} else {
		t.Fatalf("expected ExitError from child process, got %T, err=%v", err, err)
	}
}

// --- helpers ---

// strconvI formats an int as a decimal string (avoid importing strconv at top).
func strconvI(n int) string { return fmt.Sprintf("%d", n) }
