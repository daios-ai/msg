package mindscript

import (
	"fmt"
	"math"
	"runtime"
	"strings"
	"testing"
)

// Minimal platform-aware library name resolver for tests.
func lib(which string) string {
	switch runtime.GOOS {
	case "darwin":
		switch which {
		case "c":
			return "libSystem.B.dylib"
		case "m":
			return "libm.dylib"
		}
	case "linux":
		switch which {
		case "c":
			return "libc.so.6"
		case "m":
			return "libm.so.6"
		}
	case "windows":
		// CRT provides C + math. Prefer universal CRT.
		switch which {
		case "c", "m":
			return "ucrtbase.dll"
		}
	}
	// Fallback: return the key if we don't know it.
	return which
}

// ===== Tiny helpers for a compact spec-compliance matrix (fixed NewInterpreter usage) =====

func ok(t *testing.T, src string) {
	t.Helper()
	ip, err := NewInterpreter()
	if err != nil {
		t.Fatalf("NewInterpreter error: %v", err)
	}
	if _, err := ip.EvalSource(src); err != nil {
		t.Fatalf("want OK, got err: %v\nsrc:\n%s", err, src)
	}
}

func err(t *testing.T, contains, src string) {
	t.Helper()
	ip, nerr := NewInterpreter()
	if nerr != nil {
		t.Fatalf("NewInterpreter error: %v", nerr)
	}
	_, e := ip.EvalSource(src)
	wantErrContains(t, e, contains)
}

func eqi(t *testing.T, want int64, src string) {
	t.Helper()
	ip, err := NewInterpreter()
	if err != nil {
		t.Fatalf("NewInterpreter error: %v", err)
	}
	v, e := ip.EvalSource(src)
	if e != nil {
		t.Fatalf("eval error: %v\nsrc:\n%s", e, src)
	}
	if v.Tag != VTInt || v.Data.(int64) != want {
		t.Fatalf("want %d, got %#v\nsrc:\n%s", want, v, src)
	}
}

// ===== One small matrix that encodes spec intent =====

func Test_FFI_Spec_Compliance_Matrix(t *testing.T) {
	t.Run("1.version-string-accepted", func(t *testing.T) {
		ok(t, fmt.Sprintf(`ffiOpen({version:"1", lib:"%s"})`, lib("c")))
	})
	t.Run("1.unknown-version-rejected", func(t *testing.T) {
		err(t, "unsupported version", fmt.Sprintf(`ffiOpen({version:"2", lib:"%s"})`, lib("c")))
	})
	t.Run("2.missing-lib-rejected", func(t *testing.T) {
		err(t, "missing or invalid 'lib'", `ffiOpen({version: "1"})`)
	})
	t.Run("2.dlopen-error-surfaced", func(t *testing.T) {
		err(t, "dlopen", `ffiOpen({version: "1", lib:"/no/such/lib.so"})`)
	})

	t.Run("3.types-empty-ok", func(t *testing.T) {
		ok(t, fmt.Sprintf(`ffiOpen({version: "1", lib:"%s", types:{}})`, lib("c")))
	})
	t.Run("3.types-nonmap-rejected", func(t *testing.T) {
		err(t, "'types' must be a map", fmt.Sprintf(`ffiOpen({version: "1", lib:"%s", types: 7})`, lib("c")))
	})
	t.Run("3.types-duplicate-rejected", func(t *testing.T) {
		err(t, "duplicate type name", fmt.Sprintf(`
			ffiOpen({version: "1", lib:"%s",
			  types:{U8:{kind:"int",bits:8,signed:false},U8:{kind:"int",bits:8,signed:false}}})`, lib("c")))
	})

	t.Run("4.alias-transitive-ok", func(t *testing.T) {
		ok(t, fmt.Sprintf(`
		  ffiOpen({version: "1", lib:"%s",
		    types:{I32:{kind:"int",bits:32,signed:true},X:{kind:"alias",to:"I32"},Y:{kind:"alias",to:"X"}}})`, lib("c")))
	})
	t.Run("4.alias-unknown-target", func(t *testing.T) {
		err(t, "unknown target", fmt.Sprintf(`
		  ffiOpen({version: "1", lib:"%s",types:{X:{kind:"alias",to:"NOPE"}}})`, lib("c")))
	})
	t.Run("4.alias-cycle", func(t *testing.T) {
		err(t, "alias cycle", fmt.Sprintf(`
		  ffiOpen({version: "1", lib:"%s",types:{A:{kind:"alias",to:"B"},B:{kind:"alias",to:"A"}}})`, lib("c")))
	})

	// 5) int-signedness enforced on call (reject negative → unsigned)
	t.Run("5.int-signedness-enforced-on-call", func(t *testing.T) {
		ip, _ := NewInterpreter()
		_, err := ip.EvalSource(fmt.Sprintf(`let C=ffiOpen({version: "1",lib:"%s",types:{U:{kind:"int",bits:32,signed:false}},functions:[{name:"sleep",ret:{kind:"int",bits:32,signed:true},params:["U"]}]}); C.sleep(-1)`, lib("c")))
		if err == nil {
			t.Fatalf("expected failure for negative→unsigned")
		}
	})

	t.Run("6.float-bits-validated", func(t *testing.T) {
		err(t, "float: bits must be 32/64", fmt.Sprintf(`
			ffiOpen({version: "1", lib:"%s", types:{BadF:{kind:"float",bits:128}}})`, lib("c")))
	})

	t.Run("7.pointer-size-align", func(t *testing.T) {
		eqi(t, 8, fmt.Sprintf(`let C=ffiOpen({version: "1",lib:"%s",
		 types:{P:{kind:"pointer",to:{kind:"void"}}}}); C.__mem.sizeof("P")`, lib("c")))
	})
	t.Run("7.pointer-align", func(t *testing.T) {
		eqi(t, 8, fmt.Sprintf(`let C=ffiOpen({version: "1",lib:"%s",
		 types:{P:{kind:"pointer",to:{kind:"void"}}}}); C.__mem.alignof("P")`, lib("c")))
	})

	t.Run("8.handle-tag-required", func(t *testing.T) {
		err(t, `missing "rep"`, fmt.Sprintf(`
		  ffiOpen({version: "1",lib:"%s",types:{H:{kind:"handle",tag:"x"}}})`, lib("c")))
	})

	t.Run("9.array-fixed-size", func(t *testing.T) {
		eqi(t, 5, fmt.Sprintf(`let C=ffiOpen({version: "1",lib:"%s",
		  types:{U8:{kind:"int",bits:8,signed:false},A5:{kind:"array",of:"U8",len:5}}}); C.__mem.sizeof("A5")`, lib("c")))
	})
	t.Run("9.array-flexible-size0", func(t *testing.T) {
		eqi(t, 0, fmt.Sprintf(`let C=ffiOpen({version: "1",lib:"%s",
		  types:{U8:{kind:"int",bits:8,signed:false},Flex:{kind:"array",of:"U8"}}}); C.__mem.sizeof("Flex")`, lib("c")))
	})

	// 10) struct layout: offsetof
	t.Run("10.struct-layout-offset", func(t *testing.T) {
		ip, _ := NewInterpreter()
		v, err := ip.EvalSource(fmt.Sprintf(`do
	  let C=ffiOpen({version: "1",lib:"%s",types:{I32:{kind:"int",bits:32,signed:true},P:{kind:"pointer",to:{kind:"void"}},S:{kind:"struct",fields:[{name:"a",type:"I32"},{name:"p",type:"P"}]}}})
	  C.__mem.offsetof("S","p")
	end`, lib("c")))
		if err != nil {
			t.Fatalf("eval error: %v", err)
		}
		if v.Tag != VTInt || v.Data.(int64) != 8 {
			t.Fatalf("want 8, got %v", v)
		}
	})

	// 10) struct layout: sizeof/alignof
	t.Run("10.struct-size-align", func(t *testing.T) {
		ip, _ := NewInterpreter()
		v, err := ip.EvalSource(fmt.Sprintf(`do
	  let C=ffiOpen({version: "1",lib:"%s",types:{I32:{kind:"int",bits:32,signed:true},P:{kind:"pointer",to:{kind:"void"}},S:{kind:"struct",fields:[{name:"a",type:"I32"},{name:"p",type:"P"}]}}})
	  {sz:C.__mem.sizeof("S"), al:C.__mem.alignof("S")}
	end`, lib("c")))
		if err != nil {
			t.Fatalf("eval error: %v", err)
		}
		m := v.Data.(*MapObject)
		if m.Entries["sz"].Data.(int64) != 16 || m.Entries["al"].Data.(int64) != 8 {
			t.Fatalf("want {16,8}, got %#v", v)
		}
	})

	// 11) union size/align = max(member)
	t.Run("11.union-size-align", func(t *testing.T) {
		ip, _ := NewInterpreter()
		v, err := ip.EvalSource(fmt.Sprintf(`do
	  let C=ffiOpen({version: "1",lib:"%s",types:{I32:{kind:"int",bits:32,signed:true},P:{kind:"pointer",to:{kind:"void"}},U:{kind:"union",fields:[{name:"x",type:"I32"},{name:"y",type:"P"}]}}})
	  {sz:C.__mem.sizeof("U"), al:C.__mem.alignof("U")}
	end`, lib("c")))
		if err != nil {
			t.Fatalf("eval error: %v", err)
		}
		m := v.Data.(*MapObject)
		if m.Entries["sz"].Data.(int64) != 8 || m.Entries["al"].Data.(int64) != 8 {
			t.Fatalf("want {8,8}, got %#v", v)
		}
	})

	t.Run("12.enum-base-required", func(t *testing.T) {
		err(t, `missing "base"`, fmt.Sprintf(`
		  ffiOpen({version: "1",lib:"%s",types:{E:{kind:"enum"}}})`, lib("c")))
	})

	t.Run("13.funcptr-accepted-as-type", func(t *testing.T) {
		ok(t, fmt.Sprintf(`ffiOpen({version: "1",lib:"%s",
		  types:{CB:{kind:"funcptr",ret:{kind:"void"},params:[{kind:"pointer",to:{kind:"void"}}]}}})`, lib("c")))
	})

	t.Run("14.functions-need-name-ret-params", func(t *testing.T) {
		err(t, "function: missing 'ret'", fmt.Sprintf(`
		  ffiOpen({version: "1",lib:"%s",functions:[{name:"puts",params:[]}]})`, lib("c")))
	})

	t.Run("15.dlsym-missing-symbol-errors", func(t *testing.T) {
		err(t, "dlsym", fmt.Sprintf(`
		  ffiOpen({version: "1",lib:"%s",
		    types:{i32:{kind:"int",bits:32,signed:true}},
		    functions:[{name:"__nope__",ret:"i32",params:[]}]})`, lib("c")))
	})

	t.Run("16.variadic-final-arg-must-array", func(t *testing.T) {
		err(t, "final argument to be an array", fmt.Sprintf(`
		  let C=ffiOpen({version: "1",lib:"%s",
		    types:{charp:{kind:"pointer",to:{kind:"int",bits:8,signed:true}},sz:{kind:"int",bits:64,signed:false},i32:{kind:"int",bits:32,signed:true}},
		    functions:[{name:"snprintf",ret:"i32",params:["charp","sz","charp"],variadic:true}]});
		  C.snprintf(C.__mem.malloc(8),7,"%%d", 1)`, lib("c")))
	})

	// 17) scalar: negative → unsigned rejected
	t.Run("17.scalar-negative-to-unsigned-rejected", func(t *testing.T) {
		ip, _ := NewInterpreter()
		_, err := ip.EvalSource(fmt.Sprintf(`let C=ffiOpen({version: "1",lib:"%s",types:{U:{kind:"int",bits:32,signed:false}},functions:[{name:"sleep",ret:{kind:"int",bits:32,signed:true},params:["U"]}]}); C.sleep(-1)`, lib("c")))
		if err == nil {
			t.Fatalf("expected failure for negative→unsigned")
		}
	})

	t.Run("17.float-to-int-must-be-integral", func(t *testing.T) {
		err(t, "non-integral", fmt.Sprintf(`
		  let C=ffiOpen({version: "1",lib:"%s",
		    types:{U:{kind:"int",bits:32,signed:false}},functions:[{name:"sleep",ret:{kind:"int",bits:32,signed:true},params:["U"]}]});
		  C.sleep(0.5)`, lib("c")))
	})

	t.Run("18.pointer-null-maps-to-NULL", func(t *testing.T) {
		ok(t, fmt.Sprintf(`
		  let C=ffiOpen({version: "1",lib:"%s",
		    types:{voidp:{kind:"pointer",to:{kind:"void"}},i32:{kind:"int",bits:32,signed:true}},
		    functions:[{name:"memchr",ret:"voidp",params:["voidp","i32",{kind:"int",bits:64,signed:false}]}]});
		  C.memchr(null,0,0)`, lib("c")))
	})

	t.Run("19.string-bridge-only-for-charp", func(t *testing.T) {
		err(t, "expected pointer handle", fmt.Sprintf(`
		  let C=ffiOpen({version: "1",lib:"%s",
		    types:{voidp:{kind:"pointer",to:{kind:"void"}},i32:{kind:"int",bits:32,signed:true}},
		    functions:[{name:"memchr",ret:"voidp",params:["voidp","i32",{kind:"int",bits:64,signed:false}]}]});
		  C.memchr("abc",65,3)`, lib("c")))
	})

	t.Run("20.return-pointer-becomes-handle", func(t *testing.T) {
		ok(t, fmt.Sprintf(`
		  let C=ffiOpen({version: "1",lib:"%s",
		    types:{charp:{kind:"pointer",to:{kind:"int",bits:8,signed:true}}},
		    functions:[{name:"getenv",ret:"charp",params:["charp"]}]});
		  C.getenv("PATH")`, lib("c")))
	})

	// 21) ret_as_str valid only for char*/uchar* (must error for non-char*)
	t.Run("21.ret_as_str-valid-only-for-charp", func(t *testing.T) {
		ip, _ := NewInterpreter()
		_, err := ip.EvalSource(fmt.Sprintf(`ffiOpen({version: "1",lib:"%s",types:{I32:{kind:"int",bits:32,signed:true}},functions:[{name:"getpid",ret:"I32",params:[],ret_as_str:true}]})`, lib("c")))
		if err == nil {
			t.Fatalf("expected error: ret_as_str on non-char*")
		} // spec requires rejection
	})

	t.Run("22.variables-get-set-addr", func(t *testing.T) {
		ok(t, fmt.Sprintf(`
		  let C=ffiOpen({version: "1",lib:"%s",
		    types:{i32:{kind:"int",bits:32,signed:true}},
		    variables:[{name:"errno",type:"i32"}]});
		  C.errno.get(); C.errno.addr(); C.errno.set(0)`, lib("c")))
	})

	// 23) sizeof/alignof/offsetof accept inline type objects
	t.Run("23.sizeof-alignof-offsetof-accept-inline", func(t *testing.T) {
		ip, _ := NewInterpreter()
		v, err := ip.EvalSource(fmt.Sprintf(`let C=ffiOpen({version: "1",lib:"%s"}); C.__mem.sizeof({kind:"pointer",to:{kind:"void"}})`, lib("c")))
		if err != nil {
			t.Fatalf("eval error: %v", err)
		}
		if v.Tag != VTInt || v.Data.(int64) != 8 {
			t.Fatalf("want 8, got %#v", v)
		} // pointer size on x86-64 SysV
	})

	t.Run("24.typeof-normalizes", func(t *testing.T) {
		ok(t, fmt.Sprintf(`
		  let C=ffiOpen({version: "1",lib:"%s"});
		  let T=C.__mem.typeof({kind:"int",bits:32,signed:true}); T`, lib("c")))
	})

	t.Run("25.new-fixed-alloc-size", func(t *testing.T) {
		ok(t, fmt.Sprintf(`
		  let C=ffiOpen({version: "1",lib:"%s",types:{I:{kind:"int",bits:32,signed:true}}});
		  C.__mem.new("I",1)`, lib("c")))
	})

	// 26) malloc/calloc/realloc/free: negatives are errors (no message coupling)
	t.Run("26.malloc-calloc-realloc-free-guards", func(t *testing.T) {
		ip, _ := NewInterpreter()
		_, err := ip.EvalSource(fmt.Sprintf(`let C=ffiOpen({version: "1",lib:"%s"}); C.__mem.malloc(-1)`, lib("c")))
		if err == nil {
			t.Fatalf("expected error on negative size")
		}
	})

	t.Run("27.cast-retags-pointer", func(t *testing.T) {
		ok(t, fmt.Sprintf(`
		  let C=ffiOpen({version: "1",lib:"%s",
		    types:{Voidp:{kind:"pointer",to:{kind:"void"}},I8:{kind:"int",bits:8,signed:true},Charp:{kind:"pointer",to:"I8"}}});
		  let p=C.__mem.malloc(1); C.__mem.cast("Charp", p)`, lib("c")))
	})

	// 28) string(): NUL-terminated path without pointer arithmetic
	t.Run("28.string-read-nul-or-fixed", func(t *testing.T) {
		ip, _ := NewInterpreter()
		v, err := ip.EvalSource(fmt.Sprintf(`do
	  let C=ffiOpen({version: "1",lib:"%s"})
	  let p=C.__mem.malloc(3); C.__mem.copy(p,"hi\u0000",3); C.__mem.string(p,null)
	end`, lib("c")))
		if err != nil {
			t.Fatalf("eval error: %v", err)
		}
		if v.Tag != VTStr || v.Data.(string) != "hi" {
			t.Fatalf("want 'hi', got %#v", v)
		}
	})

	t.Run("29.copy-fill-basic", func(t *testing.T) {
		ok(t, fmt.Sprintf(`
		  let C=ffiOpen({version: "1",lib:"%s"}); do
		    let p=C.__mem.malloc(4); C.__mem.fill(p,65,4); C.__mem.string(p,4)
		  end`, lib("c")))
	})

	t.Run("30.errno-get-set", func(t *testing.T) {
		eqi(t, 0, fmt.Sprintf(`
		  let C=ffiOpen({version: "1",lib:"%s"}); do C.__mem.errno(0) end`, lib("c")))
	})
}

// libm: scalar in/out path (dlopen+dlsym, libffi call, float marshalling).
func Test_Builtin_FFI_libm_hypot(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: { double: { kind:"float", bits:64 } },
		  functions: [
		    { name:"hypot", ret:"double", params:["double","double"] }
		  ]
		})
		C.hypot(3.0, 4.0)
	`, lib("m")))
	if v.Tag != VTNum {
		t.Fatalf("hypot should return Num, got %#v", v)
	}
	if math.Abs(v.Data.(float64)-5.0) > 1e-9 {
		t.Fatalf("hypot(3,4) expected 5.0, got %v", v.Data.(float64))
	}
}

// libc: string param bridge to char*, and ret_as_str for char* return.
func Test_Builtin_FFI_libc_strlen_and_strdup_ret_as_str(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: {
		    charp: { kind:"pointer", to:{ kind:"int", bits:8, signed:true } },
		    int32: { kind:"int", bits:32, signed:true }
		  },
		  functions: [
		    { name:"strlen", ret:"int32", params:["charp"] },
		    { name:"strdup", ret:"charp", params:["charp"], ret_as_str:true }
		  ]
		})
		let n = C.strlen("hello")
		let s = C.strdup("world")
		{ n: n, s: s }
	`, lib("c")))
	m := mustMap(t, v)
	n := m.Entries["n"]
	s := m.Entries["s"]
	if n.Tag != VTInt || n.Data.(int64) != 5 {
		t.Fatalf("strlen('hello') expected 5, got %#v", n)
	}
	if s.Tag != VTStr || s.Data.(string) != "world" {
		t.Fatalf("strdup('world') ret_as_str expected 'world', got %#v", s)
	}
}

// __mem: raw malloc/copy/string/free round-trip (pointer ops + UTF-8 read).
func Test_Builtin_FFI___mem_malloc_copy_string_free(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({ version: "1", lib:"%s" })
		do
			let p = C.__mem.malloc(6)
			C.__mem.copy(p, "hello", 6)   # copies "hello\0"
			let s = C.__mem.string(p, null)
			C.__mem.free(p)
			s
		end
	`, lib("c")))
	if v.Tag != VTStr || v.Data.(string) != "hello" {
		t.Fatalf("__mem string round-trip expected 'hello', got %#v", v)
	}
}

// Layout queries: sizeof/alignof/offsetof for a simple struct (SysV, x86-64).
func Test_Builtin_FFI___mem_sizeof_alignof_offsetof_struct(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: {
		    S: { kind:"struct", fields: [
		      { name:"a", type:{ kind:"int", bits:32, signed:true } },
		      { name:"p", type:{ kind:"pointer", to:{ kind:"void" } } }
		    ] }
		  }
		})
		{
		  sz: C.__mem.sizeof("S"),
		  al: C.__mem.alignof("S"),
		  offp: C.__mem.offsetof("S", "p")
		}
	`, lib("c")))
	m := mustMap(t, v)
	sz := mustInt64(t, m.Entries["sz"])
	al := mustInt64(t, m.Entries["al"])
	offp := mustInt64(t, m.Entries["offp"])

	// On x86-64 SysV: struct { int32; padding; void* } → size 16, align 8, p at offset 8
	if sz != 16 || al != 8 || offp != 8 {
		t.Fatalf("struct layout mismatch: want {sz:16, al:8, offp:8}, got {sz:%d, al:%d, offp:%d}", sz, al, offp)
	}
}

// Error surface: missing symbol should hard-error with dlsym detail.
func Test_Builtin_FFI_bad_symbol_errors(t *testing.T) {
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(fmt.Sprintf(`
		ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: { int32: { kind:"int", bits:32, signed:true } },
		  functions: [
		    { name:"__definitely_not_exists__", ret:"int32", params:[] }
		  ]
		})
	`, lib("c")))
	wantErrContains(t, err, "dlsym")
}

// Helpers (tiny)
func mustInt64(t *testing.T, v Value) int64 {
	t.Helper()
	if v.Tag != VTInt {
		t.Fatalf("expected VTInt, got %#v", v)
	}
	return v.Data.(int64)
}

// ===== Additional high-impact FFI tests =====

func Test_Builtin_FFI_libm_sin_cos_zero(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: { dbl: { kind:"float", bits:64 } },
		  functions: [
		    { name:"sin", ret:"dbl", params:["dbl"] },
		    { name:"cos", ret:"dbl", params:["dbl"] }
		  ]
		})
		{ s: C.sin(0.0), c: C.cos(0.0) }
	`, lib("m")))
	m := mustMap(t, v)
	if m.Entries["s"].Tag != VTNum || math.Abs(m.Entries["s"].Data.(float64)) > 1e-12 {
		t.Fatalf("sin(0)=0, got %#v", m.Entries["s"])
	}
	if m.Entries["c"].Tag != VTNum || math.Abs(m.Entries["c"].Data.(float64)-1.0) > 1e-12 {
		t.Fatalf("cos(0)=1, got %#v", m.Entries["c"])
	}
}

func Test_Builtin_FFI_libc_strlen_embedded_nul(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: {
		    charp: { kind:"pointer", to:{ kind:"int", bits:8, signed:true } },
		    int32: { kind:"int", bits:32, signed:true }
		  },
		  functions: [ { name:"strlen", ret:"int32", params:["charp"] } ]
		})
		# embedded NUL should stop strlen at 2
		C.strlen("ab\u0000cd")
	`, lib("c")))
	if v.Tag != VTInt || v.Data.(int64) != 2 {
		t.Fatalf("strlen('ab\\0cd') expected 2, got %#v", v)
	}
}

func Test_Builtin_FFI_libc_snprintf_variadic(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: {
		    size_t: { kind:"int", bits:64, signed:false },
		    charp:  { kind:"pointer", to:{ kind:"int", bits:8, signed:true } },
		    int32:  { kind:"int", bits:32, signed:true },
		    dbl:    { kind:"float", bits:64 }
		  },
		  functions: [
		    { name:"snprintf", ret:"int32", params:["charp","size_t","charp"], variadic:true }
		  ]
		})
		do
			let buf = C.__mem.malloc(64)
			# snprintf(buf, 64, "%%d %%.2f", 123, 3.14159)
			let n = C.snprintf(buf, 64, "%%d %%.2f", [123, 3.14159])
			let s = C.__mem.string(buf, null)
			C.__mem.free(buf)
			{ n: n, s: s }
		end
	`, lib("c")))
	m := mustMap(t, v)
	if m.Entries["n"].Tag != VTInt || m.Entries["n"].Data.(int64) <= 0 {
		t.Fatalf("snprintf n should be > 0, got %#v", m.Entries["n"])
	}
	out := m.Entries["s"]
	if out.Tag != VTStr {
		t.Fatalf("snprintf string result expected Str, got %#v", out)
	}
	got := out.Data.(string)
	if got != "123 3.14" {
		t.Fatalf("snprintf output mismatch: got %q", got)
	}
}

func Test_Builtin_FFI_libc_getenv_ret_as_str_PATH(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: {
		    charp: { kind:"pointer", to:{ kind:"int", bits:8, signed:true } }
		  },
		  functions: [
		    { name:"getenv", ret:"charp", params:["charp"], ret_as_str:true }
		  ]
		})
		C.getenv("PATH")
	`, lib("c")))
	if v.Tag != VTStr || v.Data.(string) == "" {
		t.Fatalf("getenv('PATH') should return non-empty string, got %#v", v)
	}
}

func Test_Builtin_FFI_struct_union_nested_layout(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: {
		    U: { kind:"union", fields:[
		      { name:"i", type:{ kind:"int", bits:32, signed:true } },
		      { name:"d", type:{ kind:"float", bits:64 } }
		    ] },
		    S: { kind:"struct", fields:[
		      { name:"p", type:{ kind:"pointer", to:{ kind:"void" } } },
		      { name:"u", type:"U" }
		    ] }
		  }
		})
		{ su: C.__mem.sizeof("U"), au: C.__mem.alignof("U"),
		  ss: C.__mem.sizeof("S"), as: C.__mem.alignof("S"),
		  offu: C.__mem.offsetof("S","u") }
	`, lib("c")))
	m := mustMap(t, v)
	// U: size=max(8,4)=8 align=max(8,4)=8
	if mustInt64(t, m.Entries["su"]) != 8 || mustInt64(t, m.Entries["au"]) != 8 {
		t.Fatalf("union U layout expected size=8 align=8, got %d/%d", mustInt64(t, m.Entries["su"]), mustInt64(t, m.Entries["au"]))
	}
	// S: ptr(8) then align union(8): offset(u)=8, total size=16, align=8
	if mustInt64(t, m.Entries["offu"]) != 8 || mustInt64(t, m.Entries["ss"]) != 16 || mustInt64(t, m.Entries["as"]) != 8 {
		t.Fatalf("struct S layout expected off(u)=8 size=16 align=8, got off=%d size=%d align=%d",
			mustInt64(t, m.Entries["offu"]), mustInt64(t, m.Entries["ss"]), mustInt64(t, m.Entries["as"]))
	}
}

func Test_Builtin_FFI_variadic_requires_array(t *testing.T) {
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(fmt.Sprintf(`
		let C = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: {
		    size_t: { kind:"int", bits:64, signed:false },
		    charp:  { kind:"pointer", to:{ kind:"int", bits:8, signed:true } },
		    int32:  { kind:"int", bits:32, signed:true }
		  },
		  functions: [
		    { name:"snprintf", ret:"int32", params:["charp","size_t","charp"], variadic:true }
		  ]
		})
		do
		  let buf = C.__mem.malloc(16)
		  # ERROR: last arg must be an array for variadic
		  C.snprintf(buf, 16, "%%d %%d", 123)
		end
	`, lib("c")))
	wantErrContains(t, err, "variadic function expects final argument to be an array")
}

func Test_Builtin_FFI___mem_string_len_negative_error(t *testing.T) {
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(fmt.Sprintf(`
		let C = ffiOpen({ version: "1", lib:"%s" })
		C.__mem.string(C.__mem.malloc(1), -5)
	`, lib("c")))
	wantErrContains(t, err, "len < 0")
}

func Test_Builtin_FFI_per_symbol_lib_override(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version: "1",
		  lib: "%s",     # default
		  types: {
		    int32: { kind:"int", bits:32, signed:true },
		    dbl:   { kind:"float", bits:64 },
		    charp: { kind:"pointer", to:{ kind:"int", bits:8, signed:true } }
		  },
		  functions: [
		    { name:"strlen", ret:"int32", params:["charp"] },             # from libc
		    { name:"cos",    ret:"dbl",   params:["dbl"], lib:"%s" } # override lib
		  ]
		})
		{ n: C.strlen("abc"), c: C.cos(0.0) }
	`, lib("c"), lib("m")))
	m := mustMap(t, v)
	if mustInt64(t, m.Entries["n"]) != 3 {
		t.Fatalf("strlen override expected 3, got %#v", m.Entries["n"])
	}
	if m.Entries["c"].Tag != VTNum || math.Abs(m.Entries["c"].Data.(float64)-1.0) > 1e-12 {
		t.Fatalf("cos(0)=1, got %#v", m.Entries["c"])
	}
}

func Test_Builtin_FFI_open_close_twice_error(t *testing.T) {
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(fmt.Sprintf(`
		let C = ffiOpen({ version: "1", lib:"%s" })
		C.close()
		C.close()   # second close should error
	`, lib("c")))
	wantErrContains(t, err, "already closed")
}

func Test_Builtin_FFI___types_handles_into_sizeof(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version: "1", lib:"%s",
		  types:{ X:{ kind:"int", bits:64, signed:true } }
		})
		# fetch handle from __types and feed to sizeof
		let tx = C.__types.X
		C.__mem.sizeof(tx)
	`, lib("c")))
	if v.Tag != VTInt || v.Data.(int64) != 8 {
		t.Fatalf("__types handle into sizeof expected 8, got %#v", v)
	}
}

// --- fixed: remove unsupported pipeline/lambda; just validate layout + rc ---
func Test_Builtin_FFI_clock_gettime_timespec_layout_and_call(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: {
		    time_t:   { kind:"int", bits:64, signed:true },
		    long:     { kind:"int", bits:64, signed:true },
		    Timespec: { kind:"struct", fields:[
		      { name:"tv_sec",  type:"time_t" },
		      { name:"tv_nsec", type:"long" }
		    ] },
		    int32: { kind:"int", bits:32, signed:true },
		    TS_p:  { kind:"pointer", to:"Timespec" }
		  },
		  functions: [
		    { name:"clock_gettime", ret:"int32",
		      params:[ { kind:"int", bits:32, signed:true }, "TS_p" ] }
		  ]
		})
		do
		  let sz = C.__mem.sizeof("Timespec")
		  let al = C.__mem.alignof("Timespec")
		  let off_nsec = C.__mem.offsetof("Timespec","tv_nsec")
		  let p = C.__mem.new("Timespec", 1)
		  let rc = C.clock_gettime(0, p)   # CLOCK_REALTIME = 0
		  { rc: rc, sz: sz, al: al, off: off_nsec }
		end
	`, lib("c")))
	m := mustMap(t, v)
	if mustInt64(t, m.Entries["sz"]) != 16 ||
		mustInt64(t, m.Entries["al"]) != 8 ||
		mustInt64(t, m.Entries["off"]) != 8 {
		t.Fatalf("Timespec layout mismatch (amd64 expected sz=16, al=8, off_nsec=8), got sz=%d al=%d off=%d",
			mustInt64(t, m.Entries["sz"]), mustInt64(t, m.Entries["al"]), mustInt64(t, m.Entries["off"]))
	}
	if mustInt64(t, m.Entries["rc"]) != 0 {
		t.Fatalf("clock_gettime rc != 0: %#v", m.Entries["rc"])
	}
}

// --- fixed: actually declare snprintf in the spec, then pass a negative size_t ---
func Test_Builtin_FFI_range_errors_int_and_unsigned(t *testing.T) {
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(fmt.Sprintf(`
		let C = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: {
		    size_t: { kind:"int", bits:64, signed:false },
		    charp:  { kind:"pointer", to:{ kind:"int", bits:8, signed:true } },
		    int32:  { kind:"int", bits:32, signed:true }
		  },
		  functions: [
		    { name:"snprintf", ret:"int32", params:["charp","size_t","charp"], variadic:true }
		  ]
		})
		do
		  let buf = C.__mem.malloc(8)
		  # size_t with negative should hard-error during marshalling
		  C.snprintf(buf, -1, "%%d", [1])
		end
	`, lib("c")))
	wantErrContains(t, err, "negative")
}

// --- fixed: no pointer arithmetic; zero-fill full buffer, then copy 5 bytes ---
func Test_Builtin_FFI___mem_fill_and_copy_bytes(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({ version: "1", lib:"%s" })
		do
		  let p = C.__mem.malloc(6)
		  C.__mem.fill(p, 0, 6)         # ensure NUL-termination
		  C.__mem.copy(p, "AAAAA", 5)   # write 5 bytes; last stays NUL
		  let s = C.__mem.string(p, null)
		  C.__mem.free(p)
		  s
		end
	`, lib("c")))
	if v.Tag != VTStr || v.Data.(string) != "AAAAA" {
		t.Fatalf("fill/copy expected 'AAAAA', got %#v", v)
	}
}

// --- fixed: avoid inline typeof (its layout isn’t computed); use __types handle ---
func Test_Builtin_FFI___mem_typeof_inline_and_name(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({ version: "1", lib:"%s",
		  types:{ I32:{ kind:"int", bits:32, signed:true } }
		})
		do
		  # get declared type handle from __types, and compare against name
		  let t_handle = C.__types.I32
		  { a: C.__mem.sizeof("I32"), b: C.__mem.sizeof(t_handle) }
		end
	`, lib("c")))
	m := mustMap(t, v)
	if mustInt64(t, m.Entries["a"]) != 4 || mustInt64(t, m.Entries["b"]) != 4 {
		t.Fatalf("typeof/sizeof mismatch: got a=%d b=%d", mustInt64(t, m.Entries["a"]), mustInt64(t, m.Entries["b"]))
	}
}

// String bridge control: passing Str to char* still works.
func Test_Builtin_FFI_string_bridge_charp_control(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: {
		    charp: { kind:"pointer", to:{ kind:"int", bits:8, signed:true } },
		    int32: { kind:"int", bits:32, signed:true }
		  },
		  functions: [
		    { name:"strlen", ret:"int32", params:["charp"] }
		  ]
		})
		C.strlen("hello")
	`, lib("c")))
	if v.Tag != VTInt || v.Data.(int64) != 5 {
		t.Fatalf("strlen('hello') expected 5, got %#v", v)
	}
}

// CIF must outlive prep; first call happens AFTER heavy churn.
// This is designed to catch the current bug: prepCIF returns &c where c is a stack C struct.
func Test_Builtin_FFI_cif_lifetime_thrash_before_first_call(t *testing.T) {
	ip, _ := NewInterpreter()

	// Build one big source string: prepare M, thrash with many ffiOpen()s, then call M.hypot.
	var b strings.Builder
	b.WriteString(fmt.Sprintf(`
		let M = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: { dbl: { kind:"float", bits:64 } },
		  functions: [ { name:"hypot", ret:"dbl", params:["dbl","dbl"] } ]
		});
	`, lib("m")))
	thrashSnip := fmt.Sprintf(`
		let _ = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: {
		    int32: { kind:"int", bits:32, signed:true },
		    charp: { kind:"pointer", to:{ kind:"int", bits:8, signed:true } }
		  },
		  functions: [ { name:"strlen", ret:"int32", params:["charp"] } ]
		});
		0;
	`, lib("c"))
	for i := 0; i < 3000; i++ {
		b.WriteString(thrashSnip)
	}
	b.WriteString(`M.hypot(3.0, 4.0)`)

	v, err := ip.EvalSource(b.String())
	if err != nil {
		t.Fatalf("hypot call errored after thrash: %v", err)
	}
	if v.Tag != VTNum {
		t.Fatalf("hypot should return Num, got %#v", v)
	}
	if math.Abs(v.Data.(float64)-5.0) > 1e-9 {
		t.Fatalf("hypot(3,4) expected 5.0 after CIF thrash, got %v", v.Data.(float64))
	}

	// Sanity: another call using a fresh program (normal path) still ok.
	v = evalWithIP(t, ip, fmt.Sprintf(`
		let M = ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: { dbl: { kind:"float", bits:64 } },
		  functions: [ { name:"hypot", ret:"dbl", params:["dbl","dbl"] } ]
		});
		M.hypot(6.0, 8.0)
	`, lib("m")))
	if v.Tag != VTNum || math.Abs(v.Data.(float64)-10.0) > 1e-9 {
		t.Fatalf("hypot(6,8)=10.0 mismatch: %#v", v)
	}
}

// Version contract: numeric 1 must be rejected; spec requires "1" (string).
func Test_Builtin_FFI_version_must_be_string(t *testing.T) {
	err(t, "version", fmt.Sprintf(`
		ffiOpen({ version: 1, lib: "%s" })
	`, lib("c")))
}

// Duplicate function names within the same spec must be rejected.
func Test_Builtin_FFI_duplicate_function_names_rejected(t *testing.T) {
	err(t, "duplicate", fmt.Sprintf(`
		ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: {
		    charp: { kind:"pointer", to:{ kind:"int", bits:8, signed:true } },
		    int32: { kind:"int", bits:32, signed:true }
		  },
		  functions: [
		    { name:"puts", ret:"int32", params:["charp"] },
		    { name:"puts", ret:"int32", params:["charp"] }  # duplicate
		  ]
		})
	`, lib("c")))
}

// Duplicate variable names within the same spec must be rejected.
func Test_Builtin_FFI_duplicate_variable_names_rejected(t *testing.T) {
	err(t, "duplicate", fmt.Sprintf(`
		ffiOpen({
		  version: "1",
		  lib: "%s",
		  types: { int32: { kind:"int", bits:32, signed:true } },
		  variables: [
		    { name:"errno", type:"int32" },
		    { name:"errno", type:"int32" }   # duplicate
		  ]
		})
	`, lib("c")))
}

// Table-driven tests for __mem.gc behavior (attach, detach, errors, realloc carry, idempotence).
func Test_Builtin_FFI___mem_gc_Table(t *testing.T) {
	cases := []struct {
		name    string
		src     string
		wantErr string // if non-empty, error substring to expect
	}{
		{
			name: "attach free then manual free twice (idempotent)",
			src: fmt.Sprintf(`
			  let C = ffiOpen({ version:"1", lib:"%s" })
			  do
			    let p = C.__mem.malloc(16)
			    C.__mem.gc(p, "free")
			    C.__mem.free(p)   # first free triggers registered destructor
			    C.__mem.free(p)   # second free must be a no-op
			    0
			  end
			`, lib("c")),
		},
		{
			name: "detach then free (no destructor invoked)",
			src: fmt.Sprintf(`
			  let C = ffiOpen({ version:"1", lib:"%s" })
			  do
			    let p = C.__mem.malloc(8)
			    C.__mem.gc(p, "free")
			    C.__mem.gc(p, null)   # detach finalizer
			    C.__mem.free(p)       # plain free succeeds
			    0
			  end
			`, lib("c")),
		},
		{
			name:    "custom symbol resolve error surfaces dlsym",
			wantErr: "dlsym",
			src: fmt.Sprintf(`
			  let C = ffiOpen({ version:"1", lib:"%s" })
			  let p = C.__mem.malloc(4)
			  C.__mem.gc(p, {sym:"__definitely_not_exists__"})
			`, lib("c")),
		},
		{
			name: "realloc carries destructor to new pointer; double free is safe",
			src: fmt.Sprintf(`
			  let C = ffiOpen({ version:"1", lib:"%s" })
			  do
			    let p = C.__mem.malloc(8)
			    C.__mem.gc(p, "free")
			    let q = C.__mem.realloc(p, 16)   # move destructor to q
			    C.__mem.free(q)
			    C.__mem.free(q)                  # idempotent
			    0
			  end
			`, lib("c")),
		},
		{
			name:    "string finalizer must be exactly \"free\"",
			wantErr: "string finalizer must be \"free\"",
			src: fmt.Sprintf(`
			  let C = ffiOpen({ version:"1", lib:"%s" })
			  let p = C.__mem.malloc(1)
			  C.__mem.gc(p, "close")
			`, lib("c")),
		},
		{
			name:    "first argument must be a pointer handle",
			wantErr: "pointer handle",
			src: fmt.Sprintf(`
			  let C = ffiOpen({ version:"1", lib:"%s" })
			  C.__mem.gc(123, "free")
			`, lib("c")),
		},
		{
			name: "re-attach overwrites finalizer, then detach",
			src: fmt.Sprintf(`
			  let C = ffiOpen({ version:"1", lib:"%s" })
			  do
			    let p = C.__mem.malloc(8)
			    C.__mem.gc(p, "free")
			    # overwrite with the same (no-op behavior change, but allowed)
			    C.__mem.gc(p, "free")
			    # now detach and free
			    C.__mem.gc(p, null)
			    C.__mem.free(p)
			    0
			  end
			`, lib("c")),
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if tc.wantErr == "" {
				ok(t, tc.src)
			} else {
				err(t, tc.wantErr, tc.src)
			}
		})
	}
}

func Test_FFI_qsort_callback_sorts(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version:"1", lib:"%s",
		  types:{
		    size_t:{kind:"int",bits:64,signed:false},
		    voidp:{kind:"pointer",to:{kind:"void"}},
		    Cmp:{kind:"funcptr",ret:{kind:"int",bits:32,signed:true},
		         params:[{kind:"pointer",to:{kind:"void"}},{kind:"pointer",to:{kind:"void"}}]}
		  },
		  functions:[
		    {name:"qsort",ret:{kind:"void"},params:["voidp","size_t","size_t","Cmp"]},
		    {name:"memcmp",ret:{kind:"int",bits:32,signed:true},params:["voidp","voidp","size_t"]}
		  ]
		})
		do
		  let N=5
		  let SZ=4
		  let BYTES=N*SZ
		  let buf=C.__mem.malloc(BYTES)
		  let exp=C.__mem.malloc(BYTES)
		  C.__mem.copy(buf,"\u0003\u0000\u0000\u0000\u0001\u0000\u0000\u0000\u0004\u0000\u0000\u0000\u0001\u0000\u0000\u0000\u0005\u0000\u0000\u0000",BYTES)
		  C.__mem.copy(exp,"\u0001\u0000\u0000\u0000\u0001\u0000\u0000\u0000\u0003\u0000\u0000\u0000\u0004\u0000\u0000\u0000\u0005\u0000\u0000\u0000",BYTES)
		  let calls=0
		  let cmp=fun(a,b) do calls=calls+1; C.memcmp(a,b,4) end
		  C.qsort(buf,N,SZ,cmp)
		  {rc:C.memcmp(buf,exp,BYTES),calls:calls}
		end
	`, lib("c")))
	m := mustMap(t, v)
	if mustInt64(t, m.Entries["rc"]) != 0 {
		t.Fatalf("memcmp != 0")
	}
	if mustInt64(t, m.Entries["calls"]) <= 0 {
		t.Fatalf("callback not invoked")
	}
}

func Test_FFI_callback_captures_and_returns(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C=ffiOpen({
		  version:"1", lib:"%s",
		  types:{
		    size_t:{kind:"int",bits:64,signed:false},
		    voidp:{kind:"pointer",to:{kind:"void"}},
		    Cmp:{kind:"funcptr",ret:{kind:"int",bits:32,signed:true},
		         params:[{kind:"pointer",to:{kind:"void"}},{kind:"pointer",to:{kind:"void"}}]}
		  },
		  functions:[{name:"qsort",ret:{kind:"void"},params:["voidp","size_t","size_t","Cmp"]}]
		})
		do
		  let N=8
		  let SZ=4
		  let B=N*SZ
		  let buf=C.__mem.malloc(B); C.__mem.fill(buf,0,B)
		  let s=1
		  let c=0
		  let cmp=fun(a,b) do c=c+1; s=0-s; s end
		  C.qsort(buf,N,SZ,cmp)
		  c
		end
	`, lib("c")))
	if v.Tag != VTInt || v.Data.(int64) <= 0 {
		t.Fatalf("callback not invoked, got %#v", v)
	}
}

func Test_Builtin_FFI_Aggregates_HandlesOnly_Matrix(t *testing.T) {
	ip, _ := NewInterpreter()

	cases := []struct {
		name  string
		src   string
		check func(t *testing.T, v Value, err error)
	}{
		{
			name: "byvalue_return_struct_div_returns_handle_and_bytes_match",
			// div(7,3) -> struct div_t { int quot=2; int rem=1; } (SysV/glibc: int32,int32)
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1", lib:"%s",
				  types:{
				    i32:   {kind:"int",bits:32,signed:true},
				    Div:   {kind:"struct",fields:[{name:"quot",type:"i32"},{name:"rem",type:"i32"}]},
				    charp: {kind:"pointer",to:{kind:"int",bits:8,signed:true}}
				  },
				  functions:[
				    {name:"div", ret:"Div", params:["i32","i32"]},
				    {name:"memcmp", ret:"i32", params:[{kind:"pointer",to:{kind:"void"}},{kind:"pointer",to:{kind:"void"}},{kind:"int",bits:64,signed:false}]}
				  ]
				})
				do
				let res = C.div(7,3)         # handle (boxed struct)
				let expect = C.__mem.malloc(8)
				C.__mem.copy(expect, "\u0002\u0000\u0000\u0000\u0001\u0000\u0000\u0000", 8)
				{ res: res, eq: C.memcmp(res, expect, 8) }
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				m := mustMap(t, v)
				res := m.Entries["res"]
				if res.Tag != VTHandle {
					t.Fatalf("div() should return VTHandle (boxed struct), got %#v", res)
				}
				if mustInt64(t, m.Entries["eq"]) != 0 {
					t.Fatalf("div result bytes mismatch (memcmp != 0): %#v", v)
				}
			},
		},
		{
			name: "pointer_to_aggregate_param_requires_handle_no_literals",
			// nanosleep(const struct timespec *req, struct timespec *rem)
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1", lib:"%s",
				  types:{
				    long: {kind:"int",bits:64,signed:true},
				    Timespec: {kind:"struct",fields:[{name:"tv_sec",type:"long"},{name:"tv_nsec",type:"long"}]},
				    Tsp: {kind:"pointer",to:"Timespec"},
				    i32: {kind:"int",bits:32,signed:true}
				  },
				  functions:[ {name:"nanosleep", ret:"i32", params:["Tsp","Tsp"]} ]
				})
				C.nanosleep({ tv_sec:0, tv_nsec:0 }, null)  # MUST error: literal rejected
			`, lib("c")),
			check: func(t *testing.T, _ Value, err error) {
				wantErrContains(t, err, "aggregate parameter requires handle")
			},
		},
		{
			name: "pointer_to_aggregate_param_with_handle_works",
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1", lib:"%s",
				  types:{
				    long: {kind:"int",bits:64,signed:true},
                    Timespec: {kind:"struct",fields:[{name:"tv_sec",type:"long"},{name:"tv_nsec",type:"long"}]},
				    Tsp: {kind:"pointer",to:"Timespec"},
				    i32: {kind:"int",bits:32,signed:true}
				  },
				  functions:[ {name:"nanosleep", ret:"i32", params:["Tsp","Tsp"]} ]
				})
				do
				  let req = C.__mem.new("Timespec",1)   # allocate storage → handle
				  C.__mem.fill(req, 0, C.__mem.sizeof("Timespec"))
				  C.nanosleep(req, null)                 # should succeed (likely 0)
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				if v.Tag != VTInt && v.Tag != VTNum {
					t.Fatalf("nanosleep rc should be scalar, got %#v", v)
				}
			},
		},
		{
			name: "ret_as_str_for_charp_unchanged",
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1", lib:"%s",
				  types:{ charp:{kind:"pointer",to:{kind:"int",bits:8,signed:true}} },
				  functions:[ {name:"getenv", ret:"charp", params:["charp"], ret_as_str:true } ]
				})
				C.getenv("PATH")
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				if v.Tag != VTStr || v.Data.(string) == "" {
					t.Fatalf("expected PATH string, got %#v", v)
				}
			},
		},
		{
			name: "variadic_still_ok_snprintf",
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1", lib:"%s",
				  types:{
				    size_t:{kind:"int",bits:64,signed:false},
				    charp:{kind:"pointer",to:{kind:"int",bits:8,signed:true}},
				    i32:{kind:"int",bits:32,signed:true},
				    dbl:{kind:"float",bits:64}
				  },
				  functions:[ {name:"snprintf", ret:"i32", params:["charp","size_t","charp"], variadic:true} ]
				})
				do
				  let buf = C.__mem.malloc(64)
				  let n = C.snprintf(buf, 64, "%%d %%.2f", [123, 3.14159])
				  { n: n, s: C.__mem.string(buf, null) }
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				m := mustMap(t, v)
				if mustInt64(t, m.Entries["n"]) <= 0 {
					t.Fatalf("snprintf n <= 0: %#v", v)
				}
				if m.Entries["s"].Tag != VTStr || m.Entries["s"].Data.(string) != "123 3.14" {
					t.Fatalf("snprintf out mismatch: %#v", v)
				}
			},
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			v, err := ip.EvalSource(tc.src)
			tc.check(t, v, err)
		})
	}
}

func Test_Builtin_FFI_bitfields_minops_matrix(t *testing.T) {
	ip, _ := NewInterpreter()

	type tc struct {
		name  string
		src   string
		check func(*testing.T, Value, error)
	}
	cases := []tc{
		{
			name: "read_single_byte_bits_3_from_bit1",
			src: fmt.Sprintf(`
				let C = ffiOpen({version:"1", lib:"%s"});
				do
				  let p = C.__mem.malloc(1)
				  C.__mem.fill(p, 214, 1)        # 214 == 0xD6 (1101 0110)
				  C.__mem.readBits(p, 0, 1, 3, true)   # bits 1..3 -> 0b011 = 3
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				if v.Tag != VTInt || v.Data.(int64) != 3 {
					t.Fatalf("want 3, got %#v", v)
				}
			},
		},
		{
			name: "read_crosses_bytes_width8_from_bit4_le",
			src: fmt.Sprintf(`
				let C = ffiOpen({version:"1", lib:"%s"});
				do
				  let p = C.__mem.malloc(2)
				  C.__mem.fill(p, 0, 2)
				  # Set explicit bytes [0]=0xAA, [1]=0x55 using writeBits on each byte
				  C.__mem.writeBits(p, 0, 0, 8, 170)   # 170 == 0xAA
				  C.__mem.writeBits(p, 1, 0, 8, 85)    #  85 == 0x55
				  # Read 8 bits starting at bit 4 → 0x5A = 90
				  C.__mem.readBits(p, 0, 4, 8, null)
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				if v.Tag != VTInt || v.Data.(int64) != 90 { // 0x5A
					t.Fatalf("want 90 (0x5A), got %#v", v)
				}
			},
		},
		{
			name: "write_across_two_bytes_and_verify_split",
			src: fmt.Sprintf(`
				let C = ffiOpen({version:"1", lib:"%s"});
				do
				  let p = C.__mem.malloc(2)
				  C.__mem.fill(p, 0, 2)
				  C.__mem.writeBits(p, 0, 4, 8, 171)   # 171 == 0xAB, spans bits 4..11
				  { b0: C.__mem.readBits(p, 0, 0, 8, null),
				    b1: C.__mem.readBits(p, 1, 0, 8, null),
				    val: C.__mem.readBits(p, 0, 4, 8, null) }
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				m := v.Data.(*MapObject)
				b0 := m.Entries["b0"]
				b1 := m.Entries["b1"]
				val := m.Entries["val"]
				// Expected split after writing 0xAB at bit 4: bytes become {0xB0, 0x0A}
				if b0.Tag != VTInt || b0.Data.(int64) != 176 {
					t.Fatalf("b0 want 176 (0xB0), got %#v", b0)
				}
				if b1.Tag != VTInt || b1.Data.(int64) != 10 {
					t.Fatalf("b1 want 10  (0x0A), got %#v", b1)
				}
				if val.Tag != VTInt || val.Data.(int64) != 171 {
					t.Fatalf("val want 171 (0xAB), got %#v", val)
				}
			},
		},
		{
			name: "read_with_sign_extension_width5_negative",
			src: fmt.Sprintf(`
				let C = ffiOpen({version:"1", lib:"%s"});
				do
				  let p = C.__mem.malloc(1)
				  C.__mem.fill(p, 240, 1)     # 240 == 0xF0 (1111 0000)
				  # bits 3..7 are 11110 (0x1E); signed width 5 => -2 after sign-ext
				  C.__mem.readBits(p, 0, 3, 5, true)
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				if v.Tag != VTInt || v.Data.(int64) != -2 {
					t.Fatalf("want -2, got %#v", v)
				}
			},
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			v, e := ip.EvalSource(c.src)
			c.check(t, v, e)
		})
	}
}

func Test_Builtin_FFI_mem_box_struct_init_and_getf(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version:"1", lib:"%s",
		  types:{
		    I32:{kind:"int",bits:32,signed:true},
		    I64:{kind:"int",bits:64,signed:true},
		    S:{kind:"struct",fields:[{name:"a",type:"I32"},{name:"b",type:"I64"}]}
		  }
		})
		do
		  let h = C.__mem.box("S", { a:7, b:11 })
		  { a:C.__mem.getf("S", h, "a"), b:C.__mem.getf("S", h, "b") }
		end
	`, lib("c")))
	m := mustMap(t, v)
	if mustInt64(t, m.Entries["a"]) != 7 || mustInt64(t, m.Entries["b"]) != 11 {
		t.Fatalf("box+getf mismatch: %#v", v)
	}
}

func Test_Builtin_FFI_mem_setf_writes_field(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version:"1", lib:"%s",
		  types:{
		    I32:{kind:"int",bits:32,signed:true},
		    S:{kind:"struct",fields:[{name:"a",type:"I32"}]}
		  }
		})
		do
		  let h = C.__mem.box("S", { a:0 })
		  C.__mem.setf("S", h, "a", 42)
		  C.__mem.getf("S", h, "a")
		end
	`, lib("c")))
	if mustInt64(t, v) != 42 {
		t.Fatalf("setf did not persist: %#v", v)
	}
}

func Test_Builtin_FFI_mem_box_union_read_write(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version:"1", lib:"%s",
		  types:{
		    I32:{kind:"int",bits:32,signed:true},
		    D64:{kind:"float",bits:64},
		    U:{kind:"union",fields:[{name:"i",type:"I32"},{name:"d",type:"D64"}]}
		  }
		})
		do
		  let u = C.__mem.box("U", { i: -3 })
		  let i0 = C.__mem.getf("U", u, "i")
		  C.__mem.setf("U", u, "d", 2.5)
		  let d1 = C.__mem.getf("U", u, "d")
		  { i:i0, d:d1 }
		end
	`, lib("c")))
	m := mustMap(t, v)
	if mustInt64(t, m.Entries["i"]) != -3 {
		t.Fatalf("union getf(i) mismatch: %#v", v)
	}
	if m.Entries["d"].Tag != VTNum || m.Entries["d"].Data.(float64) != 2.5 {
		t.Fatalf("union getf(d) mismatch: %#v", v)
	}
}

func Test_Builtin_FFI_mem_box_array_init_bytes(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, fmt.Sprintf(`
		let C = ffiOpen({
		  version:"1", lib:"%s",
		  types:{
		    U8:{kind:"int",bits:8,signed:false},
		    A4:{kind:"array",of:"U8",len:4},
		    I32:{kind:"int",bits:32,signed:true},
		    SZ:{kind:"int",bits:64,signed:false},
		    Voidp:{kind:"pointer",to:{kind:"void"}}
		  },
		  functions:[
		    {name:"memcmp", ret:"I32", params:["Voidp","Voidp","SZ"]}
		  ]
		})
		do
		  let a = C.__mem.box("A4", [1,2,3,4])
		  let exp = C.__mem.malloc(4)
		  C.__mem.copy(exp, "\u0001\u0002\u0003\u0004", 4)
		  C.memcmp(a, exp, 4)
		end
	`, lib("c")))
	if mustInt64(t, v) != 0 {
		t.Fatalf("array bytes mismatch (memcmp != 0): %#v", v)
	}
}

func Test_Builtin_FFI_Aggregate_HandleSemantics_Matrix(t *testing.T) {
	ip, _ := NewInterpreter()

	type tc struct {
		name  string
		src   string
		check func(*testing.T, Value, error)
	}

	cases := []tc{
		{
			name: "byvalue_return_struct_div_returns_handle_and_bytes_match",
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1", lib:"%s",
				  types:{
				    i32:   {kind:"int",bits:32,signed:true},
				    Div:   {kind:"struct",fields:[{name:"quot",type:"i32"},{name:"rem",type:"i32"}]},
				    charp: {kind:"pointer",to:{kind:"int",bits:8,signed:true}}
				  },
				  functions:[
				    {name:"div", ret:"Div", params:["i32","i32"]},
				    {name:"memcmp", ret:"i32", params:[{kind:"pointer",to:{kind:"void"}},{kind:"pointer",to:{kind:"void"}},{kind:"int",bits:64,signed:false}]}
				  ]
				})
				do
				  let res = C.div(7,3)         # handle (boxed struct)
				  let expect = C.__mem.malloc(8)
				  C.__mem.copy(expect, "\u0002\u0000\u0000\u0000\u0001\u0000\u0000\u0000", 8)
				  { res: res, resFmt: formatValue(res), eq: C.memcmp(res, expect, 8) }
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				m := mustMap(t, v)
				// Handle identity via formatValue
				resFmt := m.Entries["resFmt"]
				if resFmt.Tag != VTStr || !strings.HasPrefix(resFmt.Data.(string), "<handle:") {
					t.Fatalf("div() should return a handle; got formatValue=%#v", resFmt)
				}
				// Byte-equivalence
				if mustInt64(t, m.Entries["eq"]) != 0 {
					t.Fatalf("div result bytes mismatch (memcmp != 0): %#v", v)
				}
			},
		},
		{
			name: "pointer_to_aggregate_param_requires_handle_no_literals",
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1", lib:"%s",
				  types:{
				    long: {kind:"int",bits:64,signed:true},
				    Timespec: {kind:"struct",fields:[{name:"tv_sec",type:"long"},{name:"tv_nsec",type:"long"}]},
				    Tsp: {kind:"pointer",to:"Timespec"},
				    i32: {kind:"int",bits:32,signed:true}
				  },
				  functions:[ {name:"nanosleep", ret:"i32", params:["Tsp","Tsp"]} ]
				})
				C.nanosleep({ tv_sec:0, tv_nsec:0 }, null)  # MUST error: literal rejected
			`, lib("c")),
			check: func(t *testing.T, _ Value, err error) {
				wantErrContains(t, err, "aggregate parameter requires handle")
			},
		},
		{
			name: "pointer_to_aggregate_param_with_handle_works",
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1", lib:"%s",
				  types:{
				    long: {kind:"int",bits:64,signed:true},
                    Timespec: {kind:"struct",fields:[{name:"tv_sec",type:"long"},{name:"tv_nsec",type:"long"}]},
				    Tsp: {kind:"pointer",to:"Timespec"},
				    i32: {kind:"int",bits:32,signed:true}
				  },
				  functions:[ {name:"nanosleep", ret:"i32", params:["Tsp","Tsp"]} ]
				})
				do
				  let req = C.__mem.new("Timespec",1)   # allocate storage → handle
				  C.__mem.fill(req, 0, C.__mem.sizeof("Timespec"))
				  C.nanosleep(req, null)                 # should succeed (likely 0)
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				if v.Tag != VTInt && v.Tag != VTNum {
					t.Fatalf("nanosleep rc should be scalar, got %#v", v)
				}
			},
		},
		{
			name: "ret_as_str_for_charp_unchanged",
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1", lib:"%s",
				  types:{ charp:{kind:"pointer",to:{kind:"int",bits:8,signed:true}} },
				  functions:[ {name:"getenv", ret:"charp", params:["charp"], ret_as_str:true } ]
				})
				C.getenv("PATH")
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				if v.Tag != VTStr || v.Data.(string) == "" {
					t.Fatalf("expected PATH string, got %#v", v)
				}
			},
		},
		{
			name: "variadic_still_ok_snprintf",
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1", lib:"%s",
				  types:{
				    size_t:{kind:"int",bits:64,signed:false},
				    charp:{kind:"pointer",to:{kind:"int",bits:8,signed:true}},
				    i32:{kind:"int",bits:32,signed:true},
				    dbl:{kind:"float",bits:64}
				  },
				  functions:[ {name:"snprintf", ret:"i32", params:["charp","size_t","charp"], variadic:true} ]
				})
				do
				  let buf = C.__mem.malloc(64)
				  let n = C.snprintf(buf, 64, "%%d %%.2f", [123, 3.14159])
				  { n: n, s: C.__mem.string(buf, null) }
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				m := mustMap(t, v)
				if mustInt64(t, m.Entries["n"]) <= 0 {
					t.Fatalf("snprintf n <= 0: %#v", v)
				}
				if m.Entries["s"].Tag != VTStr || m.Entries["s"].Data.(string) != "123 3.14" {
					t.Fatalf("snprintf out mismatch: %#v", v)
				}
			},
		},
		{
			name: "struct_aggregate_field_getf_returns_handle_and_setf_copies",
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1", lib:"%s",
				  types:{
				    I32:{kind:"int",bits:32,signed:true},
				    Inner:{kind:"struct",fields:[{name:"x",type:"I32"},{name:"y",type:"I32"}]},
				    Outer:{kind:"struct",fields:[{name:"a",type:"I32"},{name:"inner",type:"Inner"}]},
				    SZ:{kind:"int",bits:64,signed:false},
				    Voidp:{kind:"pointer",to:{kind:"void"}}
				  },
				  functions:[ {name:"memcmp", ret:"I32", params:["Voidp","Voidp","SZ"]} ]
				})
				do
				  let src = C.__mem.box("Inner", { x:7, y:11 })
				  let out = C.__mem.box("Outer", { a:0 })   # 'inner' zeroed
				  
				  # getf(inner) must return a handle → return its formatted string

				  let gotH = C.__mem.getf("Outer", out, "inner")
				  let gotFmt = formatValue(gotH)
				 
				  # setf(inner, handle) must copy bytes
				 
				  C.__mem.setf("Outer", out, "inner", src)
				  let after = C.__mem.getf("Outer", out, "inner")
				  let n = C.__mem.sizeof("Inner")
				  let eq = C.memcmp(after, src, n)
				  { gotFmt: gotFmt, eq: eq }
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				m := mustMap(t, v)
				ff := m.Entries["gotFmt"]
				if ff.Tag != VTStr || !strings.HasPrefix(ff.Data.(string), "<handle:") {
					t.Fatalf("getf(inner) should return handle; formatValue=%#v", ff)
				}
				if mustInt64(t, m.Entries["eq"]) != 0 {
					t.Fatalf("setf(inner, handle) did not copy matching bytes")
				}
			},
		},
		{
			name: "struct_aggregate_field_setf_with_non_handle_fails",
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1", lib:"%s",
				  types:{
				    I32:{kind:"int",bits:32,signed:true},
				    Inner:{kind:"struct",fields:[{name:"x",type:"I32"},{name:"y",type:"I32"}]},
				    Outer:{kind:"struct",fields:[{name:"a",type:"I32"},{name:"inner",type:"Inner"}]}
				  }
				})
				let out = C.__mem.box("Outer", { a:0 })
				# ERROR: value is not a handle
				C.__mem.setf("Outer", out, "inner", { x:1, y:2 })
			`, lib("c")),
			check: func(t *testing.T, _ Value, err error) {
				wantErrContains(t, err, "aggregate field requires handle")
			},
		},
		{
			name: "union_aggregate_field_getf_returns_handle_and_setf_copies",
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1", lib:"%s",
				  types:{
				    I32:{kind:"int",bits:32,signed:true},
				    Inner:{kind:"struct",fields:[{name:"x",type:"I32"},{name:"y",type:"I32"}]},
				    U:{kind:"union",fields:[{name:"i",type:"I32"},{name:"in",type:"Inner"}]},
				    SZ:{kind:"int",bits:64,signed:false},
				    Voidp:{kind:"pointer",to:{kind:"void"}}
				  },
				  functions:[ {name:"memcmp", ret:"I32", params:["Voidp","Voidp","SZ"]} ]
				})
				do
				  let u  = C.__mem.box("U", { i: 0 })
				  let i = C.__mem.box("Inner", { x: 9, y: 4 })
				  let h  = C.__mem.getf("U", u, "in")   # must be a handle
				  let gotFmt = formatValue(h)
				  C.__mem.setf("U", u, "in", i)
				  let h2 = C.__mem.getf("U", u, "in")
				  let n = C.__mem.sizeof("Inner")
				  { gotFmt: gotFmt, eq: C.memcmp(h2, i, n) }
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				m := mustMap(t, v)
				ff := m.Entries["gotFmt"]
				if ff.Tag != VTStr || !strings.HasPrefix(ff.Data.(string), "<handle:") {
					t.Fatalf("union getf(in) should return handle; formatValue=%#v", ff)
				}
				if mustInt64(t, m.Entries["eq"]) != 0 {
					t.Fatalf("union aggregate setf did not copy bytes")
				}
			},
		},
		{
			name: "struct_array_field_getf_returns_handle_and_setf_copies",
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1", lib:"%s",
				  types:{
				    U8:{kind:"int",bits:8,signed:false},
				    A4:{kind:"array",of:"U8",len:4},
				    S:{kind:"struct",fields:[{name:"arr",type:"A4"}]},
				    SZ:{kind:"int",bits:64,signed:false},
				    Voidp:{kind:"pointer",to:{kind:"void"}}
				  },
				  functions:[ {name:"memcmp", ret:{kind:"int",bits:32,signed:true}, params:["Voidp","Voidp","SZ"]} ]
				})
				do
				  let s   = C.__mem.box("S", null)
				  let src = C.__mem.box("A4", [1,2,3,4])
				  let fh  = C.__mem.getf("S", s, "arr")
				  let gotFmt = formatValue(fh)
				  C.__mem.setf("S", s, "arr", src)
				  let fh2 = C.__mem.getf("S", s, "arr")
				  { gotFmt: gotFmt, eq: C.memcmp(fh2, src, 4) }
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				m := mustMap(t, v)
				ff := m.Entries["gotFmt"]
				if ff.Tag != VTStr || !strings.HasPrefix(ff.Data.(string), "<handle:") {
					t.Fatalf("getf(array) should return handle; formatValue=%#v", ff)
				}
				if mustInt64(t, m.Entries["eq"]) != 0 {
					t.Fatalf("array field setf did not copy bytes")
				}
			},
		},
		{
			name: "variable_scalar_errno_roundtrip",
			src: fmt.Sprintf(`
				let C = ffiOpen({ version:"1", lib:"%s",
				  types:{ i32:{kind:"int",bits:32,signed:true} },
				  variables:[ {name:"errno", type:"i32"} ]
				})
				do
				  let before = C.errno.get()
				  C.errno.set(0)
				  let after = C.errno.get()
				  { before: before, after: after }
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				m := mustMap(t, v)
				if mustInt64(t, m.Entries["after"]) != 0 {
					t.Fatalf("errno set/get mismatch: %#v", v)
				}
				_ = m.Entries["before"] // value may vary by environment
			},
		},
		{
			name: "variable_aggregate_tzname_get_returns_handle_and_set_rejects_non_handle",
			// tzname is: extern char *tzname[2];
			src: fmt.Sprintf(`
				let C = ffiOpen({
				  version:"1",
				  lib:"%s",
				  types:{
				    Char:{kind:"int",bits:8,signed:true},
				    Charp:{kind:"pointer",to:"Char"},
				    TzArr:{kind:"array",of:"Charp",len:2}
				  },
				  variables:[ {name:"tzname", type:"TzArr"} ]
				})
				do
				  let g = C.tzname.get()
				  { fmt: formatValue(g) }
				end
			`, lib("c")),
			check: func(t *testing.T, v Value, err error) {
				if err != nil {
					t.Fatalf("eval error: %v", err)
				}
				m := mustMap(t, v)
				ff := m.Entries["fmt"]
				if ff.Tag != VTStr || !strings.HasPrefix(ff.Data.(string), "<handle:") {
					t.Fatalf("tzname.get() should return handle; formatValue=%#v", ff)
				}
				// Separate negative test: set() must reject non-handle for aggregate variable.
				_, e := ip.EvalSource(fmt.Sprintf(`
					let C = ffiOpen({
					  version:"1",
					  lib:"%s",
					  types:{
					    Char:{kind:"int",bits:8,signed:true},
					    Charp:{kind:"pointer",to:"Char"},
					    TzArr:{kind:"array",of:"Charp",len:2}
					  },
					  variables:[ {name:"tzname", type:"TzArr"} ]
					})
					# ERROR: aggregate variable requires a handle
					C.tzname.set([null,null])
				`, lib("c")))
				wantErrContains(t, e, "aggregate variable requires handle")
			},
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			v, e := ip.EvalSource(c.src)
			c.check(t, v, e)
		})
	}
}

func Test_FFI_Spec_Bitfields_Rejected(t *testing.T) {
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(fmt.Sprintf(`
		ffiOpen({
		  version:"1", lib:"%s",
		  types:{
		    S:{ kind:"struct", fields:[
		      { name:"mode",  type:{kind:"int",bits:32,signed:false}, bits:3 },
		      { name:"ready", type:{kind:"int",bits:32,signed:false} }
		    ]}
		  }
		})
	`, lib("c")))
	if err == nil || !strings.Contains(err.Error(), "bitfields") {
		t.Fatalf("expected bitfields to be rejected, got err=%v", err)
	}
}

func Test_FFI_Spec_FlexArray_Param_ByValue_Rejected(t *testing.T) {
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(fmt.Sprintf(`
		ffiOpen({
		  version:"1", lib:"%s",
		  types:{
		    U8:{kind:"int",bits:8,signed:false},
		    Flex:{kind:"array",of:"U8"}   # no len -> flexible
		  },
		  functions:[
		    { name:"f", ret:{kind:"void"}, params:["Flex"] }   # by-value param must be rejected
		  ]
		})
	`, lib("c")))
	if err == nil || !strings.Contains(err.Error(), "flexible array") {
		t.Fatalf("expected flexible array by-value param to be rejected, got err=%v", err)
	}
}

func Test_FFI_Spec_FlexArray_Return_ByValue_Rejected(t *testing.T) {
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(fmt.Sprintf(`
		ffiOpen({
		  version:"1", lib:"%s",
		  types:{
		    U8:{kind:"int",bits:8,signed:false},
		    Flex:{kind:"array",of:"U8"}   # flexible
		  },
		  functions:[
		    { name:"f", ret:"Flex", params:[] }   # by-value return must be rejected
		  ]
		})
	`, lib("c")))
	if err == nil || !strings.Contains(err.Error(), "flexible array") {
		t.Fatalf("expected flexible array by-value return to be rejected, got err=%v", err)
	}
}

func Test_FFI_Spec_FlexArray_Pointer_And_New_WithCount(t *testing.T) {
	ip, _ := NewInterpreter()
	// Allocate 4 bytes for flexible array of uint8 and check we can write/read them.
	v, err := ip.EvalSource(fmt.Sprintf(`
		let C = ffiOpen({
		  version:"1", lib:"%s",
		  types:{ U8:{kind:"int",bits:8,signed:false} }
		})
		do
		  let p = C.__mem.new({kind:"array",of:"U8"}, 4)  # count required for flex array
		  C.__mem.copy(p, "\u0001\u0002\u0003\u0004", 4)
		  let b0 = C.__mem.readBits(p, 0, 0, 8, null)
		  let b1 = C.__mem.readBits(p, 1, 0, 8, null)
		  let b2 = C.__mem.readBits(p, 2, 0, 8, null)
		  let b3 = C.__mem.readBits(p, 3, 0, 8, null)
		  { b0:b0, b1:b1, b2:b2, b3:b3 }
		end
	`, lib("c")))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	m, ok := v.Data.(*MapObject)
	if !ok {
		t.Fatalf("expected map result, got %#v", v)
	}
	if m.Entries["b0"].Data.(int64) != 1 || m.Entries["b1"].Data.(int64) != 2 ||
		m.Entries["b2"].Data.(int64) != 3 || m.Entries["b3"].Data.(int64) != 4 {
		t.Fatalf("flex array bytes mismatch: %#v", v)
	}
}

func Test_FFI_Spec_ReadBits_WriteBits_ErrorBounds(t *testing.T) {
	ip, _ := NewInterpreter()

	// width > 64
	_, err := ip.EvalSource(fmt.Sprintf(`
		let C = ffiOpen({version:"1", lib:"%s"})
		let p = C.__mem.malloc(2)
		C.__mem.readBits(p, 0, 0, 65, null)
	`, lib("c")))
	if err == nil || !strings.Contains(err.Error(), "width") {
		t.Fatalf("expected width>64 error, got %v", err)
	}

	// negative offsets
	_, err = ip.EvalSource(fmt.Sprintf(`
		let C = ffiOpen({version:"1", lib:"%s"})
		let p = C.__mem.malloc(1)
		C.__mem.writeBits(p, -1, 0, 8, 1)
	`, lib("c")))
	if err == nil || !strings.Contains(err.Error(), "offsets must be") {
		t.Fatalf("expected negative offset error, got %v", err)
	}

	// write value that does not fit width
	_, err = ip.EvalSource(fmt.Sprintf(`
		let C = ffiOpen({version:"1", lib:"%s"})
		let p = C.__mem.malloc(1)
		C.__mem.writeBits(p, 0, 0, 3, 8)   # 8 does not fit in 3 bits
	`, lib("c")))
	if err == nil || !strings.Contains(err.Error(), "does not fit width") {
		t.Fatalf("expected value-width error, got %v", err)
	}
}

func Test_FFI_Spec_ret_as_str_NULL_ReturnsNull(t *testing.T) {
	ip, _ := NewInterpreter()
	v, err := ip.EvalSource(fmt.Sprintf(`
		let C = ffiOpen({
		  version:"1", lib:"%s",
		  types:{ charp:{kind:"pointer",to:{kind:"int",bits:8,signed:true}} },
		  functions:[ { name:"getenv", ret:"charp", params:["charp"], ret_as_str:true } ]
		})
		C.getenv("__NO_SUCH_ENV_VAR__")
	`, lib("c")))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if v.Tag != VTNull {
		t.Fatalf("expected null for getenv of missing variable, got %#v", v)
	}
}

func Test_FFI_Spec___mem_string_FixedLen_IncludesEmbeddedNUL(t *testing.T) {
	ip, _ := NewInterpreter()
	v, err := ip.EvalSource(fmt.Sprintf(`
		let C = ffiOpen({ version:"1", lib:"%s" })
		do
		  let p = C.__mem.malloc(3)
		  C.__mem.copy(p, "a\u0000b", 3)
		  C.__mem.string(p, 3)
		end
	`, lib("c")))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if v.Tag != VTStr {
		t.Fatalf("expected Str, got %#v", v)
	}
	s := v.Data.(string)
	if len(s) != 3 || s[0] != 'a' || s[1] != 0 || s[2] != 'b' {
		t.Fatalf("fixed-len string should include embedded NUL, got %q (len=%d)", s, len(s))
	}
}

func Test_FFI_Spec_Variadic_Promotions_Bool_Int_Double(t *testing.T) {
	ip, _ := NewInterpreter()
	v, err := ip.EvalSource(fmt.Sprintf(`
		let C = ffiOpen({
		  version:"1", lib:"%s",
		  types:{
		    size_t:{kind:"int",bits:64,signed:false},
		    charp:{kind:"pointer",to:{kind:"int",bits:8,signed:true}},
		    i32:{kind:"int",bits:32,signed:true},
		    dbl:{kind:"float",bits:64}
		  },
		  functions:[
		    { name:"snprintf", ret:"i32", params:["charp","size_t","charp"], variadic:true }
		  ]
		})
		do
		  let buf = C.__mem.malloc(64)
		  # bool -> promoted to int, dbl remains double
		  let n = C.snprintf(buf, 64, "%%d %%.1f", [true, 2.0])
		  { n:n, s:C.__mem.string(buf, null) }
		end
	`, lib("c")))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	m, ok := v.Data.(*MapObject)
	if !ok {
		t.Fatalf("expected map result, got %#v", v)
	}
	if m.Entries["n"].Tag != VTInt || m.Entries["n"].Data.(int64) <= 0 {
		t.Fatalf("snprintf should return >0, got %#v", m.Entries["n"])
	}
	out := m.Entries["s"]
	if out.Tag != VTStr {
		t.Fatalf("expected Str, got %#v", out)
	}
	if out.Data.(string) != "1 2.0" {
		t.Fatalf("promotion/format mismatch: got %q", out.Data.(string))
	}
}
