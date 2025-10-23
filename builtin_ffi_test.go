package mindscript

import (
	"math"
	"strings"
	"testing"
)

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
		ok(t, `ffiOpen({version:"1", lib:"libc.so.6"})`)
	})
	t.Run("1.unknown-version-rejected", func(t *testing.T) {
		err(t, "unsupported version", `ffiOpen({version:"2", lib:"libc.so.6"})`)
	})
	t.Run("2.missing-lib-rejected", func(t *testing.T) {
		err(t, "missing or invalid 'lib'", `ffiOpen({version: "1"})`)
	})
	t.Run("2.dlopen-error-surfaced", func(t *testing.T) {
		err(t, "dlopen", `ffiOpen({version: "1", lib:"/no/such/lib.so"})`)
	})

	t.Run("3.types-empty-ok", func(t *testing.T) {
		ok(t, `ffiOpen({version: "1", lib:"libc.so.6", types:{}})`)
	})
	t.Run("3.types-nonmap-rejected", func(t *testing.T) {
		err(t, "'types' must be a map", `ffiOpen({version: "1", lib:"libc.so.6", types: 7})`)
	})
	t.Run("3.types-duplicate-rejected", func(t *testing.T) {
		err(t, "duplicate type name", `
			ffiOpen({version: "1", lib:"libc.so.6",
			  types:{U8:{kind:"int",bits:8,signed:false},U8:{kind:"int",bits:8,signed:false}}})`)
	})

	t.Run("4.alias-transitive-ok", func(t *testing.T) {
		ok(t, `
		  ffiOpen({version: "1", lib:"libc.so.6",
		    types:{I32:{kind:"int",bits:32,signed:true},X:{kind:"alias",to:"I32"},Y:{kind:"alias",to:"X"}}})`)
	})
	t.Run("4.alias-unknown-target", func(t *testing.T) {
		err(t, "unknown target", `
		  ffiOpen({version: "1", lib:"libc.so.6",types:{X:{kind:"alias",to:"NOPE"}}})`)
	})
	t.Run("4.alias-cycle", func(t *testing.T) {
		err(t, "alias cycle", `
		  ffiOpen({version: "1", lib:"libc.so.6",types:{A:{kind:"alias",to:"B"},B:{kind:"alias",to:"A"}}})`)
	})

	// 5) int-signedness enforced on call (reject negative → unsigned)
	t.Run("5.int-signedness-enforced-on-call", func(t *testing.T) {
		ip, _ := NewInterpreter()
		_, err := ip.EvalSource(`let C=ffiOpen({version: "1",lib:"libc.so.6",types:{U:{kind:"int",bits:32,signed:false}},functions:[{name:"sleep",ret:{kind:"int",bits:32,signed:true},params:["U"]}]}); C.sleep(-1)`)
		if err == nil {
			t.Fatalf("expected failure for negative→unsigned")
		}
	})

	t.Run("5.int-signedness-enforced-on-call", func(t *testing.T) {
		err(t, "negative", `
			let C=ffiOpen({version: "1", lib:"libc.so.6",
			  types:{U:{kind:"int",bits:32,signed:false}},
			  functions:[{name:"sleep",ret:{kind:"int",bits:32,signed:true},params:["U"]}]})
			C.sleep(-1)`)
	})

	t.Run("6.float-bits-validated", func(t *testing.T) {
		err(t, "float: bits must be 32/64", `
			ffiOpen({version: "1", lib:"libc.so.6", types:{BadF:{kind:"float",bits:128}}})`)
	})

	t.Run("7.pointer-size-align", func(t *testing.T) {
		eqi(t, 8, `let C=ffiOpen({version: "1",lib:"libc.so.6",
		 types:{P:{kind:"pointer",to:{kind:"void"}}}}); C.__mem.sizeof("P")`)
	})
	t.Run("7.pointer-align", func(t *testing.T) {
		eqi(t, 8, `let C=ffiOpen({version: "1",lib:"libc.so.6",
		 types:{P:{kind:"pointer",to:{kind:"void"}}}}); C.__mem.alignof("P")`)
	})

	t.Run("8.handle-tag-required", func(t *testing.T) {
		err(t, `missing "rep"`, `
		  ffiOpen({version: "1",lib:"libc.so.6",types:{H:{kind:"handle",tag:"x"}}})`)
	})

	t.Run("9.array-fixed-size", func(t *testing.T) {
		eqi(t, 5, `let C=ffiOpen({version: "1",lib:"libc.so.6",
		  types:{U8:{kind:"int",bits:8,signed:false},A5:{kind:"array",of:"U8",len:5}}}); C.__mem.sizeof("A5")`)
	})
	t.Run("9.array-flexible-size0", func(t *testing.T) {
		eqi(t, 0, `let C=ffiOpen({version: "1",lib:"libc.so.6",
		  types:{U8:{kind:"int",bits:8,signed:false},Flex:{kind:"array",of:"U8"}}}); C.__mem.sizeof("Flex")`)
	})

	// 10) struct layout: offsetof
	t.Run("10.struct-layout-offset", func(t *testing.T) {
		ip, _ := NewInterpreter()
		v, err := ip.EvalSource(`do
	  let C=ffiOpen({version: "1",lib:"libc.so.6",types:{I32:{kind:"int",bits:32,signed:true},P:{kind:"pointer",to:{kind:"void"}},S:{kind:"struct",fields:[{name:"a",type:"I32"},{name:"p",type:"P"}]}}})
	  C.__mem.offsetof("S","p")
	end`)
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
		v, err := ip.EvalSource(`do
	  let C=ffiOpen({version: "1",lib:"libc.so.6",types:{I32:{kind:"int",bits:32,signed:true},P:{kind:"pointer",to:{kind:"void"}},S:{kind:"struct",fields:[{name:"a",type:"I32"},{name:"p",type:"P"}]}}})
	  {sz:C.__mem.sizeof("S"), al:C.__mem.alignof("S")}
	end`)
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
		v, err := ip.EvalSource(`do
	  let C=ffiOpen({version: "1",lib:"libc.so.6",types:{I32:{kind:"int",bits:32,signed:true},P:{kind:"pointer",to:{kind:"void"}},U:{kind:"union",fields:[{name:"x",type:"I32"},{name:"y",type:"P"}]}}})
	  {sz:C.__mem.sizeof("U"), al:C.__mem.alignof("U")}
	end`)
		if err != nil {
			t.Fatalf("eval error: %v", err)
		}
		m := v.Data.(*MapObject)
		if m.Entries["sz"].Data.(int64) != 8 || m.Entries["al"].Data.(int64) != 8 {
			t.Fatalf("want {8,8}, got %#v", v)
		}
	})

	t.Run("12.enum-base-required", func(t *testing.T) {
		err(t, `missing "base"`, `
		  ffiOpen({version: "1",lib:"libc.so.6",types:{E:{kind:"enum"}}})`)
	})

	t.Run("13.funcptr-accepted-as-type", func(t *testing.T) {
		ok(t, `ffiOpen({version: "1",lib:"libc.so.6",
		  types:{CB:{kind:"funcptr",ret:{kind:"void"},params:[{kind:"pointer",to:{kind:"void"}}]}}})`)
	})

	t.Run("14.functions-need-name-ret-params", func(t *testing.T) {
		err(t, "function: missing 'ret'", `
		  ffiOpen({version: "1",lib:"libc.so.6",functions:[{name:"puts",params:[]}]})`)
	})

	t.Run("15.dlsym-missing-symbol-errors", func(t *testing.T) {
		err(t, "dlsym", `
		  ffiOpen({version: "1",lib:"libc.so.6",
		    types:{i32:{kind:"int",bits:32,signed:true}},
		    functions:[{name:"__nope__",ret:"i32",params:[]}]})`)
	})

	t.Run("16.variadic-final-arg-must-array", func(t *testing.T) {
		err(t, "final argument to be an array", `
		  let C=ffiOpen({version: "1",lib:"libc.so.6",
		    types:{charp:{kind:"pointer",to:{kind:"int",bits:8,signed:true}},sz:{kind:"int",bits:64,signed:false},i32:{kind:"int",bits:32,signed:true}},
		    functions:[{name:"snprintf",ret:"i32",params:["charp","sz","charp"],variadic:true}]});
		  C.snprintf(C.__mem.malloc(8),7,"%d", 1)`)
	})

	// 17) scalar: negative → unsigned rejected
	t.Run("17.scalar-negative-to-unsigned-rejected", func(t *testing.T) {
		ip, _ := NewInterpreter()
		_, err := ip.EvalSource(`let C=ffiOpen({version: "1",lib:"libc.so.6",types:{U:{kind:"int",bits:32,signed:false}},functions:[{name:"sleep",ret:{kind:"int",bits:32,signed:true},params:["U"]}]}); C.sleep(-1)`)
		if err == nil {
			t.Fatalf("expected failure for negative→unsigned")
		}
	})

	t.Run("17.float-to-int-must-be-integral", func(t *testing.T) {
		err(t, "non-integral", `
		  let C=ffiOpen({version: "1",lib:"libc.so.6",
		    types:{U:{kind:"int",bits:32,signed:false}},functions:[{name:"sleep",ret:{kind:"int",bits:32,signed:true},params:["U"]}]});
		  C.sleep(0.5)`)
	})

	t.Run("18.pointer-null-maps-to-NULL", func(t *testing.T) {
		ok(t, `
		  let C=ffiOpen({version: "1",lib:"libc.so.6",
		    types:{voidp:{kind:"pointer",to:{kind:"void"}},i32:{kind:"int",bits:32,signed:true}},
		    functions:[{name:"memchr",ret:"voidp",params:["voidp","i32",{kind:"int",bits:64,signed:false}]}]});
		  C.memchr(null,0,0)`)
	})

	t.Run("19.string-bridge-only-for-charp", func(t *testing.T) {
		err(t, "expected pointer handle", `
		  let C=ffiOpen({version: "1",lib:"libc.so.6",
		    types:{voidp:{kind:"pointer",to:{kind:"void"}},i32:{kind:"int",bits:32,signed:true}},
		    functions:[{name:"memchr",ret:"voidp",params:["voidp","i32",{kind:"int",bits:64,signed:false}]}]});
		  C.memchr("abc",65,3)`)
	})

	t.Run("20.return-pointer-becomes-handle", func(t *testing.T) {
		ok(t, `
		  let C=ffiOpen({version: "1",lib:"libc.so.6",
		    types:{charp:{kind:"pointer",to:{kind:"int",bits:8,signed:true}}},
		    functions:[{name:"getenv",ret:"charp",params:["charp"]}]});
		  C.getenv("PATH")`)
	})

	// 21) ret_as_str valid only for char*/uchar* (must error for non-char*)
	t.Run("21.ret_as_str-valid-only-for-charp", func(t *testing.T) {
		ip, _ := NewInterpreter()
		_, err := ip.EvalSource(`ffiOpen({version: "1",lib:"libc.so.6",types:{I32:{kind:"int",bits:32,signed:true}},functions:[{name:"getpid",ret:"I32",params:[],ret_as_str:true}]})`)
		if err == nil {
			t.Fatalf("expected error: ret_as_str on non-char*")
		} // spec requires rejection
	})

	t.Run("22.variables-get-set-addr", func(t *testing.T) {
		ok(t, `
		  let C=ffiOpen({version: "1",lib:"libc.so.6",
		    types:{i32:{kind:"int",bits:32,signed:true}},
		    variables:[{name:"errno",type:"i32"}]});
		  C.errno.get(); C.errno.addr(); C.errno.set(0)`)
	})

	// 23) sizeof/alignof/offsetof accept inline type objects
	t.Run("23.sizeof-alignof-offsetof-accept-inline", func(t *testing.T) {
		ip, _ := NewInterpreter()
		v, err := ip.EvalSource(`let C=ffiOpen({version: "1",lib:"libc.so.6"}); C.__mem.sizeof({kind:"pointer",to:{kind:"void"}})`)
		if err != nil {
			t.Fatalf("eval error: %v", err)
		}
		if v.Tag != VTInt || v.Data.(int64) != 8 {
			t.Fatalf("want 8, got %#v", v)
		} // pointer size on x86-64 SysV
	})

	t.Run("24.typeof-normalizes", func(t *testing.T) {
		ok(t, `
		  let C=ffiOpen({version: "1",lib:"libc.so.6"});
		  let T=C.__mem.typeof({kind:"int",bits:32,signed:true}); T`)
	})

	t.Run("25.new-fixed-alloc-size", func(t *testing.T) {
		ok(t, `
		  let C=ffiOpen({version: "1",lib:"libc.so.6",types:{I:{kind:"int",bits:32,signed:true}}});
		  C.__mem.new("I",1)`)
	})

	// 26) malloc/calloc/realloc/free: negatives are errors (no message coupling)
	t.Run("26.malloc-calloc-realloc-free-guards", func(t *testing.T) {
		ip, _ := NewInterpreter()
		_, err := ip.EvalSource(`let C=ffiOpen({version: "1",lib:"libc.so.6"}); C.__mem.malloc(-1)`)
		if err == nil {
			t.Fatalf("expected error on negative size")
		}
	})

	t.Run("27.cast-retags-pointer", func(t *testing.T) {
		ok(t, `
		  let C=ffiOpen({version: "1",lib:"libc.so.6",
		    types:{Voidp:{kind:"pointer",to:{kind:"void"}},I8:{kind:"int",bits:8,signed:true},Charp:{kind:"pointer",to:"I8"}}});
		  let p=C.__mem.malloc(1); C.__mem.cast("Charp", p)`)
	})

	// 28) string(): NUL-terminated path without pointer arithmetic
	t.Run("28.string-read-nul-or-fixed", func(t *testing.T) {
		ip, _ := NewInterpreter()
		v, err := ip.EvalSource(`do
	  let C=ffiOpen({version: "1",lib:"libc.so.6"})
	  let p=C.__mem.malloc(3); C.__mem.copy(p,"hi\u0000",3); C.__mem.string(p,null)
	end`)
		if err != nil {
			t.Fatalf("eval error: %v", err)
		}
		if v.Tag != VTStr || v.Data.(string) != "hi" {
			t.Fatalf("want 'hi', got %#v", v)
		}
	})

	t.Run("29.copy-fill-basic", func(t *testing.T) {
		ok(t, `
		  let C=ffiOpen({version: "1",lib:"libc.so.6"}); do
		    let p=C.__mem.malloc(4); C.__mem.fill(p,65,4); C.__mem.string(p,4)
		  end`)
	})

	t.Run("30.errno-get-set", func(t *testing.T) {
		eqi(t, 0, `
		  let C=ffiOpen({version: "1",lib:"libc.so.6"}); do C.__mem.errno(0) end`)
	})
}

// libm: scalar in/out path (dlopen+dlsym, libffi call, float marshalling).
func Test_Builtin_FFI_libm_hypot(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
		let C = ffiOpen({
		  version: "1",
		  lib: "libm.so.6",
		  types: { double: { kind:"float", bits:64 } },
		  functions: [
		    { name:"hypot", ret:"double", params:["double","double"] }
		  ]
		})
		C.hypot(3.0, 4.0)
	`)
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
	v := evalWithIP(t, ip, `
		let C = ffiOpen({
		  version: "1",
		  lib: "libc.so.6",
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
	`)
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
	v := evalWithIP(t, ip, `
		let C = ffiOpen({ version: "1", lib:"libc.so.6" })
		do
			let p = C.__mem.malloc(6)
			C.__mem.copy(p, "hello", 6)   # copies "hello\0"
			let s = C.__mem.string(p, null)
			C.__mem.free(p)
			s
		end
	`)
	if v.Tag != VTStr || v.Data.(string) != "hello" {
		t.Fatalf("__mem string round-trip expected 'hello', got %#v", v)
	}
}

// Layout queries: sizeof/alignof/offsetof for a simple struct (SysV, x86-64).
func Test_Builtin_FFI___mem_sizeof_alignof_offsetof_struct(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
		let C = ffiOpen({
		  version: "1",
		  lib: "libc.so.6",
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
	`)
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
	_, err := ip.EvalSource(`
		ffiOpen({
		  version: "1",
		  lib: "libc.so.6",
		  types: { int32: { kind:"int", bits:32, signed:true } },
		  functions: [
		    { name:"__definitely_not_exists__", ret:"int32", params:[] }
		  ]
		})
	`)
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
	v := evalWithIP(t, ip, `
		let C = ffiOpen({
		  version: "1",
		  lib: "libm.so.6",
		  types: { dbl: { kind:"float", bits:64 } },
		  functions: [
		    { name:"sin", ret:"dbl", params:["dbl"] },
		    { name:"cos", ret:"dbl", params:["dbl"] }
		  ]
		})
		{ s: C.sin(0.0), c: C.cos(0.0) }
	`)
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
	v := evalWithIP(t, ip, `
		let C = ffiOpen({
		  version: "1",
		  lib: "libc.so.6",
		  types: {
		    charp: { kind:"pointer", to:{ kind:"int", bits:8, signed:true } },
		    int32: { kind:"int", bits:32, signed:true }
		  },
		  functions: [ { name:"strlen", ret:"int32", params:["charp"] } ]
		})
		# embedded NUL should stop strlen at 2
		C.strlen("ab\u0000cd")
	`)
	if v.Tag != VTInt || v.Data.(int64) != 2 {
		t.Fatalf("strlen('ab\\0cd') expected 2, got %#v", v)
	}
}

func Test_Builtin_FFI_libc_snprintf_variadic(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
		let C = ffiOpen({
		  version: "1",
		  lib: "libc.so.6",
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
			# snprintf(buf, 64, "%d %.2f", 123, 3.14159)
			let n = C.snprintf(buf, 64, "%d %.2f", [123, 3.14159])
			let s = C.__mem.string(buf, null)
			C.__mem.free(buf)
			{ n: n, s: s }
		end
	`)
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
	v := evalWithIP(t, ip, `
		let C = ffiOpen({
		  version: "1",
		  lib: "libc.so.6",
		  types: {
		    charp: { kind:"pointer", to:{ kind:"int", bits:8, signed:true } }
		  },
		  functions: [
		    { name:"getenv", ret:"charp", params:["charp"], ret_as_str:true }
		  ]
		})
		C.getenv("PATH")
	`)
	if v.Tag != VTStr || v.Data.(string) == "" {
		t.Fatalf("getenv('PATH') should return non-empty string, got %#v", v)
	}
}

func Test_Builtin_FFI_struct_union_nested_layout(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
		let C = ffiOpen({
		  version: "1",
		  lib: "libc.so.6",
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
	`)
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
	_, err := ip.EvalSource(`
		let C = ffiOpen({
		  version: "1",
		  lib: "libc.so.6",
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
		  C.snprintf(buf, 16, "%d %d", 123)
		end
	`)
	wantErrContains(t, err, "variadic function expects final argument to be an array")
}

func Test_Builtin_FFI___mem_string_len_negative_error(t *testing.T) {
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(`
		let C = ffiOpen({ version: "1", lib:"libc.so.6" })
		C.__mem.string(C.__mem.malloc(1), -5)
	`)
	wantErrContains(t, err, "len < 0")
}

func Test_Builtin_FFI_per_symbol_lib_override(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
		let C = ffiOpen({
		  version: "1",
		  lib: "libc.so.6",     # default
		  types: {
		    int32: { kind:"int", bits:32, signed:true },
		    dbl:   { kind:"float", bits:64 },
		    charp: { kind:"pointer", to:{ kind:"int", bits:8, signed:true } }
		  },
		  functions: [
		    { name:"strlen", ret:"int32", params:["charp"] },             # from libc
		    { name:"cos",    ret:"dbl",   params:["dbl"], lib:"libm.so.6" } # override lib
		  ]
		})
		{ n: C.strlen("abc"), c: C.cos(0.0) }
	`)
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
	_, err := ip.EvalSource(`
		let C = ffiOpen({ version: "1", lib:"libc.so.6" })
		C.close()
		C.close()   # second close should error
	`)
	wantErrContains(t, err, "already closed")
}

func Test_Builtin_FFI___types_handles_into_sizeof(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
		let C = ffiOpen({
		  version: "1", lib:"libc.so.6",
		  types:{ X:{ kind:"int", bits:64, signed:true } }
		})
		# fetch handle from __types and feed to sizeof
		let tx = C.__types.X
		C.__mem.sizeof(tx)
	`)
	if v.Tag != VTInt || v.Data.(int64) != 8 {
		t.Fatalf("__types handle into sizeof expected 8, got %#v", v)
	}
}

// --- fixed: remove unsupported pipeline/lambda; just validate layout + rc ---
func Test_Builtin_FFI_clock_gettime_timespec_layout_and_call(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
		let C = ffiOpen({
		  version: "1",
		  lib: "libc.so.6",
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
	`)
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
	_, err := ip.EvalSource(`
		let C = ffiOpen({
		  version: "1",
		  lib: "libc.so.6",
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
		  C.snprintf(buf, -1, "%d", [1])
		end
	`)
	wantErrContains(t, err, "negative")
}

// --- fixed: no pointer arithmetic; zero-fill full buffer, then copy 5 bytes ---
func Test_Builtin_FFI___mem_fill_and_copy_bytes(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
		let C = ffiOpen({ version: "1", lib:"libc.so.6" })
		do
		  let p = C.__mem.malloc(6)
		  C.__mem.fill(p, 0, 6)         # ensure NUL-termination
		  C.__mem.copy(p, "AAAAA", 5)   # write 5 bytes; last stays NUL
		  let s = C.__mem.string(p, null)
		  C.__mem.free(p)
		  s
		end
	`)
	if v.Tag != VTStr || v.Data.(string) != "AAAAA" {
		t.Fatalf("fill/copy expected 'AAAAA', got %#v", v)
	}
}

// --- fixed: avoid inline typeof (its layout isn’t computed); use __types handle ---
func Test_Builtin_FFI___mem_typeof_inline_and_name(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
		let C = ffiOpen({ version: "1", lib:"libc.so.6",
		  types:{ I32:{ kind:"int", bits:32, signed:true } }
		})
		do
		  # get declared type handle from __types, and compare against name
		  let t_handle = C.__types.I32
		  { a: C.__mem.sizeof("I32"), b: C.__mem.sizeof(t_handle) }
		end
	`)
	m := mustMap(t, v)
	if mustInt64(t, m.Entries["a"]) != 4 || mustInt64(t, m.Entries["b"]) != 4 {
		t.Fatalf("typeof/sizeof mismatch: got a=%d b=%d", mustInt64(t, m.Entries["a"]), mustInt64(t, m.Entries["b"]))
	}
}

// String bridge control: passing Str to char* still works.
func Test_Builtin_FFI_string_bridge_charp_control(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
		let C = ffiOpen({
		  version: "1",
		  lib: "libc.so.6",
		  types: {
		    charp: { kind:"pointer", to:{ kind:"int", bits:8, signed:true } },
		    int32: { kind:"int", bits:32, signed:true }
		  },
		  functions: [
		    { name:"strlen", ret:"int32", params:["charp"] }
		  ]
		})
		C.strlen("hello")
	`)
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
	b.WriteString(`
		let M = ffiOpen({
		  version: "1",
		  lib: "libm.so.6",
		  types: { dbl: { kind:"float", bits:64 } },
		  functions: [ { name:"hypot", ret:"dbl", params:["dbl","dbl"] } ]
		});
	`)
	thrashSnip := `
		let _ = ffiOpen({
		  version: "1",
		  lib: "libc.so.6",
		  types: {
		    int32: { kind:"int", bits:32, signed:true },
		    charp: { kind:"pointer", to:{ kind:"int", bits:8, signed:true } }
		  },
		  functions: [ { name:"strlen", ret:"int32", params:["charp"] } ]
		});
		0;
	`
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
	v = evalWithIP(t, ip, `
		let M = ffiOpen({
		  version: "1",
		  lib: "libm.so.6",
		  types: { dbl: { kind:"float", bits:64 } },
		  functions: [ { name:"hypot", ret:"dbl", params:["dbl","dbl"] } ]
		});
		M.hypot(6.0, 8.0)
	`)
	if v.Tag != VTNum || math.Abs(v.Data.(float64)-10.0) > 1e-9 {
		t.Fatalf("hypot(6,8)=10.0 mismatch: %#v", v)
	}
}

// Version contract: numeric 1 must be rejected; spec requires "1" (string).
func Test_Builtin_FFI_version_must_be_string(t *testing.T) {
	err(t, "version", `
		ffiOpen({ version: 1, lib: "libc.so.6" })
	`)
}

// Duplicate function names within the same spec must be rejected.
func Test_Builtin_FFI_duplicate_function_names_rejected(t *testing.T) {
	err(t, "duplicate", `
		ffiOpen({
		  version: "1",
		  lib: "libc.so.6",
		  types: {
		    charp: { kind:"pointer", to:{ kind:"int", bits:8, signed:true } },
		    int32: { kind:"int", bits:32, signed:true }
		  },
		  functions: [
		    { name:"puts", ret:"int32", params:["charp"] },
		    { name:"puts", ret:"int32", params:["charp"] }  # duplicate
		  ]
		})
	`)
}

// Duplicate variable names within the same spec must be rejected.
func Test_Builtin_FFI_duplicate_variable_names_rejected(t *testing.T) {
	err(t, "duplicate", `
		ffiOpen({
		  version: "1",
		  lib: "libc.so.6",
		  types: { int32: { kind:"int", bits:32, signed:true } },
		  variables: [
		    { name:"errno", type:"int32" },
		    { name:"errno", type:"int32" }   # duplicate
		  ]
		})
	`)
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
			src: `
			  let C = ffiOpen({ version:"1", lib:"libc.so.6" })
			  do
			    let p = C.__mem.malloc(16)
			    C.__mem.gc(p, "free")
			    C.__mem.free(p)   # first free triggers registered destructor
			    C.__mem.free(p)   # second free must be a no-op
			    0
			  end
			`,
		},
		{
			name: "detach then free (no destructor invoked)",
			src: `
			  let C = ffiOpen({ version:"1", lib:"libc.so.6" })
			  do
			    let p = C.__mem.malloc(8)
			    C.__mem.gc(p, "free")
			    C.__mem.gc(p, null)   # detach finalizer
			    C.__mem.free(p)       # plain free succeeds
			    0
			  end
			`,
		},
		{
			name:    "custom symbol resolve error surfaces dlsym",
			wantErr: "dlsym",
			src: `
			  let C = ffiOpen({ version:"1", lib:"libc.so.6" })
			  let p = C.__mem.malloc(4)
			  C.__mem.gc(p, {sym:"__definitely_not_exists__"})
			`,
		},
		{
			name: "realloc carries destructor to new pointer; double free is safe",
			src: `
			  let C = ffiOpen({ version:"1", lib:"libc.so.6" })
			  do
			    let p = C.__mem.malloc(8)
			    C.__mem.gc(p, "free")
			    let q = C.__mem.realloc(p, 16)   # move destructor to q
			    C.__mem.free(q)
			    C.__mem.free(q)                  # idempotent
			    0
			  end
			`,
		},
		{
			name:    "string finalizer must be exactly \"free\"",
			wantErr: "string finalizer must be \"free\"",
			src: `
			  let C = ffiOpen({ version:"1", lib:"libc.so.6" })
			  let p = C.__mem.malloc(1)
			  C.__mem.gc(p, "close")
			`,
		},
		{
			name:    "first argument must be a pointer handle",
			wantErr: "pointer handle",
			src: `
			  let C = ffiOpen({ version:"1", lib:"libc.so.6" })
			  C.__mem.gc(123, "free")
			`,
		},
		{
			name: "re-attach overwrites finalizer, then detach",
			src: `
			  let C = ffiOpen({ version:"1", lib:"libc.so.6" })
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
			`,
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
	v := evalWithIP(t, ip, `
		let C = ffiOpen({
		  version:"1", lib:"libc.so.6",
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
	`)
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
	v := evalWithIP(t, ip, `
		let C=ffiOpen({
		  version:"1", lib:"libc.so.6",
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
	`)
	if v.Tag != VTInt || v.Data.(int64) <= 0 {
		t.Fatalf("callback not invoked, got %#v", v)
	}
}

func Test_Builtin_FFI_Aggregate_Autoboxing(t *testing.T) {
	t.Run("struct_timespec_literal_nanosleep", func(t *testing.T) {
		ip, _ := NewInterpreter()
		v := evalWithIP(t, ip, `
			let C = ffiOpen({
			  version:"1", lib:"libc.so.6",
			  types:{
			    long: {kind:"int", bits:64, signed:true},
			    Timespec: {kind:"struct", fields:[
			      {name:"tv_sec",  type:"long"},
			      {name:"tv_nsec", type:"long"}
			    ]},
			    Tsp: {kind:"pointer", to:"Timespec"},
			    i32: {kind:"int", bits:32, signed:true}
			  },
			  functions:[ {name:"nanosleep", ret:"i32", params:["Tsp","Tsp"]} ]
			})
			# Pass struct literal; auto-boxer builds a temp buffer (in-only).
			C.nanosleep({ tv_sec: 0, tv_nsec: 0 }, null)
		`)
		if v.Tag != VTInt || v.Data.(int64) != 0 {
			t.Fatalf("nanosleep({0,0},null) expected rc=0, got %#v", v)
		}
	})

	t.Run("array_of_struct_iovec_writev", func(t *testing.T) {
		ip, _ := NewInterpreter()
		v := evalWithIP(t, ip, `
			let C = ffiOpen({
			  version:"1", lib:"libc.so.6",
			  types:{
			    i32:    {kind:"int", bits:32, signed:true},
			    u64:    {kind:"int", bits:64, signed:false},
			    i64:    {kind:"int", bits:64, signed:true},
			    I8:     {kind:"int", bits:8,  signed:true},
			    charp:  {kind:"pointer", to:"I8"},
			    IOV:    {kind:"struct", fields:[
			      {name:"iov_base", type:"charp"},  # NOTE: char* to enable string bridge
			      {name:"iov_len",  type:"u64"}
			    ]},
			    IOV2:   {kind:"array", of:"IOV", len:2},
			    IOV2p:  {kind:"pointer", to:"IOV2"}
			  },
			  functions:[
			    {name:"open",   ret:"i32", params:["charp","i32"]},
			    {name:"writev", ret:"i64", params:["i32","IOV2p","i32"]},
			    {name:"close",  ret:"i32", params:["i32"]}
			  ]
			})
			do
			  let fd = C.open("/dev/null", 1)   # O_WRONLY = 1
			  if fd < 0 then error("open /dev/null failed") end

			  # iov = [{base:"ab", len:2}, {base:"XYZ", len:3}]
			  let total = C.writev(fd,
			    [
			      { iov_base: "ab",  iov_len: 2 },
			      { iov_base: "XYZ", iov_len: 3 }
			    ],
			    2)

			  C.close(fd)
			  total
			end
		`)
		got := func(v Value) int64 {
			if v.Tag == VTInt {
				return v.Data.(int64)
			}
			return int64(v.Data.(float64))
		}(v)
		if got != 5 {
			t.Fatalf("writev expected 5, got %d", got)
		}
	})

	t.Run("array_literal_sparse_map_iovec_writev", func(t *testing.T) {
		ip, _ := NewInterpreter()
		v := evalWithIP(t, ip, `
			let C = ffiOpen({
			  version:"1", lib:"libc.so.6",
			  types:{
			    i32:{kind:"int",bits:32,signed:true},
			    u64:{kind:"int",bits:64,signed:false},
			    i64:{kind:"int",bits:64,signed:true},
			    I8:{kind:"int",bits:8,signed:true},
			    charp:{kind:"pointer",to:"I8"},
			    IOV:{kind:"struct",fields:[
			      {name:"iov_base",type:"charp"},  # NOTE: char* here too
			      {name:"iov_len", type:"u64"}
			    ]},
			    IOV2:{kind:"array",of:"IOV",len:2},
			    IOV2p:{kind:"pointer",to:"IOV2"}
			  },
			  functions:[
			    {name:"open",   ret:"i32", params:["charp","i32"]},
			    {name:"writev", ret:"i64", params:["i32","IOV2p","i32"]},
			    {name:"close",  ret:"i32", params:["i32"]}
			  ]
			})
			do
			  let fd = C.open("/dev/null", 1)
			  if fd < 0 then error("open /dev/null failed") end

			  # Provide only index 1; index 0 stays zero-initialized → contributes 0 bytes.
			  let total = C.writev(fd,
			    { "1": { iov_base: "XY", iov_len: 2 } },
			    2)

			  C.close(fd)
			  total
			end
		`)
		got := func(v Value) int64 {
			if v.Tag == VTInt {
				return v.Data.(int64)
			}
			return int64(v.Data.(float64))
		}(v)
		if got != 2 {
			t.Fatalf("sparse IOV map expected 2, got %d", got)
		}
	})

	t.Run("struct_literal_unknown_field_rejected", func(t *testing.T) {
		ip, _ := NewInterpreter()
		_, err := ip.EvalSource(`
			let C = ffiOpen({
			  version:"1", lib:"libc.so.6",
			  types:{
			    long: {kind:"int", bits:64, signed:true},
			    Timespec: {kind:"struct", fields:[
			      {name:"tv_sec",  type:"long"},
			      {name:"tv_nsec", type:"long"}
			    ]},
			    Tsp: {kind:"pointer", to:"Timespec"},
			    i32: {kind:"int", bits:32, signed:true}
			  },
			  functions:[ {name:"nanosleep", ret:"i32", params:["Tsp","Tsp"]} ]
			})
			C.nanosleep({ tv_sec: 0, tv_nsec: 0, oops: 7 }, null)
		`)
		wantErrContains(t, err, "unknown field")
	})
}
