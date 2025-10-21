package mindscript

import (
	"math"
	"testing"
)

// libm: scalar in/out path (dlopen+dlsym, libffi call, float marshalling).
func Test_Builtin_FFI_libm_hypot(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
		let C = ffiOpen({
		  version: 1,
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
		  version: 1,
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
		let C = ffiOpen({ version:1, lib:"libc.so.6" })
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
		  version: 1,
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
		  version: 1,
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
