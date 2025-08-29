package mindscript

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// Helper process that the tests spawn via the exec builtin.
// It runs ONLY when GO_WANT_HELPER_PROCESS=1 is set in the environment.
func TestExecHelperProcess(t *testing.T) {
	if os.Getenv("GO_WANT_HELPER_PROCESS") != "1" {
		return
	}
	// Find the args after "--"
	args := os.Args
	i := 0
	for i < len(args) && args[i] != "--" {
		i++
	}
	if i+1 >= len(args) {
		fmt.Fprint(os.Stderr, "bad-invoke")
		os.Exit(2)
	}
	mode := args[i+1]

	switch mode {
	case "stdout":
		// Write to stdout, exit 0
		fmt.Fprint(os.Stdout, "hello-out")
		os.Exit(0)
	case "stderr":
		// Write to stderr, exit with 3
		fmt.Fprint(os.Stderr, "bad-thing")
		os.Exit(3)
	case "stdin":
		// Echo stdin prefixed, exit 0
		data, _ := os.ReadFile(os.Stdin.Name())
		// os.ReadFile on Stdin.Name() doesn't work on all platforms; fallback:
		if len(data) == 0 {
			// best-effort read via io.ReadAll(os.Stdin)
			b := make([]byte, 0, 1024)
			buf := make([]byte, 1024)
			for {
				n, err := os.Stdin.Read(buf)
				if n > 0 {
					b = append(b, buf[:n]...)
				}
				if err != nil {
					break
				}
			}
			data = b
		}
		fmt.Fprint(os.Stdout, "got:"+string(data))
		os.Exit(0)
	case "env":
		// Print FOO from env
		fmt.Fprint(os.Stdout, os.Getenv("FOO"))
		os.Exit(0)
	case "cwd":
		// Print the base name of current working directory
		wd, _ := os.Getwd()
		fmt.Fprint(os.Stdout, filepath.Base(wd))
		os.Exit(0)
	default:
		fmt.Fprint(os.Stderr, "unknown-mode")
		os.Exit(2)
	}
}

func Test_Builtin_Exec_SimpleStdout(t *testing.T) {
	ip, _ := NewRuntime()
	ip.Global.Define("SELF", Str(os.Args[0]))

	src := `
exec([SELF, "-test.run=TestExecHelperProcess", "--", "stdout"], {
  env: {"GO_WANT_HELPER_PROCESS": "1"}
})
`
	v := evalWithIP(t, ip, src)
	m := mustMap(t, v)
	if s, _ := mget(m, "status"); s.Tag != VTInt || s.Data.(int64) != 0 {
		t.Fatalf("status want 0, got %#v", v)
	}
	if out, _ := mget(m, "stdout"); out.Tag != VTStr || out.Data.(string) != "hello-out" {
		t.Fatalf("stdout mismatch: %#v", out)
	}
	if errv, _ := mget(m, "stderr"); errv.Tag != VTStr || errv.Data.(string) != "" {
		t.Fatalf("stderr mismatch: %#v", errv)
	}
}

func Test_Builtin_Exec_NonZeroExit_Stderr(t *testing.T) {
	ip, _ := NewRuntime()
	ip.Global.Define("SELF", Str(os.Args[0]))

	src := `
exec([SELF, "-test.run=TestExecHelperProcess", "--", "stderr"], {
  env: {"GO_WANT_HELPER_PROCESS": "1"}
})
`
	v := evalWithIP(t, ip, src)
	m := mustMap(t, v)
	if s, _ := mget(m, "status"); s.Tag != VTInt || s.Data.(int64) != 3 {
		t.Fatalf("status want 3, got %#v", v)
	}
	if out, _ := mget(m, "stdout"); out.Tag != VTStr || out.Data.(string) != "" {
		t.Fatalf("stdout should be empty, got %#v", out)
	}
	if errv, _ := mget(m, "stderr"); errv.Tag != VTStr || !strings.Contains(errv.Data.(string), "bad-thing") {
		t.Fatalf("stderr mismatch: %#v", errv)
	}
}

func Test_Builtin_Exec_EnvOverlay(t *testing.T) {
	ip, _ := NewRuntime()
	ip.Global.Define("SELF", Str(os.Args[0]))

	src := `
exec([SELF, "-test.run=TestExecHelperProcess", "--", "env"], {
  env: {
    "GO_WANT_HELPER_PROCESS": "1",
    "FOO": "bar-value"
  }
})
`
	v := evalWithIP(t, ip, src)
	m := mustMap(t, v)
	if out, _ := mget(m, "stdout"); out.Tag != VTStr || out.Data.(string) != "bar-value" {
		t.Fatalf("env overlay failed, stdout=%#v", out)
	}
}

func Test_Builtin_Exec_Cwd(t *testing.T) {
	ip, _ := NewRuntime()
	ip.Global.Define("SELF", Str(os.Args[0]))

	tmp := t.TempDir()
	base := filepath.Base(tmp)

	src := fmt.Sprintf(`
exec([SELF, "-test.run=TestExecHelperProcess", "--", "cwd"], {
  env: {"GO_WANT_HELPER_PROCESS": "1"},
  cwd: %q
})
`, tmp)

	v := evalWithIP(t, ip, src)
	m := mustMap(t, v)
	if out, _ := mget(m, "stdout"); out.Tag != VTStr || out.Data.(string) != base {
		t.Fatalf("cwd not respected, want %q, got %#v", base, out)
	}
}

func Test_Builtin_Exec_Stdin(t *testing.T) {
	ip, _ := NewRuntime()
	ip.Global.Define("SELF", Str(os.Args[0]))

	src := `
exec([SELF, "-test.run=TestExecHelperProcess", "--", "stdin"], {
  env: {"GO_WANT_HELPER_PROCESS": "1"},
  stdin: "ABC123"
})
`
	v := evalWithIP(t, ip, src)
	m := mustMap(t, v)
	if out, _ := mget(m, "stdout"); out.Tag != VTStr || out.Data.(string) != "got:ABC123" {
		t.Fatalf("stdin not delivered, stdout=%#v", out)
	}
}

func Test_Builtin_Exec_SpawnError_AnnotatedNull(t *testing.T) {
	ip, _ := NewRuntime()

	// An obviously invalid command name should fail to spawn on all platforms.
	// IMPORTANT: exec is curried; pass the optional second arg explicitly to invoke now.
	src := `exec(["__definitely_not_a_real_command__mindscript__"], null)`
	v := evalWithIP(t, ip, src)
	wantAnnotatedContains(t, v, "exec:")
}
