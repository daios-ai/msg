// process_builtins.go
//
// Builtins surfaced:
//  1. exec(cmd: [Str], opts: { cwd: Str, env: {}, stdin: Str }?) -> {
//     status!: Int,
//     stdout!: Str,
//     stderr!: Str
//     }?
//
// Conventions:
//   - Functions are camelCase; docs are docstring-style (first line, blank, details).
//   - Uses public API only; contract mistakes are hard errors via fail(...).
//   - System-level failures (e.g., spawn errors) return annotated null.
//   - Tabs for indentation.
package mindscript

import (
	"bytes"
	"fmt"
	"os"
	osexec "os/exec"
	"strings"
)

func registerExecBuiltins(ip *Interpreter, target *Env) {
	ip.RegisterRuntimeBuiltin(
		target,
		"exec",
		[]ParamSpec{
			// cmd: [Str]
			{Name: "cmd", Type: S{"array", S{"id", "Str"}}},
			// opts?: { cwd: Str, env: {}, stdin: Str }
			{
				Name: "opts",
				Type: S{
					"unop", "?", S{
						"map",
						// Optional fields (open-world map); all are soft-optional keys.
						S{"pair", S{"str", "cwd"}, S{"id", "Str"}},
						S{"pair", S{"str", "env"}, S{"map"}}, // map Str -> Str (validated at runtime)
						S{"pair", S{"str", "stdin"}, S{"id", "Str"}},
					},
				},
			},
		},
		// Return: { status!: Int, stdout!: Str, stderr!: Str }?  (annotated null on spawn error)
		S{
			"unop", "?", S{"map",
				S{"pair!", S{"str", "status"}, S{"id", "Int"}},
				S{"pair!", S{"str", "stdout"}, S{"id", "Str"}},
				S{"pair!", S{"str", "stderr"}, S{"id", "Str"}},
			},
		},
		func(_ *Interpreter, ctx CallCtx) Value {
			// --- Parse cmd: [Str] ---
			cmdVal := ctx.Arg("cmd")
			if cmdVal.Tag != VTArray {
				fail("exec: cmd must be [Str]")
			}
			arr := cmdVal.Data.(*ArrayObject).Elems
			if len(arr) == 0 {
				fail("exec: cmd must not be empty")
			}
			args := make([]string, len(arr))
			for i, v := range arr {
				if v.Tag != VTStr {
					fail(fmt.Sprintf("exec: cmd[%d] must be Str", i))
				}
				args[i] = v.Data.(string)
			}

			// --- Parse opts (optional) ---
			var (
				cwd        string
				stdinText  string
				haveStdin  bool
				envOverlay map[string]string // nil if not provided
			)
			if ov := ctx.Arg("opts"); ov.Tag != VTNull {
				if ov.Tag != VTMap {
					fail("exec: opts must be {}")
				}
				opts := ov.Data.(*MapObject)

				// cwd: Str
				if v, ok := opts.Entries["cwd"]; ok {
					if v.Tag != VTStr {
						fail("exec: opts.cwd must be Str")
					}
					cwd = v.Data.(string)
				}

				// stdin: Str
				if v, ok := opts.Entries["stdin"]; ok {
					if v.Tag != VTStr {
						fail("exec: opts.stdin must be Str")
					}
					stdinText = v.Data.(string)
					haveStdin = true
				}

				// env: {}  (map Str -> Str)
				if v, ok := opts.Entries["env"]; ok {
					if v.Tag != VTMap {
						fail("exec: opts.env must be {}")
					}
					envOverlay = make(map[string]string, len(v.Data.(*MapObject).Entries))
					for k, vv := range v.Data.(*MapObject).Entries {
						if vv.Tag != VTStr {
							fail(fmt.Sprintf("exec: opts.env[%q] must be Str", k))
						}
						envOverlay[k] = vv.Data.(string)
					}
				}
			}

			// --- Build command ---
			cmd := osexec.Command(args[0], args[1:]...)
			if cwd != "" {
				cmd.Dir = cwd
			}
			if haveStdin {
				cmd.Stdin = strings.NewReader(stdinText)
			}
			// Environment: start from current process env, overlay provided kvs.
			if envOverlay != nil {
				cmd.Env = overlayEnv(os.Environ(), envOverlay)
			}

			var stdoutBuf, stderrBuf bytes.Buffer
			cmd.Stdout = &stdoutBuf
			cmd.Stderr = &stderrBuf

			err := cmd.Run()
			if err != nil {
				// Distinguish "process ran and exited non-zero" vs "spawn failed".
				if ee, ok := err.(*osexec.ExitError); ok && ee.ProcessState != nil {
					// Non-zero exit; still return outputs and status code.
					return execResult(ee.ProcessState.ExitCode(), stdoutBuf.String(), stderrBuf.String())
				}
				// Spawn/start error or unknown failure → annotated null.
				return Value{Tag: VTNull, Annot: "exec: " + err.Error()}
			}

			// Success (exit 0).
			return execResult(0, stdoutBuf.String(), stderrBuf.String())
		},
	)

	setBuiltinDoc(target, "exec", `Run an external program.

Executes a command synchronously and returns its exit status and captured output.
Contract mistakes (types/shapes) are hard errors; spawn failures return an annotated null.

Params:
	cmd:  [Str]                     # argv vector; cmd[0] is the executable
	opts: {                         # optional
		cwd:   Str,                 # working directory
		env:   {},                  # map Str -> Str; overlaid on current environment
		stdin: Str                  # text passed to the process' stdin
	}?

Returns:
	{
		status!: Int,               # exit code (0 on success)
		stdout!: Str,               # captured stdout (text)
		stderr!: Str                # captured stderr (text)
	}?                             # annotated null on spawn error (e.g., executable not found)

Notes:
	• Non-zero exit codes are NOT errors; they return a result with status>0.
	• To stream large I/O or interact incrementally, prefer adding higher-level process APIs later.`)
}

// execResult builds the {status, stdout, stderr} map with stable key order.
func execResult(status int, stdout, stderr string) Value {
	m := &MapObject{
		Entries: map[string]Value{
			"status": Int(int64(status)),
			"stdout": Str(stdout),
			"stderr": Str(stderr),
		},
		Keys: []string{"status", "stdout", "stderr"},
	}
	return Value{Tag: VTMap, Data: m}
}

// overlayEnv overlays k/v pairs onto a base environment (KEY=VAL strings).
func overlayEnv(base []string, overlay map[string]string) []string {
	envMap := make(map[string]string, len(base)+len(overlay))
	for _, kv := range base {
		if i := strings.IndexByte(kv, '='); i > 0 {
			envMap[kv[:i]] = kv[i+1:]
		}
	}
	for k, v := range overlay {
		envMap[k] = v
	}
	out := make([]string, 0, len(envMap))
	for k, v := range envMap {
		out = append(out, k+"="+v)
	}
	return out
}
