// cmd/cli/main.go
package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"

	"github.com/peterh/liner"

	// Adjust this to your actual module path
	mindscript "github.com/daios-ai/msg/internal/mindscript"
)

const (
	colorReset = "\x1b[0m"
	colorRed   = "\x1b[31m"
	colorGreen = "\x1b[32m"
	colorBlue  = "\x1b[94m"
)

func red(s string) string   { return colorRed + s + colorReset }
func green(s string) string { return colorGreen + s + colorReset }
func blue(s string) string  { return colorBlue + s + colorReset }

// splitInlineComment finds the first unquoted '#' (preceded by space/tab) that
// looks like an inline comment. It ignores '#' inside double-quoted strings and
// respects backslash escapes. Returns left/code and right/comment (starting at '#').
func splitInlineComment(line string) (left, comment string, ok bool) {
	inStr := false
	for i := 0; i < len(line); i++ {
		c := line[i]

		if inStr {
			if c == '\\' {
				if i+1 < len(line) {
					i++
				}
				continue
			}
			if c == '"' {
				inStr = false
			}
			continue
		}

		switch c {
		case '"':
			inStr = true
		case '#':
			if i > 0 && (line[i-1] == ' ' || line[i-1] == '\t') {
				return line[:i], line[i:], true
			}
		}
	}
	return "", "", false
}

// Colorize a formatted value:
//   - Lines whose first non-space char is '#' → whole line green.
//   - Other non-empty lines: if they contain an unquoted inline '#' → left blue, trailing comment green.
//   - Otherwise → whole line blue.
//
// Empty lines unchanged.
func colorizeValue(val string) string {
	lines := strings.Split(val, "\n")
	for i, ln := range lines {
		trimLeft := strings.TrimLeft(ln, " \t")
		if strings.HasPrefix(trimLeft, "#") {
			lines[i] = green(ln)
			continue
		}
		if strings.TrimSpace(ln) == "" {
			continue
		}
		if left, comment, ok := splitInlineComment(ln); ok {
			lines[i] = blue(left) + green(comment)
		} else {
			lines[i] = blue(ln)
		}
	}
	return strings.Join(lines, "\n")
}

const (
	appName     = "msg"
	historyFile = ".mindscript_history"
	promptMain  = "==> "
	promptCont  = "... "
	banner      = "MindScript REPL — Ctrl+C cancels input, Ctrl+D exits. Type :help for commands."
	helpText    = `
REPL commands:
  :help            Show this help
  :quit / :exit    Exit the REPL
`
)

// -----------------------------------------------------------------------------
// main
// -----------------------------------------------------------------------------

func main() {
	if len(os.Args) < 2 {
		usage()
		os.Exit(2)
	}

	cmd := os.Args[1]
	switch cmd {
	case "run":
		os.Exit(cmdRun(os.Args[2:]))
	case "repl":
		os.Exit(cmdRepl(os.Args[2:]))
	case "fmt":
		os.Exit(cmdFmt(os.Args[2:]))
	case "test":
		os.Exit(cmdTest(os.Args[2:]))
	case "get":
		os.Exit(cmdGet(os.Args[2:]))
	case "-h", "--help", "help":
		usage()
		os.Exit(0)
	default:
		fmt.Fprintf(os.Stderr, "%s: unknown command %q\n", appName, cmd)
		usage()
		os.Exit(2)
	}
}

func usage() {
	fmt.Printf(`MindScript CLI

Usage:
  %s run <file.ms> [--] [args...]         Run a script (argv via runtime.argv)
  %s repl                                 Start the REPL
  %s fmt [--check] [path ...]             Format file(s) or tree(s) canonically
  %s test [path] [-p] [-v] [-timeout <ms>]  Run tests (default root=".")
  %s get <module>@<version?>              Install a third-party module (stub)

`, appName, appName, appName, appName, appName)
}

// -----------------------------------------------------------------------------
// run
// -----------------------------------------------------------------------------

func cmdRun(args []string) int {
	if len(args) < 1 {
		fmt.Fprintf(os.Stderr, "usage: %s run <file.ms> [--] [args...]\n", appName)
		return 2
	}

	// Split script path and argv (after optional "--")
	file := args[0]
	argv := []string{}
	if len(args) > 1 {
		sep := -1
		for i, a := range args[1:] {
			if a == "--" {
				sep = i + 1
				break
			}
		}
		if sep >= 0 {
			argv = args[1+sep:]
		} else {
			argv = args[1:]
		}
	}

	src, err := os.ReadFile(file)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: cannot read %s: %v\n", appName, file, err)
		return 1
	}

	ip, rtErr := mindscript.NewInterpreter()
	if rtErr != nil {
		fmt.Fprintln(os.Stderr, red(rtErr.Error()))
		return 1
	}

	// Build AST so we can evaluate in a custom environment (fresh child with runtime map)
	ast, perr := mindscript.ParseSExpr(string(src))
	if perr != nil {
		fmt.Fprintln(os.Stderr, perr.Error()) // already pretty-printed
		return 1
	}

	child := mindscript.NewEnv(ip.Global)
	rt := &mindscript.MapObject{
		Entries: map[string]mindscript.Value{
			"isEntry": mindscript.Bool(true),
			"path":    mindscript.Str(fileAbsOrOrig(file)),
			"argv":    mindscript.Arr(strSliceToVals(argv)),
		},
		Keys: []string{"isEntry", "path", "argv"},
	}
	child.Define("runtime", mindscript.Value{Tag: mindscript.VTMap, Data: rt})

	val, err := ip.EvalAST(ast, child)
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		return 1
	}
	// Plain pretty output in file mode (no REPL color accents)
	fmt.Println(mindscript.FormatValue(val))
	return 0
}

func fileAbsOrOrig(p string) string {
	if abs, err := filepath.Abs(p); err == nil {
		return abs
	}
	return p
}

func strSliceToVals(xs []string) []mindscript.Value {
	out := make([]mindscript.Value, 0, len(xs))
	for _, s := range xs {
		out = append(out, mindscript.Str(s))
	}
	return out
}

// -----------------------------------------------------------------------------
// repl
// -----------------------------------------------------------------------------

func cmdRepl(_ []string) (ret int) {
	fmt.Println(banner)

	home, _ := os.UserHomeDir()
	histPath := filepath.Join(home, historyFile)

	ln := liner.NewLiner()
	defer ln.Close()
	ln.SetCtrlCAborts(true)

	// Persist history on any return/panic.
	defer func() {
		if f, err := os.Create(histPath); err == nil {
			_, _ = ln.WriteHistory(f)
			_ = f.Close()
		}
	}()

	// Trap common termination signals and restore TTY before exiting.
	sigc := make(chan os.Signal, 1)
	signal.Notify(sigc, os.Interrupt, syscall.SIGTERM, syscall.SIGHUP)
	defer signal.Stop(sigc)
	go func() {
		<-sigc
		ln.Close()
		os.Exit(130)
	}()

	// Load history (best-effort)
	if f, err := os.Open(histPath); err == nil {
		_, _ = ln.ReadHistory(f)
		_ = f.Close()
	}

	ip, rtErr := mindscript.NewInterpreter()
	if rtErr != nil {
		fmt.Fprintln(os.Stderr, red(rtErr.Error()))
		return 1
	}

	for {
		// Accumulate possibly-multiline input until parser says it's complete.
		code, ok := readByParseProbe(ln, promptMain, promptCont)
		if !ok { // user pressed Ctrl+D or EOF
			fmt.Println()
			break
		}

		// Minimal REPL commands
		if strings.HasPrefix(strings.TrimSpace(code), ":") {
			switch strings.TrimSpace(strings.ToLower(code)) {
			case ":help":
				fmt.Print(helpText)
			case ":quit", ":exit":
				return 0
			default:
				fmt.Printf("unknown command. Type :help for help.\n")
			}
			continue
		}

		// Skip blank
		if strings.TrimSpace(code) == "" {
			continue
		}

		// Evaluate (persistent session)
		v, err := ip.EvalPersistentSource(code)
		if err != nil {
			fmt.Fprintln(os.Stderr, red(err.Error()))
			continue
		}
		fmt.Println(colorizeValue(mindscript.FormatValue(v)))

		// Save to history
		ln.AppendHistory(strings.ReplaceAll(code, "\n", " "))
	}

	return 0
}

// readByParseProbe reads one or more lines until the parser accepts the buffer
// as a complete program, or returns early if the parser reports a non-recoverable error.
func readByParseProbe(ln *liner.State, prompt, cont string) (string, bool) {
	var b strings.Builder

	for {
		var line string
		var err error
		if b.Len() == 0 {
			line, err = ln.Prompt(prompt)
		} else {
			line, err = ln.Prompt(cont)
		}
		if errors.Is(err, io.EOF) {
			return "", false
		}
		if err != nil {
			// Ctrl+C aborts the current input; let user start again.
			return "", true
		}

		if b.Len() > 0 {
			b.WriteByte('\n')
		}
		b.WriteString(line)

		src := b.String()
		_, _, perr := mindscript.ParseSExprInteractiveWithSpans(src)
		if perr == nil {
			return src, true
		}
		if mindscript.IsIncomplete(perr) {
			continue
		}
		// Real error → return current buffer so the caller can print it.
		return src, true
	}
}

// -----------------------------------------------------------------------------
// fmt
// -----------------------------------------------------------------------------

func cmdFmt(args []string) int {
	fs := flag.NewFlagSet("fmt", flag.ContinueOnError)
	check := fs.Bool("check", false, "check format; exit 1 if any file would change")
	_ = fs.Parse(args)
	paths := fs.Args()
	if len(paths) == 0 {
		paths = []string{"."}
	}

	ip, rtErr := mindscript.NewInterpreter()
	if rtErr != nil {
		fmt.Fprintln(os.Stderr, red(rtErr.Error()))
		return 1
	}

	call := func(code string) (mindscript.Value, error) {
		return ip.EvalPersistentSource(code)
	}

	if *check {
		var bad int64 = 0
		for _, p := range paths {
			info, err := os.Stat(p)
			if err != nil {
				fmt.Fprintf(os.Stderr, "%s: %v\n", appName, err)
				return 1
			}
			if !info.IsDir() {
				v, err := call(fmt.Sprintf(`import("canon").checkFile(%q)`, p))
				if err != nil {
					fmt.Fprintln(os.Stderr, err.Error())
					return 1
				}
				if v.Tag == mindscript.VTBool && !v.Data.(bool) {
					fmt.Println(p)
					bad++
				}
				continue
			}
			// dir: get file list and check each
			vlist, err := call(fmt.Sprintf(`import("canon").files(%q)`, p))
			if err != nil {
				fmt.Fprintln(os.Stderr, err.Error())
				return 1
			}
			if vlist.Tag != mindscript.VTArray {
				continue
			}
			for _, ev := range vlist.Data.([]mindscript.Value) {
				if ev.Tag != mindscript.VTStr {
					continue
				}
				f := ev.Data.(string)
				vok, err := call(fmt.Sprintf(`import("canon").checkFile(%q)`, f))
				if err != nil {
					fmt.Fprintln(os.Stderr, err.Error())
					return 1
				}
				if vok.Tag == mindscript.VTBool && !vok.Data.(bool) {
					fmt.Println(f)
					bad++
				}
			}
		}
		if bad > 0 {
			return 1
		}
		return 0
	}

	// Write mode: file → formatFile; dir → formatTree
	for _, p := range paths {
		info, err := os.Stat(p)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: %v\n", appName, err)
			return 1
		}
		if info.IsDir() {
			if _, err := ip.EvalPersistentSource(fmt.Sprintf(`import("canon").formatTree(%q)`, p)); err != nil {
				fmt.Fprintln(os.Stderr, err.Error())
				return 1
			}
		} else {
			if _, err := ip.EvalPersistentSource(fmt.Sprintf(`import("canon").formatFile(%q)`, p)); err != nil {
				fmt.Fprintln(os.Stderr, err.Error())
				return 1
			}
		}
	}
	return 0
}

// -----------------------------------------------------------------------------
// test
// -----------------------------------------------------------------------------

func cmdTest(args []string) int {
	// Defaults
	pathPrefix := "."
	parallel := false
	verbose := false
	timeoutMs := 0

	// Tolerant manual parse so flags can appear before/after the path.
	// Supports: -p, -v, -timeout=123, -timeout 123, and optional "--".
	for i := 0; i < len(args); i++ {
		a := args[i]

		// End-of-flags marker: next non-flag (if any) becomes the path prefix.
		if a == "--" {
			if i+1 < len(args) {
				pathPrefix = args[i+1]
			}
			break
		}

		if strings.HasPrefix(a, "-") {
			switch {
			case a == "-p":
				parallel = true
			case a == "-v":
				verbose = true
			case strings.HasPrefix(a, "-timeout="):
				val := strings.TrimPrefix(a, "-timeout=")
				n, err := strconv.Atoi(val)
				if err != nil {
					fmt.Fprintf(os.Stderr, "%s: invalid -timeout value %q\n", appName, val)
					return 2
				}
				timeoutMs = n
			case a == "-timeout":
				if i+1 >= len(args) {
					fmt.Fprintf(os.Stderr, "%s: -timeout requires a value\n", appName)
					return 2
				}
				i++
				val := args[i]
				n, err := strconv.Atoi(val)
				if err != nil {
					fmt.Fprintf(os.Stderr, "%s: invalid -timeout value %q\n", appName, val)
					return 2
				}
				timeoutMs = n
			default:
				fmt.Fprintf(os.Stderr, "%s: unknown flag %q\n", appName, a)
				return 2
			}
			continue
		}

		// First non-flag token → path prefix (if not already set by "--").
		if pathPrefix == "." {
			pathPrefix = a
		}
	}

	ip, rtErr := mindscript.NewInterpreter()
	if rtErr != nil {
		fmt.Fprintln(os.Stderr, red(rtErr.Error()))
		return 1
	}

	code := fmt.Sprintf(`import("testing").run({
  pathPrefix: %q,
  parallel: %v,
  verbose: %v,
  timeoutMs: %d
})`, pathPrefix, parallel, verbose, timeoutMs)

	v, err := ip.EvalPersistentSource(code)
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		return 1
	}

	// testing.run returns Summary: {passed:Int, failed:Int, total:Int, durationMs:Int}
	if v.Tag == mindscript.VTMap {
		if mo, ok := v.Data.(*mindscript.MapObject); ok {
			if f, ok := mo.Entries["failed"]; ok && f.Tag == mindscript.VTInt && f.Data.(int64) == 0 {
				return 0
			}
		}
	}
	return 1
}

// -----------------------------------------------------------------------------
// get (stub)
// -----------------------------------------------------------------------------

func cmdGet(args []string) int {
	if len(args) < 1 {
		fmt.Fprintf(os.Stderr, "usage: %s get <module>@<version?>\n", appName)
		return 2
	}
	spec := args[0]
	fmt.Printf("Installing %s ... (stub)\n", spec)
	fmt.Printf("Downloaded to %s (not really yet)\n", filepath.Join(os.Getenv("HOME"), ".msg", "modules"))
	return 0
}
