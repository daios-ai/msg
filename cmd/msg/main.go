package main

import (
	"bufio"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/ergochat/readline"

	// Adjust this to your actual module path
	mindscript "github.com/daios-ai/msg/mindscript"
)

const (
	appName     = "msg"
	historyFile = ".mindscript_history"
	promptMain  = "==> "
	promptCont  = "... "
)

var (
	banner   = fmt.Sprintf("MindScript %s REPL\nCtrl+C cancels input, Ctrl+D exits. Type :quit to exit.", mindscript.Version)
	helpText = `
REPL commands:
  :quit    Exit the REPL
`
)

func red(s string) string   { return "\x1b[31m" + s + "\x1b[0m" }
func green(s string) string { return "\x1b[32m" + s + "\x1b[0m" }
func blue(s string) string  { return "\x1b[94m" + s + "\x1b[0m" }

// decorateForHistory prefixes continuation lines with "... " so multiline
// entries look like true continuations when recalled from history.
func decorateForHistory(src string) string {
	lines := strings.Split(src, "\n")
	for i := 1; i < len(lines); i++ {
		if strings.HasPrefix(lines[i], promptCont) {
			continue
		}
		lines[i] = promptCont + lines[i]
	}
	return strings.Join(lines, "\n")
}

// stripHistoryDecor removes the literal "... " prefix from continuation lines
// (lines 2+) before parsing/evaluating.
func stripHistoryDecor(src string) string {
	lines := strings.Split(src, "\n")
	for i := 1; i < len(lines); i++ {
		if strings.HasPrefix(lines[i], promptCont) {
			lines[i] = strings.TrimPrefix(lines[i], promptCont)
		}
	}
	return strings.Join(lines, "\n")
}

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
	case "version":
		fmt.Println(mindscript.Version)
		return
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
	fmt.Printf(`MindScript %s (built %s)

Usage:
  %s run <file.ms> [--] [args...]         Run a script.
  %s repl                                 Start the REPL.
  %s fmt [--check] [path ...]             Format file(s) by path prefix (default ".")
  %s test [path] [-p] [-v] [-t <ms>]      Run test file(s) by path prefix (default ".")
  %s get <module>@<version?>              Install a third-party module (not implemented)
  %s version                              Print the compiled version

`, mindscript.Version, mindscript.BuildDate, appName, appName, appName, appName, appName, appName)
}

// -----------------------------------------------------------------------------
// run
// -----------------------------------------------------------------------------

func cmdRun(args []string) int {
	if len(args) < 1 {
		fmt.Fprintf(os.Stderr, "usage: %s run <file.ms> [--] [args...]\n", appName)
		return 2
	}

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

	rt := &mindscript.MapObject{
		Entries: map[string]mindscript.Value{
			"isEntry": mindscript.Bool(true),
			"path":    mindscript.Str(fileAbsOrOrig(file)),
			"argv":    mindscript.Arr(strSliceToVals(argv)),
		},
		Keys: []string{"isEntry", "path", "argv"},
	}
	// Define in Global so EvalSource()'s ephemeral child can see it.
	ip.Global.Define("runtime", mindscript.Value{Tag: mindscript.VTMap, Data: rt})

	_, err = ip.EvalSource(string(src))
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		return 1
	}
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
// repl history (JSONL)
// -----------------------------------------------------------------------------

func loadHistoryJSONL(path string, rl *readline.Instance) []string {
	f, err := os.Open(path)
	if err != nil {
		return nil
	}
	defer f.Close()

	var items []string
	sc := bufio.NewScanner(f)
	sc.Buffer(make([]byte, 0, 64*1024), 1024*1024)
	for sc.Scan() {
		b := sc.Bytes()
		var s string
		if err := json.Unmarshal(b, &s); err != nil {
			// Back-compat: treat as raw single-line entries.
			s = string(b)
		}
		items = append(items, s)
		_ = rl.SaveToHistory(decorateForHistory(s))
	}
	return items
}

func saveHistoryJSONL(path string, items []string) {
	tmp := path + ".tmp"
	f, err := os.OpenFile(tmp, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0600)
	if err != nil {
		return
	}
	defer f.Close()

	w := bufio.NewWriter(f)
	for _, s := range items {
		b, _ := json.Marshal(s)
		_, _ = w.Write(b)
		_ = w.WriteByte('\n')
	}
	_ = w.Flush()
	_ = os.Rename(tmp, path)
}

// -----------------------------------------------------------------------------
// repl
// -----------------------------------------------------------------------------

func cmdRepl(_ []string) (ret int) {
	fmt.Println(banner)

	home, _ := os.UserHomeDir()
	histPath := filepath.Join(home, historyFile)

	rl, err := readline.NewFromConfig(&readline.Config{
		Prompt:                 promptMain,
		HistoryFile:            "",   // we persist history ourselves (JSONL)
		DisableAutoSaveHistory: true, // we call SaveToHistory manually
	})
	if err != nil {
		fmt.Fprintln(os.Stderr, red(err.Error()))
		return 1
	}
	defer rl.Close()
	rl.CaptureExitSignal()

	history := loadHistoryJSONL(histPath, rl)
	defer saveHistoryJSONL(histPath, history)

	ip, rtErr := mindscript.NewInterpreter()
	if rtErr != nil {
		fmt.Fprintln(os.Stderr, red(rtErr.Error()))
		return 1
	}

	for {
		code, ok := readByParseProbe(rl, promptMain, promptCont)
		if !ok {
			fmt.Println()
			break
		}

		if strings.HasPrefix(strings.TrimSpace(code), ":") {
			switch strings.TrimSpace(strings.ToLower(code)) {
			case ":quit":
				return 0
			default:
				fmt.Printf("unknown command. Type :quit to exit.\n")
			}
			continue
		}

		if strings.TrimSpace(code) == "" {
			continue
		}

		// Store even failing inputs (so quick edits via history work).
		history = append(history, code)
		_ = rl.SaveToHistory(decorateForHistory(code))

		v, err := ip.EvalPersistentSource(code)
		if err != nil {
			fmt.Fprintln(os.Stderr, red(err.Error()))
			continue
		}
		fmt.Println(colorizeValue(mindscript.FormatValue(v)))
	}

	return 0
}

func readByParseProbe(rl *readline.Instance, prompt, cont string) (string, bool) {
	var b strings.Builder

	for {
		if b.Len() == 0 {
			rl.SetPrompt(prompt)
		} else {
			rl.SetPrompt(cont)
		}

		line, err := rl.ReadLine()
		if err == nil {
			// ok
		} else if errors.Is(err, io.EOF) {
			return "", false
		} else if errors.Is(err, readline.ErrInterrupt) {
			return "", true // Ctrl+C cancels current input
		} else {
			return "", false
		}

		if b.Len() > 0 {
			b.WriteByte('\n')
		}
		b.WriteString(line)

		src := stripHistoryDecor(b.String())
		_, _, perr := mindscript.ParseSExprInteractiveWithSpans(src)
		if perr == nil {
			return src, true
		}
		if mindscript.IsIncomplete(perr) {
			continue
		}
		return src, true
	}
}

// -----------------------------------------------------------------------------
// fmt
// -----------------------------------------------------------------------------

func cmdFmt(args []string) int {
	fs := flag.NewFlagSet("fmt", flag.ContinueOnError)
	check := fs.Bool("check", false, "check format; exit 1 if any file would change")
	if err := fs.Parse(args); err != nil {
		return 2
	}
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
		var bad int64

		for _, prefix := range paths {
			vlist, err := call(fmt.Sprintf(`import("canon").files(%q)`, prefix))
			if err != nil {
				fmt.Fprintln(os.Stderr, err.Error())
				return 1
			}
			if vlist.Tag == mindscript.VTNull {
				fmt.Fprintf(os.Stderr, "%s: discovery failed for %s\n", appName, prefix)
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
				if vok.Tag == mindscript.VTNull {
					fmt.Fprintf(os.Stderr, "%s: cannot check %s\n", appName, f)
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

	for _, prefix := range paths {
		v, err := call(fmt.Sprintf(`import("canon").formatTree(%q)`, prefix))
		if err != nil {
			fmt.Fprintln(os.Stderr, err.Error())
			return 1
		}
		if v.Tag == mindscript.VTNull {
			fmt.Fprintf(os.Stderr, "%s: discovery/formatting failed for %s\n", appName, prefix)
			return 1
		}
		if v.Tag == mindscript.VTMap {
			if mo, ok := v.Data.(*mindscript.MapObject); ok {
				if e, ok := mo.Entries["errors"]; ok && e.Tag == mindscript.VTInt && e.Data.(int64) > 0 {
					return 1
				}
			}
		}
	}

	return 0
}

// -----------------------------------------------------------------------------
// test
// -----------------------------------------------------------------------------

func cmdTest(args []string) int {
	pathPrefix := "."
	parallel := false
	verbose := false
	timeoutMs := 0

	for i := 0; i < len(args); i++ {
		a := args[i]
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
			case strings.HasPrefix(a, "-t="):
				val := strings.TrimPrefix(a, "-t=")
				n, err := strconv.Atoi(val)
				if err != nil {
					fmt.Fprintf(os.Stderr, "%s: invalid -t value %q\n", appName, val)
					return 2
				}
				timeoutMs = n
			case a == "-t":
				if i+1 >= len(args) {
					fmt.Fprintf(os.Stderr, "%s: -t requires a value\n", appName)
					return 2
				}
				i++
				val := args[i]
				n, err := strconv.Atoi(val)
				if err != nil {
					fmt.Fprintf(os.Stderr, "%s: invalid -t value %q\n", appName, val)
					return 2
				}
				timeoutMs = n
			default:
				fmt.Fprintf(os.Stderr, "%s: unknown flag %q\n", appName, a)
				return 2
			}
			continue
		}

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
	fmt.Printf("Installing third-party modules is not implemented yet.\n")
	return 0
}
