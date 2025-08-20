// cmd/cli/mindscript.go
package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/peterh/liner"

	// Adjust this to your actual module path
	mindscript "github.com/DAIOS-AI/msg"
)

const (
	colorReset = "\x1b[0m"
	colorRed   = "\x1b[31m"
	colorGreen = "\x1b[32m"
	colorBlue  = "\x1b[34m"
)

func red(s string) string   { return colorRed + s + colorReset }
func green(s string) string { return colorGreen + s + colorReset }
func blue(s string) string  { return colorBlue + s + colorReset }

// Colorize a formatted value: annotation lines (trim-left starts with "#") → green,
// everything else → blue.
func colorizeValue(val string) string {
	lines := strings.Split(val, "\n")
	for i, ln := range lines {
		if strings.HasPrefix(strings.TrimLeft(ln, " \t"), "#") {
			lines[i] = green(ln)
		} else if ln != "" {
			lines[i] = blue(ln)
		}
	}
	return strings.Join(lines, "\n")
}

const (
	appName     = "mindscript"
	historyFile = ".mindscript_history"
	promptMain  = "==> "
	promptCont  = "... "
	banner      = "MindScript REPL — Ctrl+C to cancel input, Ctrl+D to exit. Type :help for commands."
	helpText    = `
REPL commands:
  :help            Show this help
  :quit / :exit    Exit the REPL
  :load <file>     Load & execute a file into the current session
  :fmt [code]      Pretty-print code (multiline if no code provided)
  :pretty [code]   Alias for :fmt
  :reset           Reset the interpreter (new empty global scope)
`
)

// ---- main ------------------------------------------------------------------

func main() {
	var evalStr string
	flag.StringVar(&evalStr, "e", "", "Evaluate the given MindScript snippet and exit")
	flag.Parse()

	args := flag.Args()

	switch {
	case evalStr != "":
		os.Exit(runEvalString(evalStr))
	case len(args) > 0:
		os.Exit(runFile(args[0]))
	default:
		os.Exit(runREPL())
	}
}

// ---- file & string modes ---------------------------------------------------

func runFile(path string) int {
	// File mode: keep colors OFF (library default).
	src, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: cannot read %s: %v\n", appName, path, err)
		return 1
	}

	ip := mindscript.NewRuntime()
	v, err := ip.EvalSource(string(src))
	if err != nil {
		// Parse/Lex errors are already pretty-printed by the library.
		fmt.Fprintf(os.Stderr, "%s: %v\n", appName, err)
		return 1
	}
	fmt.Println(mindscript.FormatValue(v))
	return 0
}

func runEvalString(code string) int {
	// -e mode: keep colors OFF (library default).
	ip := mindscript.NewRuntime()
	v, err := ip.EvalSource(code)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: %v\n", appName, err)
		return 1
	}
	fmt.Println(mindscript.FormatValue(v))
	return 0
}

// ---- REPL ------------------------------------------------------------------

func runREPL() int {
	fmt.Println(banner)

	home, _ := os.UserHomeDir()
	histPath := filepath.Join(home, historyFile)

	ln := liner.NewLiner()
	defer ln.Close()
	ln.SetCtrlCAborts(true)

	// Load history (best-effort)
	if f, err := os.Open(histPath); err == nil {
		_, _ = ln.ReadHistory(f)
		_ = f.Close()
	}

	ip := mindscript.NewRuntime()

	for {
		// Accumulate possibly-multiline input until parser says it's complete.
		code, ok := readByParseProbe(ln, promptMain, promptCont)
		if !ok { // user pressed Ctrl+D or EOF
			fmt.Println()
			break
		}

		// REPL commands (prefixed with ':')
		if strings.HasPrefix(strings.TrimSpace(code), ":") {
			if done := handleReplCommand(ip, ln, code); done {
				break
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
		val := mindscript.FormatValue(v)
		fmt.Println(colorizeValue(val))

		// Save to history
		ln.AppendHistory(strings.ReplaceAll(code, "\n", " "))
	}

	// Persist history (best-effort)
	if f, err := os.Create(histPath); err == nil {
		_, _ = ln.WriteHistory(f)
		_ = f.Close()
	}
	return 0
}

// ---- REPL helpers ----------------------------------------------------------

// handleReplCommand handles :help, :quit, :load, :type, :reset, :fmt
func handleReplCommand(ip *mindscript.Interpreter, ln *liner.State, line string) (exit bool) {
	fields := strings.Fields(line)
	if len(fields) == 0 {
		return false
	}
	cmd := strings.ToLower(fields[0])

	switch cmd {
	case ":help":
		fmt.Print(helpText)

	case ":quit", ":exit":
		return true

	case ":reset":
		// fresh runtime (built-ins only)
		*ip = *mindscript.NewRuntime()
		fmt.Println("interpreter reset.")

	case ":load":
		if len(fields) < 2 {
			fmt.Println("usage: :load <file>")
			return false
		}
		path := fields[1]
		src, err := os.ReadFile(path)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s\n", red(fmt.Sprintf("cannot read %s: %v", path, err)))
			return false
		}
		if v, err := ip.EvalPersistentSource(string(src)); err != nil {
			fmt.Fprintln(os.Stderr, red(err.Error()))
		} else {
			// Print last value
			fmt.Println(mindscript.FormatValue(v))
			ln.AppendHistory(fmt.Sprintf(":load %s", path))
		}

	case ":fmt", ":pretty":
		// Inline snippet?
		inline := strings.TrimSpace(strings.TrimPrefix(line, fields[0]))
		if inline != "" {
			runFormat(inline)
			return false
		}
		// Otherwise, capture multiline snippet
		code, ok := readByParseProbe(ln, "fmt> ", "... ")
		if !ok {
			fmt.Println() // Ctrl+D
			return false
		}
		if strings.TrimSpace(code) == "" {
			return false
		}
		runFormat(code)

	default:
		fmt.Printf("unknown command. Type :help for help.\n")
	}
	return false
}

// readByParseProbe reads one or more lines until the parser
// accepts the current buffer as a complete program, or returns early
// if the parser reports a non-recoverable error.
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
		_, perr := mindscript.ParseSExpr(src)
		if perr == nil {
			// Complete and valid.
			return src, true
		}
		// Decide if this looks incomplete (keep reading) or a real error (stop).
		if looksIncomplete(perr) || endsWithOpenAnnotation(src) {
			continue
		}
		// Real error → return current buffer so the caller can print it.
		return src, true
	}
}

// endsWithOpenAnnotation reports whether the current source ends with
// one or more annotation lines ('# ...' with optional leading spaces,
// but NOT '##' comments) and no following expression yet.
func endsWithOpenAnnotation(src string) bool {
	s := strings.TrimRight(src, " \t\r\n")
	if s == "" {
		return false
	}
	if i := strings.LastIndexByte(s, '\n'); i >= 0 {
		s = s[i+1:]
	}
	line := strings.TrimLeft(s, " \t")
	if strings.HasPrefix(line, "##") {
		return false // true comment
	}
	return strings.HasPrefix(line, "#") // annotation needs an expr next
}

// looksIncomplete classifies parse/lex errors that likely mean “need more input”.
func looksIncomplete(err error) bool {
	msg := strings.ToLower(err.Error())
	// Typical parser “expected …” when a block/paren/string isn’t closed.
	if strings.Contains(msg, "expected ')'") ||
		strings.Contains(msg, "expected ']'") ||
		strings.Contains(msg, "expected '}'") ||
		strings.Contains(msg, "expected 'end'") ||
		strings.Contains(msg, "expected '(' to start parameters") ||
		strings.Contains(msg, "expected ':' after key") ||
		strings.Contains(msg, "expected 'then'") {
		return true
	}
	// Lexer “unfinished” cases (strings/escapes/annotation blocks).
	if strings.Contains(msg, "string was not terminated") ||
		strings.Contains(msg, "unfinished escape sequence") ||
		strings.Contains(msg, "unicode escape was not terminated") ||
		strings.Contains(msg, "incomplete annotation") {
		return true
	}
	return false
}

// stripStrings is retained only for endsWithOpenAnnotation helper if you keep it;
// otherwise you can delete it from the CLI (we no longer use bracket heuristics).

// ---- pretty printing -------------------------------------------------------

func runFormat(src string) {
	formatted, err := mindscript.Pretty(src)
	if err != nil {
		fmt.Fprintln(os.Stderr, red(err.Error()))
		return
	}
	fmt.Println(formatted)
}
