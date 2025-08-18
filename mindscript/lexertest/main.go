package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"strings"

	// replace with your module path from go.mod
	mindscript "github.com/DAIOS-AI/msg"
)

var (
	flagJSON      = flag.Bool("json", false, "emit NDJSON: one JSON object per token")
	flagWithEOF   = flag.Bool("with-eof", false, "include EOF token in output")
	flagNoLexeme  = flag.Bool("no-lexeme", false, "hide raw lexeme in output")
	flagNoLiteral = flag.Bool("no-literal", false, "hide parsed literal in output")
	flagAST       = flag.Bool("ast", false, "parse and print s-expr AST (JSON)")
)

func main() {
	flag.Parse()
	args := flag.Args()

	// When no files are given, read stdin.
	if len(args) == 0 {
		if err := process(os.Stdin, "stdin"); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		return
	}

	exit := 0
	for _, path := range args {
		f, err := os.Open(path)
		if err != nil {
			fmt.Fprintf(os.Stderr, "open %s: %v\n", path, err)
			exit = 1
			continue
		}
		if err := process(f, path); err != nil {
			fmt.Fprintln(os.Stderr, err)
			exit = 1
		}
		f.Close()
	}
	os.Exit(exit)
}

func process(r io.Reader, filename string) error {
	src, err := slurp(r)
	if err != nil {
		return fmt.Errorf("%s: %w", filename, err)
	}

	if *flagAST {
		sexpr, err := mindscript.ParseSExpr(src)
		if err != nil {
			return fmt.Errorf("%s: %v", filename, err)
		}
		b, _ := json.MarshalIndent(sexpr, "", "  ")
		fmt.Println(string(b))
		return nil
	}

	lex := mindscript.NewLexer(src)
	toks, err := lex.Scan()
	if err != nil {
		return fmt.Errorf("%s: %v", filename, err)
	}

	// Optionally drop EOF
	if !*flagWithEOF && len(toks) > 0 && toks[len(toks)-1].Type == mindscript.EOF {
		toks = toks[:len(toks)-1]
	}

	if *flagJSON {
		enc := json.NewEncoder(os.Stdout)
		for _, t := range toks {
			out := toOutToken(filename, t)
			if *flagNoLexeme {
				out.Lexeme = ""
			}
			if *flagNoLiteral {
				out.Literal = nil
			}
			if err := enc.Encode(out); err != nil {
				return fmt.Errorf("encode json: %w", err)
			}
		}
		return nil
	}

	// Text mode
	header := filename
	if header != "" {
		fmt.Printf("== %s ==\n", header)
	}
	for _, t := range toks {
		typeName := tokenTypeName(t.Type)
		parts := []string{
			fmt.Sprintf("%4d:%-3d", t.Line, t.Col),
			fmt.Sprintf("%-12s", typeName),
		}
		if !*flagNoLexeme {
			parts = append(parts, fmt.Sprintf("lexeme=%q", t.Lexeme))
		}
		if !*flagNoLiteral && t.Literal != nil {
			parts = append(parts, fmt.Sprintf("literal=%#v", t.Literal))
		}
		fmt.Println(strings.Join(parts, "  "))
	}
	return nil
}

func slurp(r io.Reader) (string, error) {
	var b strings.Builder
	br := bufio.NewReader(r)
	for {
		chunk, err := br.ReadString(0)
		b.WriteString(chunk)
		if err == io.EOF {
			return b.String(), nil
		}
		if err != nil {
			if err == bufio.ErrBufferFull {
				// restart loop; we already wrote chunk
				continue
			}
			return "", err
		}
	}
}

type outToken struct {
	File    string      `json:"file,omitempty"`
	Type    string      `json:"type"`
	Lexeme  string      `json:"lexeme,omitempty"`
	Literal interface{} `json:"literal,omitempty"`
	Line    int         `json:"line"`
	Col     int         `json:"col"`
}

func toOutToken(file string, t mindscript.Token) outToken {
	return outToken{
		File:    file,
		Type:    tokenTypeName(t.Type),
		Lexeme:  t.Lexeme,
		Literal: t.Literal,
		Line:    t.Line,
		Col:     t.Col,
	}
}

func tokenTypeName(tt mindscript.TokenType) string {
	switch tt {
	case mindscript.EOF:
		return "EOF"
	case mindscript.ILLEGAL:
		return "ILLEGAL"
	case mindscript.LROUND:
		return "LROUND"
	case mindscript.CLROUND:
		return "CLROUND"
	case mindscript.RROUND:
		return "RROUND"
	case mindscript.LSQUARE:
		return "LSQUARE"
	case mindscript.CLSQUARE:
		return "CLSQUARE"
	case mindscript.RSQUARE:
		return "RSQUARE"
	case mindscript.LCURLY:
		return "LCURLY"
	case mindscript.RCURLY:
		return "RCURLY"
	case mindscript.COLON:
		return "COLON"
	case mindscript.COMMA:
		return "COMMA"
	case mindscript.PERIOD:
		return "PERIOD"
	case mindscript.QUESTION:
		return "QUESTION"
	case mindscript.PLUS:
		return "PLUS"
	case mindscript.MINUS:
		return "MINUS"
	case mindscript.MULT:
		return "MULT"
	case mindscript.DIV:
		return "DIV"
	case mindscript.MOD:
		return "MOD"
	case mindscript.ASSIGN:
		return "ASSIGN"
	case mindscript.EQ:
		return "EQ"
	case mindscript.NEQ:
		return "NEQ"
	case mindscript.LESS:
		return "LESS"
	case mindscript.LESS_EQ:
		return "LESS_EQ"
	case mindscript.GREATER:
		return "GREATER"
	case mindscript.GREATER_EQ:
		return "GREATER_EQ"
	case mindscript.BANG:
		return "BANG"
	case mindscript.ARROW:
		return "ARROW"
	case mindscript.ID:
		return "ID"
	case mindscript.STRING:
		return "STRING"
	case mindscript.INTEGER:
		return "INTEGER"
	case mindscript.NUMBER:
		return "NUMBER"
	case mindscript.BOOLEAN:
		return "BOOLEAN"
	case mindscript.NULL:
		return "NULL"
	case mindscript.AND:
		return "AND"
	case mindscript.OR:
		return "OR"
	case mindscript.NOT:
		return "NOT"
	case mindscript.LET:
		return "LET"
	case mindscript.DO:
		return "DO"
	case mindscript.END:
		return "END"
	case mindscript.RETURN:
		return "RETURN"
	case mindscript.BREAK:
		return "BREAK"
	case mindscript.CONTINUE:
		return "CONTINUE"
	case mindscript.IF:
		return "IF"
	case mindscript.THEN:
		return "THEN"
	case mindscript.ELIF:
		return "ELIF"
	case mindscript.ELSE:
		return "ELSE"
	case mindscript.FUNCTION:
		return "FUNCTION"
	case mindscript.ORACLE:
		return "ORACLE"
	case mindscript.FOR:
		return "FOR"
	case mindscript.IN:
		return "IN"
	case mindscript.FROM:
		return "FROM"
	case mindscript.TYPECONS:
		return "TYPECONS"
	case mindscript.TYPE:
		return "TYPE"
	case mindscript.ENUM:
		return "ENUM"
	case mindscript.ANNOTATION:
		return "ANNOTATION"
	default:
		return fmt.Sprintf("TokenType(%d)", int(tt))
	}
}
