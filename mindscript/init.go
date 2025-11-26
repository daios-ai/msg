package mindscript

import (
	"fmt"
	"os"
	"path/filepath"
)

const MindScriptRootEnv = "MSGPATH"

var installRoot string

func init() {
	installRoot = root()
	if err := needDirs(installRoot, "bin", "lib", "data"); err != nil {
		fmt.Fprintf(os.Stderr,
			`MindScript installation at 
%q 

is invalid: %v

Fix: ensure these directories exist and are readable:
  %s
  %s
  %s
(Set %s to override the MindScript root directory.)
`, installRoot, err,
			filepath.Join(installRoot, "bin"),
			filepath.Join(installRoot, "lib"),
			filepath.Join(installRoot, "data"),
			MindScriptRootEnv,
		)
		os.Exit(1)
	}
}

func root() string {
	if env := os.Getenv(MindScriptRootEnv); env != "" {
		if abs, err := filepath.Abs(env); err == nil {
			return filepath.Clean(abs)
		}
		return filepath.Clean(env)
	}
	exe, _ := os.Executable()
	return filepath.Clean(filepath.Join(filepath.Dir(exe), ".."))
}

func needDirs(root string, subs ...string) error {
	for _, s := range subs {
		p := filepath.Join(root, s)
		fi, err := os.Stat(p)
		switch {
		case os.IsNotExist(err):
			return fmt.Errorf("missing directory %q", p)
		case err != nil:
			return fmt.Errorf("cannot stat %q: %w", p, err)
		case !fi.IsDir():
			return fmt.Errorf("%q exists but is not a directory", p)
		}
	}
	return nil
}
