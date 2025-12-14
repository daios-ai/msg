//go:build tinygo

package mindscript

// TinyGo / non-libffi build: no-op FFI registration.
func registerFFIBuiltins(ip *Interpreter, target *Env) {
	// FFI not available under TinyGo.
	// Leaving this empty keeps SeedRuntimeInto happy.
}
