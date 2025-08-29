// crypto_builtins.go
//
// Builtins surfaced:
//  1. randBytes(n: Int) -> Str?          // CSPRNG bytes (annotated-null on OS RNG error)
//  2. sha256(x: Str) -> Str              // raw 32-byte digest
//  3. hmacSha256(key: Str, msg: Str) -> Str   // raw 32-byte MAC
//  4. ctEqual(a: Str, b: Str) -> Bool    // constant-time equality
//
// Conventions:
//   - Functions are camelCase; docs via setBuiltinDoc(...).
//   - Types use S-expr forms (Str? = ("unop","?",("id","Str"))).
//   - Contract violations -> hard fail(...). Environmental errors -> annotated null.
//   - Digests return *raw bytes* in Str. Render with hex/base64 in userland.
package mindscript

import (
	"crypto/hmac"
	"crypto/rand"
	"crypto/sha256"
	"crypto/subtle"
)

func registerCryptoBuiltins(ip *Interpreter) {
	// randBytes(n: Int) -> Str?
	// Return n cryptographically secure random bytes.
	ip.RegisterNative(
		"randBytes",
		[]ParamSpec{{Name: "n", Type: S{"id", "Int"}}},
		S{"unop", "?", S{"id", "Str"}}, // Str?
		func(_ *Interpreter, ctx CallCtx) Value {
			nv := ctx.MustArg("n")
			n := nv.Data.(int64)
			if n < 0 {
				fail("randBytes: n must be >= 0")
			}
			if n == 0 {
				return Str("")
			}
			buf := make([]byte, n)
			if _, err := rand.Read(buf); err != nil {
				return Value{Tag: VTNull, Annot: "randBytes: " + err.Error()}
			}
			return Str(string(buf))
		},
	)
	setBuiltinDoc(ip, "randBytes", `Uniform cryptographically secure random bytes.

Params:
	n: Int — number of bytes (>= 0)

Returns:
	Str? — raw bytes; annotated null on OS RNG failure

Notes:
	• Returns raw bytes in Str (may contain non-UTF-8).
	• Use hex/base64 helpers in userland to render textually.`)

	// sha256(x: Str) -> Str
	// Return the SHA-256 digest (raw 32 bytes).
	ip.RegisterNative(
		"sha256",
		[]ParamSpec{{Name: "x", Type: S{"id", "Str"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			x := ctx.MustArg("x").Data.(string)
			sum := sha256.Sum256([]byte(x))
			return Str(string(sum[:]))
		},
	)
	setBuiltinDoc(ip, "sha256", `SHA-256 digest (raw bytes).

Params:
	x: Str — input bytes (may be arbitrary, not necessarily UTF-8)

Returns:
	Str — 32-byte digest (raw). Use hex/base64 in userland to display.`)

	// hmacSha256(key: Str, msg: Str) -> Str
	// Return HMAC-SHA256(key, msg) (raw 32 bytes).
	ip.RegisterNative(
		"hmacSha256",
		[]ParamSpec{
			{Name: "key", Type: S{"id", "Str"}},
			{Name: "msg", Type: S{"id", "Str"}},
		},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			key := []byte(ctx.MustArg("key").Data.(string))
			msg := []byte(ctx.MustArg("msg").Data.(string))
			m := hmac.New(sha256.New, key)
			_, _ = m.Write(msg)
			mac := m.Sum(nil)
			return Str(string(mac))
		},
	)
	setBuiltinDoc(ip, "hmacSha256", `HMAC-SHA256 authentication tag (raw bytes).

Params:
	key: Str — secret key (raw bytes)
	msg: Str — message bytes

Returns:
	Str — 32-byte MAC (raw). Use hex/base64 to render as text.

Notes:
	• Suitable for signatures, request auth, etc.`)

	// ctEqual(a: Str, b: Str) -> Bool
	// Constant-time equality compare for byte strings.
	ip.RegisterNative(
		"ctEqual",
		[]ParamSpec{
			{Name: "a", Type: S{"id", "Str"}},
			{Name: "b", Type: S{"id", "Str"}},
		},
		S{"id", "Bool"},
		func(_ *Interpreter, ctx CallCtx) Value {
			a := []byte(ctx.MustArg("a").Data.(string))
			b := []byte(ctx.MustArg("b").Data.(string))
			ok := subtle.ConstantTimeCompare(a, b) == 1
			return Bool(ok)
		},
	)
	setBuiltinDoc(ip, "ctEqual", `Constant-time equality for byte strings.

Params:
	a: Str
	b: Str

Returns:
	Bool — true iff a and b are byte-for-byte equal

Notes:
	• Time is proportional to length; comparison avoids data-dependent early exits.
	• Use for verifying MACs, tokens, etc.`)
}
