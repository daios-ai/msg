let testing = import("testing")
let mtml = import("mtml")

# 1) Interpolation + autoescape (HTML text)
testing.test("mtml/interpolate autoescape", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "<p>{{ raw }}</p>", "auto.html")
	testing.assert(t != null, "compile null")
	let out = mtml.render(e, t, {raw: "<b>bold</b>"})
	testing.assert(out != null, "render null")
	testing.assertEq("<p>&lt;b&gt;bold&lt;/b&gt;</p>", out)
end)

# 2) Built-in filters: default + upper
testing.test("mtml/filters default+upper", fun(_: Null) do
	let e = mtml.env({})
	let _ = mtml.installBuiltins(e)
	let t = mtml.fromString(e, "{{ name | default(\"anon\") | upper }}", "filt.html")
	testing.assert(t != null, "compile null")
	let out = mtml.render(e, t, {})
	testing.assert(out != null, "render null")
	testing.assertEq("ANON", out)
end)

# 3) Custom filter (prefix)
testing.test("mtml/custom filter prefix", fun(_: Null) do
	let e = mtml.env({})
	let f = fun(x) do
		fun(p: Str) -> Str do
			let sx = str(x)
			if sx == null then
				""
			else
				p + sx
			end
		end
	end
	let _ = mtml.registerFilter(e, "prefix", f)
	let t = mtml.fromString(e, "{{ who | prefix(\"Hi \") }}", "pref.html")
	testing.assert(t != null, "compile null")
	let out = mtml.render(e, t, {who: "Ada"})
	testing.assert(out != null, "render null")
	testing.assertEq("Hi Ada", out)
end)

# 4) Helper call (repeat)
testing.test("mtml/helper call repeat", fun(_: Null) do
	let e = mtml.env({})
	# repeat(s: Str, n: Int) -> Str
	let rep = fun(s: Str) do
		fun(n: Int) -> Str do
			let i = 0
			let acc = ""
			while i < n do
				acc = acc + s
				i = i + 1
			end
			acc
		end
	end
	let _ = mtml.registerHelper(e, "repeat", rep)
	let t = mtml.fromString(e, "{{ repeat(\"ha\", 3) }}", "rep.html")
	testing.assert(t != null, "compile null")
	let out = mtml.render(e, t, {})
	testing.assert(out != null, "render null")
	testing.assertEq("hahaha", out)
end)

# 5) For over array + loop meta (index)
testing.test("mtml/for array loop meta", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "{% for x in xs %}{{ loop.index }}:{{ x }};{% end %}", "loop.html")
	testing.assert(t != null, "compile null")
	let out = mtml.render(e, t, {xs: ["a", "b"]})
	testing.assert(out != null, "render null")
	testing.assertEq("1:a;2:b;", out)
end)

# 6) strictUndefined -> error (annotated null)
testing.test("mtml/strictUndefined error", fun(_: Null) do
	let e = mtml.env({}) # strict=true default

	let t = mtml.fromString(e, "X: {{ nope }}", "strict.html")
	testing.assert(t != null, "compile null")
	let out = mtml.render(e, t, {})
	testing.assertEq(null, out)
end)

# 7) strictUndefined=false -> missing renders empty string
testing.test("mtml/strictUndefined false -> empty", fun(_: Null) do
	let e = mtml.env({strictUndefined: false})
	let t = mtml.fromString(e, "A{{ missing }}B", "nonstrict.html")
	testing.assert(t != null, "compile null")
	let out = mtml.render(e, t, {})
	testing.assert(out != null, "render null")
	testing.assertEq("AB", out)
end)

# 8) include with inline context (uses temp dir)
testing.test("mtml/include with ctx", fun(_: Null) do
	let dir = tempDir()
	let p = pathJoin([dir, "part.html"])
	let n = writeFile(p, "<em>{{ x }}</em>")
	testing.assert(n != null, "write part failed")

	let e = mtml.env({baseDir: dir})
	let t = mtml.fromString(e, "<p>Start {% include \"part.html\" with {x: \"ok\"} %} End</p>", "incl.html")
	testing.assert(t != null, "compile null")
	let out = mtml.render(e, t, {})
	testing.assert(out != null, "render null")
	testing.assertEq("<p>Start <em>ok</em> End</p>", out)
end)

# 9) extends + block override (single inheritance; uses temp dir)
testing.test("mtml/extends block override", fun(_: Null) do
	let dir = tempDir()
	let basePath = pathJoin([dir, "base.html"])
	let _ = writeFile(basePath, "<html><body>{% block content %}base{% end %}</body></html>")

	let e = mtml.env({baseDir: dir})
	let child = "{% extends \"base.html\" %}{% block content %}hi{% end %}"
	let t = mtml.fromString(e, child, "child.html")
	testing.assert(t != null, "compile null")

	let out = mtml.render(e, t, {})
	testing.assert(out != null, "render null")
	testing.assertEq("<html><body>hi</body></html>", out)
end)

# 10) URLs: escaped vs safeURL in attribute
testing.test("mtml/url escaped vs safe", fun(_: Null) do
	let e = mtml.env({})
	let t1 = mtml.fromString(e, "<a href=\"{{ url }}\">go</a>", "url1.html")
	testing.assert(t1 != null, "compile1 null")
	let out1 = mtml.render(e, t1, {url: "https://ex.com/?q=1&x=\"y\""})
	testing.assert(out1 != null, "render1 null")
	# quotes and & must be escaped
	testing.assertEq("<a href=\"https://ex.com/?q=1&amp;x=&quot;y&quot;\">go</a>", out1)

	let t2 = mtml.fromString(e, "<a href=\"{{ url }}\">go</a>", "url2.html")
	testing.assert(t2 != null, "compile2 null")
	let out2 = mtml.render(e, t2, {
		url: mtml.safeURL("https://ex.com/?q=1&x=\"y\"")
	})
	testing.assert(out2 != null, "render2 null")
	# safeURL bypasses escaping
	testing.assertEq("<a href=\"https://ex.com/?q=1&x=\"y\"\">go</a>", out2)
end)

# --- Ternary ---------------------------------------------------------------

testing.test("mtml/ternary basic", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "{{ 'yes' if ok else 'no' }}", "tern1.html")
	testing.assert(t != null, "compile null")
	testing.assertEq("yes", mtml.render(e, t, {ok: true}))
	testing.assertEq("no", mtml.render(e, t, {ok: false}))
end)

testing.test("mtml/ternary with filters", fun(_: Null) do
	let e = mtml.env({})
	let _ = mtml.installBuiltins(e)
	let t = mtml.fromString(e, "{{ ('hi' if loud else 'ok') | upper }}", "tern2.html")
	testing.assertEq("HI", mtml.render(e, t, {loud: true}))
	testing.assertEq("OK", mtml.render(e, t, {loud: false}))
end)

# --- Whitespace trim around statements ------------------------------------

testing.test("mtml/trim around if", fun(_: Null) do
	let e = mtml.env({})
	let src = "A\n{%- if true -%}\nX\n{%- end -%}\nB"
	let t = mtml.fromString(e, src, "trim-if.html")
	testing.assertEq("A\nXB", mtml.render(e, t, {}))
end)

# --- Strict undefined: deterministic & nested ------------------------------

testing.test("mtml/strictUndefined bare name annotated", fun(_: Null) do
	let e = mtml.env({}) # strict true

	let t = mtml.fromString(e, "X={{ missing }}", "strict-bare.html")
	let out = mtml.render(e, t, {})
	testing.assertEq(null, out)
	# must carry an annotation with a clear reason
	testing.assert(toLower(noteGet(out)) != null, "no annotation on error")
	testing.assert(match("render: missing name", toLower(noteGet(out))) != null, "missing reason")
end)

testing.test("mtml/strictUndefined nested helper arg", fun(_: Null) do
	let e = mtml.env({})
	let _ = mtml.registerHelper(e, "wrap", fun(x) do
		fun(y) do
			str(x) + str(y)
		end
	end)
	let t = mtml.fromString(e, "{{ wrap('x', missing) }}", "strict-nested.html")
	let out = mtml.render(e, t, {})
	testing.assertEq(null, out)
	testing.assert(match("render:", toLower(noteGet(out))) != null, "no render error annotation")
end)

# --- Error annotations for parse/load/filter failures ----------------------

testing.test("mtml/error annotate: unclosed tag", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "<p>{{ x </p>", "bad1.html")
	testing.assertEq(null, t)
	testing.assert(match("parse:", toLower(noteGet(t))) != null, "expected parse annotation")
end)

testing.test("mtml/error annotate: unknown filter", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "{{ 1 | nope() }}", "bad2.html")
	let out = mtml.render(e, t, {})
	testing.assertEq(null, out)
	testing.assert(match("render: unknown filter", toLower(noteGet(out))) != null, "expected filter error")
end)

testing.test("mtml/error annotate: include missing", fun(_: Null) do
	let e = mtml.env({baseDir: tempDir()})
	let t = mtml.fromString(e, "A {% include \"nope.html\" %} B", "incl-missing.html")
	let out = mtml.render(e, t, {})
	testing.assertEq(null, out)
	testing.assert(match("load: include", toLower(noteGet(out))) != null, "expected include load error")
end)

# --- safeURL allowlist -----------------------------------------------------

testing.test("mtml/safeURL allowed https", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "<a href=\"{{ url }}\">go</a>", "safeurl-ok.html")
	let out = mtml.render(e, t, {
		url: mtml.safeURL("https://ex.com/?q=1&x=\"y\"")
	})
	testing.assertEq("<a href=\"https://ex.com/?q=1&x=\"y\"\">go</a>", out)
end)

testing.test("mtml/safeURL rejects javascript", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "<a href=\"{{ url }}\">go</a>", "safeurl-bad.html")
	let out = mtml.render(e, t, {url: mtml.safeURL("javascript:alert(1)")})
	testing.assertEq(null, out)
	testing.assert(match("disallowed url scheme", toLower(noteGet(out))) != null, "expected scheme error")
end)

# --- Map iteration ergonomics (destructuring) ------------------------------

testing.test("mtml/for object destructuring", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "{% for [k, v] in m %}{{ k }}={{ v }};{% end %}", "for-map-destr.html")
	let out = mtml.render(e, t, {m: {a: 1, b: 2}})
	testing.assertEq("a=1;b=2;", out) # map order preserved per spec
end)

# --- Extends: missing parent annotated -------------------------------------

testing.test("mtml/extends missing parent annotates", fun(_: Null) do
	let e = mtml.env({baseDir: tempDir()})
	let child = "{% extends \"no-base.html\" %}{% block content %}x{% end %}"
	let t = mtml.fromString(e, child, "child-miss.html")
	let out = mtml.render(e, t, {})
	testing.assertEq(null, out)
	testing.assert(match("load: extends", toLower(noteGet(out))) != null, "expected extends load error")
end)

# loader jail
testing.test("mtml/jail blocks parent escape", fun(_) do
	let dir = tempDir()
	let e = mtml.env({baseDir: dir})
	let t = mtml.fromString(e, "{% include \"../hack.html\" %}", "jail.html")
	let out = mtml.render(e, t, {})
	testing.assertEq(null, out)
	testing.assert(match("path escapes basedir", toLower(noteGet(out))) != null, "expected jail error")
end)

testing.test("mtml/href plain string allowed", fun(_) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "<a href=\"{{ u }}\">x</a>", "href.html")
	let out = mtml.render(e, t, {u: "/ok?q=1"})
	testing.assertEq("<a href=\"/ok?q=1\">x</a>", out)
end)

# (Optional but recommended) add a guard test for disallowed schemes with plain strings:
testing.test("mtml/href plain string rejects javascript", fun(_) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "<a href=\"{{ u }}\">x</a>", "href-bad.html")
	let out = mtml.render(e, t, {u: "javascript:alert(1)"})
	testing.assertEq(null, out)
end)

# event handler blocked
testing.test("mtml/event handler blocked", fun(_) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "<a onclick='{{ x }}'>x</a>", "onclick.html")
	testing.assertEq(null, mtml.render(e, t, {x: "alert(1)"}))
end)

# script requires tojson
testing.test("mtml/script requires tojson", fun(_: Null) do
	let e = mtml.env({})
	let t1 = mtml.fromString(e, "<script>var x={{ v }}</script>", "script1.html")
	testing.assertEq(null, mtml.render(e, t1, {v: "<b>"}))
	let _ = mtml.installBuiltins(e)
	let t2 = mtml.fromString(e, "<script>var x={{ v | tojson }}</script>", "script2.html")
	# Expect JSON-escaped angle brackets inside the string literal
	testing.assertEq("<script>var x=\"\\u003cb\\u003e\"</script>", mtml.render(e, t2, {
		v: "<b>"
	}))
end)

# |safe must be string or already-safe
testing.test("mtml/safe guard", fun(_) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "{{ 42 | safe }}", "safe-guard.html")
	testing.assertEq(null, mtml.render(e, t, {}))
end)

# --- srcset: preserves descriptors and validates each URL -------------------

testing.test("mtml/srcset preserves descriptors", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "<img srcset=\"{{ s }}\">", "srcset-ok.html")
	let out = mtml.render(e, t, {s: "/a.jpg 1x, https://ex.com/b.jpg 2x"})
	testing.assertEq("<img srcset=\"/a.jpg 1x, https://ex.com/b.jpg 2x\">", out)
end)

testing.test("mtml/srcset rejects bad candidate", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "<img srcset=\"{{ s }}\">", "srcset-bad.html")
	let out = mtml.render(e, t, {s: "/ok.jpg 1x, javascript:alert(1) 2x"})
	testing.assertEq(null, out)
end)

# --- loader jail: Windows-style backslash escape should be blocked ---------

testing.test("mtml/jail blocks backslash parent escape", fun(_: Null) do
	let dir = tempDir()
	let e = mtml.env({baseDir: dir})
	let t = mtml.fromString(e, "{% include \"..\\\\hack.html\" %}", "jail-win.html")
	let out = mtml.render(e, t, {})
	testing.assertEq(null, out)
end)

# --- error spans: annotated message should include a line:col --------------

testing.test("mtml/errors include line and col", fun(_: Null) do
	let e = mtml.env({})
	let src = "line1\nline2 {{ missing }} line2b"
	let t = mtml.fromString(e, src, "spans.html")
	let out = mtml.render(e, t, {})
	testing.assertEq(null, out)
	testing.assert(match("\\d+:\\d+", noteGet(out)) != null, "expected line:col in annotation")
end)

# --- Error spans: parse-time unclosed tag includes line:col -----------------

testing.test("mtml/error spans: parse unclosed shows line:col", fun(_: Null) do
	let e = mtml.env({})
	let src = "line1\n<p>{{ x </p>\nline3"
	let t = mtml.fromString(e, src, "unclosed.html")
	testing.assertEq(null, t)
	testing.assert(match("^\\[parse\\].*\\d+:\\d+", noteGet(t)) != null, "expected [parse] ... L:C")
end)

# --- Error spans: runtime unknown filter includes line:col ------------------

testing.test("mtml/error spans: unknown filter shows line:col", fun(_: Null) do
	let e = mtml.env({})
	let src = "a\nb {{ 1 | nope }} c\nd"
	let t = mtml.fromString(e, src, "unknown-filter.html")
	testing.assert(t != null, "compile should succeed")
	let out = mtml.render(e, t, {})
	testing.assertEq(null, out)
	let msg = toLower(noteGet(out))
	testing.assert(match("render: unknown filter", msg) != null, "expected unknown filter")
	testing.assert(match("\\d+:\\d+", msg) != null, "expected line:col in annotation")
end)

let testing = import("testing")
let mtml = import("mtml")

# 1) Unforgeable SafeHTML — forged shape must NOT bypass escaping
testing.test("mtml/safehtml forgery escapes", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "<div>{{ x }}</div>", "f.html")
	let out = mtml.render(e, t, {
		x: {__safe_html__: true, html: "<img src=x onerror=1>"}
	})
	testing.assertEq(null, out)
	testing.assert(match("forged safehtml", toLower(noteGet(out))) != null, "expected forged SafeHTML annotation")
end)

# 1) Unforgeable SafeURL — forged shape must NOT bypass checks
testing.test("mtml/safeurl forgery rejected", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "<a href=\"{{ u }}\">go</a>", "fu.html")
	let out = mtml.render(e, t, {
		u: {__safe_url__: true, url: "javascript:alert(1)"}
	})
	testing.assertEq(null, out)
end)

# 2) Correct context: <script src="{{ ... }}"> is URL attribute, no tojson needed
testing.test("mtml/script src attribute is URLAttr", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "<script src=\"{{ u }}\"></script>", "scattr.html")
	let out = mtml.render(e, t, {u: "https://cdn.ex/x.js"})
	testing.assertEq("<script src=\"https://cdn.ex/x.js\"></script>", out)
end)

# 2) Still strict inside script content: requires tojson
testing.test("mtml/script content requires tojson (still)", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "<script>var x={{ v }}</script>", "sccontent.html")
	let out = mtml.render(e, t, {v: "<b>"})
	testing.assertEq(null, out)
end)

# 3) Re-validate SafeURL at render time (policy tightened: mailto disallowed)
testing.test("mtml/safeurl revalidated at render", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "<a href=\"{{ u }}\">x</a>", "reval.html")
	let out = mtml.render(e, t, {u: mtml.safeURL("mailto:sales@example.com")})
	testing.assertEq(null, out)
end)

# 4A) Arity check: helper called with too many args -> annotated error
testing.test("mtml/helper arity mismatch annotated", fun(_: Null) do
	let e = mtml.env({})
	let rep = fun(s: Str) do
		fun(n: Int) -> Str do
			"s" # stub content, arity is what we care about
		end
	end
	let _ = mtml.registerHelper(e, "repeat", rep)
	let t = mtml.fromString(e, "{{ repeat(\"ha\", 3, \"oops\") }}", "arity.html")
	let out = mtml.render(e, t, {})
	testing.assertEq(null, out)
	testing.assert(match("arity", toLower(noteGet(out))) != null, "expected arity in message")
end)

# 4B) Ternary laziness: untaken branch must NOT be evaluated
testing.test("mtml/ternary untaken branch not evaluated", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "{{ 'ok' if ok else missing.deep.field }}", "tern-ok.html")
	let out = mtml.render(e, t, {ok: true})
	testing.assertEq("ok", out)
end)

# 4C) Ternary laziness: taken branch with missing should error
testing.test("mtml/ternary taken branch errors", fun(_: Null) do
	let e = mtml.env({})
	let t = mtml.fromString(e, "{{ 'ok' if ok else missing.deep.field }}", "tern-bad.html")
	let out = mtml.render(e, t, {ok: false})
	testing.assertEq(null, out)
end)