# MTML — A Gentle Guide

MTML is a tiny, safe, HTML templating engine for MindScript. It aims for “less is more”: a short, ergonomic surface with strong defaults (context-aware autoescape, jailed loader, strict undefined) and a clear extension story (typed filters/helpers).

This tutorial walks you from zero to productive in ~15 minutes.

---

## 1) What you get (and what you don’t)

**You get**

* Interpolation: `{{ expr | filter(a,b) }}`
* Control blocks: `{% if %}…{% elif %}…{% else %}…{% end %}`, `{% for x in xs %}…{% end %}`
* Composition: `{% include "path.html" [with expr] %}`
* Layouts: `{% extends "base.html" %}` + `{% block name %}…{% end %}`
* Whitespace control: `{%- … -%}`, `{{- … -}}`
* Built-in filters: `upper`, `lower`, `default`, `join`, `tojson`, `url`
* Context-aware escaping (HTML / attributes / URL attr validation / script rules)

**You do not get**

* `|safe` in templates (forbidden by policy)
* Logic-heavy code inside templates (helpers/filters are allowed, mutation and side effects are not)
* Interpolation in `style=` or event handler attributes (e.g., `onclick`)

---

## 2) Quick start

### 2.1 Create an environment

```mindscript
let mtml = import("mtml")

# Jailed loader (baseDir), strict undefined on by default, autoescape on by default.
let e = mtml.env({ baseDir: "/templates" })
let _ = mtml.installBuiltins(e)  # optional but recommended
```

### 2.2 Render from a string

```mindscript
let t = mtml.fromString(e, "<p>Hello, {{ name | default('friend') }}</p>", "hello.html")
let out = mtml.render(e, t, { name: "Ada" })
# => "<p>Hello, Ada</p>"
```

### 2.3 Render from a file

```mindscript
# With baseDir set, paths are jailed inside it.
let t = mtml.load(e, "page.html")
let html = mtml.render(e, t, { title: "Home" })
```

---

## 3) Interpolation & expressions

* Anything inside `{{ … }}` is an **expression**.
* Pipes apply filters left→right: `{{ price | fmt("GBP") | upper }}`.
* Ternary is supported: `{{ 'yes' if ok else 'no' }}` (lazy branches).
* Missing names:

  * **strictUndefined = true (default):** bare `{{ missing }}` → **error** (annotated null).
  * **strictUndefined = false:** renders as `""`.

Examples:

```html
<p>{{ user.name | default('Anonymous') }}</p>
<p>{{ ('HI' if loud else 'hi') | lower }}</p>
```

---

## 4) Control flow

### 4.1 If / elif / else

```html
{% if user %}
  <p>Hi, {{ user.name }}</p>
{% elif guest %}
  <p>Welcome, guest</p>
{% else %}
  <p>Please sign in</p>
{% end %}
```

### 4.2 For loops (+ loop meta)

```html
<ul>
{% for x in items %}
  <li>{{ loop.index }}. {{ x }}</li>
{% end %}
</ul>
```

`loop` exposes: `index` (1-based), `index0`, `first`, `last`, `length`.

Map iteration:

```html
{% for [k, v] in person %}
  <p>{{ k }}={{ v }}</p>
{% end %}
```

---

## 5) Includes, blocks, and layout inheritance

### 5.1 Include a partial

```html
<!-- page.html -->
<h1>{{ title }}</h1>
{% include "card.html" with {title: "Card", body: "Hello"} %}
```

The `with` expression must evaluate to a **map**. It’s merged on top of the current scope.

### 5.2 Layouts with extends + blocks

```html
<!-- base.html -->
<html>
  <head><title>{% block title %}Untitled{% end %}</title></head>
  <body>{% block content %}{% end %}</body>
</html>

<!-- child.html -->
{% extends "base.html" %}
{% block title %}Home{% end %}
{% block content %}
  <h1>Welcome</h1>
{% end %}
```

> MTML does full replacement of blocks (no `super()` by design).

---

## 6) Whitespace control

Trim markers remove surrounding whitespace/newlines:

* `{%- … -%}` for statements
* `{{- … -}}` for expressions

Example:

```html
A
{%- if show -%}
X
{%- end -%}
B
```

Renders as:

```
A
XB
```

---

## 7) Safety model (read this!)

MTML is **context-aware**:

* **HTML text** → escaped
* **Generic attributes** → escaped (must be quoted when interpolating)
* **URL attributes** (`href`, `src`, `action`, `poster`, `formaction`, `xlink:href`, `data-src`, `srcset`, `imagesrcset`)

  * Plain strings are HTML-escaped and **validated** against an allowlist (e.g., `/`, `./`, `../`, `//`, `http(s)://`, `tel:`)
  * Special handling for `srcset`: each candidate URL is checked; descriptors are preserved
  * For trusted URLs, prefer the `url` filter (below)
* **Event handler attributes** (`on*`) → interpolation **forbidden**
* **`style=`** and **`<style>`** → interpolation **forbidden**
* **`<script>` content** → interpolation **requires** `| tojson` as the **final** filter

### 7.1 SafeURL & `url` filter

If you know a URL is safe, use the built-in `url` filter to wrap and mark it:

```html
<a href="{{ product.link | url }}">View</a>
```

Under the hood this produces a `SafeURL` value that passes brand checks and policy validation. MTML **re-validates at render time** (defense in depth).

### 7.2 JSON inside `<script>`

```html
<script>
  var data = {{ model | tojson }};
</script>
```

`tojson` emits a `SafeHTML` string with dangerous sequences escaped (`</script>`, `<`, `>`, `&`, U+2028/2029), and it is required for script contexts.

### 7.3 Why no `|safe` in templates?

To prevent accidental XSS. If you truly must insert trusted HTML, create it host-side:

```mindscript
let trusted = mtml.safeHTML("<em>trusted</em>")   # host only
let out = mtml.render(e, t, {snippet: trusted})
```

Template usage:

```html
<div>{{ snippet }}</div>  <!-- inserted verbatim; brand checked -->
```

---

## 8) Built-in filters

`mtml.installBuiltins(e)` registers:

* `upper(s: Str) -> Str`
* `lower(s: Str) -> Str`
* `default(x: Any, fallback: Any) -> Any`
  Empty string counts as “missing”.
* `join(xs: [Any], sep: Str) -> Str`
* `tojson(x: Any) -> SafeHTML` (for `<script>`)
* `url(u: Str) -> SafeURL?` (policy-checked)

---

## 9) Your own filters & helpers

Filters and helpers are **curried** functions (MindScript style). MTML performs a simple arity check and returns an **annotated error** on mismatch.

### 9.1 Filter example: `prefix`

```mindscript
let prefix = fun(x) do              # x is the value from the pipe
  fun(p: Str) -> Str do             # filter argument(s) come next
    let sx = str(x)
    if sx == null then "" else p + sx end
  end
end

let _ = mtml.registerFilter(e, "prefix", prefix)
# Usage: {{ name | prefix("Hi ") }}
```

### 9.2 Helper example: `repeat(s, n)`

```mindscript
let repeat = fun(s: Str) do
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

let _ = mtml.registerHelper(e, "repeat", repeat)
# Usage: {{ repeat("ha", 3) }}  => "hahaha"
```

---

## 10) Rendering APIs

* **Compile from string:** `mtml.fromString(e, src:Str, name?:Str) -> Template?`
* **Load from file (jailed):** `mtml.load(e, "path.html") -> Template?`
* **Render to string:** `mtml.render(e, template, data:{}) -> Str?`
* **Render to writer:** `mtml.renderTo(e, template, writer, data) -> Int?`

> All error cases return **annotated null** (MindScript idiom). Use `noteGet(err)` to read the reason; messages include `[kind]` and `line:col` when available.

Example error handling:

```mindscript
let out = mtml.render(e, t, {})
if out == null then
  println("Render failed: " + noteGet(out))
end
```

---

## 11) Project layout example

```
/templates
  base.html
  card.html
  page.html
/app.ms
```

**base.html**

```html
<html>
  <head><title>{% block title %}Untitled{% end %}</title></head>
  <body>
    <main>{% block content %}{% end %}</main>
  </body>
</html>
```

**page.html**

```html
{% extends "base.html" %}
{% block title %}Products{% end %}
{% block content %}
  <h1>Products</h1>
  <ul>
  {% for p in products %}
    <li>
      <a href="{{ p.url | url }}">{{ p.name }}</a>
      – £{{ p.price }}
    </li>
  {% end %}
  </ul>
{% end %}
```

**app.ms**

```mindscript
let mtml = import("mtml")
let e = mtml.env({ baseDir: "/templates" })
let _ = mtml.installBuiltins(e)

let t = mtml.load(e, "page.html")
let data = {
  products: [
    { name: "Z1", price: 10, url: "https://example.com/z1" },
    { name: "Z2", price: 12, url: "/z2" }
  ]
}
let html = mtml.render(e, t, data)
```

---

## 12) Common pitfalls & how to avoid them

* **Unquoted attributes:**
  `<a href={{ url }}>` → **forbidden**. Always quote: `<a href="{{ url }}">`.
* **Missing `tojson` in `<script>`:**
  `<script>var x={{ data }}</script>` → **error**. Use `| tojson`.
* **Event handlers & style:**
  `<div onclick="{{ … }}">` or `<div style="{{ … }}">` → **forbidden**.
* **Unknown filter/helper names:** Make sure you `installBuiltins` or register your custom ones.
* **Path escapes:** `{% include "../x.html" %}` → blocked by the loader jail.

---

## 13) FAQ

**Q: Can I turn off autoescape?**
A: The environment has `autoescape` in options, but you generally shouldn’t. Prefer `safeHTML()` host-side for trusted markup and `url()` for trusted URLs.

**Q: How do I pass local data to an include?**
A: `{% include "row.html" with { item: it, i: loop.index } %}`

**Q: Is there `super()` in blocks?**
A: No. MTML favors full replacement for simplicity.

**Q: Can I access globals?**
A: Yes. Register them via `registerGlobal(e, "name", value)`. They merge into scope (locals win).

---

## 14) Cheatsheet

* **Interpolation:** `{{ expr | filter(a,b) }}`
* **Ternary:** `A if cond else B`
* **If:** `{% if … %}…{% elif … %}…{% else %}…{% end %}`
* **For:** `{% for x in xs %}…{% end %}`; map: `{% for [k, v] in m %}`
* **Include:** `{% include "path.html" [with expr] %}`
* **Extends/Blocks:** `{% extends "base.html" %}`, `{% block name %}…{% end %}`
* **Trim:** `{%- … -%}` and `{{- … -}}`
* **Script JSON:** `{{ data | tojson }}`
* **URL attrs:** `{{ url | url }}` (or plain strings that pass policy)
* **Built-ins:** `upper`, `lower`, `default`, `join`, `tojson`, `url`

---

## 15) Troubleshooting with diagnostics

On parse or render errors, MTML returns **annotated null** with messages like:

```
[parse] unclosed tag at 3:12
[render] missing name user at 1:9
[render] interpolation inside <script> requires | tojson at 5:3
[render] disallowed url scheme at 1:15
```

Use these to jump straight to the offending spot.

---

That’s it! With this small surface you can build clean, safe templates, and push complexity into typed filters/helpers where it belongs. If you’d like, I can add a few “exercise” templates (blog, dashboard card grid, email layout) to practice each feature.
