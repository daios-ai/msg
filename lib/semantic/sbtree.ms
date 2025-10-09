# sbtree — Semantic B-tree (two-field Node), with safe oracle wrappers

# --------------------------------------------
# Types
# --------------------------------------------
let Node = type {children!: [Node], content!: Str}

# --------------------------------------------
# Oracles (nullable returns by design)
# --------------------------------------------

# Given a query and a set of options, pick the index in {0, 1, ...} of the most
# relevant option in the set.
let chooseChild = oracle(query: Str, options: [Str]) -> Int?

let summarizeExamples = [
	[
		[
			"HTTP caching: ETag, Cache-Control, and CDN basics",
			"Cache validation with ETag and Last-Modified headers",
			"CDN edge caching strategies and cache-control directives"
		],
		"Caches validate responses with ETag/Last-Modified and apply Cache-Control at CDNs to serve hits and shrink origin load."
	],
	[
		[
			"TLS handshakes and certificate chains",
			"Mutual TLS and client certificate verification"
		],
		"TLS handshakes exchange certificate chains, verify identities (optionally mutual), and derive session keys for encrypted transport."
	],
	[
		[
			"Docker images, layers, and registries",
			"Kubernetes deployments and rolling updates"
		],
		"Build layered container images, push to registries, then roll out updates on Kubernetes without dropping traffic."
	],
	[
		[
			"Vector databases vs inverted indexes for search",
			"Transformer attention and positional encodings",
			"Neural networks: overfitting, regularization, dropout"
		],
		"Compare vector and inverted indexes for matching, use attention with positional encodings for sequence modeling, and curb overfitting via regularization/dropout."
	],
	[
		[
			"PostgreSQL indexing strategies and B-Tree performance",
			"Covering indexes and index-only scans in Postgres"
		],
		"Tune B-Tree and covering indexes to enable index-only scans and cut I/O in PostgreSQL queries."
	]
]

# Given a non-empty list of texts, summarize them into a short paragraph
# of no more than 3 lines.
let summarize = oracle(texts: [Str]) -> Str? from summarizeExamples

# Given a collection of `N` texts, partition them into two coherent groups
# called `left` and `right`. For each group, indicate the members through
# their indicies in `0...(N-1)`.
let split = oracle(children: [Str]) -> {left!: [Int], right!: [Int]}?

# --------------------------------------------
# Tiny safe wrappers (prevent nulls from breaking Node)
# --------------------------------------------
let _safeChoose = fun(query: Str, opts: [Str]) -> Int do
	println("before chooseChild: " + query)
	let i = chooseChild(query, opts)
	println("after chooseChild: " + query)
	if i == null or i < 0 or i >= len(opts) then
		0
	else
		i
	end
end

let _safeSumm = fun(texts: [Str]) -> Str do
	println("before summarize")
	let s = summarize(texts)
	println("after summarize")
	if s == null then
		texts[0]
	else
		s
	end
end

let _safeSplit = fun(cs: [Str]) -> {left!: [Int], right!: [Int]} do
	println("before split")
	let g = split(cs)
	println("after split")
	if g != null and len(g.left) > 0 and len(g.right) > 0 then
		return g
	end
	# Fallback: balanced halving by index
	let n = len(cs)
	let mid = if n % 2 == 0 then
		n / 2
	else
		n / 2 + 1
	end
	let L = []
	let i = 0
	while i < mid do
		L = L + [i]
		i = i + 1
	end
	let R = []
	while i < n do
		R = R + [i]
		i = i + 1
	end
	{left: L, right: R}
end

# --------------------------------------------
# Helpers
# --------------------------------------------
let leaf = fun(text: Str) -> Node do
	{content: text, children: []}
end

let _centroids = fun(kids: [Node]) -> [Str] do
	let out = []
	let i = 0
	while i < len(kids) do
		out = out + [kids[i].content]
		i = i + 1
	end
	out
end

let internal = fun(kids: [Node]) -> Node do
	{
		content: _safeSumm(_centroids(kids)),
		children: kids
	}
end

let isLeaf = fun(n: Node) -> Bool do
	len(n.children) == 0
end

let kidCents = fun(n: Node) -> [Str] do
	_centroids(n.children)
end

let _splitIfNeeded = fun(n: Node, b: Int) -> Node do
	if len(n.children) <= b then
		n
	else
		let ks = n.children
		let grp = _safeSplit(_centroids(ks))

		let leftKids = []
		let li = 0
		while li < len(grp.left) do
			leftKids = leftKids + [ks[grp.left[li]]]
			li = li + 1
		end

		let rightKids = []
		let ri = 0
		while ri < len(grp.right) do
			rightKids = rightKids + [ks[grp.right[ri]]]
			ri = ri + 1
		end

		internal([internal(leftKids), internal(rightKids)])
	end
end

# --------------------------------------------
# Search (top-1 → leaf Node)
# --------------------------------------------
let search = fun(root: Node, query: Str) -> Node do
	let n = root
	while not isLeaf(n) do
		let i = _safeChoose(query, kidCents(n))
		n = n.children[i]
	end
	n
end

# --------------------------------------------
# Insert (promote leaf; refresh/split upward) → Node
# --------------------------------------------
# Splice grandchildren into parent (after a leaf promotion).
let _splicePromoted = fun(ks: [Node], i: Int, child: Node) -> [Node] do
	let out = []
	let j = 0
	while j < len(ks) do
		if j != i then
			out = out + [ks[j]]
		else
			out = out + [child.children[0]]
			out = out + [child.children[1]]
		end
		j = j + 1
	end
	out
end

# Insert (Node, not Type) — returns possibly new root.
let insert = fun(root: Node, text: Str, b: Int) -> Node do
	let rec = fun(n: Node) -> {node!: Node, promoted!: Bool} do
		if len(n.children) == 0 then
			return {
				node: internal([leaf(n.content), leaf(text)]),
				promoted: true
			}
		end

		let i = _safeChoose(text, kidCents(n))
		let ks = clone(n.children)
		let r = rec(ks[i])

		if r.promoted then
			let ks2 = _splicePromoted(ks, i, r.node)
			return {node: _splitIfNeeded(internal(ks2), b), promoted: false}
		end

		ks[i] = r.node
		{node: _splitIfNeeded(internal(ks), b), promoted: false}
	end

	rec(root).node
end

# --------------------------------------------
# Delete (nullable recursion) → Node
# --------------------------------------------
let remove = fun(root: Node, text: Str, b: Int) -> Node do
	let rec = fun(n: Node) -> Node? do
		if isLeaf(n) then
			if n.content == text then
				return null # deleted

			end
			return n
		end

		let i = _safeChoose(text, kidCents(n))
		let ks = clone(n.children)
		let child = rec(ks[i])

		if child == null then
			# drop child i
			let out = []
			let j = 0
			while j < len(ks) do
				if j != i then
					out = out + [ks[j]]
				end
				j = j + 1
			end
			if len(out) == 0 then
				return null # prune empty internal

			elif len(out) == 1 then
				return out[0] # collapse single-child internal

			end
			return internal(out)
		end

		ks[i] = child
		internal(ks)
	end

	let r = rec(root)
	if r == null then
		leaf("") # empty-tree sentinel: single empty leaf (still a valid tree)

	else
		if not isLeaf(r) and len(r.children) == 1 then
			r.children[0]
		else
			r
		end
	end
end