# Task loader & report (concurrency + JSON + schema)

# --- Types ------------------------------------------------------------

let Status = type Enum["todo","doing","done"]

let Task = type {
  id!:    Int,
  title!: Str,
  status!: Status,
  tags:   [Str]?,
  # Unix ms
  due:    Int?
}

# --- Helpers ----------------------------------------------------------

# safeGet: fetch key from map, else null
let safeGet = fun(m, k) do
  if m.has(k) then m[k] else null end
end

# coerceTask: json -> Task?
# Returns a Task map if it validates; otherwise null (with a note in the value printer).
let coerceTask = 
# Validate and coerce JSON object into Task
fun(x: Any) -> Task? do
  if not (isType(x, type {id!: Any, title!: Any, status!: Any})) then
    null
  else
    let idv = safeGet(x, "id")
    let tl  = safeGet(x, "title")
    let st  = safeGet(x, "status")
    let tg  = safeGet(x, "tags")
    let du  = safeGet(x, "due")

    # Basic shape checks
    if not isType(idv, type Int) then null else
    if not isType(tl,  type Str) then null else
    if not isType(st,  type Str) then null else

    # status must be in Status enum
    let stOk = (st == "todo" or st == "doing" or st == "done")
    if not stOk then null else

    # tags is optional [Str]
    let tg1 = if tg == null then null else
                if isType(tg, type [Str]) then tg else null end
              end

    # due is optional Int
    let due1 = if du == null then null else
                 if isType(du, type Int) then du else null end
               end

    { id: idv, title: tl, status: st, tags: tg1, due: due1 }
  end
end

# prettyTask: compact line for a task
let prettyTask = fun(t: Task) -> Str do
  let tagStr = if t.tags == null then "" else " [" + jsonStringify(t.tags) + "]" end
  let dueStr = if t.due == null then "" else " (due:" + t.due + ")" end
  "[" + t.status + "] " + t.title + " #" + t.id + tagStr + dueStr
end

# --- IO: read one JSON file -------------------------------------------

let readTaskFile = 
# Read a single JSON task file; returns Task? (null if invalid)
fun(path: Str) -> Task? do
  let s = readFile(path)
  let obj = jsonParse(s)
  coerceTask(obj)
end

# --- Concurrency loader -----------------------------------------------

let loadTasks =
# Concurrently read all *.json files under dir, returning [Task]
fun(dir: Str) -> [Task] do
  let names = listDir(dir)
  let c = chan()

  # worker to read a single file path and push Task? result
  let worker = fun(p: Str) do
    let t = readTaskFile(p)
    chanSend(c, {path: p, task: t})
    "ok"
  end

  # spawn workers for JSON files
  let paths = []
  for let nm in names do
    if endsWith(nm, ".json") then
      let p = dir + "/" + nm
      paths = push(paths, p)
      spawn(fun() do worker(p) end)
    end
  end

  # collect results (non-joining: we consume exactly len(paths) messages)
  let out = []
  let i = 0
  while i < len(paths) do
    let rec = chanRecv(c)
    if rec == null then
      # channel closed unexpectedly
      break(0)
    end
    if rec.task != null then
      out = push(out, rec.task)
    end
    i = i + 1
  end
  chanClose(c)
  out
end

# --- Analytics ---------------------------------------------------------

let groupByStatus = 
# Count tasks per status
fun(ts: [Task]) -> {todo: Int, doing: Int, done: Int} do
  let todo = 0
  let doing = 0
  let done = 0
  for let t in ts do
    if t.status == "todo" then
      todo = todo + 1
    elif t.status == "doing" then
      doing = doing + 1
    else
      done = done + 1
    end
  end
  {todo: todo, doing: doing, done: done}
end

let overdue =
# Extract tasks whose due < nowMillis()
fun(ts: [Task], nowMs: Int) -> [Task] do
  let out = []
  for let t in ts do
    if t.due != null and t.due < nowMs and t.status != "done" then
      out = push(out, t)
    end
  end
  out
end

# --- Small string helpers ---------------------------------------------

let endsWith = fun(s: Str, suf: Str) -> Bool do
  let n = len(s)
  let m = len(suf)
  if m > n then false else substring(s, n-m, n) == suf end
end

# substring: inclusive start, exclusive end (naive version using slices through Str indexing semantics)
let substring = fun(s: Str, i: Int, j: Int) -> Str do
  # Use a tiny loop to slice since we don't expose slices natively
  # Inefficient but fine for demo
  let out = ""
  let k = i
  while k < j do
    out = out + s[k]
    k = k + 1
  end
  out
end

# push: append to array
let push = fun(a, x) do a + [x] end

# len for Str or [Any]
let len = fun(x) -> Int do
  # Assume engine has length operator; if not, emulate via string/array
  __len(x)
end

# --- Main --------------------------------------------------------------

# CHANGE THIS to your folder containing *.json tasks
let DIR = "./tasks"

# Run
let ts = loadTasks(DIR)
write(stdout, "Loaded " + len(ts) + " tasks\n")

let counts = groupByStatus(ts)
write(stdout, "By status: " + jsonStringify(counts) + "\n")

let now = nowMillis()
let late = overdue(ts, now)
write(stdout, "Overdue (" + len(late) + "):\n")
for let t in late do
  write(stdout, "  - " + prettyTask(t) + "\n")
end
flush(stdout)

# Also return a structured result if run non-interactively
{loaded: ts, counts: counts, overdue: late}
end
end
end


