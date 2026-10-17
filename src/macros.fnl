;; Macros for the tools: (import-macros {: each-repo} :src.macros)
;; Careful: macros are compiled when the tool loads and stay cached; if this file
;; changes the REPL has to be restarted (hot reload only covers tools/).

(fn each-repo [[name repo] ...]
  "Iterates over `repo` if given, or over all of config.repo-names, with `name` bound
to the repo of each iteration. The body returns the list of elements for that repo;
they are all gathered into a single list, and every element without :repo gets `name`.

  (each-repo [name repo]
    (json.decode (gh-something-of (. config.repos name))))"
  `(let [out# []
         config# (require :config)]
     (each [_# ,name (ipairs (if ,repo [,repo] (. config# :repo-names)))]
       (each [_# item# (ipairs (or (do ,...) []))]
         (when (and (= (type item#) :table) (= (. item# :repo) nil))
           (tset item# :repo ,name))
         (table.insert out# item#)))
     out#))

(fn each-local-repo [[name dir repo] ...]
  "Like each-repo, but for tools that work with the local checkout: also binds
`dir` to the repo's local path (config.local-repos). If a repo without a local
path was requested, it fails; when iterating over all, it skips those without one.

  (each-local-repo [name dir repo]
    (git-something-in dir))"
  `(do
     (when (and ,repo (not (. (require :config) :local-repos ,repo)))
       (error (.. "no local path for " ,repo) 0))
     ,(each-repo [name repo]
        `(let [,dir (. (require :config) :local-repos ,name)]
           (when ,dir ,...)))))


;; deftool: defines a tool, and checks at compile time the mistakes that otherwise only show up
;; when the planner (or you) run it.
;;   (deftool open-prs
;;     {:short "..." :description "..." :params [(params.repo "...")] :run (fn [{: repo}] ...)})
;; The :name comes from the symbol, and has to match the file name. `:worktree true` adds the
;; :repo and :ticket params of the tools that work on a worktree (so the :wt mode can fill them in).

(local tool-keys ["short" "description" "input" "accepts" "returns" "text" "tee"
                  "params" "run" "show" "command" "parse" "worktree"])
(local param-types ["string" "integer" "date"])

(fn member? [xs x]
  (accumulate [found false _ v (ipairs xs)] (or found (= v x))))

(fn distance [a b]
  "Edit distance between two short strings (to suggest the key that was probably meant)."
  (let [prev []]
    (for [j 0 (length b)] (tset prev j j))
    (for [i 1 (length a)]
      (var diag (. prev 0))
      (tset prev 0 i)
      (for [j 1 (length b)]
        (let [above (. prev j)
              cost (if (= (a:sub i i) (b:sub j j)) 0 1)]
          (tset prev j (math.min (+ above 1) (+ (. prev (- j 1)) 1) (+ diag cost)))
          (set diag above))))
    (. prev (length b))))

(fn suggestion [key]
  (let [best (accumulate [b nil _ k (ipairs tool-keys)]
               (if (and (<= (distance key k) 2) (or (not b) (< (distance key k) (distance key b)))) k b))]
    (if best (.. " (did you mean :" best "?)") "")))

;; The src.params constructors and the param each one declares. A macro cannot load src.params
;; (the compiler sandbox has no `os`, which config.fnl needs), so this is copied by hand:
;; keep it in sync with src/params.fnl.
(local param-ctors {"repo" "repo" "repo-required" "repo" "ticket" "ticket"
                    "pr-number" "number" "pr-ticket" "ticket" "jira-key" "ticket"})

(fn param-name [p tool]
  "The name a :params entry declares, or nil if it cannot be known at compile time: a table
literal declares it in :name, and (params.x ...) is one of the src.params constructors."
  (if (table? p)
      (. p "name")
      (list? p)
      (let [ctor (: (tostring (. p 1)) :match "^params%.(.+)$")]
        (when ctor
          (assert-compile (. param-ctors ctor)
                          (.. tool ": params." ctor " does not exist (valid: "
                              (table.concat (icollect [k _ (pairs param-ctors)] k) ", ") ")") p)
          (. param-ctors ctor)))))

(fn check-param [p tool]
  (when (table? p)
    (assert-compile (= (type (. p "name")) :string) (.. tool ": a param without a :name") p)
    (let [name (. p "name")
          type* (. p "type")]
      (assert-compile (member? param-types type*)
                      (.. tool ": param " name " has type " (tostring type*)
                          " (valid: " (table.concat param-types ", ") ")") p)
      (assert-compile (or (= (. p "enum") nil) (= type* "string"))
                      (.. tool ": param " name ": :enum only makes sense for :string") p)
      (assert-compile (not= (. p "doc") nil)
                      (.. tool ": param " name " has no :doc (the planner reads it)") p))))

(fn destructured-keys [run]
  "The keys of the table the run function destructures as its first arg, e.g. `{: repo : ticket}`."
  (let [args (and (list? run) (. run 2))
        first (and (sequence? args) (. args 1))]
    (when (table? first)
      (icollect [k _ (pairs first)] (when (and (= (type k) :string) (not= k "&as")) k)))))

(fn deftool [name spec]
  (assert-compile (sym? name) "deftool: the first argument is the tool name (a symbol)" name)
  (assert-compile (table? spec) "deftool: the second argument is the table with the fields" spec)
  (let [tool (tostring name)
        file (let [src (ast-source name)]
               (and src src.filename (src.filename:match "([^/]+)%.fnl$")))
        out {"name" tool}]
    (when file
      (assert-compile (= file tool)
                      (.. "the tool is called " tool " but the file is " file ".fnl: they have to match") name))
    (each [k v (pairs spec)]
      (assert-compile (member? tool-keys k)
                      (.. tool ": unknown field :" (tostring k) (suggestion (tostring k))) spec)
      (tset out k v))
    (each [_ k (ipairs ["short" "description" "run"])]
      (assert-compile (not= (. spec k) nil) (.. tool ": missing :" k) spec))
    (let [desc (. spec "description")]
      (when (= (type desc) :string)
        (assert-compile (desc:match "%S") (.. tool ": the :description is empty") spec)
        ;; the planner also picks the tool for the step it is described as useful for
        (assert-compile (not (: (desc:lower) :find "useful for" 1 true))
                        (.. tool ": do not say \"useful for X\" in the :description: the planner picks the tool for X") spec)))
    (let [short (. spec "short")]
      (when (= (type short) :string)
        (assert-compile (<= (length short) 60) (.. tool ": the :short is longer than 60 characters") spec)))
    (let [input (. spec "input")]
      (assert-compile (or (= input nil) (= input true) (= input "text"))
                      (.. tool ": :input is `true` or `:text`") spec)
      (each [_ k (ipairs ["accepts" "tee"])]
        (assert-compile (or (= (. spec k) nil) (not= input nil))
                        (.. tool ": :" k " only makes sense with :input") spec)))
    (assert-compile (= (not= (. spec "command") nil) (not= (. spec "parse") nil))
                    (.. tool ": :command and :parse go together") spec)
    ;; :accepts and :returns, as lists of names; the tools that are accepted have to exist
    (each [_ k (ipairs ["accepts" "returns"])]
      (let [xs (. spec k)]
        (assert-compile (or (= xs nil) (sequence? xs)) (.. tool ": :" k " is a list of names") spec)
        (when (and xs (= k "accepts"))
          (each [_ n (ipairs xs)]
            (let [f (io.open (.. "tools/" (tostring n) ".fnl"))]
              (assert-compile (or f (not (io.open "tools"))) ; no tools/ dir here: nothing to compare against
                              (.. tool ": :accepts " (tostring n) ", but there is no tools/" (tostring n) ".fnl") spec)
              (when f (f:close)))))))
    ;; params: :worktree adds the ones for worktrees, the rest are checked
    (let [declared (let [ps (or (. spec "params") (sequence))]
                     (assert-compile (sequence? ps) (.. tool ": :params is a list") spec)
                     ps)
          ps (icollect [_ p (ipairs declared)] p)
          names (icollect [_ p (ipairs ps)] (param-name p tool))]
      (when (. spec "worktree")
        (var at 1)
        (each [_ [n ctor] (ipairs [["repo" `((. (require :src.params) :repo-required))]
                                   ["ticket" `((. (require :src.params) :ticket))]])]
          (when (not (member? names n))
            (table.insert ps at ctor)
            (table.insert names at n)
            (set at (+ at 1)))))
      (set out.worktree nil)
      (each [_ p (ipairs ps)] (check-param p tool))
      (each [i n (ipairs names)]
        (assert-compile (not (member? [(table.unpack names (+ i 1))] n))
                        (.. tool ": param " n " is declared twice") spec))
      ;; if every param is known, what run destructures has to be among them
      (when (= (length names) (length ps))
        (each [_ k (ipairs (or (destructured-keys (. spec "run")) []))]
          (assert-compile (member? names k)
                          (.. tool ": run uses " k ", which is not in :params (" (table.concat names ", ") ")")
                          (. spec "run"))))
      (tset out "params" (sequence (table.unpack ps))))
    out))

{: each-repo : each-local-repo : deftool}
