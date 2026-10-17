;; The guide :dev starts with: how to build a ñoqui tool. The format and the
;; lessons are written here (next to the code that defines them); the current
;; tools and the examples are read from tools/ every time.
(local tools (require :src.tools))
(local {: read} (require :src.files))

(local format "
# What a ñoqui tool looks like

A file `tools/<name>.fnl` (Fennel) with a single `deftool` form. It reloads by itself
(hot reload) when it changes:

    (import-macros {: deftool} :src.macros)
    (deftool my-tool          ; the name: same as the file name, in kebab-case
      {:short \"...\" :description \"...\" :params [...] :run (fn [{: repo}] ...)})

`deftool` checks the table when the file compiles, and the REPL shows the error (the previous
version keeps being used): unknown or misspelled fields, missing :short/:description/:run, a name that
differs from the file, invalid param types, params without :doc, duplicated params, what `:run`
destructures that is not in :params, `:accepts` of tools that do not exist, and \"useful for\"
in the description. `:worktree true` adds the `:repo` and `:ticket` params of the tools that work on a
worktree (the :wt mode fills them in). Fields of the table:

- `:short` (string): one-line description for `:tools`.
- `:description` (string): what it does. A local model (qwen3:8b) reads it to
  decide when to use it, so it has to be concrete. Do not say \"useful for X\"
  if X is another step: the planner picks it for that step (a step with code-outline and
  \"write the PR description\").
- `:params`: list of `{:name :repo :type :string :required true :enum [...] :doc \"...\"}`.
  Types: `:string` (with optional `:enum`), `:integer`, `:date` (accepts YYYY-MM-DD or
  relative 1w/10d/1m; the tool always receives YYYY-MM-DD). An optional param
  with a value outside the enum is discarded (the default is used).
- `:run (fn [args inputs text prompt] ...)`: ALWAYS returns a string.
  - `args`: the already validated params (`(fn [{: repo : since}] ...)`).
  - `inputs`: list of strings, the output of the steps it depends on.
  - `text`: the literal text of the step (only if the tool has `:text true`).
  - `prompt`: the user's full request.
  - To fail: `(assert cond \"message\")` or `(error \"message\" 0)`; the panel shows it.
- `:show (fn [out] [lines])` (optional): how the result looks in the panel. The
  first line is the summary. Without :show a generic view of the JSON is shown.

Behavior fields (all optional):
- `:input true`: processes the output (JSON) of the previous step; only used in pipes.
  `:input :text`: same, but accepts any text (e.g. append-file).
- `:accepts [:other-tool]`: only after those tools.
- `:returns [:repo :number :title]`: returns a JSON list whose elements carry
  those fields. A tool whose required params are all there can be
  run once per element (e.g. open-prs -> pr-ci).
- `:text true`: receives the literal text of the step in `text`, without the planner copying it.
- `:tee true`: transparent; passes its input through (e.g. debug).

Available modules (`(local x (require :src.x))`):
- `src.sh`: `(sh.run cmd)` -> (values output exit-code), stderr included;
  `(sh.quote s)` to pass arguments to the shell.
- `config`: `user` (GitHub), `repo-names` [:backend :frontend :ml :docs],
  `repos` (name -> GitHub owner/repo), `local-repos` (name -> local path),
  `jira-project` (key of the tickets, e.g. \"ABC\").
- `src.params`: the params tools repeat, one fresh table each: `(params.repo \"If omitted, ...\")`
  (optional), `(params.repo-required)`, `(params.ticket)` (required, branch ticket),
  `(params.pr-number)` + `(params.pr-ticket)` (PR by number or by ticket), `(params.jira-key)`.
  Use them in `:params [...]` instead of writing the tables by hand.
- `src.worktrees`: `(git dir args)` (fails if the exit code is not 0),
  `(find repo ticket)` -> {: path : branch} of the ticket's worktree, and
  `(pr-ref repo number ticket)`: what `gh pr view` takes to name a PR (its number, or the
  worktree's branch).
- `src.gh`: `(run args)` runs `gh <args>` (fails with its output); `(clean-body text)` strips a
  wrapping code fence; `(edit-body repo ticket make-body labels)` edits a PR's description with backup
  and NOQUI_DRY_RUN (see pr-edit-description).
- `src.jira`: `(request {: path : method : body : query})` -> decoded JSON (fails if status >= 300),
  `(normalize-key ticket)`, `(browse-url key)`. Credentials come from JIRA_EMAIL / JIRA_TOKEN.
- `src.files`: `(check-path path)`, for tools that touch files: only
  relative paths inside the project.
- `src.ollama`: `(chat messages schema)` -> JSON from the local model, forced to `schema`.
  Ask for numbers as text and convert them in code (with :number the 8b read
  \"$0.2389\" as 2389).
- `src.utils`: list utilities: `(find xs pred)`, `(find-index xs pred)`,
  `(any? xs pred)`, `(includes? xs v)`, `(some xs f)`; strings: `(trim s)`, `(lines s)`.
- `dkjson` (`(local json (require :dkjson))`): `json.encode` / `json.decode`.

Macros (`(import-macros {: deftool : each-repo : each-local-repo} :src.macros)`):
- `(deftool name {...})`: defines the tool (see above).
- `(each-repo [name repo] body)`: iterates over `repo` if given, or over all of
  `config.repo-names`, with `name` bound to the repo of each iteration. The body
  returns the list of elements for that repo (or nil); they are gathered into a single list
  and every element without `:repo` gets `name`. Use it instead of writing the
  per-repo loop by hand (see open-prs).
- `(each-local-repo [name dir repo] body)`: same, for tools that use the local
  checkout: `dir` is bound to the repo's local path. If a repo without a
  local path was requested it fails; when iterating over all, it skips those without one (see list-worktrees).

Fennel lessons (mistakes that already happened):
- `quote` and `values` are special forms: they cannot be used as local names.
- `gsub` returns 2 values: `(pick-values 1 (s:gsub ...))` if it goes into a table.insert or a `..`.
- Calling a method on an expression: `(: (s:lower) :find \"x\")`, not `(s:lower):find`.
- Lists for the model and for chaining go as JSON (`json.encode`).
- GitHub: `gh` with `config.user`. Tools that only read are the safest;
  if one writes something, it should validate its input and not overwrite things without a backup.
")

(local examples [:open-prs :count :append-file])

(fn source [name]
  (read (.. "tools/" name ".fnl")))

(fn build []
  (let [available (tools.load-all "tools")]
    (.. format
        "\n# Tools that already exist\n\n"
        (table.concat (icollect [_ t (ipairs available)] (tools.describe t)) "\n")
        "\n\n# Complete examples\n"
        (table.concat (icollect [_ name (ipairs examples)]
                        (case (source name)
                          code (.. "\n## tools/" name ".fnl\n```fennel\n" code "```\n")))
                      ""))))

{: build}
