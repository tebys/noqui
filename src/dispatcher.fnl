(local tools (require :src.tools))
(local planner (require :src.planner))
(local acp (require :src.acp))
(local json (require :dkjson))
(local ui (require :src.ui))
(local config (require :config))
(local context (require :src.context))
(local utils (require :src.utils))

(fn no-tool? [name]
  (or (= name nil) (= name "") (= name "null")))

(fn show-args [args]
  (let [keys (icollect [k (pairs args)] k)]
    (table.sort keys)
    (table.concat (icollect [_ k (ipairs keys)] (.. k "=" (tostring (. args k)))) " ")))

;; In worktree mode, repo and ticket for tools that work on a worktree are the
;; mode's: whatever the planner invents is overwritten.
(fn pin-context [tool args mapped]
  (let [ctx (when (not mapped) (context.defaults-for tool))]
    (if ctx
        (let [out (collect [k v (pairs args)] k v)]
          (each [k v (pairs ctx)] (tset out k v))
          ;; the PR is the ticket's: a number coming from the planner is made up
          (tset out :number nil)
          out)
        args)))

(fn dedupe [steps]
  "Merges steps with the same tool, args and dependencies (the planner sometimes
repeats the same step, and in worktree mode they end up identical once repo and ticket are pinned)."
  (let [out []
        kept {}     ; key -> step that was kept
        redirect {}] ; discarded id -> id of the step that was kept
    (each [_ s (ipairs steps)]
      (set s.deps (icollect [_ d (ipairs s.deps)] (or (. redirect d) d)))
      (let [key (when (and s.tool (not s.map))
                  (.. s.tool.name " " (show-args s.args) " " (table.concat s.deps ",")))
            prev (when key (. kept key))]
        (if prev
            (tset redirect s.id prev.id)
            (do (when key (tset kept key s))
                (table.insert out s)))))
    out))

(fn check [tasks by-name]
  "Validates the model's plan. Raises an error if it uses a tool that does not exist,
args are missing, or it depends on a subtask that did not run before."
  (let [seen {}] ; id -> tool of that subtask (false if it goes to Claude)
    (icollect [_ t (ipairs tasks)]
      (let [deps (or t.depends_on [])]
        (each [_ d (ipairs deps)]
          (when (= (. seen d) nil)
            (error (.. "subtask " t.id " depends on " d ", which did not run before") 0)))
        (if (no-tool? t.tool)
            (do (tset seen t.id false)
                {:id t.id :task t.task :deps deps})
            (let [tool (or (. by-name t.tool)
                           (error (.. "unknown tool: " t.tool) 0))
                  ;; if it depends on a tool that returns elements with the params
                  ;; this one needs (e.g. open-prs -> pr-ci), it runs once per element
                   mapped (utils.some deps (fn [d] (tools.map-params tool (. seen d))))]
              ;; a :tee tool (e.g. debug) is transparent: whatever depends on it
              ;; sees the previous tool (for :accepts and the map)
              (tset seen t.id (if (and tool.tee (. deps 1)) (. seen (. deps 1)) tool))
              (when (and tool.input (= (length deps) 0))
                (error (.. tool.name " needs the result of another subtask") 0))
              (each [_ d (ipairs deps)]
                (when (and tool.input (not (tools.accepts? tool (. seen d))))
                  (error (.. tool.name " only accepts the output of " (table.concat tool.accepts ", ")) 0)))
              {:id t.id :task t.task :deps deps :tool tool :map mapped
               :args (tools.validate tool (pin-context tool (or t.args {}) mapped) mapped)}))))))

(fn run-map [s inputs]
  "Runs the tool once per element of the inputs, taking the s.map params from
each one. Returns a JSON list of {params... :title :result}."
  (let [out []]
    (each [_ input (ipairs inputs)]
      (each [_ item (ipairs (or (json.decode input) []))]
        (let [args (collect [k v (pairs s.args)] k v)
              entry {:title item.title}]
          (each [_ p (ipairs s.map)]
            (tset args p (. item p))
            (tset entry p (. item p)))
          (let [(ok result) (pcall #(s.tool.run (tools.validate s.tool args)))]
            (set entry.result (if ok (or (json.decode result) result) (.. "ERROR: " (tostring result))))
            (table.insert out entry)))))
    (json.encode out)))

(fn merge-claude [steps]
  "If a toolless subtask depends on another toolless one, merges them into
a single Claude call (one call fewer)."
  (let [out []
        by-id {}  ; original id -> step it ended up in (itself or the merged one)
        used {}]  ; steps someone already depends on
    (each [_ s (ipairs steps)]
      ;; deps may point to steps that were already merged: they are redirected
      (set s.deps (let [seen {}]
                    (icollect [_ d (ipairs s.deps)]
                      (let [id (. by-id d :id)]
                        (when (not (. seen id))
                          (tset seen id true)
                          id)))))
      ;; only merges with a step nobody else depends on, because it moves to the end
      (let [prev (when (not s.tool)
                   (utils.some s.deps (fn [d]
                     (let [p (. by-id d)]
                       (if (and (not p.tool) (not (. used p))) p nil)))))]
        (if prev
            (do
              (set prev.task (.. prev.task "; " s.task))
              (each [_ d (ipairs s.deps)]
                (when (not= d prev.id)
                  (table.insert prev.deps d)
                  (tset used (. by-id d) true)))
              ;; moved to the end so it runs after everything it needs
              (each [i o (ipairs out)]
                (when (= o prev)
                  (table.remove out i)))
              (table.insert out prev)
              (tset by-id s.id prev))
            (do
              (each [_ d (ipairs s.deps)]
                (tset used (. by-id d) true))
              (table.insert out s)
              (tset by-id s.id s)))))
    out))

(fn source [s]
  "Where the data for subtask `s` comes from, so Claude knows what it covers."
  (if s.tool
      (.. "the tool:\n" (tools.describe s.tool)
          "\nrun with: " (show-args s.args) " (anything not specified uses its default value)"
          (if s.map (.. "\nonce per element of the input (" (table.concat s.map ", ") " come from each element)") ""))
      (.. "the subtask: " s.task)))

(fn feeds-tool? [step steps]
  "If any tool that processes input depends on `step`, its output must be JSON."
  (utils.any? steps (fn [s]
    (and s.tool s.tool.input
         (utils.any? s.deps (fn [d] (= d step.id)))
         (if s.tool.tee
             (feeds-tool? s steps)
             (not= s.tool.input :text))))))

(fn as-json [answer]
  "Strips the ```json ... ``` if Claude added it and validates that it is JSON."
  (let [body (or (answer:match "```%w*%s*(.-)%s*```") answer)]
    (if (json.decode body)
        body
        (error (.. "Claude did not return valid JSON: " answer) 0))))

(fn claude [prompt step results by-id json? on-chunk]
  "Returns (values answer cost)."
  (let [data (icollect [_ d (ipairs step.deps)]
               (.. "Result of " (source (. by-id d)) "\n" (tostring (. results d))))
        text (.. "Original user request: " prompt
                 "\n\nYour task: " step.task
                 (if (next data)
                     (.. "\n\nSolve it using only this data, without using tools."
                         (if json?
                             " Your answer is processed by a program: reply ONLY with valid JSON (a list with the relevant elements, in the same format as the data), with no explanations or markdown."
                             " Reply briefly.")
                         "\n\n" (table.concat data "\n\n"))
                     "\n\nYou have no tools available (you cannot run commands or search for anything): answer with what you know, briefly."))
        (answer cost) (acp.prompt text on-chunk)]
    (values (if json? (as-json answer) answer) cost)))

(fn preview [out]
  "Lines to display a step's result when the tool does not define :show."
  (let [data (json.decode out)]
    (case (type data)
      :table (if (= (next data) nil)
                 ["(empty)"]
                 (= (. data 1) nil) ; object
                 (icollect [k v (pairs data)]
                   (when (not= (type v) :table) (.. k ": " (tostring v))))
                 (icollect [_ item (ipairs data)]
                   (if (and (= (type item) :table) item.title)
                       (.. (if item.number (.. "#" item.number "  ") "") item.title)
                       (json.encode item))))
      _ (icollect [line (out:gmatch "[^\n]+")] line))))

(fn show-one [tool out]
  (if tool.show
      (let [(ok lines) (pcall tool.show out)]
        (if ok lines (preview out)))
      (preview out)))

(fn show [s out]
  (if s.map
      ;; one line per element: whose it is + the first line of what the tool would show
      (icollect [_ entry (ipairs (json.decode out))]
        (let [label (table.concat (icollect [_ p (ipairs s.map)]
                                    (if (= p :number) (.. "#" (. entry p)) (tostring (. entry p))))
                                  " ")
              result (if (= (type entry.result) :table) (json.encode entry.result) (tostring entry.result))
              first (or (. (show-one s.tool result) 1) "")]
          (string.format "%-16s %s%s" label first (if entry.title (.. "  — " entry.title) ""))))
      (show-one s.tool out)))

(fn run [prompt ?snippet]
  "Solves `prompt`. With ?snippet ({:name :tasks :header}) there is no planner: its
steps run as-is, and if they do not validate it is an error (not sent to Claude)."
  (ui.begin)
  (let [t0 (ui.now)
        available (tools.load-all "tools")
        by-name (collect [_ t (ipairs available)] t.name t)
        kind (if ?snippet "snippet" "plan")
        _ (do (ui.header "ñoqui" (if ?snippet ?snippet.header prompt)
                         (.. config.model " · " (or config.claude-model "claude")))
              (ui.guide)
              (ui.start kind (if ?snippet ?snippet.name "splitting the request…")))
        tp (ui.now)
        (ok steps) (pcall #(dedupe (check (if ?snippet ?snippet.tasks (planner.plan prompt available)) by-name)))
        count-steps #(.. (length $) (if (= (length $) 1) " step" " steps"))
        steps (if ok
                  (let [merged (merge-claude steps)]
                    (ui.finish :ok kind (if ?snippet (.. ?snippet.name " · " (count-steps merged)) (count-steps merged))
                               (when (not ?snippet) (- (ui.now) tp)))
                    merged)
                  ?snippet
                  (do (ui.finish :fail kind (.. ?snippet.name ": " steps))
                      [])
                  (do (ui.finish :warn "plan" (.. "invalid (" steps ") → everything to Claude") (- (ui.now) tp))
                      [{:id 1 :task prompt :deps []}]))
        by-id (collect [_ s (ipairs steps)] s.id s)
        results {}
        stats {:tools 0 :claude 0 :fail (if (and ?snippet (not ok)) 1 0) :cost 0}]
    (each [i s (ipairs steps)]
      (let [final? (= i (length steps))
            style (if final? :final :data)
            ts (ui.now)]
        (ui.guide)
        (if s.tool
            (let [detail (if s.map
                             (.. "for each element " (show-args s.args))
                             (show-args s.args))
                  inputs (icollect [_ d (ipairs s.deps)] (. results d))
                  _ (ui.start s.tool.name detail)
                  ;; literal text of the step for tools with :text: in a pipe it is
                  ;; the segment exactly as written; otherwise, the whole request
                  text (if (or ?snippet (planner.pipe? prompt)) s.task prompt)
                  (ok out) (if s.map
                               (pcall run-map s inputs)
                               (pcall s.tool.run s.args inputs text prompt))]
              (set stats.tools (+ stats.tools 1))
              (ui.finish (if ok :ok :fail) s.tool.name detail (- (ui.now) ts))
              (if ok
                  (ui.body (show s out) style)
                  (do (set stats.fail (+ stats.fail 1))
                      (ui.body [(tostring out)] :error)))
              (tset results s.id (if ok out (.. "ERROR: " (tostring out)))))
            (let [json? (feeds-tool? s steps)
                  ;; another tool processes the JSON: it is not shown as it arrives
                  (write done) (when (not json?) (ui.stream style))
                  _ (if json? (ui.start "claude" s.task) (ui.finish :claude "claude" s.task))
                  (ok out cost) (pcall claude prompt s results by-id json? write)]
              (set stats.claude (+ stats.claude 1))
              (set stats.cost (+ stats.cost (or cost 0)))
              (when done (done))
              (when json? (ui.finish (if ok :ok :fail) "claude" s.task (- (ui.now) ts)))
              (if ok
                  (when json? (ui.body (preview out) style))
                  (do (set stats.fail (+ stats.fail 1))
                      (ui.body [(tostring out)] :error)
                      (acp.stop))) ; in case it failed midway through a call
              (tset results s.id (if ok out (.. "ERROR: " (tostring out))))))))
    (ui.guide)
    (let [parts []]
      (when (> stats.tools 0)
        (table.insert parts (ui.c :green (.. stats.tools (if (= stats.tools 1) " tool" " tools")))))
      (when (> stats.claude 0)
        (table.insert parts (ui.c :mauve (.. stats.claude " claude" (let [m (acp.model-name)] (if m (.. " (" m ")") ""))))))
      (when (> stats.fail 0)
        (table.insert parts (ui.c :red (.. stats.fail " with errors"))))
      (when (> stats.cost 0)
        (table.insert parts (ui.c :peach (string.format "US$%.2f" stats.cost))))
      (table.insert parts (ui.c :faint (ui.secs (- (ui.now) t0))))
      (ui.footer parts))
    results))

{: run}
