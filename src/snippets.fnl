;; Snippets: fixed plans, no planner. One file per snippet in snippets/ (the `!` variant
;; lives in the same file), defined with `defsnippet` (src/macros.fnl) and hot-reloaded like the
;; tools (src/hotload.fnl). A file returns {name snippet ...}, each snippet being:
;;   {:description "text with $arg" :args [:arg ...]
;;    :steps [[:tool {:param "$arg"}] [:other] [:claude "task"]]}
;; Each step depends on the previous one, as in a pipe, unless it carries a third
;; element with the steps it depends on: [:claude "task" [1 2 3]] ([] = none).
(local hotload (require :src.hotload))
(local context (require :src.context))

(local dir "snippets")

(local loader (hotload.make (fn [file] (let [names (icollect [n (pairs file)] n)]
                                         (table.sort names)
                                         names))))

(fn load-all []
  "All the snippets (name -> snippet), reloading the files that changed."
  (let [all {}]
    (each [_ file (ipairs (loader dir))]
      (each [name snippet (pairs file)]
        (tset all name snippet)))
    all))

(fn substitute [x vals]
  "Replaces $arg with its value in all strings of x."
  (case (type x)
    :string (pick-values 1 (x:gsub "%$([%w_%-]+)" #(or (. vals $1) (.. "$" $1))))
    :table (collect [k v (pairs x)] k (substitute v vals))
    _ x))

(fn usage [name snippet]
  (.. ":" name (table.concat (icollect [_ a (ipairs (or snippet.args []))] (.. " <" a ">")))))

(fn expand [name words]
  "Builds the plan of snippet `name` with the arguments `words` (in order)."
  (let [snippet (or (. (load-all) name) (error (.. "snippet :" name " does not exist (see :snippets)") 0))
        args (or snippet.args [])]
    ;; in worktree mode, the ticket and repo args can be omitted: they come from the mode
    (let [ctx (or (context.current) {})
          omittable (icollect [_ a (ipairs args)] (when (. ctx a) a))
          omit? (and (not= (length words) (length args))
                     (= (length words) (- (length args) (length omittable))))
          vals (do
                 (when (and (not omit?) (not= (length words) (length args)))
                   (error (.. "usage: " (usage name snippet)
                              (if (next omittable) " (in this mode these can be omitted: " (table.concat omittable ", ") ")" "")) 0))
                 (var i 0)
                 (collect [_ a (ipairs args)]
                   (if (and omit? (. ctx a))
                       (values a (. ctx a))
                       (do (set i (+ i 1)) (values a (. words i))))))
          steps (substitute snippet.steps vals)]
      {:name name
       :header (.. ":" name " " (table.concat (icollect [_ a (ipairs args)] (. vals a)) " "))
       :prompt (substitute (or snippet.description (.. ":" name)) vals)
       :tasks (icollect [i [tool x deps] (ipairs steps)]
                (let [claude? (= tool :claude)]
                  {:id i
                   :task (if claude? x tool)
                   :tool (when (not claude?) tool)
                   :args (if (and (not claude?) (= (type x) :table)) x {})
                   ;; explicit deps: list of step numbers (can be []); otherwise, the previous one
                   :depends_on (or deps (if (> i 1) [(- i 1)] []))}))})))

(fn list []
  "Lines for :snippets."
  (let [all (load-all)
        names (icollect [k (pairs all)] k)]
    (table.sort names)
    (icollect [_ n (ipairs names)]
      (let [s (. all n)]
        (.. (usage n s) (if s.description (.. "  — " s.description) ""))))))

{: expand : list : load-all}
