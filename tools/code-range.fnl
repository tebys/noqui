(import-macros {: deftool} :src.macros)
(local config (require :src.config))
(local worktrees (require :src.worktrees))

(fn read-range [file from to]
  "The lines from..to of `file` as \"  12 | code\"."
  (let [out []]
    (var n 0)
    (each [line (io.lines file)]
      (set n (+ n 1))
      (when (and (>= n from) (<= n to))
        (table.insert out (string.format "%5d │ %s" n line))))
    (assert (> (length out) 0) (.. "the file has no lines " from "-" to))
    (table.concat out "\n")))

(deftool code-range
 {:short "lines of a file of a ticket's worktree"
 :description "Returns a range of lines (numbered) of a file in the worktree of a ticket. Read-only."
 :params [{:name :repo :type :string :enum config.repo-names
           :doc "backend, frontend, ml or docs (with ticket)"}
          {:name :ticket :type :string :doc "ticket of the worktree, e.g. SOX-101203"}
          {:name :dir :type :string :doc "directory of the checkout, instead of repo + ticket"}
          {:name :path :type :string :required true :doc "path relative to the repo"}
          {:name :from :type :integer :required true :doc "first line"}
          {:name :to :type :integer :doc "last line (defaults to from)"}
          {:name :context :type :integer :doc "extra lines before and after (defaults to 0)"}]
 :run (fn [{: repo : ticket : dir : path : from : to : context}]
        (assert (not (path:find "%.%.")) "the path must stay inside the repo")
        (let [root (or dir (. (assert (worktrees.find repo ticket) (.. "no worktree for " ticket " in " repo)) :path))
              ctx (or context 0)
              a (math.max 1 (- from ctx))
              b (+ (or to from) ctx)
              file (.. root "/" path)]
          (assert (io.open file) (.. "does not exist: " path))
          (.. path " " a "-" b "\n" (read-range file a b))))})
