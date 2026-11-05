(local json (require :dkjson))
(local config (require :src.config))
(local {: git : find} (require :src.worktrees))

(fn parse-diff [diff]
  "Parsea `git diff -U0`: por archivo, estado, lineas agregadas/borradas y hunks."
  (let [files []]
    (var f nil)
    (each [line (diff:gmatch "[^\n]*")]
      (if (line:match "^diff %-%-git ")
          (do (set f {:path (line:match " b/(.+)$") :status "M" :added 0 :deleted 0 :hunks []})
              (table.insert files f))
          (not f) nil
          (line:match "^new file mode") (set f.status "A")
          (line:match "^deleted file mode") (set f.status "D")
          (line:match "^rename from ") (do (set f.status "R") (set f.from (line:match "^rename from (.+)")))
          (line:match "^Binary files") (set f.binary true)
          (line:match "^@@ ")
          (let [(o-start o-lines n-start n-lines) (line:match "^@@ %-(%d+),?(%d*) %+(%d+),?(%d*) @@")]
            ;; sin ",N" el hunk es de una sola linea
            (table.insert f.hunks {:old_start (tonumber o-start) :old_lines (if (= o-lines "") 1 (tonumber o-lines))
                                   :new_start (tonumber n-start) :new_lines (if (= n-lines "") 1 (tonumber n-lines))}))
          (and (line:match "^%+") (not (line:match "^%+%+%+ "))) (set f.added (+ f.added 1))
          (and (line:match "^%-") (not (line:match "^%-%-%- "))) (set f.deleted (+ f.deleted 1))))
    files))

{:name "branch-diff"
 :short "cambios del branch de un ticket contra develop"
 :description "Trae los cambios del branch de un ticket (su worktree local) contra develop: commits, archivos cambiados y lineas de cada cambio."
 :params [{:name :repo :type :string :enum config.repo-names :required true
           :doc "backend, frontend, ml o docs"}
          {:name :ticket :type :string :required true
           :doc "ticket del branch, ej: SOX-101203"}]
 :run (fn [{: repo : ticket}]
        (let [wt (find repo ticket)
              base (pick-values 1 (: (git wt.path "merge-base HEAD origin/develop") :gsub "%s+$" ""))
              commits (icollect [l (: (git wt.path (.. "log --no-merges --format=%s " base "..HEAD")) :gmatch "[^\n]+")] l)
              files (parse-diff (git wt.path (.. "diff -U0 --no-color " base "...HEAD")))]
          (json.encode {: repo : ticket :worktree wt.path :branch wt.branch : base : commits : files})))
 :show (fn [out]
         (let [d (json.decode out)
               added (accumulate [n 0 _ f (ipairs d.files)] (+ n f.added))
               deleted (accumulate [n 0 _ f (ipairs d.files)] (+ n f.deleted))]
           (doto (icollect [_ f (ipairs d.files)]
                   (string.format "%s %s  (+%d −%d)" f.status f.path f.added f.deleted))
             (table.insert 1 (string.format "%s · %d commits · %d archivos (+%d −%d)"
                                            d.branch (length d.commits) (length d.files) added deleted)))))}
