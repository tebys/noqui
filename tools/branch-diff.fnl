(local json (require :dkjson))
(local config (require :src.config))
(local sh (require :src.sh))
(local params (require :src.params))
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

;; hunks of the `patch` field that the GitHub API returns for each file
(fn patch-hunks [patch]
  (icollect [o-start o-lines n-start n-lines (: (or patch "") :gmatch "@@ %-(%d+),?(%d*) %+(%d+),?(%d*) @@")]
    {:old_start (tonumber o-start) :old_lines (if (= o-lines "") 1 (tonumber o-lines))
     :new_start (tonumber n-start) :new_lines (if (= n-lines "") 1 (tonumber n-lines))}))

(local statuses {:added "A" :removed "D" :renamed "R" :modified "M" :changed "M" :copied "A"})

(fn pr-diff [repo ref]
  "The PR `ref` (number or branch), read from GitHub: {: base : commits : files}, or
nil if there is none. It is the PR's real diff: it does not depend on a local branch."
  (let [slug (. config.repos repo)
        (out code) (sh.run (.. "gh pr view " (sh.quote ref) " --repo " (sh.quote slug)
                               " --json number,baseRefName,commits"))
        pr (and (= code 0) (json.decode out))]
    (when (and pr pr.number)
      (let [(raw fcode) (sh.run (.. "gh api --paginate repos/" slug "/pulls/" pr.number "/files"
                                    " --jq '.[] | @json'"))
            _ (assert (= fcode 0) (.. "gh api failed: " raw))
            files (icollect [l (raw:gmatch "[^\n]+")]
                    (let [f (json.decode l)]
                      {:path f.filename :status (or (. statuses f.status) "M")
                       :from f.previous_filename :added f.additions :deleted f.deletions
                       :hunks (patch-hunks f.patch) :binary (and (not f.patch) (= f.additions 0) (= f.deletions 0))}))]
        {:base pr.baseRefName :pr pr.number
         :commits (icollect [_ c (ipairs pr.commits)] c.messageHeadline)
         : files}))))

{:name "branch-diff"
 :short "cambios del branch de un ticket contra su base"
 :description "Trae los cambios del branch de un ticket (su worktree local) contra la base de su PR (develop si no tiene PR): commits, archivos cambiados y lineas de cada cambio. Si hay PR lee el diff real desde GitHub, aunque el branch local este desactualizado."
 :params [{:name :repo :type :string :enum config.repo-names :required true
           :doc "backend, frontend, ml o docs"}
          {:name :ticket :type :string
           :doc "ticket del branch, ej: SOX-101203 (necesita el worktree). Si se da number no hace falta."}
          (params.pr-number)]
 :run (fn [{: repo : ticket : number}]
        (if number
            (let [d (assert (pr-diff repo (tostring number)) (.. "PR " number " not found in " repo))]
              (json.encode {: repo :base d.base :pr d.pr :commits d.commits :files d.files}))
            (let [_ (assert ticket "missing the ticket or the PR number")
                  wt (find repo ticket)
                  d (or (pr-diff repo wt.branch)
                        (let [base (pick-values 1 (: (git wt.path "merge-base HEAD origin/develop") :gsub "%s+$" ""))]
                          {: base
                           :commits (icollect [l (: (git wt.path (.. "log --no-merges --format=%s " base "..HEAD")) :gmatch "[^\n]+")] l)
                           :files (parse-diff (git wt.path (.. "diff -U0 --no-color " base "...HEAD")))}))]
              (json.encode {: repo : ticket :worktree wt.path :branch wt.branch
                            :base d.base :pr d.pr :commits d.commits :files d.files}))))
 :show (fn [out]
         (let [d (json.decode out)
               added (accumulate [n 0 _ f (ipairs d.files)] (+ n f.added))
               deleted (accumulate [n 0 _ f (ipairs d.files)] (+ n f.deleted))]
           (doto (icollect [_ f (ipairs d.files)]
                   (string.format "%s %s  (+%d −%d)" f.status f.path f.added f.deleted))
             (table.insert 1 (string.format "%s · %d commits · %d archivos (+%d −%d)"
                                            (or d.branch (.. "PR #" d.pr)) (length d.commits) (length d.files) added deleted)))))}
