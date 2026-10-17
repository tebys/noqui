(local sh (require :src.sh))
(local config (require :config))
(local utils (require :src.utils))

(fn git [dir args]
  (let [(out code) (sh.run (.. "git -C " (sh.quote dir) " " args))]
    (assert (= code 0) out)
    out))

(fn find [repo ticket]
  "The local worktree of `repo` whose branch or folder contains the ticket (case
insensitive): {: path : branch}."
  (let [main (. config.local-repos repo)
        t (ticket:lower)
        worktrees []]
    (var current nil)
    (each [line (: (git main "worktree list --porcelain") :gmatch "[^\n]+")]
      (case (line:match "^worktree (.+)")
        path (do (set current {: path}) (table.insert worktrees current)))
      (case (line:match "^branch refs/heads/(.+)")
        branch (set current.branch branch)))
    (or (utils.some worktrees (fn [w]
          (when (or (and w.branch (: (w.branch:lower) :find t 1 true))
                    (: (: (w.path:match "[^/]+$") :lower) :find t 1 true))
            w)))
        (error (.. "could not find a worktree of " repo " for " ticket ". Available: "
                   (table.concat (icollect [_ w (ipairs worktrees)] (or w.branch w.path)) ", "))
               0))))

(fn pr-ref [repo number ticket]
  "What `gh pr view` takes to name a PR: its number, or the branch of the ticket's worktree."
  (if number
      (tostring number)
      (do (assert ticket "missing the PR number or the ticket")
          (. (find repo ticket) :branch))))

{: git : find : pr-ref}
