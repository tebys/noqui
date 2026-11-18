(import-macros {: deftool} :src.macros)
(local sh (require :src.sh))
(local config (require :config))
(local worktrees (require :src.worktrees))

;; the repo's default branch (origin/HEAD), since not every repo uses develop
(fn base-of [path]
  (let [out (sh.run (.. "cd " (sh.quote path)
                        " && git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null"))]
    (or (out:match "^origin/(%S+)") config.base-branch)))

;; url of the branch's open PR, or nil if it has none (a closed or merged one does not count)
(fn open-pr-url [path repo branch]
  (let [(out code) (sh.run (.. "cd " (sh.quote path)
                               " && gh pr view " (sh.quote branch)
                               " --repo " (sh.quote (. config.repos repo))
                               " --json url,state --jq " (sh.quote "(.state) + \" \" + (.url)")))]
    (when (= code 0)
      (out:match "^OPEN (https://%S+)"))))

(deftool push-pr
 {:short "pushes a ticket's branch, opens or updates its PR"
 :description "Pushes the branch of a ticket's worktree to origin (git push -u origin). If the branch already has an open PR, it only pushes the new commits and returns that PR's url; otherwise it creates the PR on GitHub against the repo's default branch (develop, main...) with gh, filling title and body from the commits. Returns the full URL of the PR. Modifies the remote repo and GitHub."
 :worktree true
 :params [{:name :branch :type :string
           :doc "branch name to push. If omitted, the branch of the ticket's worktree."}
          {:name :title :type :string
           :doc "PR title, only used when the PR is created. If omitted, it is filled from the commits."}]
 :run (fn [{: repo : ticket : branch : title}]
        (let [wt (worktrees.find repo ticket)
              _ (assert wt (.. "no worktree for " ticket " in " repo))
              branch (or branch wt.branch)
              base (base-of wt.path)
              (ahead _) (sh.run (.. "cd " (sh.quote wt.path)
                                    " && git rev-list --count origin/" base ".." (sh.quote branch)))
              dirty (sh.run (.. "cd " (sh.quote wt.path) " && git status --porcelain"))
              _ (assert (not= (tonumber (ahead:match "%d+")) 0)
                        (.. "no commits ahead of origin/" base " in " branch
                            (if (dirty:match "%S") ": there are uncommitted changes, commit them first" "")))
              (push-out push-code) (sh.run (.. "cd " (sh.quote wt.path)
                                               " && git push -u origin " (sh.quote branch)))
              _ (assert (= push-code 0) (.. "push failed: " push-out))
              existing (open-pr-url wt.path repo branch)]
          (if existing
              (.. existing " (updated)")
              (let [(out code) (sh.run (.. "cd " (sh.quote wt.path)
                                           " && gh pr create"
                                           " --repo " (sh.quote (. config.repos repo))
                                           " --head " (sh.quote branch)
                                           " --base " (sh.quote base)
                                           (if title
                                               (.. " --title " (sh.quote title) " --body " (sh.quote ""))
                                               " --fill")))
                    url (out:match "https://%S+")]
                (assert (= code 0) (.. "gh pr create failed: " out))
                (assert url (.. "no PR url in gh output: " out))
                url))))
 :show (fn [out]
         (let [url (out:match "^(%S+) %(updated%)$")]
           (if url
               ["PR updated (pushed to the open PR)" url]
               ["PR created" out])))})
