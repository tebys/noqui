(import-macros {: deftool} :src.macros)
(local sh (require :src.sh))
(local config (require :config))
(local worktrees (require :src.worktrees))

(deftool push-pr
 {:short "pushes a ticket's branch and opens its PR"
 :description "Pushes the branch of a ticket's worktree to origin (git push -u origin) and creates the PR on GitHub against develop with gh, filling title and body from the commits. Returns the full URL of the PR. Modifies the remote repo and GitHub."
 :worktree true
 :params [{:name :branch :type :string
           :doc "branch name to push. If omitted, the branch of the ticket's worktree."}
          {:name :title :type :string
           :doc "PR title. If omitted, it is filled from the commits."}]
 :run (fn [{: repo : ticket : branch : title}]
        (let [wt (worktrees.find repo ticket)
              _ (assert wt (.. "no worktree for " ticket " in " repo))
              branch (or branch wt.branch)
              (ahead _) (sh.run (.. "cd " (sh.quote wt.path)
                                    " && git rev-list --count origin/develop.." (sh.quote branch)))
              dirty (sh.run (.. "cd " (sh.quote wt.path) " && git status --porcelain"))
              _ (assert (not= (tonumber (ahead:match "%d+")) 0)
                        (.. "no commits ahead of origin/develop in " branch
                            (if (dirty:match "%S") ": there are uncommitted changes, commit them first" "")))
              (push-out push-code) (sh.run (.. "cd " (sh.quote wt.path)
                                               " && git push -u origin " (sh.quote branch)))
              _ (assert (= push-code 0) (.. "push failed: " push-out))
              (out code) (sh.run (.. "cd " (sh.quote wt.path)
                                     " && gh pr create"
                                     " --repo " (sh.quote (. config.repos repo))
                                     " --head " (sh.quote branch)
                                     " --base develop"
                                     (if title
                                         (.. " --title " (sh.quote title) " --body " (sh.quote ""))
                                         " --fill")))
              url (out:match "https://%S+")]
          (assert (= code 0) (.. "gh pr create failed: " out))
          (assert url (.. "no PR url in gh output: " out))
          url))
 :show (fn [out]
         ["PR created" out])})
