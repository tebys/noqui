(local json (require :dkjson))
(local sh (require :src.sh))
(local config (require :config))
(local params (require :src.params))
(import-macros {: each-local-repo : deftool} :src.macros)

;; parses `git worktree list --porcelain`: one block per worktree
(fn parse [out repo]
  (let [wts []]
    (var cur nil)
    (each [line (out:gmatch "[^\n]+")]
      (let [path (line:match "^worktree (.+)$")
            branch (line:match "^branch refs/heads/(.+)$")]
        (if path (do (set cur {: repo : path})
                     (table.insert wts cur))
            (and cur branch) (set cur.branch branch))))
    wts))

;; most recent PR of a branch (any state), or nil if it has none
(fn find-pr [repo branch]
  (let [(out code) (sh.run (.. "gh pr list --state all --limit 1"
                               " --repo " (sh.quote (. config.repos repo))
                               " --head " (sh.quote branch)
                               " --json number,state,mergedAt"))]
    (assert (= code 0) out)
    (. (json.decode out) 1)))

(deftool list-worktrees
 {:short "lists the local worktrees per repo"
 :description "Lists the local git worktrees of each repo, with the path, the branch, the PR number of that branch and whether the PR has already been merged."
 :params [(params.repo "If omitted, lists those of all.")]
 :returns [:repo :path :branch]
 :run (fn [{: repo}]
        (json.encode
          (each-local-repo [name dir repo]
            (let [(out code) (sh.run (.. "git -C " (sh.quote dir)
                                         " worktree list --porcelain"))]
              (assert (= code 0) out)
              (icollect [i wt (ipairs (parse out name))]
                ;; the first one is the main checkout; no branch = detached
                (when (and (> i 1) wt.branch)
                  (let [pr (find-pr name wt.branch)]
                    (set wt.number (and pr pr.number))
                    (set wt.merged (and pr (not= pr.mergedAt nil) (not= pr.mergedAt json.null)))
                    wt)))))))
 :show (fn [out]
         (let [wts (json.decode out)]
           (if (= (length wts) 0)
               ["no worktrees"]
               (icollect [_ wt (ipairs wts)]
                 (string.format "%-9s %-8s %-10s %s"
                                wt.repo
                                (if wt.number (.. "#" wt.number) "no PR")
                                (if wt.merged "merged" "")
                                wt.path)))))})
