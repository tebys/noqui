(local json (require :dkjson))
(local sh (require :src.sh))
(local params (require :src.params))
(import-macros {: each-local-repo : deftool} :src.macros)

(deftool prune-worktrees
 {:short "prunes orphaned worktree records"
 :description "Prunes the git worktrees of each repo: deletes the records of worktrees whose directory no longer exists (git worktree prune). Does not delete worktrees still on disk. Modifies the local .git."
 :params [(params.repo "If omitted, prunes those of all.")]
 :run (fn [{: repo}]
        (json.encode
          (each-local-repo [name dir repo]
            (let [(out code) (sh.run (.. "git -C " (sh.quote dir) " worktree prune -v"))]
              (assert (= code 0) out)
              (icollect [line (out:gmatch "[^\n]+")]
                {:detail line})))))
 :show (fn [out]
         (let [items (json.decode out)]
           (if (= (length items) 0)
               ["nothing to prune"]
               (let [lines [(.. (length items) " worktrees pruned")]]
                 (each [_ item (ipairs items)]
                   (table.insert lines (string.format "%-9s %s" item.repo item.detail)))
                 lines))))})
