(import-macros {: deftool} :src.macros)
(local json (require :dkjson))
(local {: git : find} (require :src.worktrees))
(local {: lines} (require :src.utils))

(local max-diff 20000)

(deftool staged-changes
 {:short "staged changes of a ticket"
 :description "Lists the staged changes (git add) of a ticket's worktree: branch, files with their status (added, modified, deleted), added and deleted lines, the staged diff and the latest commits. Does not write text: it fetches the information so a commit message can be drafted."
 :worktree true
 :run (fn [{: repo : ticket}]
        (let [wt (find repo ticket)
              path wt.path
              stats {}
              _ (each [_ line (ipairs (lines (git path "diff --cached --numstat --no-renames")))]
                  (let [(add del file) (line:match "^(%S+)\t(%S+)\t(.+)$")]
                    (when file
                      (tset stats file {:additions (tonumber add) :deletions (tonumber del)}))))
              files (icollect [_ line (ipairs (lines (git path "diff --cached --name-status --no-renames")))]
                      (let [(status file) (line:match "^(%S+)\t(.+)$")
                            s (or (. stats file) {})]
                        {: file
                         :status (or (. {:A "added" :M "modified" :D "deleted"} status) status)
                         :additions s.additions
                         :deletions s.deletions}))
              diff (git path "diff --cached --no-renames")]
          (json.encode
            {:branch wt.branch
             : files
             :diff (if (> (length diff) max-diff)
                       (.. (diff:sub 1 max-diff) "\n... [diff truncated]")
                       diff)
             :recent_commits (lines (git path "log -n 5 --format=%s"))})))
 :show (fn [out]
         (let [d (json.decode out)]
           (if (= (length d.files) 0)
               [(.. d.branch ": nothing staged")]
               (let [res [(.. d.branch ": " (length d.files) " files staged")]]
                 (each [_ f (ipairs d.files)]
                   (table.insert res (string.format "%-10s %s  (+%s -%s)" f.status f.file
                                                    (or f.additions "bin") (or f.deletions "bin"))))
                 res))))})
