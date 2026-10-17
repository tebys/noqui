(import-macros {: deftool} :src.macros)
(local sh (require :src.sh))
(local worktrees (require :src.worktrees))

;; pattern for a code fence (with optional language), built without writing the three backticks
(local fence (.. (string.rep "%`" 3) "%w*"))

(fn clean [text]
  (let [no-fences (pick-values 1 (text:gsub fence ""))]
    (pick-values 1 (: (pick-values 1 (no-fences:gsub "^%s+" "")) :gsub "%s+$" ""))))

(deftool commit-staged
 {:short "commits a ticket's staged changes"
 :description "Commits the staged changes of a ticket's worktree, using as message the text received from the previous step (for example the commit message generated from staged-changes). Modifies the local repo. Does not push."
 :input :text
 :worktree true
 :run (fn [{: repo : ticket} inputs]
        (let [wt (worktrees.find repo ticket)
              message (clean (table.concat inputs "\n"))
              _ (assert (not= message "") "the commit message is empty")
              (_ staged) (sh.run (.. "git -C " (sh.quote wt.path) " diff --cached --quiet"))
              _ (assert (= staged 1) (.. "no staged changes in " wt.path))
              tmp (os.tmpname)]
          (with-open [f (io.open tmp :w)]
            (f:write message "\n"))
          (let [(out code) (sh.run (.. "git -C " (sh.quote wt.path)
                                       " commit -F " (sh.quote tmp)))]
            (os.remove tmp)
            (assert (= code 0) out)
            (.. wt.branch ": " (or (message:match "[^\n]+") message)))))})
