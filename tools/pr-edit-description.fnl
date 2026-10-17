(import-macros {: deftool} :src.macros)
;; Replaces the description of a ticket branch's PR with the text it receives
;; (e.g. the one Claude wrote). First it saves the previous one in /tmp, because gh pr edit
;; overwrites the whole body. With NOQUI_DRY_RUN=1 it does everything except the edit.
(local gh (require :src.gh))

(deftool pr-edit-description
 {:short "replaces a PR's description"
 :description "Replaces the description of a ticket branch's PR with the text received from the previous step. Modifies the PR on GitHub."
 :input :text
 :worktree true
 :run (fn [{: repo : ticket} inputs]
        (let [body (gh.clean-body (table.concat inputs "\n"))]
          ;; never overwrite a PR with nothing or with a previous step's error
          (assert (and (body:match "%S") (not (body:match "^ERROR:")))
                  "the incoming description is empty or is an error: not editing the PR")
          (gh.edit-body repo ticket (fn [] body)
                        {:done "description updated" :previous "previous"})))
 :show (fn [out]
         (icollect [l (out:gmatch "[^\n]+")] l))})
