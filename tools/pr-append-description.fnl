(import-macros {: deftool} :src.macros)
;; Appends to the description of a ticket branch's PR, under the heading "## Testing
;; Instructions", the text it receives (e.g. what Claude wrote). If the PR already has that
;; section, it replaces it instead of duplicating it; the rest of the description is untouched.
;; First it saves the previous one in /tmp, because gh pr edit overwrites the whole body.
;; With NOQUI_DRY_RUN=1 it does everything except the edit.
(local gh (require :src.gh))

(local utils (require :src.utils))

(fn splice [body section]
  "Puts `section` in `body`: replaces the \"## Testing Instructions\" section if present
(up to the next \"## \"), or appends it at the end."
  (let [body (pick-values 1 (body:gsub "\r\n" "\n"))
        out []]
    (var skipping false)
    (var found false)
    (each [line (: (.. body "\n") :gmatch "([^\n]*)\n")]
      (if (line:match "^##%s+Testing Instructions%s*$")
          (do (set found true)
              (set skipping true)
              (table.insert out section))
          (and skipping (line:match "^##%s"))
          (do (set skipping false)
              (table.insert out "")
              (table.insert out line))
          (not skipping)
          (table.insert out line)))
    (.. (utils.trim (if found
                  (table.concat out "\n")
                  (.. body "\n\n" section)))
        "\n")))

(deftool pr-append-description
 {:short "appends Testing Instructions to the PR description"
 :description "Appends to the end of the description of a ticket branch's PR, under the heading \"## Testing Instructions\", the text received from the previous step (if such a section already exists, it replaces it). Modifies the PR on GitHub."
 :input :text
 :worktree true
 :run (fn [{: repo : ticket} inputs]
        (let [text (gh.clean-body (table.concat inputs "\n"))]
          ;; never write nothing or a previous step's error into a PR
          (assert (and (text:match "%S") (not (text:match "^ERROR:")))
                  "the incoming text is empty or is an error: not editing the PR")
          ;; if Claude repeated the heading, it is not duplicated
          (let [text (pick-values 1 (text:gsub "^##%s+Testing Instructions%s*\n+" ""))]
            (gh.edit-body repo ticket
                          (fn [pr] (splice (or pr.body "") (.. "## Testing Instructions\n\n" (utils.trim text))))
                          {:done "Testing Instructions added" :previous "previous description"}))))
 :show (fn [out]
         (icollect [l (out:gmatch "[^\n]+")] l))})
