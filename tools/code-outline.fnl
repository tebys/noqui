(import-macros {: deftool} :src.macros)
;; Structural summary of a diff for writing a PR description without sending
;; the code: per file, which functions/classes/templates were added, changed or
;; deleted (signature + comment above), and the tests added or deleted.
;; The definitions come from ast-grep (tree-sitter); Glimmer <template>s are
;; blanked out beforehand because the TypeScript parser chokes on them.
(local json (require :dkjson))
(local sh (require :src.sh))

(local parseable {:ts true :tsx true :js true :mjs true :cjs true :gts true :gjs true})

(local rule "id: defs
language: TypeScript
rule:
  any:
    - kind: function_declaration
    - kind: method_definition
    - kind: class_declaration
    - kind: interface_declaration
    - kind: type_alias_declaration
    - kind: variable_declarator
      has: { field: value, kind: arrow_function }")

(fn squash [s n]
  (let [s (pick-values 1 (: (s:gsub "%s+" " ") :match "^%s*(.-)%s*$"))]
    (if (> (utf8.len s) n) (.. (s:sub 1 (utf8.offset s n)) "…") s)))

(fn git-show [dir rev path]
  (let [(out code) (sh.run (.. "git -C " (sh.quote dir) " show " (sh.quote (.. rev ":" path))))]
    (when (= code 0) out)))

(fn blank-templates [src]
  "Replaces each <template>...</template> with empty lines (so line numbers do not
shift). Returns the code and the templates as definitions."
  (let [parts []
        templates []]
    (var pos 1)
    (var done? false)
    (while (not done?)
      (let [s (src:find "<template>" pos true)
            (_ e) (when s (src:find "</template>" s true))]
        (if (and s e)
            (let [newlines (select 2 (: (src:sub s e) :gsub "\n" ""))
                  start (+ 1 (select 2 (: (src:sub 1 (- s 1)) :gsub "\n" "")))]
              (table.insert parts (src:sub pos (- s 1)))
              (table.insert parts (string.rep "\n" newlines))
              (table.insert templates {:sig "<template>" :start start :end (+ start newlines)})
              (set pos (+ e 1)))
            (do (table.insert parts (src:sub pos))
                (set done? true)))))
    (values (table.concat parts) templates)))

(fn signature [text]
  "Up to the brace that opens the body, without decorators."
  (let [lines (icollect [l (: (.. text "\n") :gmatch "(.-)\n")] l)]
    (while (and (. lines 1) (: (. lines 1) :match "^%s*@"))
      (table.remove lines 1))
    (let [body (table.concat lines "\n")]
      (squash (or (body:match "^(.-)%s*{%s*\n") (. lines 1) body) 160))))

(fn comment-above [lines start]
  "The comment right above line `start` (skipping decorators)."
  (let [found []]
    (var i (- start 1))
    (while (and (> i 0) (: (. lines i) :match "^%s*@"))
      (set i (- i 1)))
    ;; the whole block (trimmed from the start: the first sentence is the one that matters)
    (while (and (> i 0) (< (length found) 60)
                (let [l (. lines i)]
                  (or (l:match "^%s*//") (l:match "^%s*/?%*") (l:match "^%s*%*/"))))
      (table.insert found 1 (pick-values 1 (: (: (. lines i) :gsub "^%s*/?/?%**/?" "") :gsub "%s*%*/%s*$" "")))
      (set i (- i 1)))
    (when (next found)
      (let [text (squash (table.concat found " ") 240)]
        (when (not= text "") text)))))

(fn definitions [src]
  (let [(code templates) (blank-templates src)
        lines (icollect [l (: (.. src "\n") :gmatch "(.-)\n")] l)
        tmp (os.tmpname)]
    (with-open [f (io.open tmp :w)]
      (f:write code))
    (let [(out) (sh.run (.. "ast-grep scan --inline-rules " (sh.quote rule)
                            " --json=stream --stdin < " (sh.quote tmp)))
          defs templates]
      (os.remove tmp)
      (each [l (out:gmatch "[^\n]+")]
        (case (json.decode l)
          {: text : range} (let [start (+ range.start.line 1)]
                             (table.insert defs {:sig (signature text) :start start
                                                 :end (+ range.end.line 1)
                                                 :comment (comment-above lines start)}))))
      ;; the parent of each definition: the smallest one that contains it
      (each [_ d (ipairs defs)]
        (each [_ p (ipairs defs)]
          (when (and (not= p d) (<= p.start d.start) (>= p.end d.end)
                     (or (not d.parent) (< (- p.end p.start) (- d.parent.end d.parent.start))))
            (set d.parent p))))
      (values defs lines))))

(fn innermost [defs line]
  (accumulate [best nil _ d (ipairs defs)]
    (if (and (<= d.start line) (<= line d.end)
             (or (not best) (< (- d.end d.start) (- best.end best.start))))
        d
        best)))

(fn label [d]
  (let [parent (and d.parent (d.parent.sig:match "class%s+([%w_$]+)"))]
    (.. (if parent (.. parent " › ") "") d.sig)))

(fn changed-lines [hunks side]
  "Line numbers added (side :new) or deleted (side :old) according to the hunks."
  (let [out []]
    (each [_ h (ipairs hunks)]
      (let [start (. h (.. side "_start"))
            n (. h (.. side "_lines"))]
        (for [l start (+ start n -1)]
          (table.insert out l))))
    out))

(fn tests-in [lines numbers]
  (icollect [_ n (ipairs numbers)]
    (let [l (or (. lines n) "")]
      (or (l:match "^%s*test%(%s*['\"`](.-)['\"`]")
          (l:match "^%s*it%(%s*['\"`](.-)['\"`]")))))

(fn outline-file [d f]
  "Outline lines of a file."
  (let [header (.. f.status " " f.path (if f.from (.. " (was " f.from ")") "")
                   " (+" f.added " −" f.deleted ")")
        ext (f.path:match "%.(%w+)$")]
    (if (or f.binary (not (. parseable ext)))
        [header]
        (let [new-src (when (not= f.status "D") (git-show d.worktree "HEAD" f.path))
              old-src (when (not= f.status "A") (git-show d.worktree d.base (or f.from f.path)))
              (new-defs new-lines) (if new-src (definitions new-src) (values [] []))
              (old-defs old-lines) (if old-src (definitions old-src) (values [] []))
              added (changed-lines f.hunks :new)
              removed (changed-lines f.hunks :old)
              out [header]]
          (if (f.path:match "%.test%.")
              ;; in tests what matters is what was tested, not how
              (do (each [_ name (ipairs (tests-in new-lines added))]
                    (table.insert out (.. "  + test: " name)))
                  (each [_ name (ipairs (tests-in old-lines removed))]
                    (table.insert out (.. "  − test: " name))))
              (let [plus {} minus {} order []
                    old-sigs (collect [_ od (ipairs old-defs)] (label od) od)
                    new-sigs (collect [_ nd (ipairs new-defs)] (label nd) nd)]
                (var outside 0)
                (each [_ l (ipairs added)]
                  (case (innermost new-defs l)
                    nd (do (when (not (. plus nd)) (table.insert order nd))
                           (tset plus nd (+ 1 (or (. plus nd) 0))))
                    nil (set outside (+ outside 1))))
                (each [_ l (ipairs removed)]
                  (case (innermost old-defs l)
                    od (let [nd (. new-sigs (label od))]
                         (if nd
                             ;; the same definition is still in the new version: it was modified
                             (do (when (and (not (. plus nd)) (not (. minus nd))) (table.insert order nd))
                                 (tset minus nd (+ 1 (or (. minus nd) 0))))
                             (do (when (not (. minus od)) (table.insert order od))
                                 (tset minus od (+ 1 (or (. minus od) 0))))))
                    nil (set outside (+ outside 1))))
                (each [_ x (ipairs order)]
                  (let [new? (and (. plus x) (not (. old-sigs (label x))) (not= f.status "D"))
                        gone? (and (not (. plus x)) (not (. new-sigs (label x))))]
                    (table.insert out
                                  (.. "  " (if new? "+ " gone? "− "
                                               (.. "~ (+" (or (. plus x) 0) " −" (or (. minus x) 0) ") "))
                                      (label x)))
                    (when (and x.comment (or new? gone?))
                      (table.insert out (.. "      // " x.comment)))))
                (when (> outside 0)
                  (table.insert out (.. "  ~ " outside " lines outside functions (imports, constants…)")))))
          out))))

(deftool code-outline
 {:short "outline of functions/classes changed in a diff"
 ;; careful: if the description says "useful for writing a PR description", the
 ;; planner also picks it for the step that writes it (which belongs to Claude)
 :description "Builds an outline of the changes returned by branch-diff, without the code: which functions, classes and templates were added, changed or deleted, with their signature and comment, and which tests were added. Does not write new text."
 :input true
 ;; only makes sense after branch-diff (the planner does not offer it anywhere else)
 :accepts [:branch-diff]
 :params []
 :run (fn [_ inputs]
        (let [d (json.decode (. inputs 1))
              lines [(.. "branch: " d.branch " (" d.repo ")")
                     "commits:"]]
          (each [_ c (ipairs d.commits)]
            (table.insert lines (.. "- " c)))
          (table.insert lines (.. "files (" (length d.files) "):"))
          (each [_ f (ipairs d.files)]
            (each [_ l (ipairs (outline-file d f))]
              (table.insert lines l)))
          (table.concat lines "\n")))
 :show (fn [out]
         (icollect [l (out:gmatch "[^\n]+")] l))})
