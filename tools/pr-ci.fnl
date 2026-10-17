(import-macros {: deftool} :src.macros)
(local json (require :dkjson))
(local sh (require :src.sh))
(local config (require :config))
(local params (require :src.params))
(local worktrees (require :src.worktrees))

(fn command [{: repo : number : ticket}]
  "The shell command that queries the CI (used by :run, and by :watch in the background)."
  (let [target (worktrees.pr-ref repo number ticket)]
    (.. "gh pr checks " (sh.quote target)
        " --repo " (sh.quote (. config.repos repo))
        " --json name,state,bucket,link")))

(fn parse [out]
  "The checks (list) from the output of `command`. gh pr checks exits with code != 0 if
there are failing or pending checks, so we validate that it is JSON instead of looking
at the exit code. A PR without CI configured is not an error: it has no checks."
  (if (out:find "no checks reported" 1 true)
      []
      (let [checks (json.decode out)]
        (assert checks out)
        checks)))

(deftool pr-ci
 {:short "CI status of a PR"
 :description "Returns the CI status (checks) of a specific PR (by number, or by ticket: the PR of its worktree's branch): name of each check and whether it passed, failed or is pending."
 :params [(params.repo-required)
          (params.pr-number)
          (params.pr-ticket)]
 :command command
 :parse parse
 :run (fn [args]
        (let [(out) (sh.run (command args))]
          (parse out) ; validates that it is JSON
          (if (out:find "no checks reported" 1 true) "[]" out)))
 :show (fn [out]
         ;; failed first, then pending, then passed and finally skipped
         (let [order {:fail 1 :cancel 2 :pending 3 :pass 4 :skipping 5}
               glyph {:fail "✗" :cancel "✗" :pending "◒" :skipping "–" :pass "✓"}
               checks (json.decode out)
               counts {}]
           (if (= (length checks) 0)
               ["no checks"]
               (do
                 (table.sort checks #(< (or (. order $1.bucket) 9) (or (. order $2.bucket) 9)))
                 (each [_ ch (ipairs checks)]
                   (tset counts ch.bucket (+ 1 (or (. counts ch.bucket) 0))))
                 (doto (icollect [_ ch (ipairs checks)]
                         (.. (or (. glyph ch.bucket) "·") " " ch.name))
                   (table.insert 1 (table.concat (icollect [_ b (ipairs [:pass :fail :pending :skipping :cancel])]
                                                   (when (. counts b) (.. (. counts b) " " b)))
                                                 " · ")))))))})
