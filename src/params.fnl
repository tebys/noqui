;; The params the tools repeat the most, so each tool does not copy them:
;;   :params [(params.repo "If omitted, searches all.") (params.number) (params.pr-ticket)]
;; Each one returns a new table. Like the macros, this file is cached: restart the REPL if it changes.
(local config (require :config))

(local repo-list
  (let [names config.repo-names
        n (length names)]
    (if (<= n 1)
        (or (. names 1) "")
        (.. (table.concat names ", " 1 (- n 1)) " or " (. names n)))))

(fn repo [omitted]
  "Optional `repo`. `omitted` says what happens without it (e.g. \"If omitted, searches all.\")."
  {:name :repo :type :string :enum config.repo-names
   :doc (if omitted (.. repo-list ". " omitted) repo-list)})

(fn repo-required []
  {:name :repo :type :string :enum config.repo-names :required true :doc repo-list})

(fn ticket []
  "Required branch ticket."
  {:name :ticket :type :string :required true
   :doc (.. "branch ticket, e.g. " config.jira-project "-101203")})

(fn pr-number []
  {:name :number :type :integer
   :doc "PR number. Only if the request gives it; otherwise, omit it"})

(fn pr-ticket []
  "Optional ticket, for tools that take the PR by number or by ticket."
  {:name :ticket :type :string
   :doc (.. "branch ticket, e.g. " config.jira-project "-101203; used if there is no number")})

(fn jira-key []
  {:name :ticket :type :string :required true
   :doc (.. "Jira ticket key, e.g. " config.jira-project "-101203")})

{: repo : repo-required : ticket : pr-number : pr-ticket : jira-key}
