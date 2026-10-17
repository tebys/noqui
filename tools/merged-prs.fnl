(local json (require :dkjson))
(local sh (require :src.sh))
(local config (require :config))
(local params (require :src.params))
(import-macros {: each-repo : deftool} :src.macros)

(deftool merged-prs
 {:short "my PRs merged since a date"
 :description "Lists my PRs merged since a date, with title, url, creation date, merge date and number of comments."
 :params [(params.repo "If omitted, searches all.")
          {:name :since :type :date :required true
           :doc "date to search from"}
          {:name :until :type :date
           :doc "date to search up to, inclusive. If omitted, up to today."}]
 ;; fields each element carries: another tool that asks for repo and number can be run for each PR
 :returns [:repo :number :title]
 :run (fn [{: repo : since : until}]
        (json.encode
          (each-repo [name repo]
            (let [(out code) (sh.run (.. "gh pr list --state merged --limit 200"
                                         " --author " (sh.quote config.user)
                                         " --repo " (sh.quote (. config.repos name))
                                         " --search " (sh.quote (if until
                                                                    (.. "merged:" since ".." until)
                                                                    (.. "merged:>=" since)))
                                         " --json number,title,url,createdAt,mergedAt,comments"
                                         " --jq " (sh.quote "map(.comments |= length)")))]
              (assert (= code 0) out)
              (json.decode out)))))
 :show (fn [out]
         (let [prs (json.decode out)]
           (if (= (length prs) 0)
               ["no PRs"]
               (icollect [_ pr (ipairs prs)]
                 (string.format "%-9s #%-6s %s  (%s, %d comments)" pr.repo pr.number pr.title
                                (pr.mergedAt:sub 1 10) pr.comments)))))})
