(local json (require :dkjson))
(local sh (require :src.sh))
(local config (require :config))
(local params (require :src.params))
(import-macros {: each-repo : deftool} :src.macros)

(deftool open-prs
 {:short "my open PRs"
 :description "Lists my open PRs, with title, url, creation date, whether it is a draft, review status and number of comments."
 :params [(params.repo "If omitted, searches all.")]
 ;; fields each element carries: another tool that asks for repo and number can be run for each PR
 :returns [:repo :number :title]
 :run (fn [{: repo}]
        (json.encode
          (each-repo [name repo]
            (let [(out code) (sh.run (.. "gh pr list --state open --limit 200"
                                         " --author " (sh.quote config.user)
                                         " --repo " (sh.quote (. config.repos name))
                                         " --json number,title,url,createdAt,isDraft,reviewDecision,comments"
                                         " --jq " (sh.quote "map(.comments |= length)")))]
              (assert (= code 0) out)
              (json.decode out)))))
 :show (fn [out]
         (let [prs (json.decode out)
               review {:APPROVED "approved" :CHANGES_REQUESTED "changes requested"
                       :REVIEW_REQUIRED "awaiting review"}]
           (if (= (length prs) 0)
               ["no open PRs"]
               (icollect [_ pr (ipairs prs)]
                 (string.format "%-9s #%-6s %s  (%s%s)" pr.repo pr.number pr.title
                                (if pr.isDraft "draft" (or (. review pr.reviewDecision) "no review"))
                                (if (> pr.comments 0) (.. ", " pr.comments " comments") ""))))))})
