(import-macros {: defsnippet} :src.macros)

(defsnippet pr-desc
  {:description "Write the PR description of $ticket in $repo"
   :args [:ticket :repo]
   :steps [[:branch-diff {:repo "$repo" :ticket "$ticket"}]
           [:code-outline]
           ;; PR descriptions are always in English (the task is in English
           ;; so the Spanish of :description does not rub off)
           [:claude "Write the PR description in English."]]
   ;; also writes it to the PR (the previous one is kept in /tmp)
   :bang {:description "Write the PR description of $ticket in $repo and edit it in the PR"
          :steps [[:branch-diff {:repo "$repo" :ticket "$ticket"}]
                  [:code-outline]
                  [:claude "Write the PR description in English, in markdown. Reply with the description only, no preamble or comments: it goes straight into the PR."]
                  [:pr-edit-description {:repo "$repo" :ticket "$ticket"}]]}})
