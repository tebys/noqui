(import-macros {: defsnippet} :src.macros)

;; Claude's task (the "## Testing Instructions" title is added by pr-append-description,
;; so it is not requested)
(local testing-prompt "Write the \"Testing Instructions\" section for this PR, using the ticket, the PR description, the diff and the deploy instance. Output markdown only, no preamble and no section title, with exactly: (1) one line \"Deploy instance: <url>\" (add \"(expired)\" if expired is true); (2) a table | Flag | Value | Notes | with the feature flags that appear in the diff, the PR description or the ticket, or the single line \"No flags needed.\" if there are none; (3) a numbered list of steps to test, in simple concise English, one action per step with the expected result. Do not invent flags, URLs or behaviors that are not in the data.")

;; PR Testing Instructions: gathers ticket, description, diff and deploy instance (independent
;; steps: step 5 depends on the four) and Claude writes it. The base one only shows it; the !
;; one also appends them to the end of the PR description under "## Testing Instructions"
;; (the previous description is kept in /tmp).
(defsnippet testing
  {:description "Write the Testing Instructions of the PR of $ticket in $repo"
   :args [:ticket :repo]
   :steps [[:jira-ticket {:ticket "$ticket"} []]
           [:pr-description {:repo "$repo" :ticket "$ticket"} []]
           [:branch-diff {:repo "$repo" :ticket "$ticket"} []]
           [:pr-deploy-instance {:repo "$repo" :ticket "$ticket"} []]
           [:claude testing-prompt [1 2 3 4]]]
   :bang {:description "Write the Testing Instructions of the PR of $ticket in $repo and append them to its description"
          :steps [[:jira-ticket {:ticket "$ticket"} []]
                  [:pr-description {:repo "$repo" :ticket "$ticket"} []]
                  [:branch-diff {:repo "$repo" :ticket "$ticket"} []]
                  [:pr-deploy-instance {:repo "$repo" :ticket "$ticket"} []]
                  [:claude testing-prompt [1 2 3 4]]
                  [:pr-append-description {:repo "$repo" :ticket "$ticket"}]]}})
