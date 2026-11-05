(import-macros {: defsnippet} :src.macros)

;; Commit message for what is staged: Claude writes it in English, following the style of the
;; latest commits that staged-changes brings. The base one only shows it; the ! one commits
;; with that message (no push).
(defsnippet commit
  {:description "Write the commit message for the staged changes of $ticket in $repo"
   :args [:ticket :repo]
   :steps [[:staged-changes {:repo "$repo" :ticket "$ticket"}]
           [:claude "Write the commit message in English for the staged changes. Follow the style of the recent commits shown (ticket prefix, casing, length). Short subject line in the imperative mood; add a body only if the change needs explaining. Reply with the message only, no preamble, quotes or markdown."]]
   :bang {:description "Commit the staged changes of $ticket in $repo"
          :steps [[:staged-changes {:repo "$repo" :ticket "$ticket"}]
                  [:claude "Write the commit message in English for the staged changes. Follow the style of the recent commits shown (ticket prefix, casing, length). Short subject line in the imperative mood; add a body only if the change needs explaining. Reply with the message only, no preamble, quotes or markdown: it is used as-is for the commit."]
                  [:commit-staged {:repo "$repo" :ticket "$ticket"}]]}})
