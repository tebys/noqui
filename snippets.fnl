;; Fixed plans: they run as-is, without going through the planner.
;; Usage in the REPL: :name arg1 arg2 ...   (:snippets lists them)
;; Each step depends on the previous one. A step is [:tool {args}] or [:claude "task"],
;; and "$arg" is replaced by the argument with that name.

 :ci {:description "CI status of the PR of $ticket in $repo"
      :args [:ticket :repo]
      :steps [[:pr-ci {:repo "$repo" :ticket "$ticket"}]]} ;; commit message for what is staged: Claude writes it in English, following the style of the
 ;; latest commits that staged-changes brings. It only shows it: it does not commit.
 :commit {:description "Write the commit message for the staged changes of $ticket in $repo"
          :args [:ticket :repo]
          :steps [[:staged-changes {:repo "$repo" :ticket "$ticket"}]
                  [:claude "Write the commit message in English for the staged changes. Follow the style of the recent commits shown (ticket prefix, casing, length). Short subject line in the imperative mood; add a body only if the change needs explaining. Reply with the message only, no preamble, quotes or markdown."]]}
 ;; same as commit, but also commits with that message (no push)
 :commit! {:description "Commit the staged changes of $ticket in $repo"
           :args [:ticket :repo]
           :steps [[:staged-changes {:repo "$repo" :ticket "$ticket"}]
                   [:claude "Write the commit message in English for the staged changes. Follow the style of the recent commits shown (ticket prefix, casing, length). Short subject line in the imperative mood; add a body only if the change needs explaining. Reply with the message only, no preamble, quotes or markdown: it is used as-is for the commit."]
                   [:commit-staged {:repo "$repo" :ticket "$ticket"}]]}
