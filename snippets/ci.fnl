(import-macros {: defsnippet} :src.macros)

(defsnippet ci
  {:description "CI status of the PR of $ticket in $repo"
   :args [:ticket :repo]
   :steps [[:pr-ci {:repo "$repo" :ticket "$ticket"}]]})
