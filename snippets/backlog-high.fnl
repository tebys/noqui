(import-macros {: defsnippet} :src.macros)

;; Open bugs of the current Policies sprint, oldest first, keeping only High and Highest.
(defsnippet backlog-high
  {:description "Backlog tickets sorted by created date (oldest first), high priority only"
   :args []
   :steps [[:sprint-backlog {}]
           [:sort-by-created {}]
           [:filter-high-priority {}]]})
