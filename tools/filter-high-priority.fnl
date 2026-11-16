(import-macros {: deftool} :src.macros)
(local json (require :dkjson))
(local utils (require :src.utils))

(fn wanted? [priority level]
  (let [p (if (= (type priority) :string) (priority:lower) "")]
    (if (= level :high)
        (= p "high")
        (or (= p "high") (= p "highest")))))

(deftool filter-high-priority
 {:short "keeps only the high priority tickets of the sprint backlog"
  :description "Filters the list of tickets returned by sprint-backlog (or sort-by-created) and keeps only those with high priority (High and Highest), preserving their order. With level high, only High. Receives as input the result of the subtasks it depends on."
  :input true
  :accepts [:sprint-backlog :sort-by-created]
  :returns [:key :title]
  :params [{:name :level :type :string :required false :enum [:high-and-above :high]
            :doc "high-and-above (default) keeps High and Highest; high keeps only High"}]
  :run (fn [{: level} inputs]
         (json.encode (icollect [_ t (ipairs (utils.decode-lists inputs))]
                        (when (wanted? t.priority level) t))))
  :show (fn [out]
          (utils.show-list out {:empty "no high priority tickets"
                                :what " high priority tickets"
                                :line (fn [t] (string.format "%-12s [%s] %s" t.key (or t.priority "?") t.title))}))})
