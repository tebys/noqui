(import-macros {: deftool} :src.macros)
(local json (require :dkjson))

;; days since 1970-01-01 (proleptic Gregorian calendar)
(fn days-from-civil [y m d]
  (let [y (if (<= m 2) (- y 1) y)
        era (math.floor (/ y 400))
        yoe (- y (* era 400))
        doy (+ (math.floor (/ (+ (* 153 (if (> m 2) (- m 3) (+ m 9))) 2) 5)) (- d 1))
        doe (+ (* yoe 365) (math.floor (/ yoe 4)) (- (math.floor (/ yoe 100))) doy)]
    (+ (* era 146097) doe -719468)))

;; Jira date "2026-10-01T10:15:30.000-0500" -> seconds in UTC (nil if it does not parse)
(fn created-seconds [s]
  (when (= (type s) :string)
    (let [(y mo d h mi se) (s:match "^(%d+)-(%d+)-(%d+)T(%d+):(%d+):(%d+)")]
      (when y
        (let [(sign oh om) (s:match "([+-])(%d%d):?(%d%d)$")
              offset (if sign
                         (* (if (= sign "-") -1 1)
                            (+ (* (tonumber oh) 3600) (* (tonumber om) 60)))
                         0)]
          (- (+ (* (days-from-civil (tonumber y) (tonumber mo) (tonumber d)) 86400)
                (* (tonumber h) 3600) (* (tonumber mi) 60) (tonumber se))
             offset))))))

(deftool sort-by-created
 {:short "sorts the sprint backlog by created date"
  :description "Sorts the list of tickets returned by sprint-backlog by their Jira created date. Oldest first by default, or newest first with order newest. Tickets without a created date go last. Receives as input the result of the subtasks it depends on."
  :input true
  :accepts [:sprint-backlog]
  :returns [:key :title]
  :params [{:name :order :type :string :required false :enum [:oldest :newest]
            :doc "oldest (default) puts the oldest tickets first; newest puts the newest first"}]
  :run (fn [{: order} inputs]
         (let [items []]
           (each [_ input (ipairs inputs)]
             (let [list (json.decode input)]
               (assert (= (type list) :table) (.. "not a list: " input))
               (each [_ t (ipairs list)]
                 (table.insert items {:t t :secs (created-seconds t.created)}))))
         (let [newest? (= order :newest)]
           (table.sort items
                       (fn [a b]
                         (if (and a.secs b.secs (not= a.secs b.secs))
                             (if newest? (> a.secs b.secs) (< a.secs b.secs))
                             (and a.secs (not b.secs)) true
                             (and b.secs (not a.secs)) false
                             (< (tostring a.t.key) (tostring b.t.key))))))
           (json.encode (icollect [_ x (ipairs items)] x.t))))
  :show (fn [out]
          (let [items (json.decode out)]
            (if (= (length items) 0)
                ["no tickets"]
                (let [lines [(.. (length items) " tickets sorted by created date")]]
                  (each [_ t (ipairs items)]
                    (table.insert lines
                                  (string.format "%-12s %s  %s" t.key
                                                 (or (and t.created (t.created:sub 1 10)) "?")
                                                 t.title)))
                  lines))))})
