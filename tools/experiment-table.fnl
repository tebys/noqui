(import-macros {: deftool} :src.macros)
;; Builds an experiment's block for the COSTS.org table (table.el format):
;; a full-width row with the prompt and one row per result, with 🤑 on the
;; cheapest, ⏰ on the fastest and 💀 on the one that came out wrong. It starts
;; right at the prompt so it can be appended below the table's closing border.
(local json (require :dkjson))
(local {: seconds} (require :src.experiment))

(local widths [11 6 11 6 30])
(local aligns [:l :l :r :r :l])
(local total (- (accumulate [n 0 _ w (ipairs widths)] (+ n w 3)) 3))
(local model-order {:haiku 1 :sonnet 2 :fable 3 :opus 4})

(fn width [s]
  "Display width: emojis (🤑 ⏰ 💀) take two columns."
  (accumulate [n 0 _ cp (utf8.codes s)]
    (+ n (if (or (>= cp 0x1F000) (= cp 0x23F0)) 2 1))))

(fn fit [s w align]
  (let [pad (string.rep " " (math.max 0 (- w (width s))))]
    (if (= align :r) (.. pad s) (.. s pad))))

(fn sep []
  (.. "+" (table.concat (icollect [_ w (ipairs widths)] (string.rep "-" (+ w 2))) "+") "+"))

(fn row [cells]
  (.. "|" (table.concat (icollect [i c (ipairs cells)] (.. " " (fit c (. widths i) (. aligns i)) " ")) "|") "|"))

(fn wrap [text w]
  "Splits the text into lines of up to w columns, by words."
  (let [lines []]
    (var line "")
    (each [word (text:gmatch "%S+")]
      (if (= line "") (set line word)
          (<= (+ (width line) 1 (width word)) w) (set line (.. line " " word))
          (do (table.insert lines line) (set line word))))
    (when (not= line "") (table.insert lines line))
    lines))

(fn cost-str [n]
  ;; 0.0400 -> 0.04, 0 -> 0
  (let [s (: (string.format "%.4f" n) :gsub "0+$" "")]
    (pick-values 1 (s:gsub "%.$" ""))))

(deftool experiment-table
 {:short "builds the cost table of an experiment"
 :description "Builds the cost table block (COSTS.org format) from an experiment already read with parse-experiment."
 :input true
 ;; only makes sense after parse-experiment
 :accepts [:parse-experiment]
 :params []
 :run (fn [_ inputs]
        (let [{: prompt : rows} (json.decode (. inputs 1))
              min-cost (accumulate [m math.huge _ f (ipairs rows)] (math.min m f.cost))
              min-time (accumulate [m math.huge _ f (ipairs rows)] (math.min m (seconds f.time)))]
          ;; Ñoqui first; then Claude Code in the usual order
          (table.sort rows (fn [a b]
                              (if (not= a.noqui b.noqui) a.noqui
                                  (< (or (. model-order a.model) 9) (or (. model-order b.model) 9)))))
          (let [lines (icollect [_ l (ipairs (wrap prompt total))]
                        (.. "| " (fit l total :l) " |"))]
            (table.insert lines (sep))
            (each [_ f (ipairs rows)]
              (let [r (and f.result (f.result:lower))
                    bad? (and r (or (r:find "mal") (r:find "wrong") (r:find "incorrect")))
                    fastest? (= (seconds f.time) min-time)
                    ;; "⏰ 1m52s" does not fit in the time column: the clock goes to Result
                    clock-fits? (<= (width (.. "⏰ " f.time)) (. widths 4))
                    result (.. (if (and fastest? (not clock-fits?)) "⏰ " "")
                               (if bad? "💀 " "")
                               (or f.result ""))]
                (table.insert lines
                              (row [(if f.noqui "Ñoqui" "Claude Code")
                                    f.model
                                    (.. (if (= f.cost min-cost) "🤑 " "") (cost-str f.cost))
                                    (.. (if (and fastest? clock-fits?) "⏰ " "") f.time)
                                    result]))))
            (table.insert lines (sep))
            (table.concat lines "\n"))))
 :show (fn [out]
         (icollect [line (out:gmatch "[^\n]+")] line))})
