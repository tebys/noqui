(import-macros {: deftool} :src.macros)
(local utils (require :src.utils))

(deftool count
 {:short "counts the elements of a list"
 :description "Returns how many elements a list has, ALL of them, with no conditions. Use it only for questions like \"how many PRs are there\". It does NOT work if you need to filter (\"how many had X\"), compute, compare or find the largest/smallest: that goes with tool null."
 :input true
 :params []
 :run (fn [_ inputs]
        (tostring (length (utils.decode-lists inputs))))})
