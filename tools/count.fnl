(import-macros {: deftool} :src.macros)
(local json (require :dkjson))

(deftool count
 {:short "counts the elements of a list"
 :description "Returns how many elements a list has, ALL of them, with no conditions. Use it only for questions like \"how many PRs are there\". It does NOT work if you need to filter (\"how many had X\"), compute, compare or find the largest/smallest: that goes with tool null."
 :input true
 :params []
 :run (fn [_ inputs]
        (tostring (accumulate [total 0 _ input (ipairs inputs)]
                    (let [list (json.decode input)]
                      (assert (= (type list) :table) (.. "not a list: " input))
                      (+ total (length list))))))})
