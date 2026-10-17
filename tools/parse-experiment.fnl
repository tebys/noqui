(import-macros {: deftool} :src.macros)
(local json (require :dkjson))
(local ollama (require :src.ollama))
(local {: seconds} (require :src.experiment))

;; the model only extracts the rows; the prompt (in quotes) is taken by the code
(local schema
  {:type :object
   :properties {:rows {:type :array
                        :maxItems 12
                        :items {:type :object
                                :properties {:noqui {:type :boolean}
                                             :model {:type :string}
                                             ;; text, not number: with :number the 8b read
                                             ;; "$0.2389" as 2389. The code converts it.
                                             :cost {:type :string}
                                             :time {:type :string}
                                             :result {:type :string}}
                                :required [:noqui :model :cost :time]}}}
   :required [:rows]})

(local instructions
  "Extract the results of an experiment. For each result:
- noqui: true if it is from noqui/ñoqui; otherwise false (it is from Claude)
- model: haiku, sonnet, opus, fable, etc. in lowercase
- cost: copied exactly as it appears in the text (e.g. \"$0.2389\", \"US$0.04\")
- time: copied exactly as it appears in the text (e.g. \"14s\", \"1m52s\")
- result: only if mentioned (e.g. \"came out wrong\", \"7 correct\")
Reply with JSON only.")

(deftool parse-experiment
 {:short "parses an experiment written as text"
 :description "Reads an experiment written as text (the measured prompt in quotes and each model's results: cost and time) and returns it organized as JSON."
 :text true
 :params []
 :run (fn [_ _ text]
        (let [(prompt rest) (text:match "\"(.-)\"(.*)")]
          (assert prompt "the experiment prompt is missing (in quotes)")
          (let [{: rows} (ollama.chat [{:role :system :content instructions}
                                        {:role :user :content rest}]
                                       schema)]
            (assert (> (length rows) 0) "no results found")
            (each [_ f (ipairs rows)]
              (set f.model (f.model:lower))
              ;; the code extracts the numbers, and they must be in the text as-is:
              ;; if the model invents or distorts one, it fails instead of writing something wrong
              (let [digits (: (tostring f.cost) :match "%d*[%.,]?%d+")
                    t (pick-values 1 (: (tostring f.time) :gsub "%s" ""))]
                (assert (and digits (rest:find digits 1 true))
                        (.. "cost of " f.model " does not appear in the text: " (tostring f.cost)))
                (assert (and (seconds t) (rest:find t 1 true))
                        (.. "time of " f.model " does not appear in the text: " (tostring f.time)))
                (set f.cost (tonumber (pick-values 1 (digits:gsub "," "."))))
                (set f.time t))
              (when (or (= f.result "") (= f.result json.null))
                (set f.result nil)))
            (json.encode {: prompt : rows}))))
 :show (fn [out]
         (let [{: prompt : rows} (json.decode out)]
           (doto (icollect [_ f (ipairs rows)]
                   ;; %-11s counts bytes and the Ñ takes two: pad by hand
                   (let [who (if f.noqui "Ñoqui" "Claude Code")]
                     (string.format "%s%s %-6s %8s  %6s  %s" who (string.rep " " (- 11 (utf8.len who)))
                                    f.model f.cost f.time (or f.result ""))))
             (table.insert 1 (.. "\"" prompt "\"")))))})
