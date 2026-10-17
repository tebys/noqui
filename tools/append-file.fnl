(import-macros {: deftool} :src.macros)
(local files (require :src.files))

(deftool append-file
 {:short "appends text to the end of a file"
 :description "Appends the text received from the previous step to the end of a project file."
 ;; :text: accepts any text, the previous step does not need to return JSON
 :input :text
 :params [{:name :path :type :string :required true
           :doc "path relative to the project, e.g. COSTS.org"}]
 :run (fn [{: path} inputs]
        (let [path (files.check-path path)
              text (table.concat inputs "\n")
              existing (or (case (io.open path) f (with-open [f f] (f:read :a))) "")
              ;; the appended text starts on a new line and ends with a newline
              before (if (or (= existing "") (existing:match "\n$")) "" "\n")
              after (if (text:match "\n$") "" "\n")]
          (with-open [f (io.open path :a)]
            (f:write before text after))
          (.. path ": +" (select 2 (: (.. text after) :gsub "\n" "")) " lines")))})
