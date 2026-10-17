(import-macros {: deftool} :src.macros)
(local log-path "/tmp/noqui.log")

(deftool debug
 {:short "saves the output to /tmp/noqui.log"
 :description "Saves the previous step's output to /tmp/noqui.log, for debugging, and passes it unchanged to the next step."
 :input :text
 ;; transparent: the next step sees it as if it came straight from the previous step
 :tee true
 :params []
 :run (fn [_ inputs _ prompt]
        (let [out (table.concat inputs "\n")]
          (with-open [f (io.open log-path :a)]
            ;; clearly visible separator between dumps
            (f:write (string.rep "═" 80) "\n"
                     "  " (os.date "%Y-%m-%d %H:%M:%S") " · " (length out) " bytes\n"
                     "  request: " prompt "\n"
                     (string.rep "─" 80) "\n"
                     out "\n\n"))
          out))
 :show (fn [out]
         [(.. log-path " ← " (length out) " bytes")])})
