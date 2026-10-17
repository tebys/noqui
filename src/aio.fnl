;; Async utilities over luv.
(local uv (require :luv))

(var last-error nil) ; so a failing timer does not repeat the same notice every tick

(fn guard [f]
  "For luv callbacks: an error does not bring down the REPL, except Ctrl-C (\"interrupted!\").
Other errors are reported as a notice instead of being lost."
  (fn [...]
    (let [(ok err) (pcall f ...)]
      (when (not ok)
        (if (: (tostring err) :find "interrupted" 1 true)
            (error err 0)
            (when (not= (tostring err) last-error)
              (set last-error (tostring err))
              ((. (require :src.ui) :notice) :error last-error)))))))

(fn run [cmd cb]
  "Runs `cmd` in a shell without blocking. Calls (cb output code) with stdout+stderr
when the process finished and all the output has been read."
  (let [stdout (uv.new_pipe false)
        chunks []]
    (var pending 2) ; process exit and end of stdout (they can arrive in any order)
    (var code nil)
    (fn done []
      (set pending (- pending 1))
      (when (= pending 0)
        (cb (table.concat chunks) code)))
    (let [(handle err) (uv.spawn "/bin/sh" {:args ["-c" (.. cmd " 2>&1")] :stdio [nil stdout nil]}
                                 (guard (fn [c]
                                          (set code c)
                                          (done))))]
      (if (not handle)
          (do (stdout:close)
              (cb (tostring err) 1))
          (stdout:read_start (guard (fn [e chunk]
                                      (if chunk
                                          (table.insert chunks chunk)
                                          (do (stdout:close) (done))))))))))

{: guard : run}
