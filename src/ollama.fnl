(local json (require :dkjson))
(local sh (require :src.sh))
(local config (require :config))

(fn chat [messages schema]
  "Calls ollama forcing the output to `schema` and returns the already decoded JSON."
  (let [tmp (os.tmpname)
        body (json.encode {:model config.model
                           :stream false
                           :think false
                           ;; num_predict: token cap in case the model gets into a loop
                           :options {:temperature 0 :num_predict 1024}
                           :format schema
                           :messages messages})]
    (with-open [f (io.open tmp :w)]
      (f:write body))
    (let [(out code) (sh.run (.. "curl -s --max-time 60 " config.ollama " -d @" (sh.quote tmp)))]
      (os.remove tmp)
      (assert (= code 0) (.. "ollama: " out))
      (let [resp (json.decode out)]
        (assert (and resp resp.message) (.. "ollama: " out))
        (or (json.decode resp.message.content)
            (error (.. "invalid response from ollama (done_reason=" (tostring resp.done_reason) ")") 0))))))

{: chat}
