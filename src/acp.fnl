;; Minimal ACP client: JSON-RPC over stdio against claude-agent-acp.
;; io.popen is unidirectional: we read the agent's stdout with popen, and its stdin
;; is a regular file to which we append the messages, read with `tail -f`.
;; (With a FIFO, node stops reading when a big message arrives and everything hangs.)
(local json (require :dkjson))
(local sh (require :src.sh))
(local config (require :config))
(local utils (require :src.utils))

(local object {:__jsontype :object}) ; so that dkjson encodes {} and not []

(var agent nil)
(var next-id 0)
;; fn (toolCall) -> true if approved. Without a policy nothing is approved.
(var policy nil)

(fn set-policy [f]
  (set policy f))

;; NOQUI_ACP_LOG=/tmp/acp.log: keeps the JSON-RPC traffic in that file, to debug
(local log-path (os.getenv "NOQUI_ACP_LOG"))

(fn log [dir line]
  (when log-path
    (with-open [f (io.open log-path :a)]
      (f:write (os.date "%H:%M:%S ") dir " " (length line) "b " (line:sub 1 400) "\n"))))

(fn send [msg]
  (let [line (json.encode msg)]
    (log ">>" line)
    ;; writing to a file never blocks, regardless of the message size
    (agent.in:write line "\n")
    (agent.in:flush)))

(fn start []
  (let [inbox (os.tmpname)
        in (io.open inbox :a)
        ;; `echo $$`: the first line is the shell PID, so it can be killed
        ;; (along with tail and the agent, which are its children) in `stop`
        out (io.popen (.. "echo $$; tail -n +1 -f " (sh.quote inbox)
                          " | claude-agent-acp 2>/dev/null"))
        pid (out:read :l)]
    (set agent {: in : out : pid : inbox})))

(fn reply [m ?policy]
  "The reply to an agent request (permissions, files, terminal). Permissions
are decided by `?policy` (by default all are rejected); files and terminal are not granted."
  (if (= m.method "session/request_permission")
      (let [wanted (if (and ?policy (?policy m.params.toolCall)) :allow_once :reject_once)
            option (utils.find m.params.options (fn [o] (= o.kind wanted)))]
        {:jsonrpc "2.0" :id m.id
         :result {:outcome (if option
                               {:outcome :selected :optionId option.optionId}
                               {:outcome :cancelled})}})
      {:jsonrpc "2.0" :id m.id
       :error {:code -32601 :message (.. "not supported: " m.method)}}))

(fn answer-request [m]
  (send (reply m policy)))

(fn call [method params on-update]
  (set next-id (+ next-id 1))
  (let [id next-id]
    (send {:jsonrpc "2.0" : id : method : params})
    (var result nil)
    (while (= result nil)
      (let [line (assert (agent.out:read :l) "claude-agent-acp closed")
            _ (log "<<" line)
            m (json.decode line)]
        (if (and m.method m.id) (answer-request m)
            m.method (when (and on-update (= m.method "session/update") m.params.update)
                       (on-update m.params.update))
            (= m.id id) (set result (or m.result
                                        (error (.. method ": " (json.encode m.error)) 0))))))
    result))

(fn model-error [model option]
  "The message for a model the agent does not offer; `option` is its \"model\" config option."
  (.. "invalid Claude model: " model " (options: "
      (table.concat (icollect [_ o (ipairs option.options)] o.value) ", ") ")"))

(fn open-session [?opts]
  "Opens a session and returns its id. As long as messages keep being sent to it (`ask`),
Claude remembers the whole conversation. ?opts: {:cwd working directory
(defaults to the current one) :model model (defaults to config.claude-model)}."
  (when (not agent)
    (start)
    (call :initialize {:protocolVersion 1 :clientCapabilities (setmetatable {} object)}))
  (let [{: sessionId : configOptions} (call :session/new {:cwd (or (and ?opts ?opts.cwd) (os.getenv "PWD")) :mcpServers []})
        model (or (and ?opts ?opts.model) config.claude-model)
        model-option (utils.find (or configOptions []) (fn [o] (= o.id "model")))]
    (when (and model model-option (not= model-option.currentValue model))
      (let [(ok err) (pcall call :session/set_config_option
                            {: sessionId :configId :model :value model})]
        (when (not ok)
          (error (model-error model model-option) 0))))
    sessionId))

(fn ask [session-id text on-chunk]
  "Sends `text` to the session and returns the full response and the cost in USD
reported by the agent (or nil). `on-chunk` receives each piece as it arrives."
  (let [chunks []]
    (var cost nil)
    (call :session/prompt {:sessionId session-id :prompt [{:type :text : text}]}
          (fn [update]
            (case update
              {:sessionUpdate "agent_message_chunk" :content {:type "text" : text}}
              (do (table.insert chunks text)
                  (when on-chunk (on-chunk text)))
              {:sessionUpdate "usage_update" :cost {: amount}}
              (set cost amount))))
    (values (table.concat chunks) cost)))

(fn prompt [text on-chunk]
  "Isolated query: new session, a single message."
  (ask (open-session) text on-chunk))

(fn stop []
  "Kills the shell and its children: tail, and the agent (the volta shim launches node,
which launches Claude Code). With `tail -f` the agent never sees EOF, so it has to be
killed: otherwise, the popen close waits for it forever."
  (when agent
    (agent.in:close)
    (when (and agent.pid (agent.pid:match "^%d+$"))
      (sh.kill-tree agent.pid))
    (agent.out:close)
    (os.remove agent.inbox)
    (set agent nil)))

{: prompt : open-session : ask : stop : set-policy : reply : model-error}
