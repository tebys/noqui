;; ACP client over luv: the same as src/acp.fnl but non-blocking. Each client
;; is its own claude-agent-acp process, with real pipes (luv does not need the
;; file + tail -f trick). For background work, like :watch.
(local uv (require :luv))
(local json (require :dkjson))
(local sh (require :src.sh))
(local config (require :config))
(local acp (require :src.acp))
(local aio (require :src.aio))
(local utils (require :src.utils))

(local object {:__jsontype :object}) ; so that dkjson encodes {} and not []

(fn open [opts done]
  "Brings up an agent and opens a session. opts: {:cwd :model :policy fn(toolCall)->bool}.
Calls (done err client); the client has `ask` and `close`:
  (client.ask text on-chunk cb)   cb receives (err response cost)
  (client.close)"
  (let [stdin (uv.new_pipe false)
        stdout (uv.new_pipe false)
        client {:pending {} :next-id 0 :closed? false :on-update nil}]
    (var buffer "")
    (var process nil)
    (var pid nil)

    (fn send [msg]
      (when (not client.closed?)
        (stdin:write (.. (json.encode msg) "\n"))))

    (fn rpc [method params cb]
      (set client.next-id (+ client.next-id 1))
      (tset client.pending client.next-id cb)
      (send {:jsonrpc "2.0" :id client.next-id : method : params}))

    (fn fail-all [msg]
      (each [id cb (pairs client.pending)]
        (tset client.pending id nil)
        (cb msg)))

    (fn on-line [line]
      (let [m (json.decode line)]
        (when (= (type m) :table)
          (if (and m.method m.id) (send (acp.reply m opts.policy))
              m.method (when (and (= m.method "session/update") m.params.update client.on-update)
                         (client.on-update m.params.update))
              m.id (let [cb (. client.pending m.id)]
                     (when cb
                       (tset client.pending m.id nil)
                       (if m.error (cb (json.encode m.error)) (cb nil m.result))))))))

    (fn close []
      (when (not client.closed?)
        (set client.closed? true)
        (when pid (sh.kill-tree pid))
        (each [_ h (ipairs [stdin stdout])]
          (when (not (h:is_closing)) (h:close)))
        (when (and process (not (process:is_closing))) (process:close))))

    (set client.close close)
    (set client.ask
         (fn [text on-chunk cb]
           (let [chunks []]
             (var cost nil)
             (set client.on-update
                  (fn [update]
                    (case update
                      {:sessionUpdate "agent_message_chunk" :content {:type "text" : text}}
                      (do (table.insert chunks text)
                          (when on-chunk (on-chunk text)))
                      {:sessionUpdate "usage_update" :cost {: amount}}
                      (set cost amount))))
             (rpc :session/prompt {:sessionId client.session-id :prompt [{:type :text : text}]}
                  (fn [err]
                    (set client.on-update nil)
                    (if err (cb err) (cb nil (table.concat chunks) cost)))))))

    (let [(handle id) (uv.spawn "claude-agent-acp" {:stdio [stdin stdout nil]}
                                (aio.guard (fn []
                                             (fail-all "claude-agent-acp closed")
                                             (set pid nil)
                                             (close))))]
      (if (not handle)
          (do (stdin:close) (stdout:close)
              (done (.. "could not launch claude-agent-acp: " (tostring id))))
          (do
            (set process handle)
            (set pid id)
            (stdout:read_start
              (aio.guard (fn [err chunk]
                           (if chunk
                               (do (set buffer (.. buffer chunk))
                                   (while (buffer:find "\n" 1 true)
                                     (let [(line rest) (buffer:match "^([^\n]*)\n(.*)$")]
                                       (set buffer rest)
                                       (on-line line))))
                               (when (not (stdout:is_closing)) (stdout:close)))))) 
            (fn fail [err]
              (close)
              (done err))
            (fn pick-model [res]
              (let [model (or opts.model config.claude-model)
                    option (utils.find (or res.configOptions []) (fn [o] (= o.id "model")))]
                (set client.session-id res.sessionId)
                (if (and model option (not= option.currentValue model))
                    (rpc :session/set_config_option
                         {:sessionId res.sessionId :configId :model :value model}
                         (fn [err]
                           (if err
                               (fail (acp.model-error model option))
                               (done nil client))))
                    (done nil client))))
            (rpc :initialize {:protocolVersion 1 :clientCapabilities (setmetatable {} object)}
                 (fn [err]
                   (if err
                       (fail err)
                       (rpc :session/new {:cwd (or opts.cwd (os.getenv "PWD")) :mcpServers []}
                            (fn [err res]
                              (if err (fail err) (pick-model res))))))))))))

{: open}
