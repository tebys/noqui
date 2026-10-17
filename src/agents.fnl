;; Claude agents launched from ñoqui (:claude, inside :wt). Each one runs
;; in a new terminal, with total freedom for the user; ñoqui only watches.
;; Claude reports its state with hooks (--settings): each hook sends "<id> <state>"
;; over a unix socket served by luv, and a box at the top right shows the
;; state of all of them (one colored circle per agent). It requires the interactive
;; REPL (`start`), which is the one that runs the luv loop.
(local uv (require :luv))
(local json (require :dkjson))
(local config (require :config))
(local sh (require :src.sh))
(local hud (require :src.hud))
(local aio (require :src.aio))
(local utils (require :src.utils))

(local max-shown 6)                     ; agents in the box; the rest is "+N"
(local linger 20)                       ; seconds a closed agent stays in the box

;; the state is the color of the circle
(local dot-colors {:starting :faint :idle :green :working :blue :waiting :yellow :closed :red})

;; state for each Claude Code event. Notification only when it asks for something:
;; idle_prompt (reminder after 60s) changes nothing because Stop already left it idle.
(local hook-states
  [["SessionStart" :idle] ["UserPromptSubmit" :working] ["PostToolUse" :working]
   ["Notification" :waiting :permission_prompt] ["Notification" :waiting :elicitation_dialog]
   ["Stop" :idle] ["SessionEnd" :closed]])

(local state {:agents [] :next-id 1 :dir nil :sock nil :server nil :tick nil :winch nil
              })

(fn publish []
  "The rows of the :claude box."
  (let [n (length state.agents)
        rows []]
    (each [i a (ipairs state.agents)]
      (when (<= i max-shown)
        (table.insert rows (if (and (= i max-shown) (> n max-shown))
                               (hud.note (.. "+" (- n max-shown -1) " more"))
                               (hud.row (. dot-colors a.state) a.ticket a.repo)))))
    (hud.set-box :claude "claude" rows)))

;; --- state --------------------------------------------------------------------

(fn find [id]
  (utils.find state.agents (fn [a] (= a.id id))))

(fn receive [chunk]
  (each [id st (chunk:gmatch "(%S+) (%S+)")]
    (let [a (find id)]
      (when (and a (. dot-colors st) (not= a.state st))
        (set a.state st)
        (set a.since (os.time))
        (publish)))))

(fn on-tick []
  "Every second: removes agents that closed a while ago (and redraws only if it removed any)."
  (var removed? false)
  (for [i (length state.agents) 1 -1]
    (let [a (. state.agents i)]
      (when (and (= a.state :closed) (> (- (os.time) a.since) linger))
        (table.remove state.agents i)
        (set removed? true))))
  (when removed? (publish)))

(fn accept [server]
  (let [client (uv.new_pipe false)]
    (server:accept client)
    (client:read_start (aio.guard (fn [err chunk]
                                (when chunk (receive chunk))
                                (when (or err (not chunk)) (client:close)))))))

(fn start []
  "Brings up the hooks socket and the timers in the luv loop. Call before `uv.run`."
  (let [dir (.. "/tmp/noqui-" (uv.os_getpid))
        sock (.. dir "/hooks.sock")
        server (uv.new_pipe false)
        tick (uv.new_timer)
        winch (uv.new_signal)]
    (sh.run (.. "mkdir -m 700 -p " (sh.quote dir)))
    (os.remove sock)
    (assert (server:bind sock))
    (assert (server:listen 16 (aio.guard (fn [err] (when (not err) (accept server))))))
    ;; the timer also lets Ctrl-C through: without Lua callbacks the hook does not run
    (tick:start 1000 1000 (aio.guard on-tick))
    (winch:start :sigwinch (aio.guard hud.relayout))
    (hud.enable)
    (set state.dir dir)
    (set state.sock sock)
    (set state.server server)
    (set state.tick tick)
    (set state.winch winch)))

(fn shutdown []
  "Closes everything luv-related (so `uv.run` ends) and leaves the terminal as it was."
  (when state.dir
    (hud.disable)
    (each [_ h (ipairs [state.server state.tick state.winch])]
      (when (not (h:is_closing)) (h:close)))
    (sh.run (.. "rm -rf " (sh.quote state.dir)))
    (set state.dir nil)
    (set state.agents [])))

;; --- launch --------------------------------------------------------------------

(fn trim [s]
  (pick-values 1 (: (s:gsub "^%s+" "") :gsub "%s+$" "")))

(fn settings-for [id sock]
  (let [hooks {}]
    (each [_ [event st matcher] (ipairs hook-states)]
      (let [cmd (.. "echo '" id " " st "' | /usr/bin/nc -U -w 1 " (sh.quote sock) " || true")]
        (when (not (. hooks event)) (tset hooks event []))
        (table.insert (. hooks event)
                      {:matcher matcher :hooks [{:type "command" :command cmd}]})))
    {: hooks}))

(fn prepare [wt]
  "Writes the settings with the hooks and the script that opens claude in `wt`; registers the
agent in the box. Returns (values agent script)."
  (assert state.dir "agents only work in the interactive REPL")
  (let [claude (trim (sh.run "command -v claude"))
        _ (assert (not= claude "") "cannot find claude in the PATH")
        id (.. "a" state.next-id)
        settings (.. state.dir "/" id ".json")
        script (.. state.dir "/" id ".command")]
    (set state.next-id (+ state.next-id 1))
    (with-open [f (assert (io.open settings :w))]
      (f:write (json.encode (settings-for id state.sock))))
    (with-open [f (assert (io.open script :w))]
      (f:write (.. "#!/bin/sh\nexport PATH=" (sh.quote (os.getenv :PATH)) "\ncd " (sh.quote wt.path)
                   " || exit 1\nexec " (sh.quote claude) " --settings " (sh.quote settings) "\n")))
    (sh.run (.. "chmod +x " (sh.quote script)))
    (let [agent {: id :repo wt.repo :ticket wt.ticket :state :starting :since (os.time)}]
      (table.insert state.agents agent)
      (values agent script))))

(fn launch [wt]
  "Opens a terminal (config.terminal-run) with claude running in the worktree `wt` ({: repo : ticket : path})."
  (let [(agent script) (prepare wt)
        (out code) (sh.run (.. (or config.terminal-run "open -a Terminal ") (sh.quote script)))]
    (when (not= code 0)
      (set agent.state :closed)
      (error (trim out) 0))
    (publish)
    agent))

{: start : shutdown : launch : prepare :guard aio.guard :refresh hud.draw}
