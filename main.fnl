;; dkjson installed with `luarocks --lua-version=5.4 install --local dkjson`
;; luv: `luarocks --lua-version=5.4 install --local luv`
(local lua-version (or (and _VERSION (_VERSION:match "Lua (%d+%.%d+)")) "5.4"))
(set package.path (.. (os.getenv "HOME") "/.luarocks/share/lua/" lua-version "/?.lua;" package.path))
(set package.cpath (.. (os.getenv "HOME") "/.luarocks/lib/lua/" lua-version "/?.so;" package.cpath))

(local dispatcher (require :src.dispatcher))
(local acp (require :src.acp))
(local tools (require :src.tools))
(local config (require :config))
(local ui (require :src.ui))
(local snippets (require :src.snippets))
(local dev (require :src.dev))
(local context (require :src.context))
(local watch (require :src.watch))
(local sh (require :src.sh))
(local agents (require :src.agents))
(local hud (require :src.hud))
(local uv (require :luv))

(fn run-snippet [line]
  "`:name arg1 arg2` runs a snippet; `:snippets` lists them."
  (let [words (icollect [w (line:gmatch "%S+")] w)
        name (: (table.remove words 1) :sub 2)]
    (if (= name "snippets")
        (each [_ l (ipairs (snippets.list))]
          (ui.notice :info l))
        (case (pcall snippets.expand name words)
          (true snippet) (dispatcher.run snippet.prompt snippet)
          (false err) (ui.notice :error err)))))

(fn list-tools []
  "`:tools` lists the tools, one per line, with their short description."
  (let [all (tools.load-all "tools")
        width (accumulate [w 0 _ t (ipairs all)] (math.max w (length t.name)))]
    (each [_ t (ipairs all)]
      (ui.notice :info (.. t.name (string.rep " " (- width (length t.name))) "  " (or t.short ""))))))

(fn open-terminal []
  "`:terminal` opens a new terminal in the worktree of the :wt mode."
  (let [wt (context.current)]
    (if (not wt)
        (ui.notice :error "no worktree mode: use :wt repo ticket first")
        (let [(out code) (sh.run (.. config.terminal (sh.quote wt.path)))]
          (if (= code 0)
              (ui.notice :info (.. "terminal opened in " wt.path))
              (ui.notice :error out))))))

(fn open-claude []
  "`:claude` launches claude in a new terminal, in the worktree of the :wt mode."
  (let [wt (context.current)]
    (if (not wt)
        (ui.notice :error "no worktree mode: use :wt repo ticket first")
        (let [(ok err) (pcall agents.launch wt)]
          (if ok
              (ui.notice :info (.. "claude launched in " wt.path))
              (ui.notice :error (tostring err)))))))

(fn worktree-mode [prompt]
  "`:wt repo ticket` enters worktree mode; `:fin` leaves it."
  (if (= prompt ":fin")
      (let [was (context.leave)]
        (ui.notice :info (if was (.. "left worktree mode (" was.repo " " was.ticket ")") "no mode is active")))
      (let [(repo ticket) (prompt:match "^:wt%s+(%S+)%s+(%S+)%s*$")
            (ok res) (pcall context.enter repo ticket)]
        (if ok
            (ui.notice :info (.. "worktree mode: " res.repo " " res.ticket " (" res.branch ") · :fin to leave"))
            (ui.notice :error (tostring res))))))

(fn handle [prompt]
  (let [(ok err) (xpcall #(if (prompt:match "^:dev%s*%S*%s*$")
                              ;; :dev [model]
                              (dev.run (prompt:match "^:dev%s+(%S+)"))
                              (= prompt ":terminal")
                              (open-terminal)
                              (= prompt ":claude")
                              (open-claude)
                              (= prompt ":tools")
                              (list-tools)
                              (or (= prompt ":watch") (prompt:match "^:watch%s"))
                              (let [(ok res) (pcall watch.run (prompt:match "^:watch%s+(%S+)"))]
                                (if ok
                                    (when res (ui.notice :info (.. "watch in the background · details in " res)))
                                    (ui.notice :error (tostring res))))
                              (or (= prompt ":wt") (prompt:match "^:wt%s") (= prompt ":fin"))
                              (worktree-mode prompt)
                              (prompt:match "^:")
                              (run-snippet prompt)
                              (dispatcher.run prompt))
                         debug.traceback)]
    (when (not ok)
      (ui.notice :error err)
      ;; if it failed in the middle of a call the agent is left in a weird state: restart it
      (acp.stop))))

(fn banner []
  (ui.begin)
  (let [names (tools.load-all "tools")]
    (ui.header "ñoqui" (.. (length names) " tools")
               (.. config.model " · " (or config.claude-model "claude")))
    (ui.guide)
    (each [_ line (ipairs ["a -> b -> c to chain steps · :q to quit"
                           ":snippets · :tools · :dev [model] · :wt repo ticket · :fin"
                           ":terminal · :claude · :watch [min|stop]"])]
      (print (.. (ui.c :faint "│  ") (ui.c :faint line))))
    (print (ui.c :faint "└"))))

(fn repl-blocking []
  ;; check for changes in the tools every time the prompt comes back
  (tools.load-all "tools")
  (ui.prompt (context.label))
  (let [line (io.read :l)]
    (when (and line (not= line ":q"))
      (when (line:match "%S")
        (print)
        (handle line))
      (repl-blocking))))

(fn repl-tty []
  "REPL on top of the luv loop: while it waits for the prompt the agents' notices
arrive and the panel is redrawn. While a request runs stdin is not read (the
modes that do io.read, like :dev, read it directly) and the notices wait."
  (let [stdin (uv.new_tty 0 true)]
    (var buffer "")
    (var done? false)
    (var on-read nil)
    (fn quit []
      (set done? true)
      (stdin:read_stop)
      (stdin:close)
      (watch.shutdown)
      (agents.shutdown))
    (set on-read
         (agents.guard
           (fn [err chunk]
             (if (or err (not chunk))
                 (do (print) (quit))
                 (do
                   (stdin:read_stop)
                   (set buffer (.. buffer chunk))
                   (while (and (not done?) (buffer:find "\n" 1 true))
                     (let [(line rest) (buffer:match "^([^\n]*)\n(.*)$")]
                       (set buffer rest)
                       ;; Enter (and the output) scrolls the screen and drags the
                       ;; boxes along: they are erased beforehand, and come back when the prompt is redrawn
                       (hud.clear)
                       (if (= line ":q")
                           (quit)
                           (when (line:match "%S")
                             (print)
                             (handle line)))))
                   (when (not done?)
                     ;; check for changes in the tools every time the prompt comes back
                     (tools.load-all "tools")
                     (ui.prompt (context.label))
                     (hud.resume)
                     (stdin:read_start on-read)))))))
    (agents.start)
    (ui.prompt (context.label))
    (stdin:read_start on-read)
    (let [(ok err) (pcall uv.run)]
      (watch.shutdown)
      (agents.shutdown)
      (when (not ok)
        (print)
        (ui.notice :error (tostring err))))))

(fn repl []
  (if (= (uv.guess_handle 0) "tty")
      (repl-tty)
      (repl-blocking)))

(fn parse-args [args]
  "Extracts --model X / --model=X (overrides config.claude-model for this session) and
returns the rest of the arguments."
  (let [rest []]
    (var i 1)
    (while (<= i (length args))
      (let [a (. args i)]
        (if (= a "--model")
            (do (set config.claude-model
                     (or (. args (+ i 1))
                         (do (ui.notice :error "usage: --model <haiku|sonnet|opus|...>") (os.exit 1))))
                (set i (+ i 2)))
            (a:match "^%-%-model=")
            (do (set config.claude-model (a:match "=(.*)"))
                (set i (+ i 1)))
            (do (table.insert rest a)
                (set i (+ i 1))))))
    rest))

(local args (parse-args arg))

(if (. args 1)
    (handle (table.concat args " "))
    (do (banner)
        (repl)))
(acp.stop)
