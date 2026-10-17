;; :dev — self-development. You enter, do the work, and leave: a dedicated Claude
;; session that starts with the guide (src/guide.fnl), and ñoqui compiles and
;; validates the tools it proposes before saving them. Normal mode is unaffected.
;;
;; Until the user confirms, the code is only compiled (not run). On confirmation it
;; is saved and loaded; if that fails, it is rolled back (the new one is deleted or
;; the previous one restored) and the error goes back to Claude.
;;
;; To edit, Claude needs to see the current code: it is attached only when the
;; user names a tool, or when Claude asks for it with [see tools/x.fnl].
(local fennel (require :fennel))
(local acp (require :src.acp))
(local guide (require :src.guide))
(local ui (require :src.ui))
(local sh (require :src.sh))
(local config (require :config))
(local {: read : write} (require :src.files))

(local max-fixes 3) ; automatic fix attempts per proposal
(local max-reads 3) ; consecutive [see ...] requests answered automatically
(local valid-types {:string true :integer true :date true})

(local intro "You are the development assistant of ñoqui, a REPL that solves requests with tools (Fennel scripts) and hands Claude whatever no tool can solve. You will help create new tools and modify existing ones. The guide is below.

Rules:
- Speak English, briefly. If information is missing, ask before writing code.
- You have no tools: you cannot read or write files or run commands.
- To see a tool's code, write [see tools/<name>.fnl] and ñoqui will send it to you. When the user names a tool, ñoqui attaches its code automatically.
- When you propose a tool (new or modified), put the COMPLETE file in a single ```fennel block. To modify one, use the same :name. ñoqui compiles it; if it fails it sends you the error so you can fix it. If it compiles, it shows the user the diff (if it is a modification) and asks whether to save it.

To start, reply only: ready.")

(fn strip-ansi [s]
  (pick-values 1 (s:gsub "\27%[[%d;]*m" "")))

(fn tool-path [name]
  (.. "tools/" name ".fnl"))

(fn tool-names []
  (let [(out) (sh.run "ls tools/*.fnl 2>/dev/null")]
    (icollect [name (out:gmatch "tools/([^\n]+)%.fnl")] name)))

(fn extract-code [answer]
  "The last ```fennel block of the answer, or nil."
  (var code nil)
  (each [block (answer:gmatch "```fennel%s*\n(.-)```")]
    (set code block))
  code)

(fn check-code [code]
  "Without running anything: it must have a :name and compile. Returns the name or raises the error."
  (let [name (or (code:match ":name%s+\"([%w%-_]+)\"")
                 (error "could not find the tool's :name" 0))
        (ok err) (pcall fennel.compile-string code {:filename (tool-path name)})]
    (when (not ok)
      (error (.. "does not compile: " (strip-ansi (tostring err))) 0))
    name))

(fn check-tool [tool name]
  "The loaded table must have the shape of a tool."
  (let [problems []]
    (fn bad [msg] (table.insert problems msg))
    (if (not= (type tool) :table) (bad "the file must return a table")
        (do
          (when (not= tool.name name) (bad (.. ":name must be \"" name "\" (same as the file)")))
          (when (not (and (= (type tool.description) :string) (tool.description:match "%S")))
            (bad "missing :description"))
          (when (not= (type tool.run) :function) (bad "missing :run (a function)"))
          (when (and tool.show (not= (type tool.show) :function)) (bad ":show must be a function"))
          (when (not (or (= tool.input nil) (= tool.input true) (= tool.input :text)))
            (bad ":input must be true or :text"))
          (if (not= (type tool.params) :table) (bad "missing :params (can be [])")
              (each [i p (ipairs tool.params)]
                (when (not (and (= (type p) :table) p.name (. valid-types p.type)))
                  (bad (.. "param " i " needs a :name and a :type of string, integer or date")))))))
    (when (next problems)
      (error (table.concat problems "; ") 0))))

(fn save [code name]
  "Saves and loads the tool. If it already existed, first saves a backup in /tmp.
If it does not load properly, rolls back (deletes the new one or restores the previous one) and raises the error.
Returns the backup path, if any."
  (let [path (tool-path name)
        old (read path)
        backup (when old (.. "/tmp/noqui-tool-" name "-" (os.date "%Y%m%d-%H%M%S") ".fnl"))]
    (when backup (write backup old))
    (write path code)
    (let [(ok result) (pcall fennel.dofile path)
          (valid err) (if ok (pcall check-tool result name) (values false (strip-ansi (tostring result))))]
      (when (not valid)
        (if old (write path old) (os.remove path))
        (error (.. "does not load: " err) 0)))
    backup))

(fn diff-lines [old new]
  "Unified diff (without headers) between the current version and the proposed one."
  (let [a (os.tmpname)
        b (os.tmpname)]
    (write a old)
    (write b new)
    (let [(out) (sh.run (.. "diff -u " (sh.quote a) " " (sh.quote b)))]
      (os.remove a)
      (os.remove b)
      (icollect [l (out:gmatch "[^\n]+")]
        (when (not (or (l:match "^%-%-%- ") (l:match "^%+%+%+ "))) l)))))

(fn show-diff [old new]
  (let [lines (diff-lines old new)
        max 40]
    (ui.guide)
    (each [i l (ipairs lines) &until (> i max)]
      (print (.. (ui.c :faint "│  ")
                 (ui.c (case (l:sub 1 1) "+" :green "-" :red "@" :faint _ :subtext)
                       (ui.truncate l 100)))))
    (when (> (length lines) max)
      (print (.. (ui.c :faint "│  ") (ui.c :faint (.. "… and " (- (length lines) max) " more lines")))))))

(fn ask [session text]
  "Sends `text` to Claude, showing the answer. Returns (values answer cost)."
  (ui.guide)
  (print (.. (ui.c :mauve "◆") "  " (ui.c :text "claude")))
  (let [(writer done) (ui.stream :final)
        (answer cost) (acp.ask session text writer)]
    (done)
    (values answer cost)))

(fn confirm [question]
  ;; own style: orange (peach) is the color of code in Claude's answer
  (io.write "\n" (ui.c :yellow "?") "  " (ui.bold question) (ui.c :faint " [y/N] "))
  (io.flush)
  (= (: (or (io.read :l) "") :lower) "y"))

(fn attach [sent names]
  "Code of the tools `names`, except those Claude has already seen unchanged since
then. `sent` remembers which version it saw of each one."
  (let [parts []]
    (each [_ name (ipairs names)]
      (let [code (read (tool-path name))]
        (when (and code (not= (. sent name) code))
          (tset sent name code)
          (ui.notice :info (.. "attaching the code of tools/" name ".fnl"))
          (table.insert parts (.. "[ñoqui attaches the current code of tools/" name ".fnl]\n```fennel\n" code "```")))))
    (table.concat parts "\n\n")))

(fn mentioned [line]
  "The existing tools named in `line`."
  (icollect [_ name (ipairs (tool-names))]
    ;; whole word: "count" must not match in "account"
    (let [escaped (pick-values 1 (name:gsub "%-" "%%-"))]
      (when (line:find (.. "%f[%w%-]" escaped "%f[^%w%-]")) name))))

(fn answer-reads [session sent answer]
  "If Claude asked to see tools with [see tools/x.fnl], sends them (up to
max-reads times in a row). Returns (values last-answer cost)."
  (var answer answer)
  (var cost nil)
  (var n 0)
  (while (and answer (< n max-reads) (answer:find "[see tools/" 1 true))
    (set n (+ n 1))
    (let [names (icollect [name (answer:gmatch "%[see tools/([%w%-_]+)%.fnl%]")] name)
          missing (icollect [_ name (ipairs names)] (when (not (read (tool-path name))) name))
          text (.. (attach sent names)
                   (if (next missing) (.. "\n\nNot found:" (table.concat missing ", ")) ""))]
      (if (text:match "%S")
          (let [(a c) (ask session text)]
            (set answer a)
            (set cost c))
          ;; it already saw them all and they did not change: nothing new to send
          (set n max-reads))))
  (values answer cost))

(fn handle-proposal [session sent answer]
  "If the answer contains a tool: compiles it (and asks Claude to fix it up to
max-fixes times), shows the diff if it is a modification, asks whether to save
it, and loads it. Returns the last cost."
  (var answer answer)
  (var cost nil)
  (var tries 0)
  (var done? false)
  (while (and (not done?) (extract-code answer))
    (let [code (extract-code answer)
          (ok name) (pcall check-code code)
          old (when ok (read (tool-path name)))
          (saved? result) (if (not ok)
                              (values false name)
                              (do (when old (show-diff old code))
                                  (if (confirm (if old
                                                   (.. "Replace tools/" name ".fnl?")
                                                   (.. "Save tools/" name ".fnl?")))
                                      (pcall save code name)
                                      (values :no))))]
      (if (= saved? :no)
          (set done? true)
          saved?
          (do (tset sent name code) ; Claude already knows this version
              (ui.notice :reload (if old
                                     (.. name " replaced and loaded · the previous one is at " result)
                                     (.. "+" name " saved and loaded: ready to use")))
              (set done? true))
          (< tries max-fixes)
          (do (set tries (+ tries 1))
              (ui.notice :error result)
              (let [(a c) (ask session (.. "ñoqui error (attempt " tries " of " max-fixes "): " result
                                           "\nFix it and send the complete file."))]
                (set answer a)
                (set cost c)))
          (do (ui.notice :error (.. result " · " max-fixes " attempts so far: continue the conversation to get it unstuck"))
              (set done? true)))))
  cost)

(fn session []
  (ui.begin)
  (ui.header "ñoqui :dev" "tool self-development" (.. "claude · " (or config.claude-model "default")))
  (ui.guide)
  ;; the guide loads the tools, and reload notices must not land in the
  ;; middle of the "loading…" line
  (let [text (.. intro "\n\n" (guide.build))
        _ (ui.start "session" "loading the guide…")
        t0 (ui.now)
        session (acp.open-session)
        (_ first-cost) (acp.ask session text)
        sent {}] ; tool -> version of the code Claude has already seen
    (ui.finish :ok "session" "ready · :fin to go back" (- (ui.now) t0))
    (var cost (or first-cost 0))
    (var line nil)
    (while (do (ui.prompt "dev")
               (set line (io.read :l))
               (and line (not= line ":fin")))
      (when (line:match "%S")
        (let [(ok err) (pcall #(let [code (attach sent (mentioned line))
                                     (answer c) (ask session (if (code:match "%S") (.. line "\n\n" code) line))
                                     (answer c2) (answer-reads session sent answer)]
                                 (set cost (or c2 c cost))
                                 (set cost (or (handle-proposal session sent answer) cost))))]
          (when (not ok)
            (ui.notice :error (tostring err))))))
    ;; on exit: the session is left behind and the agent is closed; normal mode starts another
    (acp.stop)
    (ui.guide)
    (ui.footer [(ui.c :peach (string.format "session :dev · US$%.2f" cost))])))

(fn run [?model]
  "Enters :dev. With ?model (haiku, sonnet, opus, ...) the session uses that Claude
model; without it, the one from normal mode. On exit (even on error) the previous one is restored."
  (let [before config.claude-model]
    (when ?model
      (set config.claude-model ?model))
    (let [(ok err) (pcall session)]
      (set config.claude-model before)
      (when (not ok)
        ;; the agent may have been left with a half-finished :dev session
        (acp.stop)
        (error err 0)))))

{: run}
