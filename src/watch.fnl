;; :watch [minutes] | :watch stop — inside :wt mode, checks the PR's CI every so many
;; minutes, in the background (luv): the REPL stays free and the status shows in a
;; little box below the :claude one. If there are failed checks it asks Claude (sonnet,
;; its own claude-agent-acp) to fix them, commit and push. Claude runs inside the
;; worktree and can only do what `allowed?` approves. It stops when everything
;; passes, after max-attempts fixes, or if Claude changes nothing; the result stays in
;; the box until `:watch stop`. Details go to /tmp/noqui-watch-<repo>-<ticket>.log.
(local acp-async (require :src.acp-async))
(local aio (require :src.aio))
(local hud (require :src.hud))
(local uv (require :luv))
(local context (require :src.context))
(local tools (require :src.tools))
(local worktrees (require :src.worktrees))
(local utils (require :src.utils))

(local default-minutes 5)
(local max-attempts 3)
(local max-errors 3)        ; consecutive gh failures before giving up
(local max-listed 15)       ; failed checks reported to Claude
(local claude-model "sonnet")
(local approvals-check "require approvals") ; resolved by a person: neither waited for nor fixed

;; --- Claude permissions -------------------------------------------------------

(local meta-chars "[;&|<>`$\n\r]") ; one command per call, no pipes or expansions
(local safe-prefixes
  ["git status" "git diff" "git log" "git show" "git rev-parse" "git branch --show-current"
   "gh run view" "gh run list" "gh pr checks" "gh pr view" "ls" "pwd"
   "npx eslint" "npx prettier" "npx vitest" "npx tsc" "eslint" "prettier" "vitest" "tsc"])

(fn trim [s]
  (pick-values 1 (: (s:gsub "^%s+" "") :gsub "%s+$" "")))

(fn starts? [cmd prefix]
  (or (= cmd prefix) (= (cmd:sub 1 (+ (length prefix) 1)) (.. prefix " "))))

(fn tokens [cmd]
  (icollect [t (cmd:gmatch "%S+")] t))

(fn has-token? [toks wanted]
  (utils.includes? toks wanted))

(fn push-ok? [cmd branch]
  "Only `git push [-u] [origin <worktree-branch>]`: no force and no other refs."
  (let [toks (tokens cmd)
        positional []]
    (var flags-ok true)
    (for [i 3 (length toks)]
      (let [t (. toks i)]
        (if (t:match "^%-")
            (when (not (or (= t "-u") (= t "--set-upstream"))) (set flags-ok false))
            (table.insert positional t))))
    (and flags-ok
         (or (= (length positional) 0)
             (and (= (length positional) 2) (= (. positional 2) branch))))))

(fn command-ok? [cmd branch]
  (let [cmd (trim cmd)
        toks (tokens cmd)]
    (and (not (cmd:find meta-chars))
         (not (cmd:find "--output" 1 true))
         (or (accumulate [ok false _ p (ipairs safe-prefixes)] (or ok (starts? cmd p)))
             (and (starts? cmd "git add")
                  (not (has-token? toks "-A")) (not (has-token? toks "--all"))
                  (not (has-token? toks ".")) (not (has-token? toks "*")))
             (and (starts? cmd "git commit")
                  (not (has-token? toks "--amend"))
                  (not (has-token? toks "--no-verify"))
                  (not (has-token? toks "-n")))
             (and (starts? cmd "git push") (push-ok? cmd branch))
             (and (starts? cmd "pnpm")
                  (not (cmd:find "publish" 1 true))
                  (not (cmd:find "dlx" 1 true)))))))

(fn inside? [path dir]
  (and (not (path:find "/.." 1 true))
       (or (= path dir) (= (path:sub 1 (+ (length dir) 1)) (.. dir "/")))))

(fn paths-of [tool-call]
  (let [out []
        input (or tool-call.rawInput {})]
    (each [_ l (ipairs (or tool-call.locations []))]
      (when l.path (table.insert out l.path)))
    (each [_ k (ipairs [:file_path :path :notebook_path])]
      (when (= (type (. input k)) :string) (table.insert out (. input k))))
    out))

(fn allowed? [tool-call dir branch]
  "Whether to approve what Claude asks for: reading inside the worktree (or /tmp), editing
only inside the worktree, and the commands on the closed list. Anything else, no."
  (let [kind tool-call.kind
        paths (paths-of tool-call)
        all-inside? (fn [dirs]
                      (accumulate [ok true _ p (ipairs paths)]
                        (and ok (accumulate [any false _ d (ipairs dirs)] (or any (inside? p d))))))]
    (case kind
      :execute (and (= (type (?. tool-call :rawInput :command)) :string)
                    (command-ok? tool-call.rawInput.command branch))
      :edit (and (all-inside? [dir])
                 (not (accumulate [bad false _ p (ipairs paths)] (or bad (p:find "/.git/" 1 true)))))
      (where (or :read :search)) (all-inside? [dir "/tmp"])
      :think true
      _ false)))

;; --- worktree and CI --------------------------------------------------------------

(fn head [dir]
  (trim (worktrees.git dir "rev-parse HEAD")))

(fn dirty? [dir]
  (not= "" (trim (worktrees.git dir "status --porcelain --untracked-files=no"))))

(fn unpushed? [dir]
  "Local commits that are not in the upstream. With no upstream, counts as unpushed."
  (let [(ok out) (pcall worktrees.git dir "rev-list --count '@{u}..HEAD'")]
    (or (not ok) (not= (trim out) "0"))))

(fn pr-ci-tool []
  (or (utils.find (tools.load-all "tools") (fn [t] (= t.name "pr-ci")))
      (error "cannot find the pr-ci tool" 0)))

(fn approvals-check? [ch]
  (= (: (or ch.name "") :lower) approvals-check))

(fn summarize [checks]
  "Returns (values summary failed pending waiting-approval?). The approvals check
is counted in the summary but not in failed or pending."
  (let [counts {}
        failed []
        seen {}]
    (var pending 0)
    (var waiting-approval? false)
    (each [_ ch (ipairs checks)]
      (tset counts ch.bucket (+ 1 (or (. counts ch.bucket) 0)))
      (if (approvals-check? ch)
          (when (not= ch.bucket :pass) (set waiting-approval? true))
          (do
            (when (= ch.bucket :pending) (set pending (+ pending 1)))
            (when (and (= ch.bucket :fail) (not (. seen ch.name)))
              (tset seen ch.name true)
              (table.insert failed ch)))))
    (values (table.concat (icollect [_ [b glyph] (ipairs [[:pass "✓"] [:fail "✗"] [:pending "◒"]])]
                            (when (. counts b) (.. (. counts b) glyph)))
                          " ")
            failed
            pending
            waiting-approval?)))

;; --- Claude ----------------------------------------------------------------------

(fn fix-prompt [c failed attempt]
  (let [listed (icollect [i ch (ipairs failed)]
                 (when (<= i max-listed) (.. "- " ch.name "  " (or ch.link ""))))]
    (.. "The CI checks of this branch's PR are failing. You are in the worktree of "
        c.repo " (branch " c.branch ", ticket " c.ticket "); that is your working directory.\n\n"
        "Failed checks" (if (> (length failed) max-listed) (.. " (first " max-listed " of " (length failed) ")") "")
        ":\n" (table.concat listed "\n") "\n\n"
        "Attempt " attempt " of " max-attempts ". Tasks:\n"
        "1. Look at the logs of the failed checks with `gh run view <run-id> --log-failed` (the run-id is in the link).\n"
        "2. Fix the cause in the code. If the failure is infrastructure-related or flaky and there is nothing to fix in the code, change nothing and reply starting with \"NO CHANGES:\" and the reason.\n"
        "3. If you can, verify locally with lint or targeted tests.\n"
        "4. Stage with `git add <paths>` only the files you touched (never -A or `.`: there are untracked files that are not yours). Commit (message in English, look at `git log -5` to follow the style) and `git push` to this same branch.\n\n"
        "Limits: you can only edit files in this worktree. One command per call: no `&&`, `;`, pipes, redirections or `$`, and none of those characters nor `->` in the commit message. "
        "No force push, --amend or --no-verify. Do not touch anything unrelated to the failures. "
        "Reply briefly: what was failing and what you changed.")))

;; --- watches -------------------------------------------------------------------

(local watches []) ; in start order

(fn stamp []
  (os.date "%H:%M"))

(fn publish []
  (hud.set-box :watch "watch"
               (icollect [_ w (ipairs watches)]
                 (hud.row w.color w.c.ticket w.label))))

(fn wlog [w msg]
  (with-open [f (io.open w.log-path :a)]
    (f:write "[" (stamp) "] " msg "\n")))

(fn set-phase [w color label]
  (set w.color color)
  (set w.label label)
  (publish))

(fn release [w]
  "Stops the timer and the claude of this watch."
  (when w.timer
    (when (not (w.timer:is_closing)) (w.timer:close))
    (set w.timer nil))
  (when w.client
    (w.client.close)
    (set w.client nil)))

(fn finish [w outcome]
  (release w)
  (when (not w.stopped?)
    (set w.done? true)
    (wlog w (.. "end: " outcome))
    (set-phase w (if (outcome:match "^CI en verde") :green :red) outcome)
    (io.write "\7") ; bell: finished
    (io.flush)))

(fn safe [w f]
  "luv callback for `w`: if it fails, the watch ends with the error (not lost silently)."
  (aio.guard (fn [...]
               (let [(ok err) (pcall f ...)]
                 (when (not ok)
                   (if (: (tostring err) :find "interrupted" 1 true)
                       (error err 0)
                       (finish w (.. "error: " (tostring err)))))))))

(fn schedule [w note]
  "Next check in `minutes`; `note` is the latest thing that happened."
  (set-phase w :faint (.. note " · next " (os.date "%H:%M" (+ (os.time) (math.floor (* w.minutes 60))))))
  (w.timer:start (math.floor (* w.minutes 60 1000)) 0 (safe w (fn [] (w.check w)))))

(fn fix [w failed]
  (let [c w.c]
    (set w.attempts (+ w.attempts 1))
    (when (dirty? c.path)
      (error "the worktree was left with uncommitted changes" 0))
    (set-phase w :blue (.. "fixing " w.attempts "/" max-attempts))
    (wlog w (.. "attempt " w.attempts ": " (table.concat (icollect [_ ch (ipairs failed)] ch.name) ", ")))
    (let [before (head c.path)
          ask (fn [client]
                (client.ask (fix-prompt c failed w.attempts) nil
                            (safe w
                              (fn [err answer spent]
                                (if err
                                    (finish w (.. "claude failed: " (tostring err)))
                                    (do
                                      (set w.cost (+ w.cost (or spent 0)))
                                      (wlog w (.. "claude: " answer))
                                      (if (answer:match "^%s*NO CHANGES")
                                          (finish w "Claude found nothing to fix in the code")
                                          (= (head c.path) before)
                                          (finish w "Claude made no commit")
                                          (dirty? c.path)
                                          (finish w "Claude left uncommitted changes")
                                          (unpushed? c.path)
                                          (finish w "Claude committed but the push did not go through")
                                          (schedule w (.. "pushed " w.attempts "/" max-attempts)))))))))
          permission (fn [tool-call]
                       (let [ok (allowed? tool-call c.path c.branch)]
                         (wlog w (.. (if ok "claude runs: " "rejected: ")
                                     (tostring (or (?. tool-call :rawInput :command) tool-call.title tool-call.kind "?"))))
                         ok))]
      (if w.client
          (ask w.client)
          (acp-async.open {:cwd c.path :model claude-model :policy permission}
                          (safe w (fn [err client]
                                       (if err
                                           (finish w (.. "could not open claude: " err))
                                           (do (set w.client client)
                                               (ask client))))))))))

(fn on-ci [w out]
  (let [(ok checks) (pcall (. (pr-ci-tool) :parse) out)]
    (if (not ok)
        (do (set w.errors (+ w.errors 1))
            (wlog w (.. "could not read the CI: " (tostring checks)))
            (if (>= w.errors max-errors)
                (finish w "too many errors reading the CI")
                (schedule w "error reading the CI")))
        (let [(summary failed pending waiting-approval?) (summarize checks)]
          (set w.errors 0)
          (wlog w (.. "CI: " (if (= summary "") "no checks" summary)))
          (if (and (= (length failed) 0) (= pending 0))
              (finish w (if waiting-approval? "CI green, approval pending" "CI green"))
              (> (length failed) 0)
              (if (>= w.attempts max-attempts)
                  (finish w (.. "still failing after " max-attempts " attempts"))
                  (fix w failed))
              (schedule w (.. "CI " summary)))))))

(fn check [w]
  (set-phase w :blue "checking CI")
  (aio.run ((. (pr-ci-tool) :command) {:repo w.c.repo :ticket w.c.ticket})
           (safe w (fn [out] (on-ci w out)))))

(fn find-watch [c]
  (let [i (utils.find-index watches (fn [w] (and (= w.c.repo c.repo) (= w.c.ticket c.ticket))))]
    (when i (values (. watches i) i))))

(fn stop [c]
  (let [(w i) (find-watch c)]
    (when (not w) (error (.. "there is no watch for " c.ticket) 0))
    (set w.stopped? true)
    (release w)
    (table.remove watches i)
    (publish)))

(fn shutdown []
  (each [_ w (ipairs watches)] (release w))
  (while (> (length watches) 0) (table.remove watches))
  (publish))

(fn run [?arg]
  "`:watch [minutes]` starts the watch for the current worktree; `:watch stop` removes it."
  (let [c (or (context.current) (error "first enter a worktree: :wt <repo> <ticket>" 0))]
    (when (not (hud.enabled?))
      (error ":watch only works in the interactive REPL" 0))
    (if (= ?arg "stop")
        (stop c)
        (let [minutes (if ?arg
                          (let [n (tonumber ?arg)]
                            (if (and n (> n 0)) n (error (.. "invalid minutes: " ?arg) 0)))
                          default-minutes)]
          (when (find-watch c)
            (error (.. "there is already a watch for " c.ticket " (:watch stop to remove it)") 0))
          (when (dirty? c.path)
            (error (.. "the worktree has uncommitted changes in tracked files (" c.path "): commit or stash them before :watch") 0))
          (let [w {: c : minutes :attempts 0 :errors 0 :cost 0 :color :blue :label "starting"
                   :timer (uv.new_timer) :check check
                   :log-path (.. "/tmp/noqui-watch-" c.repo "-" c.ticket ".log")}]
            (table.insert watches w)
            (wlog w (.. "watch of " c.repo " " c.ticket " every " minutes " min"))
            (check w)
            w.log-path)))))

{: run : shutdown : allowed?}
