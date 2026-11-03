;; :watch[!] [minutes] | :watch stop — inside :wt mode, checks the PR's CI every so many
;; minutes, in the background (luv): the REPL stays free and the status shows in a
;; little box below the :claude one. As soon as a check has failed (even if others are
;; still running) and its log is available, the watch fetches it and asks Claude (sonnet, its own claude-agent-acp) to fix
;; them. Claude only reads and edits inside the worktree (`allowed?`): it never commits.
;;   :watch   stops after Claude's edit and tells you to review and commit.
;;   :watch!  the watch commits and pushes, and keeps watching.
;; It stops when everything passes, after max-attempts fixes, or if Claude changes
;; nothing; the result stays in the box until `:watch stop`. Details go to
;; /tmp/noqui-watch-<repo>-<ticket>.log.
(local acp-async (require :src.acp-async))
(local aio (require :src.aio))
(local hud (require :src.hud))
(local uv (require :luv))
(local context (require :src.context))
(local tools (require :src.tools))
(local worktrees (require :src.worktrees))
(local utils (require :src.utils))
(local sh (require :src.sh))
(local config (require :config))

(local default-minutes 5)
(local max-attempts 3)
(local max-errors 3)        ; consecutive gh failures before giving up
(local max-listed 15)       ; failed checks reported to Claude
(local max-runs 2)          ; runs whose failed logs are fetched for Claude
(local log-lines 300)       ; last lines of the log when no failure marker is found
(local log-chars 15000)     ; at most these characters per run
(local log-before 3)        ; lines of context before / after each failure marker
(local log-after 25)
(local failure-markers ["Failed Tests" "FAIL " "AssertionError" "##[error]"]) ; strong ones: "Error:" also matches the noise of passing tests
(local claude-model "sonnet")
(local approval-checks {"require approvals" true "require reviews" true}) ; resolved by a person: neither waited for nor fixed

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

(fn command-ok? [cmd]
  (let [cmd (trim cmd)]
    (and (not (cmd:find meta-chars))
         (not (cmd:find "--output" 1 true))
         (or (accumulate [ok false _ p (ipairs safe-prefixes)] (or ok (starts? cmd p)))
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

(fn allowed? [tool-call dir]
  "Whether to approve what Claude asks for: reading inside the worktree (or /tmp), editing
only inside the worktree, and the read-only commands on the closed list. Anything else
(including git add/commit/push: the watch does those) no."
  (let [kind tool-call.kind
        paths (paths-of tool-call)
        all-inside? (fn [dirs]
                      (accumulate [ok true _ p (ipairs paths)]
                        (and ok (accumulate [any false _ d (ipairs dirs)] (or any (inside? p d))))))]
    (case kind
      :execute (and (= (type (?. tool-call :rawInput :command)) :string)
                    (command-ok? tool-call.rawInput.command))
      :edit (and (all-inside? [dir])
                 (not (accumulate [bad false _ p (ipairs paths)] (or bad (p:find "/.git/" 1 true)))))
      (where (or :read :search)) (all-inside? [dir "/tmp"])
      :think true
      _ false)))

;; --- worktree and CI --------------------------------------------------------------

(fn dirty? [dir]
  (not= "" (trim (worktrees.git dir "status --porcelain --untracked-files=no"))))

(fn untracked [dir]
  "Set of untracked files (not ignored) of the worktree."
  (let [files {}]
    (each [f (: (worktrees.git dir "ls-files --others --exclude-standard") :gmatch "[^\n]+")]
      (tset files f true))
    files))

(fn edited-files [dir before]
  "Files Claude edited: tracked ones with changes plus untracked ones that were not
there before (`before`, from `untracked`). The other untracked files are not Claude's."
  (let [files []]
    (each [f (: (worktrees.git dir "diff --name-only HEAD") :gmatch "[^\n]+")]
      (table.insert files f))
    (each [f _ (pairs (untracked dir))]
      (when (not (. before f)) (table.insert files f)))
    (table.sort files)
    files))

(fn commit-message [c answer]
  (let [msg (answer:match ".*COMMIT:%s*([^\n]+)")]
    (if (and msg (not= (trim msg) ""))
        (: (trim msg) :sub 1 200)
        (.. c.ticket " fix CI failures"))))

(fn pr-ci-tool []
  (or (utils.find (tools.load-all "tools") (fn [t] (= t.name "pr-ci")))
      (error "cannot find the pr-ci tool" 0)))

(fn approvals-check? [ch]
  (. approval-checks (: (or ch.name "") :lower)))

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

(fn fix-prompt [c failed attempt logs]
  (let [listed (icollect [i ch (ipairs failed)]
                 (when (<= i max-listed) (.. "- " ch.name "  " (or ch.link ""))))]
    (.. "The CI checks of this branch's PR are failing. You are in the worktree of "
        c.repo " (branch " c.branch ", ticket " c.ticket "); that is your working directory.\n\n"
        "Failed checks" (if (> (length failed) max-listed) (.. " (first " max-listed " of " (length failed) ")") "")
        ":\n" (table.concat listed "\n") "\n\n"
        "Attempt " attempt " of " max-attempts ". Tasks:\n"
        "1. The logs of the failed checks (the lines around the failures in `gh run view --log-failed`) are below, already fetched: do not look for them. Each one is only an excerpt: if it does not show the failing test, assertion or stack trace, search the full log file named in its header with Grep/Read (do not use shell commands for that).\n"
        "2. Fix the cause in the code. If the failure is infrastructure-related or flaky and there is nothing to fix in the code, change nothing and reply starting with \"NO CHANGES:\" and the reason.\n"
        "3. Only if tools are already available in this worktree, verify locally with lint or targeted tests. Do not try to run formatters (oxfmt, prettier) or other tools from other checkouts, and do not try to install anything. If you cannot verify, that is NOT a reason to stop: make your best fix anyway; CI will check it, and the watch will bring you back if it still fails.\n"
        "4. Do NOT run git add, commit or push: the watch does that. Leave your edits in the working tree.\n\n"
        "Limits: you can only edit files in this worktree. One command per call: no `&&`, `;`, pipes, redirections or `$`. "
        "Do not touch anything unrelated to the failures. "
        "Reply briefly: what was failing and what you changed. End with a last line `COMMIT: <one-line commit message in English, in the style of `git log -5`>`.\n\n"
        "Failed logs:\n" (if (= logs "") "(could not fetch the logs)" logs))))

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

(fn finish [w outcome ?color]
  (release w)
  (when (not w.stopped?)
    (set w.done? true)
    (wlog w (.. "end: " outcome))
    (set-phase w (or ?color (if (outcome:match "^CI green") :green :red)) outcome)
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

(fn after-claude [w answer]
  "Claude left its edits in the working tree. With :watch the user reviews and commits;
with :watch! it is the watch who commits, pushes and goes on watching."
  (let [c w.c
        files (edited-files c.path w.untracked-before)
        msg (commit-message c answer)]
    (wlog w (.. "edited: " (table.concat files " ") " | message: " msg))
    (if (= (length files) 0)
        (finish w "Claude made no changes")
        (not w.auto?)
        (finish w (.. "Claude edited " (length files) " file(s): review and commit (" msg ")") :yellow)
        (do
          (set-phase w :blue (.. "committing " w.attempts "/" max-attempts))
          (aio.run (.. "git -C " (sh.quote c.path) " add -- " (table.concat (icollect [_ f (ipairs files)] (sh.quote f)) " ")
                       " && git -C " (sh.quote c.path) " commit -m " (sh.quote msg)
                       " && git -C " (sh.quote c.path) " push -u origin " (sh.quote c.branch))
                   (safe w (fn [out code]
                             (wlog w (.. "commit/push: code " (tostring code) "\n" out))
                             (if (= code 0)
                                 (schedule w (.. "pushed " w.attempts "/" max-attempts))
                                 (finish w (.. "commit or push failed: " (or (out:match "[^\n]+$") "see the log")))))))))))

(fn start-fix [w failed logs]
  (let [c w.c]
    (set-phase w :blue (.. "fixing " w.attempts "/" max-attempts))
    (set w.untracked-before (untracked c.path))
    (let [ask (fn [client]
                (client.ask (fix-prompt c failed w.attempts logs) nil
                            (safe w
                              (fn [err answer spent]
                                (if err
                                    (finish w (.. "claude failed: " (tostring err)))
                                    (do
                                      (set w.cost (+ w.cost (or spent 0)))
                                      (wlog w (.. "claude: " answer))
                                      (if (answer:match "^%s*NO CHANGES")
                                          (finish w "Claude found nothing to fix in the code")
                                          (after-claude w answer))))))))
          permission (fn [tool-call]
                       (let [ok (allowed? tool-call c.path)]
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

(fn log-targets [failed by-job?]
  "What to fetch logs of, without repeats: the failed jobs (from their links) if `by-job?`,
else the runs of the failed checks. Each is {:id .. :job? ..}."
  (let [targets []
        seen {}]
    (each [_ ch (ipairs failed)]
      (let [link (or ch.link "")
            (run job) (link:match "/actions/runs/(%d+)/job/(%d+)")
            run (or run (link:match "/actions/runs/(%d+)"))
            id (if (and by-job? job) job run)]
        (when (and id (not (. seen id)))
          (tset seen id true)
          (table.insert targets {: id :job? (and by-job? job true)}))))
    targets))

(fn failure-line? [line]
  (accumulate [hit false _ m (ipairs failure-markers) &until hit]
    (and (line:find m 1 true) true)))

(fn relevant-log [text]
  "Failures are usually followed by minutes of teardown, so the end of the log is not
enough: keeps the lines around the failure markers (without consecutive repeats) and falls
back to the end of the log when there are none."
  (let [lines (icollect [l (text:gmatch "[^\n]*")] l)
        keep {}]
    (each [i l (ipairs lines)]
      (when (failure-line? l)
        (for [j (math.max 1 (- i log-before)) (math.min (length lines) (+ i log-after))]
          (tset keep j true))))
    (let [out [] ]
      (var last nil)
      (var size 0)
      (each [i l (ipairs lines)]
        (let [bare (l:gsub "^%S+Z " "")]
          (when (and (. keep i) (not= bare last) (< size log-chars))
            (table.insert out l)
            (set size (+ size (length l) 1))
            (set last bare))))
      (if (= (length out) 0)
          (let [tail (icollect [i l (ipairs lines)] (when (> i (- (length lines) log-lines)) l))
                joined (table.concat tail "\n")]
            (joined:sub (math.max 1 (- (length joined) log-chars -1))))
          (table.concat out "\n")))))

(fn save-log [w id text]
  "Writes the whole log to /tmp (Claude may read it there); returns the path or nil."
  (let [path (.. "/tmp/noqui-watch-" id ".log")
        f (io.open path "w")]
    (when f
      (f:write text)
      (f:close)
      path)))

(fn fetch-logs [w failed by-job? cb]
  "Fetches (without blocking) the end of the failed logs of the first targets and calls
(cb text). It is the watch who runs the commands: Claude receives the result. With the
run still in progress `gh run view --log-failed` gives nothing, but the log of a job that
has already finished is available through the API (`by-job?`)."
  (let [targets (log-targets failed by-job?)
        repo (. config.repos w.c.repo)
        parts []]
    (fn step [i]
      (if (or (> i (length targets)) (> i max-runs))
          (cb (table.concat parts "\n\n"))
          (let [{: id : job?} (. targets i)
                source (if job?
                           (.. "gh api --allow-escape-sequences " (sh.quote (.. "repos/" repo "/actions/jobs/" id "/logs")))
                           (.. "gh run view " id " --repo " (sh.quote repo) " --log-failed"))]
            (aio.run (.. source " | perl -pe 's/\\e\\[[0-9;]*[A-Za-z]//g'")
                     (safe w (fn [raw code]
                               (let [out (if (= code 0) (relevant-log raw) "")
                                     file (when (and (= code 0) (not= (trim raw) "")) (save-log w id raw))]
                                 (wlog w (.. "logs of " (if job? "job " "run ") id ": " (length raw) " -> " (length out) " chars, code " (tostring code) (if file (.. ", full log in " file) "")))
                                 (when (not= (trim out) "")
                                   (table.insert parts (.. "== " (if job? "job " "run ") id " ==\n"
                                                           (if file (.. "(excerpt; the FULL log is the file " file ": search it with Grep for FAIL, AssertionError, or the test file names of this branch, and Read the lines around)\n") "")
                                                           out))))
                               (step (+ i 1))))))))
    (step 1)))

(fn fix [w failed pending summary]
  "With checks still pending, tries the logs of the jobs that already failed; if none is
available yet it waits for the next check without using up an attempt."
  (when (dirty? w.c.path)
    (error "the worktree was left with uncommitted changes" 0))
  (let [attempt (+ w.attempts 1)]
    (set-phase w :blue (.. "reading logs " attempt "/" max-attempts))
    (fetch-logs w failed (> pending 0)
                (fn [logs]
                  (if (and (= logs "") (> pending 0))
                      (do (wlog w "no logs yet, waiting")
                          (schedule w (.. "CI " summary)))
                      (do (set w.attempts attempt)
                          (wlog w (.. "attempt " attempt ": " (table.concat (icollect [_ ch (ipairs failed)] ch.name) ", ")))
                          (start-fix w failed logs)))))))

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
              ;; do not wait for the pending checks: the jobs that already failed have logs
              (> (length failed) 0)
              (if (>= w.attempts max-attempts)
                  (finish w (.. "still failing after " max-attempts " attempts"))
                  (fix w failed pending summary))
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

(fn run [?arg ?auto]
  "`:watch [minutes]` starts the watch for the current worktree (Claude edits, the user
commits); `:watch! [minutes]` also commits and pushes; `:watch stop` removes it."
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
          (let [w {: c : minutes :auto? ?auto :attempts 0 :errors 0 :cost 0 :color :blue :label "starting"
                   :timer (uv.new_timer) :check check
                   :log-path (.. "/tmp/noqui-watch-" c.repo "-" c.ticket ".log")}]
            (table.insert watches w)
            (wlog w (.. "watch" (if ?auto "!" "") " of " c.repo " " c.ticket " every " minutes " min"))
            (check w)
            w.log-path)))))

{: run : shutdown : allowed?}
