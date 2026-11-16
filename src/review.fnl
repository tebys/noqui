;; :review [repo ticket] — senior code review of a PR as a dialogue. Like :dev, you enter, talk and
;; leave (:fin). The PR is gathered once with the tools (jira-ticket, pr-description, branch-diff,
;; branch-patch) and handed to a Claude session that can read the worktree (never edit it). Claude
;; first answers with a list of items (severity + one line); then you explore them: ":d 1",
;; "dismiss 2", "is there anything about tests?". The list lives here, not in Claude: it changes
;; with `:k N` / `:K N`, or with the ```items block Claude adds when you ask it in words.
(local json (require :dkjson))
(local acp (require :src.acp))
(local ui (require :src.ui))
(local tools (require :src.tools))
(local utils (require :src.utils))
(local context (require :src.context))
(local worktrees (require :src.worktrees))
(local sh (require :src.sh))
(local config (require :config))

(local review-model "sonnet")
(local max-retries 1) ; asking again when the list does not come as valid JSON
(local order {:blocking 1 :should 2 :nit 3})
(local glyphs {:blocking ["✗" :red "blocking"] :should ["◐" :yellow "should"] :nit ["·" :faint "nit"]})

(local intro "You are a senior engineer doing a code review of a pull request, as a dialogue with its author. You can read the repository (Read, Grep, Glob, git log/show/blame/diff) to check context, but you cannot edit anything or run anything else.

Look for, by order of importance: (1) bugs and logic errors (conditions, edge cases, null/empty inputs, async and race conditions, errors that are swallowed); (2) security and data safety (missing authorization or tenant checks, injection, PII or secrets in logs, destructive migrations); (3) compatibility and risk (breaking API or schema changes, behaviour callers do not expect, N+1 queries, unbounded loops or payloads); (4) tests: what the change does that is not covered, and tests that would not catch a regression; (5) design and readability. Check that the diff does what the ticket and the description say. The test files are left out of the diff on purpose: you may Read them, but do not report missing tests just because none appear in the diff. Do not comment on style that the formatter or linter catches. Only report what you can support with the diff or the code you read, and say so when you are unsure.

FIRST ANSWER: reply ONLY with JSON, no markdown fence and no text around it:
{\"summary\": \"one or two sentences: what the PR does and your verdict (approve / approve with comments / request changes)\",
 \"items\": [{\"severity\": \"blocking\" | \"should\" | \"nit\", \"title\": \"one line, 90 characters at most\", \"files\": [\"path\" or \"path:12-30\" when you can point at the lines, ...]}]}
Line numbers are those of the file in the worktree exactly as Read shows them; never estimate them from the diff (read the file to get them, or leave the range out). Only real findings, most important first; no detail yet, the author will ask for it item by item.

AFTER THAT you talk in the language of the author's messages. Each message starts with a line like [open: 1, 3 · dismissed: 2]. Answer what is asked, briefly and specifically (file, line, why it matters, a concrete suggestion). Only when the list has to change (the author dismisses or reopens an item, or you found new ones when asked to look further) end your answer with a block like:
```items
{\"dismiss\": [2], \"reopen\": [], \"add\": [{\"severity\": \"should\", \"title\": \"...\", \"files\": [\"...\"]}]}
```
Leave out the keys that are empty. Never mention this block in the text.")

;; --- what Claude may do ---------------------------------------------------------

(local meta-chars "[;&|<>`$\n\r]")
(local git-prefixes ["git log" "git show" "git blame" "git diff" "git grep" "git rev-parse"])

(fn inside? [path dir]
  (and (not (path:find "/.." 1 true))
       (or (= path dir) (= (path:sub 1 (+ (length dir) 1)) (.. dir "/")))))

(fn read-only-policy [dir]
  "Approves only reading inside the worktree (or /tmp) and a few read-only git commands."
  (fn [tool-call]
    (let [paths (let [out []]
                  (each [_ l (ipairs (or tool-call.locations []))] (when l.path (table.insert out l.path)))
                  (each [_ k (ipairs [:file_path :path])]
                    (let [v (?. tool-call :rawInput k)]
                      (when (= (type v) :string) (table.insert out v))))
                  out)
          inside-all? (accumulate [ok true _ p (ipairs paths)]
                        (and ok (or (inside? p dir) (inside? p "/tmp"))))]
      (case tool-call.kind
        (where (or :read :search)) inside-all?
        :think true
        :execute (let [cmd (utils.trim (or (?. tool-call :rawInput :command) ""))]
                   (and (not (cmd:find meta-chars))
                        (utils.any? git-prefixes (fn [p] (or (= cmd p) (= (cmd:sub 1 (+ (length p) 1)) (.. p " ")))))))
        _ false))))

;; --- gathering ------------------------------------------------------------------

(fn find-tool [name]
  (or (utils.find (tools.load-all "tools") (fn [t] (= t.name name)))
      (error (.. "tool " name " not found") 0)))

(fn step [label f]
  "Runs the tool `label` (`f` calls it) showing its step line and what the tool shows of its result,
as in a normal request. Returns the result, or nil if it failed (the review goes on without it)."
  (ui.start label "")
  (let [t0 (ui.now)
        (ok res) (pcall f)]
    (ui.finish (if ok :ok :warn) label (if ok "" (utils.trim (tostring res))) (- (ui.now) t0))
    (when ok
      (let [t (find-tool label)]
        (when t.show
          (let [(shown lines) (pcall t.show res)]
            (when shown (ui.body lines :data))))))
    (when ok res)))

(fn gather [tg]
  "The text with everything about the PR that Claude gets in its first message."
  (let [run (fn [name args inputs] ((. (find-tool name) :run) args inputs))
        {: repo : ticket : number} tg
        which (if number {: repo : number} {: repo : ticket})
        ticket-info (when ticket (step "jira-ticket" #(run :jira-ticket {: ticket} [])))
        pr (step "pr-description" #(run :pr-description which []))
        diff (step "branch-diff" #(run :branch-diff which []))
        _ (assert diff "could not read the PR's changes")
        patch (step "branch-patch" #(run :branch-patch {} [diff]))
        d (json.decode diff)]
    (.. "== Jira ticket ==\n" (or ticket-info "(not available)")
        "\n\n== PR ==\n" (or pr "(not available)")
        "\n\n== Commits ==\n" (table.concat (icollect [_ c (ipairs (or d.commits []))] (.. "- " c)) "\n")
        "\n\n== Changed files ==\n"
        (table.concat (icollect [_ f (ipairs d.files)] (string.format "%s %s (+%d -%d)" f.status f.path f.added f.deleted)) "\n")
        "\n\n== Diff ==\n" (or patch "(not available)"))))

;; --- the list of items ----------------------------------------------------------

(fn normalize [item]
  (when (and (= (type item) :table) (= (type item.title) :string))
    {:severity (if (. order item.severity) item.severity :should)
     :title (item.title:gsub "%s+" " ")
     :files (if (= (type item.files) :table) item.files [])
     :status :open}))

(fn add-items! [items new]
  (let [added []]
    (each [_ raw (ipairs (or new []))]
      (let [it (normalize raw)]
        (when it
          (set it.id (+ (length items) 1))
          (table.insert items it)
          (table.insert added it))))
    added))

(fn sort-new! [list]
  "The first list, from most to least severe (stable)."
  (let [indexed (icollect [i it (ipairs list)] {:i i : it})]
    (table.sort indexed (fn [a b]
                          (let [sa (. order a.it.severity) sb (. order b.it.severity)]
                            (if (= sa sb) (< a.i b.i) (< sa sb)))))
    (icollect [_ x (ipairs indexed)] x.it)))

(fn show-item [it]
  (let [[glyph color] (. glyphs it.severity)
        files (if (> (length it.files) 0)
                  (.. "  " (ui.c :faint (table.concat (icollect [_ f (ipairs it.files)] (f:match "[^/]+$")) ", ")))
                  "")]
    (print (.. (ui.c :faint "│  ")
               (ui.c color glyph) " "
               (ui.c :text (string.format "%-2d" it.id)) " "
               (ui.c (if (= it.status :open) :text :faint) (ui.truncate it.title 90))
               (if (= it.status :open) files (ui.c :faint "  (dismissed)"))))))

(fn show-items [items all?]
  (var shown 0)
  (each [_ it (ipairs items)]
    (when (or all? (= it.status :open))
      (set shown (+ shown 1))
      (show-item it)))
  (when (= shown 0)
    (print (.. (ui.c :faint "│  ") (ui.c :faint "no open items"))))
  shown)

(fn status-line [items]
  (let [ids (fn [status] (table.concat (icollect [_ it (ipairs items)] (when (= it.status status) (tostring it.id))) ", "))
        open (ids :open)
        gone (ids :dismissed)]
    (.. "[open: " (if (= open "") "none" open) (if (= gone "") "" (.. " · dismissed: " gone)) "]")))

(fn parse-json [text]
  "The JSON object in Claude's answer (it may come inside a fence or with text around it), or nil."
  (let [body (or (text:match "```%w*%s*(%b{})%s*```") (text:match "%b{}"))
        data (and body (json.decode body))]
    (when (= (type data) :table) data)))

(fn split-block [answer ?fence]
  "The answer without its ```items (or ?fence) block, and the block decoded (or nil)."
  (let [fence (or ?fence "items")
        block (answer:match (.. "```" fence "%s*(.-)```"))
        text (pick-values 1 (answer:gsub (.. "\n?```" fence ".-```%s*") ""))]
    (values (utils.trim text) (and block (parse-json block)))))

(fn line-count [file]
  (let [f (io.open file)]
    (when f
      (with-open [f f]
        (accumulate [n 0 _ (f:lines)] (+ n 1))))))

(fn valid-ranges [raw wt-path]
  "The ranges Claude gave ({:path :from :to :note}) that exist in the worktree; the rest is dropped."
  (icollect [_ r (ipairs (or raw []))]
    (when (and (= (type r) :table) (= (type r.path) :string) (tonumber r.from) (not (r.path:find "%.%.")))
      (let [n (line-count (.. wt-path "/" r.path))
            from (math.floor (tonumber r.from))
            to (math.floor (or (tonumber r.to) from))]
        (when (and n (<= from n) (<= from to))
          {:path r.path : from :to (math.min to n) :note (if (= (type r.note) :string) r.note "")})))))

(fn apply-changes! [items changes]
  "Applies what Claude's items block says. Returns the items it touched (changed or added)."
  (let [touched []]
  (when changes
    (each [_ [key status] (ipairs [[:dismiss :dismissed] [:reopen :open]])]
      (each [_ id (ipairs (or (. changes key) []))]
        (let [it (. items (tonumber id))]
          (when (and it (not= it.status status))
            (set it.status status)
            (table.insert touched it)))))
    (each [_ it (ipairs (add-items! items (sort-new! (icollect [_ r (ipairs (or changes.add []))] r))))]
      (table.insert touched it)))
  touched))

;; --- the dialogue ---------------------------------------------------------------

(fn ask [session text]
  "Sends `text` to Claude with a waiting line. Returns (values answer cost)."
  (ui.start "claude" "")
  (let [t0 (ui.now)
        (answer cost) (acp.ask session text)]
    (ui.finish :claude "claude" "" (- (ui.now) t0))
    (values answer cost)))

(fn first-list [session text]
  "The first answer: {:summary :items}; asks again if Claude did not reply with valid JSON."
  (var cost 0)
  (var data nil)
  (var message text)
  (for [attempt 0 max-retries &until data]
    (let [(answer c) (ask session message)]
      (set cost (+ cost (or c 0)))
      (set data (parse-json answer))
      (when (not (and data (= (type data.items) :table)))
        (set data nil)
        (set message "Your answer was not valid JSON with an \"items\" list. Reply ONLY with the JSON object, as specified."))))
  (assert data "Claude did not return the list of items")
  (values data cost))

(fn parse-ids [rest]
  (icollect [n (rest:gmatch "%d+")] (tonumber n)))

(fn refs-of [it]
  "The {:path :from :to} of an item's files: `path:12-30`, `path:12` or just `path` (no lines)."
  (icollect [_ f (ipairs it.files)]
    (let [(path a b) (f:match "^(.-):(%d+)%-?(%d*)$")]
      (if path {: path :from (tonumber a) :to (tonumber (if (= b "") a b))} {:path f}))))

(fn show-code [tg it ?n]
  "Prints the lines of the item's ranges (or the ?n-th one), read with the code-range tool. The
ranges come from `describe`; before that, from the files the item lists."
  (let [t (find-tool :code-range)
        all (if it.ranges it.ranges (refs-of it))
        refs (if ?n [(. all ?n)] all)]
    (if (= (length refs) 0)
        (ui.notice :info (if ?n (.. "the item has no range " ?n) "this item points at no files; try: :d N"))
        (each [_ r (ipairs refs)]
          (if r.from
              (let [(ok out) (pcall t.run {:dir tg.path :path r.path :from r.from :to r.to :context 2})]
                (let [state {}]
                  (each [l (: (if ok out (.. r.path ": " (tostring out))) :gmatch "[^\n]+")]
                    (let [(num code) (l:match "^(%s*%d+ │ )(.*)$")]
                      (print (.. (ui.c :faint "│     ")
                                 (if num (.. (ui.c :faint num) (ui.code-line code "ts" state (ui.vwidth num))) (ui.c :subtext l))))))))
              (ui.body [(.. r.path " (no lines given)")] :data))))))

(fn command [items line tg]
  "`:k N…`, `:K N…`, `:i`, `:s N`, `:help`. Returns true if `line` was one of them."
  (let [line (if (line:match "^show%s") (.. ":" line) line) ; "show 1.2" works without the colon too
        (name rest) (line:match "^:(%S+)%s*(.-)%s*$")]
    (case name
      (where (or :k :kill :dismiss)) (do (each [_ id (ipairs (parse-ids rest))]
                                      (let [it (. items id)]
                                        (if it (set it.status :dismissed) (ui.notice :error (.. "no item " id)))))
                                    (show-items items false) true)
      (where (or :K :reopen)) (do (each [_ id (ipairs (parse-ids rest))]
                                    (let [it (. items id)]
                                      (if it (set it.status :open) (ui.notice :error (.. "no item " id)))))
                                  (show-items items false) true)
      (where (or :i :items :list)) (do (show-items items (not= rest "")) true)
      (where (or :show :s)) (do (let [(id n) (rest:match "^(%d+)%.?(%d*)$")
                                      it (. items (or (tonumber id) 0))]
                                  (if it (show-code tg it (tonumber n)) (ui.notice :error (.. "no item " rest))))
                                true)
      :help (do (ui.notice :info ":i [all] items · :d N describe · :s N[.R] code · :k N… dismiss · :K N… reopen · :fin leave · anything else goes to Claude") true)
      (where (or :d :desc :describe)) false ; goes to Claude, in the loop
      nil false
      _ (do (ui.notice :error (if (= name :review)
                                  "already in :review: leave with :fin first"
                                  (.. "unknown command :" name " (:help)")))
            true))))

(fn describe-prompt [id]
  (.. "Describe item " id ": first a summary of the problem (2 to 4 sentences: what is wrong, why it matters, what to do). "
      "Then end your answer with the places to review as a block:\n```ranges\n"
      "{\"ranges\": [{\"path\": \"repo-relative path\", \"from\": 12, \"to\": 30, \"note\": \"what to look at there, a few words\"}]}\n```\n"
      "Line numbers exactly as Read shows them in the worktree (read the file; never estimate from the diff). Only ranges that matter, at most 6."))

(fn show-ranges [ranges]
  (each [i r (ipairs ranges)]
    (print (.. (ui.c :faint "│  ") (ui.c :text (string.format "%d." i)) " "
               (ui.c :text (string.format "%s:%d-%d" r.path r.from r.to))
               (if (= r.note "") "" (.. "  " (ui.c :faint r.note)))))))

(fn show-cost [cost]
  (print (.. (ui.c :faint "│  ") (ui.c :peach (string.format "session so far: US$%.2f" cost)))))

(fn pr-info [repo number]
  (let [(out code) (sh.run (.. "gh pr view " number " --repo " (sh.quote (. config.repos repo))
                               " --json number,headRefName,title"))]
    (assert (= code 0) (.. "PR " number " not found in " repo ": " out))
    (json.decode out)))

(fn pr-of-ticket [repo ticket]
  "The number of the PR (open or not) whose branch or title mentions the ticket."
  (let [(out code) (sh.run (.. "gh pr list --repo " (sh.quote (. config.repos repo)) " --state all --limit 20 --search "
                               (sh.quote (.. ticket " in:title,head")) " --json number,headRefName,title"))
        prs (and (= code 0) (json.decode out))
        t (ticket:lower)
        hit (and prs (utils.find prs (fn [p] (or (: (p.headRefName:lower) :find t 1 true) (: (p.title:lower) :find t 1 true)))))]
    (assert hit (.. "no worktree or PR of " repo " for " ticket))
    hit.number))

(local review-dirs "/tmp/noqui-review-") ; the checkouts kept to review PRs without a worktree
(local trash-dirs "/tmp/noqui-trash-")
(local stale-days 2) ; a checkout nobody has used for this long is thrown away

(fn tidy-up! []
  "Throws away the checkouts nobody has used for a while. Safe to interrupt at any point: first
each one is only renamed (instant) to the trash; the slow deleting happens in the background,
and whatever an interrupted run leaves behind is finished by the next one."
  (sh.run (.. "for d in $(find /tmp -maxdepth 1 -name 'noqui-review-*' -mtime +" stale-days
              "); do mv \"$d\" \"" trash-dirs "$$-$(basename \"$d\")\"; done"))
  (sh.run (.. "nohup sh -c 'rm -rf " trash-dirs "*; "
              (table.concat (icollect [_ main (pairs config.local-repos)] (.. "git -C " (sh.quote main) " worktree prune"))
                            "; ")
              "' >/dev/null 2>&1 &")))

(fn temp-checkout [repo number]
  "A detached worktree in /tmp with the PR's head, to read its code. Returns (values path reused?).
If the PR was already checked out for a previous review it is brought up to date instead of
checked out again."
  (let [main (. config.local-repos repo)
        dir (.. review-dirs repo "-" number)
        no-hooks "-c core.hooksPath=/dev/null " ; the post-checkout hook installs gigabytes
        reusable? (= 0 (select 2 (sh.run (.. "test -e " (sh.quote (.. dir "/.git"))))))]
    (worktrees.git main (.. "fetch origin pull/" number "/head"))
    (let [reused? (and reusable?
                       (= 0 (select 2 (sh.run (.. "git -C " (sh.quote dir) " " no-hooks "checkout --force --detach "
                                                  (sh.quote (utils.trim (worktrees.git main "rev-parse FETCH_HEAD"))))))))]
      (when (not reused?)
        ;; absent or broken: out of the way (instant) and a fresh one
        (sh.run (.. "mv " (sh.quote dir) " " trash-dirs "$$-broken 2>/dev/null"))
        (sh.run (.. "git -C " (sh.quote main) " worktree prune"))
        (worktrees.git main (.. no-hooks "worktree add --detach " (sh.quote dir) " FETCH_HEAD")))
      (sh.run (.. "touch " (sh.quote dir)))
      (values dir reused?))))

(fn resolve [repo ref]
  "What to review: {: repo : label : path : branch : ticket : number}. `ref` is a ticket (its
worktree, or its PR if there is none) or a PR number. A worktree of the PR's branch is used if
there is one (whatever the ref); otherwise the PR is checked out in /tmp (kept for next time)."
  (tidy-up!)
  (let [num (ref:match "^#?(%d+)$")
        found (when (not num) (let [(ok wt) (pcall worktrees.find repo ref)] (and ok wt)))]
    (if found
        {: repo :label ref :ticket ref :path found.path :branch found.branch}
        (let [number (or num (pr-of-ticket repo ref))
              pr (pr-info repo number)
              ticket (or (and (not num) ref) (: (or (pr.headRefName:match "%a+%-%d+") (pr.title:match "%a+%-%d+") "") :upper))
              own (let [(ok wt) (pcall worktrees.find repo pr.headRefName)] (and ok wt))] ; same branch, other name
          (if own
              {: repo :label (.. "#" number) :ticket (and (not= ticket "") ticket) :number pr.number
               :path own.path :branch pr.headRefName}
              (do (ui.start "checkout" (.. "PR #" number))
                  (let [t0 (ui.now)
                        (path reused?) (temp-checkout repo number)]
                    (ui.finish :ok "checkout" (.. "PR #" number (if reused? " (reused) " " ") path) (- (ui.now) t0))
                    {: repo :label (.. "#" number) :ticket (and (not= ticket "") ticket) :number pr.number
                     : path :branch pr.headRefName})))))))

(fn session [tg model]
  (let [wt tg
        ticket tg.label]
    (ui.begin)
    (ui.header "ñoqui :review" (.. tg.repo " " tg.label " · " tg.branch) (.. "claude · " model))
    (ui.guide)
    (let [material (gather tg)
          claude (acp.open-session {:cwd wt.path :model model})
          (data first-cost) (first-list claude (.. intro "\n\n" material))
          items (sort-new! (let [tmp []] (add-items! tmp data.items) tmp))]
      ;; the ids follow the order shown
      (each [i it (ipairs items)] (set it.id i))
      (var cost first-cost)
      (ui.guide)
      (when (and data.summary (= (type data.summary) :string))
        (print (.. (ui.c :faint "│  ") (ui.c :text data.summary))))
      (ui.guide)
      (print (.. (ui.c :faint "│  ") (ui.c :red "✗ blocking") (ui.c :faint " · ") (ui.c :yellow "◐ should") (ui.c :faint " · ") (ui.c :faint "· nit")))
      (show-items items false)
      (ui.guide)
      (show-cost cost)
      (print (.. (ui.c :faint "│  ") (ui.c :faint ":s N[.R] code · :d N describe · :k N dismiss · :K N reopen · :i items · :fin to leave · or ask Claude in words")))
      (var line nil)
      (while (do (ui.prompt (.. "review " ticket))
                 (set line (io.read :l))
                 (and line (not= line ":fin") (not= line ":q")))
        (when (and (line:match "%S") (not (command items line tg)))
          (let [(ok err) (pcall #(let [id (tonumber (or (line:match "^:describe%s+(%d+)%s*$") (line:match "^:d%s+(%d+)%s*$") (line:match "^:desc%s+(%d+)%s*$")))
                                       it (and id (. items id))
                                       message (if it (describe-prompt id) line)
                                       (answer c) (ask claude (.. (status-line items) " " message))
                                       (text changes) (split-block answer (if it :ranges :items))]
                                   (set cost (+ cost (or c 0)))
                                   (ui.guide)
                                   (let [(write done) (ui.stream :final)]
                                     (write text)
                                     (done))
                                   (if it
                                       (let [ranges (valid-ranges (and changes changes.ranges) wt.path)]
                                         (set it.ranges ranges)
                                         (ui.guide)
                                         (if (> (length ranges) 0)
                                             (do (show-ranges ranges)
                                                 (print (.. (ui.c :faint "│  ") (ui.c :faint (.. ":s " id " all · :s " id ".N one")))))
                                             (print (.. (ui.c :faint "│  ") (ui.c :faint "no ranges given")))))
                                       (let [touched (apply-changes! items changes)]
                                         (when (> (length touched) 0)
                                           (ui.guide)
                                           (each [_ x (ipairs touched)] (show-item x)))))
                                   (show-cost cost)))]
            (when (not ok)
              (ui.notice :error (tostring err))))))
      (ui.guide)
      (ui.footer [(ui.c :peach (string.format ":review %s · US$%.2f" ticket cost))]))))

(fn run [?repo ?ref ?model]
  "Enters :review for a repo and a ticket or PR number, or for the worktree of the :wt mode if
they are omitted. ?model (haiku, sonnet, opus, ...) is the Claude model; sonnet by default."
  (let [ctx (context.current)
        repo (or ?repo (and ctx ctx.repo))
        ref (: (or ?ref (and ctx ctx.ticket) "") :upper)]
    (when (or (not repo) (= ref ""))
      (error "usage: :review [model] <repo> <ticket or PR number> (or :wt first)" 0))
    (var tg nil)
    (let [(ok err) (pcall (fn []
                            (set tg (resolve repo ref))
                            (acp.set-policy (read-only-policy tg.path))
                            (session tg (or ?model review-model))))]
      (acp.set-policy nil)
      (when (not ok)
        (acp.stop)
        (error err 0)))))

{: run}
