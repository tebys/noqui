;; Helpers for the tools that edit a PR's description.
(local json (require :dkjson))
(local sh (require :src.sh))
(local config (require :config))
(local worktrees (require :src.worktrees))

(fn run [args]
  "Runs `gh <args>`; returns the output or fails with it."
  (let [(out code) (sh.run (.. "gh " args))]
    (assert (= code 0) out)
    out))

(fn clean-body [text]
  "Strips the ```markdown ... ``` if the whole response came wrapped in one."
  (let [text (: (text:gsub "^%s+" "") :gsub "%s+$" "")]
    (or (text:match "^```%w*\n(.-)\n```$") text)))

(fn edit-body [repo ticket make-body {: done : previous}]
  "Replaces the description of the PR of the ticket's branch with `(make-body pr)`, where
`pr` is {:number :url :body :state}. First it saves the previous description in /tmp, because
`gh pr edit` overwrites it whole. Only open PRs are edited. With NOQUI_DRY_RUN=1 it does
everything except the edit. `done` and `previous` are the labels of the result."
  (let [wt (worktrees.find repo ticket)
        full (. config.repos repo)
        pr (json.decode (run (.. "pr view " (sh.quote wt.branch) " --repo " (sh.quote full)
                                 " --json number,url,body,state")))
        ;; a merged or closed PR is not touched
        _ (assert (= pr.state "OPEN") (.. "PR " pr.url " is " pr.state ": only editing open PRs"))
        body (make-body pr)
        ;; with date and time: each edit saves its own and none overwrites another
        backup (.. "/tmp/noqui-pr-" pr.number "-" (os.date "%Y%m%d-%H%M%S") ".md")
        tmp (os.tmpname)]
    (with-open [f (io.open tmp :w)]
      (f:write body))
    ;; one line per item: in a single line, the panel cut it off with "…"
    (if (os.getenv "NOQUI_DRY_RUN")
        ;; no backup: nothing is edited
        (do (os.rename tmp "/tmp/noqui-pr-new.md")
            (.. "(dry run) would edit " pr.url "\n"
                "new: /tmp/noqui-pr-new.md"))
        (do (with-open [f (io.open backup :w)]
              (f:write (or pr.body "")))
            (run (.. "pr edit " pr.number " --repo " (sh.quote full) " --body-file " (sh.quote tmp)))
            (os.remove tmp)
            (.. done ": " pr.url "\n"
                previous ": " backup)))))

{: run : clean-body : edit-body}
