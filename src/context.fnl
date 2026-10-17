;; Worktree mode (:wt repo ticket): while it is active, the tools that
;; work on a worktree (those with a `ticket` param) take repo and
;; ticket from here if the request does not bring them. Leave with :fin.
(local config (require :config))
(local worktrees (require :src.worktrees))
(local utils (require :src.utils))

(local state {:current nil})

(fn enter [repo ticket]
  "Activates the mode. Fails if the repo does not exist or there is no worktree for the ticket."
  (when (not (and repo ticket))
    (error "usage: :wt <repo> <ticket>" 0))
  (when (not (accumulate [ok false _ r (ipairs config.repo-names)] (or ok (= r repo))))
    (error (.. "invalid repo: " repo " (" (table.concat config.repo-names ", ") ")") 0))
  (let [ticket (ticket:upper)
        wt (worktrees.find repo ticket)]
    (set state.current {: repo : ticket :path wt.path :branch wt.branch})
    state.current))

(fn leave []
  (let [was state.current]
    (set state.current nil)
    was))

(fn current [] state.current)

(fn label []
  (when state.current (.. "wt " state.current.ticket)))

(fn defaults-for [tool]
  "The {:repo :ticket} values that apply to `tool`, or nil if there is no mode or the
tool does not work on a worktree."
  (when (and state.current
              (utils.any? tool.params (fn [p] (= p.name :ticket))))
    {:repo state.current.repo :ticket state.current.ticket}))

{: enter : leave : current : label : defaults-for}
