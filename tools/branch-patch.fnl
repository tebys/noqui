(import-macros {: deftool} :src.macros)
(local json (require :dkjson))
(local config (require :config))
(local sh (require :src.sh))
(local {: git} (require :src.worktrees))

(fn split-diff [diff]
  "The unified diff as a map path -> text of that file."
  (let [files {}]
    (var current nil)
    (each [line (diff:gmatch "[^\n]*\n?")]
      (when (line:match "^diff %-%-git ")
        (set current (or (line:match " b/(.-)\n?$") "?"))
        (tset files current ""))
      (when current (tset files current (.. (. files current) line))))
    files))

(fn raw-diff [d]
  "The whole diff of what branch-diff returned: the PR's from GitHub if it has one, or the
branch's against its base in the worktree."
  (if d.pr
      (let [(out code) (sh.run (.. "gh pr diff " d.pr " --repo " (sh.quote (. config.repos d.repo)) " --color never"))]
        (assert (= code 0) out)
        out)
      (do (assert d.worktree "branch-diff gave neither a PR nor a worktree to read the diff from")
          (git d.worktree (.. "diff --no-color " (sh.quote d.base) "...HEAD")))))

;; a test is recognised by its name (x.test.ts, x.spec.js, x_test.rb) or by a folder of exactly
;; that name; nothing that merely contains the word (contest.ts, latest.ts, test-utils/ ...)
(local test-dirs {:test true :tests true :__tests__ true :__mocks__ true :__fixtures__ true
                  :fixtures true :e2e true :spec true :specs true})

(fn test-file? [path]
  (let [name (path:match "[^/]+$")]
    (or (name:match "%.test%.[%w]+$") (name:match "%.spec%.[%w]+$")
        (name:match "_test%.[%w]+$") (name:match "^test_[%w_]+%.[%w]+$")
        (let [dirs (path:match "^(.*)/[^/]+$")]
          (and dirs (accumulate [hit false seg (dirs:gmatch "[^/]+") &until hit]
                      (. test-dirs seg)))))))

(local max-chars 150000) ; the patch that is returned; the files that do not fit are only listed
(local skipped ["pnpm%-lock%.yaml$" "package%-lock%.json$" "yarn%.lock$" "%.snap$" "%.min%.js$" "%.map$"])

(fn skipped? [path]
  (accumulate [hit false _ p (ipairs skipped) &until hit]
    (and (path:find p) true)))

(fn reason [path text size]
  "Why a file's diff is not given, or nil if it is."
  (if (skipped? path) "lockfile/generated"
      (test-file? path) "test file"
      (not text) "no patch: binary or too big for GitHub"
      (> (+ size (length text)) max-chars) "over the size cap"))

(deftool branch-patch
 {:short "the actual diff of the files branch-diff returned"
 :description "Takes the changes returned by branch-diff and returns the code that changed (unified diff with context) of those files, as GitHub shows it for the PR. Test files are left out, and so are lockfiles, snapshots and generated files, and whatever goes beyond ~150k characters, are only listed by name. Read-only."
 :input true
 :accepts [:branch-diff]
 :params []
 :run (fn [_ inputs]
        (let [d (json.decode (. inputs 1))
              patches (split-diff (raw-diff d))
              parts []
              left []]
          (var size 0)
          (each [_ f (ipairs d.files)]
            (let [text (. patches f.path)]
              (if (reason f.path text size)
                  (table.insert left (.. f.path "  (" (reason f.path text size) ")"))
                  (do (table.insert parts text)
                      (set size (+ size (length text)))))))
          (.. (table.concat parts)
              (if (> (length left) 0)
                  (.. "\n== Not shown ==\n" (table.concat left "\n") "\n")
                  ""))))
 :show (fn [out]
         (let [shown (out:match "^(.-)\n== Not shown ==") ; the diff, without the list at the end
               files (icollect [p (: (or shown out) :gmatch "diff %-%-git a/%S+ b/(%S+)")] p)
               left (icollect [l (: (or (out:match "== Not shown ==\n(.*)") "") :gmatch "[^\n]+")] (.. "not shown: " l))]
           (let [lines (icollect [_ p (ipairs files)] p)]
             (table.insert lines 1 (.. (length files) " files, " (length out) " characters of diff"))
             (each [_ l (ipairs left)] (table.insert lines l))
             lines)))})
