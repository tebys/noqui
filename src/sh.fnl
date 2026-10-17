;; `quote` is a fennel special form, which is why the local name is different
(fn shell-quote [s]
  (let [(escaped) (string.gsub (tostring s) "'" "'\\''")]
    (.. "'" escaped "'")))

(fn run [cmd]
  "Runs cmd in a shell. Returns the output (stdout + stderr) and the exit code."
  (let [f (io.popen (.. cmd " 2>&1"))
        out (f:read :a)
        (ok _ code) (f:close)]
    (values out (if ok 0 code))))

(fn kill-tree [pid]
  "Kills the process `pid` and all its descendants."
  (run (.. "kill_tree() { for c in $(pgrep -P $1); do kill_tree $c; done; kill -TERM $1 2>/dev/null; }; "
           "kill_tree " pid)))

{:quote shell-quote : run : kill-tree}
