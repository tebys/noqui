;; Hot reload of the files in a directory (tools/, snippets/): each file is a Fennel
;; module that returns one item. Only what changed since the last time is recompiled.
;;   (local load (hotload.make (fn [item] [name ...])))   ; names-of: how an item is reported
;;   (load "tools")  ; -> list of items
(local fennel (require :fennel))
(local uv (require :luv))
(local ui (require :src.ui))

(fn fs-iter [dir]
  "Iterator over the names in `dir` (thin layer over luv, which also runs on macOS)."
  (let [h (uv.fs_scandir dir)]
    (when h (fn [] (uv.fs_scandir_next h)))))

(fn make [names-of]
  "A loader with its own cache (path -> {: mtime : item}). Calling it with a directory
loads the items from it, reloading the ones that changed and reporting what happened.
If a file does not compile the previous version keeps being used."
  (let [cache {}]
    (fn [dir]
      (let [first? (= (next cache) nil)
            seen {}
            loaded []
            files (icollect [name (or (fs-iter dir) (fn [] nil))]
                    (when (name:match "%.fnl$")
                      (let [path (.. dir "/" name)
                            st (uv.fs_stat path)]
                        (when (and st (= st.type :file))
                          {:path path :mtime st.mtime.sec}))))]
        (each [_ f (ipairs files)]
          (tset seen f.path true)
          (let [cached (. cache f.path)]
            (when (or (not cached) (not= cached.mtime f.mtime))
              (let [(ok item) (pcall fennel.dofile f.path)]
                (if ok
                    (do (tset cache f.path {:mtime f.mtime : item})
                        (each [_ n (ipairs (names-of item))]
                          (table.insert loaded (.. (if (or first? cached) "" "+") n))))
                    (let [(msg) (string.gsub (let [m (tostring item)] (or (m:match "^(.-)\n?stack traceback:") m)) "\27%[[%d;]*m" "")]
                      (ui.notice :error (.. (pick-values 1 (msg:gsub "%s+$" ""))
                                            (if cached "  (still using the previous version)" "")))
                      ;; the mtime is saved so the error is not repeated until you touch it again
                      (when cached (set cached.mtime f.mtime))))))))
        (each [path c (pairs cache)]
          (when (not (. seen path))
            (tset cache path nil)
            (ui.notice :error (.. (table.concat (names-of c.item) ", ") " removed"))))
        ;; the first load does not report: the REPL banner already shows the tools
        (when (and (next loaded) (not first?))
          (ui.notice :reload (table.concat loaded ", ")))
        (icollect [_ c (pairs cache)] c.item)))))

{: make}
