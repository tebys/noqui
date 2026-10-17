(fn check-path [path]
  "File tools only touch files inside the project."
  (when (or (path:match "^/") (path:match "^~") (path:match "%.%.")
            (not (path:match "%S")))
    (error (.. "only files inside the project (relative path, no ..): " path) 0))
  path)

(fn open [path mode]
  "Opens `path` with `mode` (`:r`, `:w`, `:a`, etc.). Returns a handle or nil."
  (io.open path (or mode :r)))

(fn read [path]
  "Reads the whole file `path`. Returns a string or nil if it does not exist."
  (case (io.open path :r)
    f (with-open [f f] (f:read :a))
    nil nil))

(fn write [path text]
  "Writes the whole `text` to `path` (overwrites)."
  (with-open [f (io.open path :w)]
    (f:write text)))

(fn append [path text]
  "Appends `text` to the end of `path` (creates it if it does not exist)."
  (with-open [f (io.open path :a)]
    (f:write text)))

{: check-path : open : read : write : append}
