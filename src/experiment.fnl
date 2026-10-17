(fn seconds [t]
  "\"14s\" / \"1m52s\" / \"2m\" -> seconds, or nil if it cannot be parsed."
  (let [t (t:gsub "%s" "")
        (m s) (t:match "^(%d+)m(%d+)s$")]
    (if m (+ (* 60 (tonumber m)) (tonumber s))
        (t:match "^%d+s$") (tonumber (t:match "^(%d+)"))
        (t:match "^%d+m$") (* 60 (tonumber (t:match "^(%d+)")))
        nil)))

{: seconds}
