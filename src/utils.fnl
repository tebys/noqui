(fn find [xs pred]
  "The first element of `xs` that satisfies `pred`, or nil."
  (accumulate [found nil _ x (ipairs xs)]
    (or found (when (pred x) x))))

(fn find-index [xs pred]
  "The (1-based) index of the first element of `xs` that satisfies `pred`, or nil."
  (accumulate [found nil i x (ipairs xs)]
    (or found (when (pred x) i))))

(fn any? [xs pred]
  "Whether any element of `xs` satisfies `pred`."
  (accumulate [found false _ x (ipairs xs)]
    (if (or found (pred x)) true false)))

(fn includes? [xs v]
  "Whether `v` is in `xs` (compared with =)."
  (accumulate [found false _ x (ipairs xs)]
    (or found (= x v))))

(fn some [xs f]
  "The first non-nil value of `(f x)` over `xs`, or nil."
  (accumulate [found nil _ x (ipairs xs)]
    (or found (f x))))

(fn trim [s]
  "`s` without leading and trailing whitespace."
  (pick-values 1 (: (s:gsub "^%s+" "") :gsub "%s+$" "")))

(fn lines [s]
  "The non-empty lines of `s`."
  (icollect [line (s:gmatch "[^\n]+")] line))

{: find : find-index : any? : includes? : some : trim : lines}
