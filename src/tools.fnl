(local hotload (require :src.hotload))
(local context (require :src.context))
(local utils (require :src.utils))

(local loader (hotload.make (fn [tool] [tool.name])))

(fn load-all [dir]
  "Loads the tools from `dir` (hot reload: see src.hotload), sorted by name: the order
of `pairs` changes between runs, and with it the planner prompt."
  (doto (loader dir)
    (table.sort #(< $1.name $2.name))))

(fn describe [tool]
  "Description of the tool for the planner prompt."
  (let [params (icollect [_ p (ipairs tool.params)]
                 (.. "    " p.name (if p.required "" " (optional)") ": " p.doc))]
    (.. "- " tool.name ": " tool.description
        (if tool.input " Receives as input the result of the subtasks it depends on." "")
        (if tool.text " Receives the request text as is: there is no need to copy it into args." "")
        "\n" (table.concat params "\n"))))

(fn normalize-date [s today]
  "Accepts YYYY-MM-DD or relative dates (Nd, Nw, Nm) and returns YYYY-MM-DD."
  (let [(n unit) (s:match "^(%d+)([dwm])$")]
    (if (s:match "^%d%d%d%d%-%d%d%-%d%d$")
        (if (> s (os.date "%Y-%m-%d" today))
            (error (.. "date in the future: " s) 0)
            s)
        n
        (let [t (os.date "*t" today)
              n (tonumber n)]
          (case unit
            :d (set t.day (- t.day n))
            :w (set t.day (- t.day (* 7 n)))
            :m (set t.month (- t.month n)))
          (os.date "%Y-%m-%d" (os.time t)))
        (error (.. "invalid date: " s) 0))))

(fn accepts? [tool source]
  "Whether `tool` can process the output of `source` (a tool, or nil if it comes
from Claude). Without :accepts, it accepts anything."
  (or (not tool.accepts)
      (and source (accumulate [ok false _ name (ipairs tool.accepts)] (or ok (= name source.name))))))

(fn map-params [tool source]
  "If `tool` can be run once for each element that `source` returns
(its required params all come in source's :returns), returns
those params. Otherwise, nil."
  (when (and source source.returns (not tool.input))
    (let [returned (collect [_ f (ipairs source.returns)] f true)
          required (icollect [_ p (ipairs tool.params)] (when p.required p.name))]
      (when (and (next required)
                 (accumulate [ok true _ r (ipairs required)] (and ok (. returned r))))
        ;; the optional ones that also come in each element (e.g. number in pr-ci)
        (each [_ p (ipairs tool.params)]
          (when (and (not p.required) (. returned p.name))
            (table.insert required p.name)))
        required))))

(fn validate [tool args ?mapped]
  "Checks and normalizes the args the planner proposed. Raises an error if something does not add up.
The params in `?mapped` are ignored: they come from each element of the input."
  (let [out {}
        mapped (collect [_ m (ipairs (or ?mapped []))] m true)
        ctx (or (context.defaults-for tool) {})]
    (each [_ p (ipairs tool.params)]
      (let [v (. args p.name)
            v (if (or (= v "") (= v "null") (. mapped p.name)) nil v)
            v (if (= v nil) (. ctx p.name) v)]
        (if (. mapped p.name)
            nil
            (= v nil)
            (when p.required
              (error (.. tool.name ": missing " p.name) 0))
            (tset out p.name
                  (case p.type
                    :string (let [v (tostring v)]
                              (if (or (not p.enum)
                                       (utils.any? p.enum (fn [e] (= e v))))
                                  v
                                  ;; optional with a made-up value (e.g. repo=all): the default is used
                                  (not p.required)
                                  nil
                                  (error (.. tool.name ": " p.name " invalid: " v) 0)))
                    :integer (let [n (math.tointeger (tonumber v))]
                               (or n (error (.. tool.name ": " p.name " is not an integer: " (tostring v)) 0)))
                    :date (normalize-date (tostring v) (os.time)))))))
    out))

{: load-all : describe : accepts? : map-params : validate}
