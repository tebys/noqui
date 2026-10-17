(local ollama (require :src.ollama))
(local tools (require :src.tools))
(local context (require :src.context))

(local schema
  {:type :object
   :properties {:tasks {:type :array
                        :maxItems 8
                        :items {:type :object
                                :properties {:id {:type :integer}
                                             :task {:type :string}
                                             :tool {:type [:string :null]}
                                             :args {:type :object}
                                             :depends_on {:type :array :items {:type :integer}}}
                                :required [:id :task :tool :depends_on]}}}
   :required [:tasks]})

(local weekdays [:sunday :monday :tuesday :wednesday :thursday :friday :saturday])

(fn calendar []
  "Precomputed reference dates: the model only has to copy them."
  (let [t (os.date "*t")
        day (fn [d m] (os.date "%Y-%m-%d" (os.time {:year t.year :month (+ t.month (or m 0)) :day d})))
        monday (- t.day (% (+ t.wday 5) 7))]
    (.. "Today is " (. weekdays t.wday) " " (day t.day) ".
Reference dates (weeks start on Monday):
- last week / the last 7 days: from " (day (- t.day 7)) "
- last month / the last 30 days: from " (day (- t.day 30)) "
- this week: from " (day monday) "
- this month: from " (day 1) "
- the previous week: from " (day (- monday 7)) " to " (day (- monday 1)) "
- the previous month: from " (day 1 -1) " to " (day 0))))

(fn map-hint [available]
  "Explains which tools can be chained per element (e.g. open-prs -> pr-ci)."
  (let [lines (icollect [_ source (ipairs available)]
                (let [targets (icollect [_ t (ipairs available)]
                                (when (tools.map-params t source) t.name))]
                  (when (next targets)
                    (.. "- " source.name " returns a list; each element has "
                        (table.concat source.returns ", ") ". "
                        (table.concat targets ", ")
                        " can be used depending on " source.name
                        " WITHOUT setting those args: it runs once for each element."))))]
    (if (next lines)
        (.. "\n\nChainable tools:\n" (table.concat lines "\n"))
        "")))

(fn context-note []
  "Note for the planner when worktree mode is active."
  (let [c (context.current)]
    (if c
        (.. "\n\nWorktree mode active: repo=" c.repo " ticket=" c.ticket
            ". Tools that ask for repo and ticket fill them in on their own: do NOT put them in args. Do not make up PR numbers either: if the request does not give one, omit it.")
        "")))

(fn system-prompt [available]
  (.. "You are a planner. " (calendar) (context-note) "

Available tools:
" (table.concat (icollect [_ t (ipairs available)] (tools.describe t)) "\n") (map-hint available) "

Split the user's request into atomic subtasks.
- For each subtask, put the tool name in \"tool\" if it fully solves it, or null if none does (those are solved by another model using the results of the previous ones).
- Tools only fetch data. Counting, filtering, comparing or summarizing that data is a separate subtask with tool null that depends on the one that fetches it.
- Do not repeat the same tool with the same args.
- In \"args\" put the tool's parameters. Write relative dates as Nd, Nw or Nm (e.g. \"last week\" = \"1w\", \"the last 10 days\" = \"10d\", \"last month\" = \"1m\"); absolute ones as YYYY-MM-DD. For named periods (the previous week, the previous month) use the reference dates in since and until.
- If a subtask needs the result of another, indicate it in depends_on.
- If the request is a single thing, return a single task.
Reply with JSON only."))

(local step-schema
  {:type :object
   :properties {:tool {:type [:string :null]}
                :args {:type :object}}
   :required [:tool]})

(fn step-prompt [available input? prev]
  (.. "You choose the tool for ONE step of a pipeline. " (calendar) (context-note) "

Available tools:
" (table.concat (icollect [_ t (ipairs available)]
                  (let [mapped (tools.map-params t prev)]
                    (.. (tools.describe t)
                        (if mapped
                            (.. "\n    It runs once for each element of the input ("
                                (table.concat mapped ", ") " come from each element).")
                            ""))))
                "\n") "

"
      (if input?
          "This step receives the result of the previous step as input."
          "This is the first step: it receives no input.")
      "
- In \"tool\" put the tool that does exactly what the step asks, or null if none does.
- In \"args\" put its parameters. Write relative dates as Nd, Nw or Nm (e.g. \"last week\" = \"1w\"); for named periods (the previous week, the previous month) use the reference dates in since and until.
Reply with JSON only."))

(fn split-pipe [s]
  "Splits the request on `->`, except those inside quotes (e.g. an experiment's
prompt, which may itself be a pipe)."
  (let [parts []]
    (var start 1)
    (var quoted? false)
    (var i 1)
    (while (<= i (length s))
      (if (= (s:sub i i) "\"")
          (set quoted? (not quoted?))
          (and (not quoted?) (= (s:sub i (+ i 1)) "->"))
          (do (table.insert parts (s:sub start (- i 1)))
              (set start (+ i 2))
              (set i (+ i 1))))
      (set i (+ i 1)))
    (table.insert parts (s:sub start))
    (icollect [_ p (ipairs parts)] (p:match "^%s*(.-)%s*$"))))

(fn pipe? [prompt]
  (> (length (split-pipe prompt)) 1))

(fn plan-pipe [prompt available]
  "Request written as a pipeline (`a -> b -> c`): the code splits it and the model
only picks the tool for each step, which depends on the previous one."
  (let [by-name (collect [_ t (ipairs available)] t.name t)]
    (var prev nil) ; tool of the previous step (nil if it went to Claude)
    (icollect [i segment (ipairs (split-pipe prompt))]
      ;; the first step fetches data; the following ones process the previous output:
      ;; tools with :input, or that can run for each element the previous step
      ;; returned (e.g. open-prs -> pr-ci)
      (let [input? (> i 1)
            candidates (icollect [_ t (ipairs available)]
                         (when (if input?
                                   (or (and t.input (tools.accepts? t prev))
                                       (tools.map-params t prev))
                                   (not t.input))
                           t))
            step (ollama.chat [{:role :system :content (step-prompt candidates input? prev)}
                               {:role :user :content segment}]
                              step-schema)]
        (let [chosen (. by-name (tostring step.tool))]
          ;; after a :tee (debug) the next step still sees the earlier output
          (when (not (and chosen chosen.tee))
            (set prev chosen)))
        {:id i :task segment :tool step.tool :args step.args
         :depends_on (if (> i 1) [(- i 1)] [])}))))

(fn plan [prompt available]
  (if (pipe? prompt)
      (plan-pipe prompt available)
      ;; tools that process input are only used in pipes: in natural language
      ;; the 8b does not tell well when they apply (e.g. it used count to filter)
      (. (ollama.chat [{:role :system
                        :content (system-prompt (icollect [_ t (ipairs available)]
                                                  (when (not t.input) t)))}
                       {:role :user :content prompt}]
                      schema)
         :tasks)))

{: plan : pipe?}
