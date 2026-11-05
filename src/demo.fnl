;; Demo mode, for recording videos: it alters ONLY what is shown on screen.
;; Tool results, commands and what Claude receives keep the real data, so a write
;; (pr-desc!, push-pr...) never ends up with fake text.
;;   - ticket keys: SOX-98770 -> DEMO-xxxx (same key, same fake, even across sessions)
;;   - title / summary / description / body fields of a tool's JSON: fake text
;;   - people: author / assignee / reporter / owners... fields of a tool's JSON, the
;;     user of config.fnl ($USER too, and `:demo-names` for others) and @handles
;; What it does NOT cover: text Claude writes (only the keys in it are masked) and
;; code in diffs.

(local json (require :dkjson))
(local config (require :config))

(var enabled? (and (os.getenv "NOQUI_DEMO") true))

(local fake-titles
  ["Add pagination to the activity feed"
   "Fix stale cache after a settings update"
   "Handle empty states in the dashboard widgets"
   "Retry failed webhook deliveries with backoff"
   "Validate uploaded files before processing"
   "Reduce query count on the overview page"
   "Normalize date formats in exported reports"
   "Prevent duplicate notifications on resubmit"
   "Show a clear error when a sync times out"
   "Refactor permission checks into a shared helper"
   "Improve loading state of the search results"
   "Keep filters when navigating back to a list"])

(local fake-descriptions
  ["Users reported this behavior on the main page after the last release."
   "Steps to reproduce: open the page, change a value and reload."
   "Expected: the change is kept. Actual: the previous value comes back."
   "The fix should include a test and must not change the public API."
   "Related to an earlier change; see the linked discussion for context."
   "Acceptance: it works for new and existing records."])

(local text-fields {:title true :summary true :description true :body true})

(local fake-people
  [["Dexter Morgan" "dmorgan"] ["Debra Morgan" "dmorgan2"] ["Rita Bennett" "rbennett"]
   ["Angel Batista" "abatista"] ["Vince Masuka" "vmasuka"] ["Maria LaGuerta" "mlaguerta"]
   ["Harry Morgan" "hmorgan"] ["Joey Quinn" "jquinn"] ["Lila Tournay" "ltournay"]
   ["Miguel Prado" "mprado"] ["Arthur Mitchell" "amitchell"] ["Hannah McKay" "hmckay"]
   ["Cody Bennett" "cbennett"] ["Astor Bennett" "abennett"] ["James Doakes" "jdoakes"]
   ["Louis Greene" "lgreene"]])

(local people-fields {:author true :assignee true :reporter true :creator true
                      :reviewer true :reviewers true :requester true :committer true
                      :owner true :owners true :user true :login true :displayName true})

;; real name -> fake name, filled in as people appear in results
(local known {})

(fn hash [s]
  (accumulate [h 7 ch (s:gmatch ".")] (% (+ (* h 31) (ch:byte)) 1000003)))

(fn key-pattern []
  (.. "%f[%w]" (or config.jira-project "SOX") "%-%d+"))

(fn fake-key [key]
  "Same number of digits as the real key, so the columns still line up."
  (let [n (length (key:match "%d+$"))
        low (^ 10 (- n 1))]
    (.. "DEMO-" (math.tointeger (+ low (% (hash key) (* 9 low)))))))

(fn fake-person [real]
  "Same fake for the same real name; a handle (no spaces) gets a handle. Two real
people do not share a fake while there are free ones."
  (or (. known real)
      (let [taken {}
            _ (each [_ f (pairs known)] (tset taken f true))
            n (length fake-people)
            start (% (hash real) n)
            pick (fn [i] (. fake-people (+ 1 (% (+ start i) n))))
            style (fn [[display handle]] (if (real:match "%s") display handle))
            fake (or (faccumulate [found nil i 0 (- n 1) &until found]
                       (let [f (style (pick i))] (if (not (. taken f)) f)))
                     (style (pick 0)))]
        (tset known real fake)
        fake)))

(fn seed []
  (each [_ real (ipairs [config.user (os.getenv "USER")])]
    (when (and real (> (length real) 2)) (fake-person real)))
  (each [_ real (ipairs (or config.demo-names []))]
    (fake-person real)))

(fn plain-replace [text real fake]
  (var out "")
  (var i 1)
  (while i
    (let [(a b) (text:find real i true)]
      (if a
          (do (set out (.. out (text:sub i (- a 1)) fake)) (set i (+ b 1)))
          (do (set out (.. out (text:sub i))) (set i nil)))))
  out)

(fn mask-people [text]
  ;; @handles of people first (@org/team are teams: left alone) ...
  (var out
    (pick-values 1
      (text:gsub "()@([%w_%-]+)(/?)"
                 (fn [pos handle slash]
                   (let [prev (if (> pos 1) (text:sub (- pos 1) (- pos 1)) " ")]
                     (if (or (= slash "/") (prev:match "[%w]"))
                         nil
                         (.. "@" (fake-person handle))))))))
  ;; ... then the names already seen, longest first
  (let [names (icollect [real _ (pairs known)] real)]
    (table.sort names #(> (length $1) (length $2)))
    (each [_ real (ipairs names)]
      (set out (plain-replace out real (. known real)))))
  out)

(fn mask [text]
  "Replaces the ticket keys and people of a string."
  (if (and enabled? (= (type text) :string))
      (mask-people (pick-values 1 (text:gsub (key-pattern) fake-key)))
      text))

(fn fake-text [field value]
  "Fake replacement for a title/description, always the same for the same value.
Keeps the `KEY: ` prefix some titles have."
  (let [h (hash value)
        prefix (value:match (.. "^(" (key-pattern) ":%s*)"))]
    (if (= field :title)
        (.. (or prefix "") (. fake-titles (+ 1 (% h (length fake-titles)))))
        (table.concat
          (fcollect [i 0 (+ 1 (% h 3))]
            (. fake-descriptions (+ 1 (% (+ h i) (length fake-descriptions)))))
          "\n"))))

(var walk nil)

(fn fake-person-value [v]
  "A person as Jira/GitHub return it: a string, a list, or an object."
  (case (type v)
    :string (if (not (v:match "%S")) v
                (v:match "^@.*/") v ; @org/team
                (v:match "^@") (.. "@" (fake-person (v:sub 2)))
                (fake-person v))
    :table (if (. v 1)
               (icollect [_ x (ipairs v)] (fake-person-value x))
               (collect [k x (pairs v)]
                 (case k
                   (where (or :displayName :name :login)) (values k (fake-person-value x))
                   :emailAddress (values k "person@example.com")
                   :accountId (values k "000000")
                   _ (values k (walk x)))))
    _ v))

(set walk (fn [data]
  (if (= (type data) :table)
      (if (. data 1) ; array
          (icollect [_ v (ipairs data)] (walk v))
          (collect [k v (pairs data)]
            (if (and (= (type v) :string) (. text-fields k) (v:match "%S"))
                (values k (fake-text (if (= k :description) :description :title) v))
                (. people-fields k)
                (values k (fake-person-value v))
                (values k (walk v)))))
      (= (type data) :string)
      ;; JSON inside a string (results of a :map step)
      (if (data:match "^%s*[%[{]")
          (let [inner (json.decode data)]
            (if (= (type inner) :table) (json.encode (walk inner)) data))
          data)
      data)))

(fn out [text]
  "A tool's result, with the titles and descriptions of its JSON replaced.
Does not touch non-JSON results (keys and known names are masked when drawn)."
  (if (and enabled? (= (type text) :string) (text:match "^%s*[%[{]"))
      (let [data (json.decode text)]
        (if (= (type data) :table) (json.encode (walk data)) text))
      text))

(fn toggle [?state]
  (set enabled? (if (= ?state nil) (not enabled?) ?state))
  (when enabled? (seed))
  enabled?)

(when enabled? (seed))

{: mask : out : toggle :on? (fn [] enabled?)}
