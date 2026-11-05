;; Demo mode, for recording videos: it alters ONLY what is shown on screen.
;; Tool results, commands and what Claude receives keep the real data, so a write
;; (pr-desc!, push-pr...) never ends up with fake text.
;;   - ticket keys: SOX-98770 -> DEMO-xxxx (same key, same fake, even across sessions)
;;   - title / summary / description / body fields of a tool's JSON: fake text
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

(fn hash [s]
  (accumulate [h 7 ch (s:gmatch ".")] (% (+ (* h 31) (ch:byte)) 1000003)))

(fn key-pattern []
  (.. "%f[%w]" (or config.jira-project "SOX") "%-%d+"))

(fn fake-key [key]
  "Same number of digits as the real key, so the columns still line up."
  (let [n (length (key:match "%d+$"))
        low (^ 10 (- n 1))]
    (.. "DEMO-" (math.tointeger (+ low (% (hash key) (* 9 low)))))))

(fn mask [text]
  "Replaces the ticket keys of a string."
  (if (and enabled? (= (type text) :string))
      (pick-values 1 (text:gsub (key-pattern) fake-key))
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

(fn walk [data]
  (if (= (type data) :table)
      (if (. data 1) ; array
          (icollect [_ v (ipairs data)] (walk v))
          (collect [k v (pairs data)]
            (if (and (= (type v) :string) (. text-fields k) (v:match "%S"))
                (values k (fake-text (if (= k :description) :description :title) v))
                (values k (walk v)))))
      (= (type data) :string)
      ;; JSON inside a string (results of a :map step)
      (if (data:match "^%s*[%[{]")
          (let [inner (json.decode data)]
            (if (= (type inner) :table) (json.encode (walk inner)) data))
          data)
      data))

(fn out [text]
  "A tool's result, with the titles and descriptions of its JSON replaced.
Does not touch non-JSON results (the keys are masked when they are drawn)."
  (if (and enabled? (= (type text) :string) (text:match "^%s*[%[{]"))
      (let [data (json.decode text)]
        (if (= (type data) :table) (json.encode (walk data)) text))
      text))

(fn toggle [?state]
  (set enabled? (if (= ?state nil) (not enabled?) ?state))
  enabled?)

{: mask : out : toggle :on? (fn [] enabled?)}
