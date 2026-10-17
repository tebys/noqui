;; Jira REST API v2 (curl + basic auth with JIRA_EMAIL / JIRA_TOKEN).
(local json (require :dkjson))
(local sh (require :src.sh))
(local config (require :config))
(local utils (require :src.utils))

(local base (or (os.getenv "JIRA_URL") config.jira-url))

(fn credentials []
  "(values email token) from the environment; fails if they are missing."
  (let [email (os.getenv "JIRA_EMAIL")
        token (os.getenv "JIRA_TOKEN")]
    (assert (and email token) "missing environment variables JIRA_EMAIL and JIRA_TOKEN")
    (values email token)))

(fn normalize-key [ticket]
  "The ticket key in upper case; fails if it is not of the configured project."
  (let [key (: (utils.trim ticket) :upper)]
    (assert (key:match (.. "^" config.jira-project "%-%d+$"))
            (.. "invalid ticket (expected " config.jira-project "-123): " ticket))
    key))

(fn browse-url [key]
  (.. base "/browse/" key))

(fn request [{: method : path : body : query}]
  "Calls the API and returns the decoded JSON (nil if there is no body). `query` is a list of
[name value] pairs sent as URL parameters (GET). Raises an error if Jira answers with status >= 300."
  (let [(email token) (credentials)
        cmd (.. "curl -sS -w " (sh.quote "\n%{http_code}")
                (if query " -G" "")
                (if method (.. " -X " method) "")
                " -u " (sh.quote (.. email ":" token))
                " -H " (sh.quote "Accept: application/json")
                (if body (.. " -H " (sh.quote "Content-Type: application/json")
                             " -d " (sh.quote (json.encode body)))
                    "")
                (table.concat (icollect [_ [k v] (ipairs (or query []))]
                                (.. " --data-urlencode " (sh.quote (.. k "=" v)))))
                " " (sh.quote (.. base path)))
        (out code) (sh.run cmd)
        _ (assert (= code 0) out)
        (resp status) (out:match "^(.*)\n(%d+)%s*$")]
    (assert status (.. "unexpected response from Jira: " out))
    (when (>= (tonumber status) 300)
      (error (.. "Jira (" status "): " resp) 0))
    (if (= resp "") nil (json.decode resp))))

{: base : credentials : normalize-key : browse-url : request}
