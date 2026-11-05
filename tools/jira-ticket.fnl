(local json (require :dkjson))
(local sh (require :src.sh))

(local base (or (os.getenv "JIRA_URL") "https://auditboard.atlassian.net"))

;; llamada a la API v2; devuelve el JSON decodificado
(fn api [email token path]
  (let [cmd (.. "curl -sS -w " (sh.quote "\n%{http_code}")
                " -u " (sh.quote (.. email ":" token))
                " -H " (sh.quote "Accept: application/json")
                " " (sh.quote (.. base path)))
        (out code) (sh.run cmd)
        _ (assert (= code 0) out)
        (resp status) (out:match "^(.*)\n(%d+)%s*$")]
    (assert status (.. "respuesta inesperada de Jira: " out))
    (when (>= (tonumber status) 300)
      (error (.. "Jira (" status "): " resp) 0))
    (json.decode resp)))

{:name "jira-ticket"
 :short "lee un ticket de Jira"
 :description "Devuelve la descripcion completa de un ticket de Jira (SOX-123): clave, titulo, descripcion, tipo, estado, labels y url. Solo lee."
 :params [{:name :ticket :type :string :required true
           :doc "clave del ticket de Jira, ej: SOX-101203"}]
 :run (fn [{: ticket}]
        (let [email (os.getenv "JIRA_EMAIL")
              token (os.getenv "JIRA_TOKEN")
              key (: (or (ticket:match "^%s*(.-)%s*$") "") :upper)]
          (assert (and email token) "faltan las variables de entorno JIRA_EMAIL y JIRA_TOKEN")
          (assert (key:match "^SOX%-%d+$") (.. "ticket inválido (esperaba SOX-123): " ticket))
          (let [issue (api email token
                           (.. "/rest/api/2/issue/" key
                               "?fields=summary,description,issuetype,status,labels"))
                f issue.fields]
            (json.encode {:key key
                          :title f.summary
                          :description (if (= (type f.description) :string) f.description "")
                          :type (. f.issuetype :name)
                          :status (. f.status :name)
                          :labels (or f.labels [])
                          :url (.. base "/browse/" key)}))))
 :show (fn [out]
         (let [t (json.decode out)]
           (doto (icollect [line (t.description:gmatch "[^\n]+")]
                   (when (line:match "%S") line))
             (table.insert 1 (.. t.key " " t.title "  (" t.type ", " t.status ")"))
             (table.insert 2 t.url))))}
