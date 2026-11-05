(import-macros {: deftool} :src.macros)
(local json (require :dkjson))
(local sh (require :src.sh))

(local base (or (os.getenv "JIRA_URL") "https://auditboard.atlassian.net"))
(local jql (or (os.getenv "JIRA_JQL")
               (.. "project = SOX AND issuetype = Bug AND sprint in openSprints()"
                   " AND sprint ~ \"Policies\" AND statusCategory != Done"
                   " ORDER BY priority DESC")))
(local field-list (.. "summary,description,issuetype,status,priority,labels,"
                      "components,parent,issuelinks,reporter,assignee,created"))

;; una pagina de resultados (API v2: la descripcion viene como texto, no ADF)
(fn fetch [email token page]
  (let [cmd (.. "curl -sS -G -u " (sh.quote (.. email ":" token))
                " " (sh.quote (.. base "/rest/api/2/search/jql"))
                " --data-urlencode " (sh.quote (.. "jql=" jql))
                " --data-urlencode " (sh.quote (.. "fields=" field-list))
                " --data-urlencode maxResults=100"
                (if page (.. " --data-urlencode " (sh.quote (.. "nextPageToken=" page))) ""))
        (out code) (sh.run cmd)
        _ (assert (= code 0) out)
        res (json.decode out)]
    (assert res (.. "respuesta inválida de Jira: " out))
    ;; if en vez de assert: el mensaje de assert se evalua siempre
    (when res.errorMessages
      (error (.. "Jira: " (table.concat res.errorMessages "; ")) 0))
    (assert res.issues (.. "respuesta inesperada de Jira: " out))
    res))

(fn simplify [issue]
  (let [f issue.fields
        links []]
    (each [_ l (ipairs (or f.issuelinks []))]
      (if l.outwardIssue
          (table.insert links {:relation l.type.outward :key l.outwardIssue.key
                               :title l.outwardIssue.fields.summary})
          l.inwardIssue
          (table.insert links {:relation l.type.inward :key l.inwardIssue.key
                               :title l.inwardIssue.fields.summary})))
    {:key issue.key
     :title f.summary
     :description (or f.description "")
     :type (and f.issuetype f.issuetype.name)
     :status (and f.status f.status.name)
     :priority (and f.priority f.priority.name)
     :assignee (and f.assignee f.assignee.displayName)
     :labels (or f.labels [])
     :components (icollect [_ c (ipairs (or f.components []))] c.name)
     :parent (and f.parent {:key f.parent.key :title f.parent.fields.summary})
     :reporter (and f.reporter f.reporter.displayName)
     :created f.created
     :links links
     :url (.. base "/browse/" issue.key)}))

(deftool sprint-backlog
 {:short "bugs abiertos del sprint actual de Policies"
  :description "Trae los bugs abiertos (no Done) del sprint actual de Policies en Jira, proyecto SOX, ordenados por prioridad, con clave, titulo, descripcion completa, tipo, estado, prioridad, asignado, labels, epica, links a otros tickets y url."
  :params []
  :returns [:key :title]
  :run (fn []
         (let [email (os.getenv "JIRA_EMAIL")
               token (os.getenv "JIRA_TOKEN")]
           (assert (and email token) "faltan las variables de entorno JIRA_EMAIL y JIRA_TOKEN")
           (var page nil)
           (var done false)
           (let [items []]
             (while (not done)
               (let [res (fetch email token page)]
                 (each [_ issue (ipairs res.issues)]
                   (table.insert items (simplify issue)))
                 (set page res.nextPageToken)
                 (set done (or res.isLast (not page)))))
             (json.encode items))))
  :show (fn [out]
          (let [items (json.decode out)]
            (if (= (length items) 0)
                ["ningún bug abierto en el sprint"]
                (let [lines [(.. (length items) " bugs abiertos en el sprint")]]
                  (each [_ t (ipairs items)]
                    (table.insert lines
                                  (string.format "%-12s [%s/%s] %s (%s)" t.key (or t.type "?")
                                                 (or t.priority "?") t.title
                                                 (or t.assignee "sin asignar"))))
                  lines))))})
