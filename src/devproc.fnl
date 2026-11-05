;; Procesos de desarrollo (backend/frontend de un worktree) lanzados en segundo plano
;; por ñoqui: puertos libres y un archivo de estado por proceso, en /tmp/noqui-dev.
(local json (require :dkjson))
(local sh (require :src.sh))

(local dir "/tmp/noqui-dev")

(fn state-path [repo ticket]
  (.. dir "/" repo "-" (ticket:upper) ".json"))

(fn alive? [pid]
  (and pid (let [(_ code) (sh.run (.. "kill -0 " pid))] (= code 0))))

(fn read-state [repo ticket]
  "El estado guardado de un proceso, o nil si no hay o ya murio."
  (let [f (io.open (state-path repo ticket))]
    (when f
      (let [st (json.decode (f:read :a))]
        (f:close)
        (if (and st (alive? st.pid)) st (do (os.remove (state-path repo ticket)) nil))))))

(fn write-state [repo ticket st]
  (sh.run (.. "mkdir -p " (sh.quote dir)))
  (with-open [f (assert (io.open (state-path repo ticket) :w))]
    (f:write (json.encode st))))

(fn clear-state [repo ticket]
  (os.remove (state-path repo ticket)))

(fn reserved-ports []
  "Puertos de los procesos nuestros que siguen vivos (todavia pueden no estar escuchando)."
  (let [ports {}
        (out) (sh.run (.. "ls " (sh.quote dir) "/*.json 2>/dev/null"))]
    (each [path (out:gmatch "[^\n]+%.json")]
      (let [f (io.open path)]
        (when f
          (let [st (json.decode (f:read :a))]
            (f:close)
            (when (and st st.port (alive? st.pid))
              (tset ports st.port true))))))
    ports))

(fn port-busy? [port]
  (let [(out) (sh.run (.. "lsof -nP -iTCP:" port " -sTCP:LISTEN -t"))]
    (not= (out:match "%d+") nil)))

(fn free-port [from]
  "El primer puerto >= `from` que nadie escucha ni tiene reservado."
  (let [reserved (reserved-ports)]
    (var port from)
    (while (and (< port (+ from 200)) (or (. reserved port) (port-busy? port)))
      (set port (+ port 1)))
    (assert (< port (+ from 200)) (.. "no hay puertos libres desde " from))
    port))

(fn spawn [wt-path env cmd log]
  "Lanza `cmd` (con `env`, una cadena VAR=valor ...) en wt-path, desacoplado de la
terminal y con la salida en `log`. Devuelve el pid."
  (sh.run (.. "mkdir -p " (sh.quote dir)))
  (let [inner (.. "cd " (sh.quote wt-path) " && exec env " env " " cmd)
        (out) (sh.run (.. "nohup sh -c " (sh.quote inner) " >" (sh.quote log) " 2>&1 </dev/null & echo $!"))
        pid (out:match "%d+")]
    (assert pid (.. "no pude lanzar: " out))
    pid))

(fn stop [pid]
  "Mata el proceso y todos sus hijos."
  (sh.run (.. "kill_tree() { for c in $(pgrep -P $1); do kill_tree $c; done; kill -TERM $1 2>/dev/null; }; "
              "kill_tree " pid)))

{: read-state : write-state : clear-state : free-port : spawn : stop : alive? : dir}
