(import-macros {: deftool} :src.macros)
(local files (require :src.files))

(deftool read-file
 {:short "reads a project file"
 :description "Returns the contents of a project file."
 :params [{:name :path :type :string :required true
           :doc "path relative to the project, e.g. COSTS.org"}]
 :run (fn [{: path}]
        (let [f (assert (io.open (files.check-path path)) (.. "does not exist: " path))]
          (with-open [f f]
            (f:read :a))))})
