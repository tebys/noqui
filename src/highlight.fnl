;; Fennel highlighting to show code in the panel. A small tokenizer instead
;; of tree-sitter: ast-grep ships no Fennel grammar and tree-sitter highlighting
;; requires the CLI, compiling tree-sitter-fennel and its queries; Fennel has few
;; categories (comments, strings, :keywords, numbers, special forms, delimiters).
;;
;; (tokens line state) -> [[category text] ...]. `state` goes from line to line, for
;; multi-line strings (docstrings).

(local specials
  (collect [_ s (ipairs [:fn :lambda :λ :let :local :var :set :tset :global :if :when :unless
                         :each :for :while :do :case :match :case-try :match-try :icollect
                         :collect :accumulate :faccumulate :fcollect :values :require
                         :import-macros :macro :macros :doto :-> :->> :-?> :-?>> :. :?. :..
                         :not :and :or :with-open :pick-values :partial :hashfn :quote
                         :length :lua :eval-compiler :&until :&as :where :or])]
    s true))

(fn string-end [line from]
  "Index of the quote that closes the string (skipping escapes), or nil."
  (var i from)
  (var found nil)
  (while (and (not found) (<= i (length line)))
    (let [ch (line:sub i i)]
      (if (= ch "\\") (set i (+ i 2))
          (= ch "\"") (set found i)
          (set i (+ i 1)))))
  found)

(fn tokens [line state]
  (let [out []]
    (fn emit [cat text] (table.insert out [cat text]))
    (var i 1)
    (while (<= i (length line))
      (let [ch (line:sub i i)]
        (if state.string?
            (let [j (string-end line i)]
              (emit :string (line:sub i (or j (length line))))
              (when j (set state.string? false))
              (set i (+ (or j (length line)) 1)))
            (= ch ";")
            (do (emit :comment (line:sub i))
                (set i (+ (length line) 1)))
            (= ch "\"")
            (do (emit :string "\"")
                (set state.string? true)
                (set i (+ i 1)))
            (ch:match "[%(%)%[%]{}]")
            (do (emit :delim ch)
                (set i (+ i 1)))
            (ch:match "%s")
            (let [s (line:match "^%s+" i)]
              (emit :space s)
              (set i (+ i (length s))))
            ;; an "atom": :keyword, number or symbol
            (let [atom (line:match "^[^%s%(%)%[%]{}\";]+" i)]
              (emit (if (and (atom:match "^:") (> (length atom) 1)) :keyword
                        (atom:match "^%-?%d") :number
                        (. specials atom) :special
                        :symbol)
                    atom)
              (set i (+ i (length atom)))))))
    out))

{: tokens}
