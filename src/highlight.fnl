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

;; --- TypeScript / JavaScript: the same few categories ------------------------------

(local ts-specials
  (collect [_ s (ipairs [:const :let :var :function :return :if :else :for :while :do :switch :case
                         :default :break :continue :throw :try :catch :finally :new :delete :typeof
                         :instanceof :in :of :as :await :async :yield :import :export :from :class
                         :extends :implements :interface :type :enum :namespace :public :private
                         :protected :readonly :static :abstract :declare :void :this :super :satisfies
                         :null :undefined :true :false :keyof :unknown :never :any :string :number
                         :boolean])]
    s true))

(fn quote-end [line from q]
  "Index of the quote `q` that closes the string starting before `from` (skipping escapes), or nil."
  (var i from)
  (var found nil)
  (while (and (not found) (<= i (length line)))
    (let [ch (line:sub i i)]
      (if (= ch "\\") (set i (+ i 2))
          (= ch q) (set found i)
          (set i (+ i 1)))))
  found)

(fn ts-tokens [line state]
  "state.block? (inside /* */) and state.template? (inside a `template`) go from line to line."
  (let [out []]
    (fn emit [cat text] (table.insert out [cat text]))
    (var i 1)
    (while (<= i (length line))
      (let [ch (line:sub i i)
            two (line:sub i (+ i 1))]
        (if state.block?
            (let [j (line:find "*/" i true)]
              (emit :comment (line:sub i (if j (+ j 1) (length line))))
              (when j (set state.block? false))
              (set i (+ (if j (+ j 1) (length line)) 1)))
            state.template?
            (let [j (quote-end line i "`")]
              (emit :string (line:sub i (or j (length line))))
              (when j (set state.template? false))
              (set i (+ (or j (length line)) 1)))
            (= two "//")
            (do (emit :comment (line:sub i)) (set i (+ (length line) 1)))
            (= two "/*")
            (do (set state.block? true) (emit :comment two) (set i (+ i 2)))
            (= ch "`")
            (do (set state.template? true) (emit :string ch) (set i (+ i 1)))
            (or (= ch "\"") (= ch "'"))
            (let [j (quote-end line (+ i 1) ch)]
              (emit :string (line:sub i (or j (length line))))
              (set i (+ (or j (length line)) 1)))
            (ch:match "%s")
            (let [sp (line:match "^%s+" i)] (emit :space sp) (set i (+ i (length sp))))
            (ch:match "[%w_$]")
            (let [word (line:match "^[%w_$]+" i)]
              (emit (if (word:match "^%d") :number
                        (. ts-specials word) :special
                        (word:match "^%u") :keyword ; types and classes
                        :symbol)
                    word)
              (set i (+ i (length word))))
            (do (emit :delim ch) (set i (+ i 1))))))
    out))

(fn tokens-for [lang]
  "The tokenizer for a code block's language, or nil if there is none."
  (case lang
    :fennel tokens
    (where (or :ts :tsx :typescript :js :javascript)) ts-tokens))

{: tokens : tokens-for}
