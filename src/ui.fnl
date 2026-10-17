;; Styles taken from Catppuccin Mocha in truecolor
;; and a vertical guide on the left in the cliclack style (┌ │ └) with one symbol per step.
;; Without luv there is no live redraw: the spinner is fixed while the step runs and
;; is replaced (\r) by the final symbol when it finishes.

(local highlight (require :src.highlight))

(local reset "\27[0m")
(local bold "\27[1m")

(fn rgb [hex]
  (let [h (hex:gsub "^#" "")]
    (string.format "\27[38;2;%d;%d;%dm"
                   (tonumber (h:sub 1 2) 16) (tonumber (h:sub 3 4) 16) (tonumber (h:sub 5 6) 16))))

;; Catppuccin Mocha, same theme as capataz
(local th {:text "#cdd6f4" :subtext "#a6adc8" :faint "#585b70"
           :green "#a6e3a1" :red "#f38ba8" :yellow "#f9e2af"
           :mauve "#cba6f7" :peach "#fab387" :blue "#89b4fa"})

(fn c [color text]
  (.. (rgb (. th color)) text reset))

(fn vwidth [s]
  "Visible width in codepoints (`length` counts bytes)."
  (select 2 (s:gsub "[^\128-\191]" "")))

(fn pad [s n]
  (let [w (vwidth s)] (if (>= w n) s (.. s (string.rep " " (- n w))))))

(fn truncate [s n]
  "Truncates to n codepoints without splitting a character, with an ellipsis."
  (if (<= (vwidth s) n)
      s
      (let [out []]
        (each [ch (s:gmatch utf8.charpattern) &until (>= (length out) (- n 1))]
          (table.insert out ch))
        (.. (table.concat out) "…"))))

(fn one-line [s]
  (pick-values 1 (s:gsub "%s+" " ")))

;; ── screen state ──

;; A margin is left free on the right for the little boxes (src/hud.fnl): all the
;; content fits the width left to their left.
(local gutter 46)
(local min-width 60)
(var width 80)

(fn begin []
  "Measures the terminal (like capataz: stty on /dev/tty, works even if stdout is not a tty)."
  (let [f (io.popen "(stty size < /dev/tty) 2>/dev/null")
        cols (tonumber (or (: (f:read :a) :match "%d+%s+(%d+)") ""))]
    (f:close)
    (set width (math.min (or cols 80) (math.max min-width (math.min 110 (- (or cols 80) gutter)))))))

(fn now []
  "Seconds with decimals (os.time only has whole seconds)."
  (let [f (io.popen "perl -MTime::HiRes=time -e 'printf \"%.3f\", time'")
        t (tonumber (f:read :a))]
    (f:close)
    t))

(fn secs [t]
  (if (< t 10) (string.format "%.1fs" t) (string.format "%ds" (math.floor t))))

;; ── pieces ──

(local guide-bar (c :faint "│"))
(local body-prefix (.. guide-bar "     "))

(fn guide []
  (print guide-bar))

(fn header [title subtitle right]
  (let [right (or right "")
        room (- width 3 (vwidth title) 5 (vwidth right) 2)
        sub (truncate (one-line subtitle) (math.max 10 room))
        gap (math.max 2 (- width 3 (vwidth title) 5 (vwidth sub) (vwidth right)))]
    (print (.. (c :faint "┌  ") (rgb th.mauve) "⬢ " bold title reset
               (c :faint "  ·  ") (c :subtext sub) (string.rep " " gap) (c :faint right)))))

(local symbols {:running ["◒" :mauve] :ok ["◇" :green] :warn ["▲" :yellow]
                :fail ["■" :red] :claude ["◆" :mauve]})

(local name-width 20)

(fn step-line [state name detail right]
  (let [[sym color] (. symbols state)
        right (or right "")
        room (math.max 8 (- width 3 name-width (vwidth right) 2))
        detail (truncate (one-line (or detail "")) room)
        gap (math.max 2 (- width 3 name-width (vwidth detail) (vwidth right)))]
    (.. (c color sym) "  "
        (c (if (= state :ok) :subtext :text) (pad name name-width))
        (c :faint detail) (string.rep " " gap) (c :faint right))))

(fn start [name detail]
  "Step line while it runs. No newline: `finish` overwrites it."
  (io.write (step-line :running name detail ""))
  (io.flush))

(fn finish [state name detail elapsed]
  (print (.. "\r\27[2K" (step-line state name detail (if elapsed (secs elapsed) "")))))

(fn body [lines style]
  "Lines below a step, with the guide on the left. `style`: :final (the
answer, highlighted), :data (intermediate results, dimmed) or :error."
  (let [color (case style :final :text :error :red _ :subtext)
        max 8]
    (each [i line (ipairs lines) &until (> i max)]
      (print (.. body-prefix (c color (truncate line (- width 6))))))
    (when (> (length lines) max)
      (print (.. body-prefix (c :faint (.. "… and " (- (length lines) max) " more")))))))

(local token-colors {:comment :faint :string :green :keyword :blue :number :peach
                     :special :mauve :delim :subtext :symbol :text})

(fn code-line [line lang state]
  "A line of a code block: Fennel highlighted, the rest in orange.
The COMPLETE line is tokenized and only then cut to the width: if it were cut
before, a cut string never closes and everything that follows comes out green."
  (let [room (- width 6)]
    (if (= lang "fennel")
        (let [out []]
          (var used 0)
          (each [_ [cat text] (ipairs (highlight.tokens line state))]
            (when (< used room)
              (let [fits (truncate text (- room used))]
                (set used (+ used (vwidth fits)))
                (table.insert out (if (= cat :space) fits (c (. token-colors cat) fits))))))
          (table.concat out))
        (c :peach (truncate line room)))))

(fn stream [style]
  "Writes text that arrives in pieces (Claude) with the guide on the left,
wrapping at words. Interprets markdown **bold** and `code`, and ``` blocks
go line by line, without wrapping, with Fennel highlighted.
Returns (values write done)."
  (let [base (rgb (. th (if (= style :final) :text :subtext)))
        max (- width 6)]
    (var col 0)
    (var word "")
    (var bold? false)
    (var code? false)
    (var started? false)
    (var at-line-start? true)
    (var pending "")  ; backticks at the start of a line: they may open a ```
    (var fence nil)   ; inside a block: {:lang :line :state}
    (fn style-code []
      (.. reset base (if bold? bold "") (if code? (rgb th.peach) "")))
    (fn render-word [w]
      (var out "")
      (var i 1)
      (while (<= i (length w))
        (if (= (w:sub i (+ i 1)) "**")
            (do (set bold? (not bold?)) (set out (.. out (style-code))) (set i (+ i 2)))
            (= (w:sub i i) "`")
            (do (set code? (not code?)) (set out (.. out (style-code))) (set i (+ i 1)))
            (do (set out (.. out (w:sub i i))) (set i (+ i 1)))))
      out)
    (fn emit-word []
      (when (> (length word) 0)
        (let [w (vwidth (pick-values 1 (word:gsub "%*%*" "")))]
          (when (and (> col 0) (> (+ col w) max))
            (io.write reset "\n" body-prefix (style-code))
            (set col 0))
          (io.write (render-word word))
          (set col (+ col w))
          (set word ""))))
    (fn text-char [ch]
      (if (= ch "\n")
          (do (emit-word)
              (io.write reset "\n" body-prefix (style-code))
              (set col 0))
          (= ch " ")
          (do (emit-word)
              (when (< col max)
                (io.write " ")
                (set col (+ col 1))))
          (set word (.. word ch)))
      (set at-line-start? (= ch "\n")))
    (fn flush-pending []
      (let [p pending]
        (set pending "")
        (each [ch (p:gmatch ".")]
          (text-char ch))))
    (fn fence-char [ch]
      (if (not= ch "\n")
          (set fence.line (.. fence.line ch))
          (= fence.lang nil)
          ;; rest of the line that opened the block: the language
          (do (set fence.lang (pick-values 1 (: (fence.line:lower) :match "^%s*(%S*)")))
              (set fence.line ""))
          (fence.line:match "^%s*```")
          ;; end of the block: it is not printed, and we go back to text
          (do (set fence nil)
              (set at-line-start? true)
              (set col 0))
          (do (io.write (code-line fence.line fence.lang fence.state) reset "\n" body-prefix base)
              (set fence.line ""))))
    (fn write [chunk]
      (when (not started?)
        (io.write body-prefix base)
        (set started? true))
      (each [ch (chunk:gmatch utf8.charpattern)]
        (if fence
            (fence-char ch)
            (and at-line-start? (= ch "`") (< (length pending) 3))
            (do (set pending (.. pending ch))
                (when (= pending "```")
                  (set pending "")
                  (emit-word)
                  (set fence {:line "" :state {}})))
            (do (flush-pending)
                (text-char ch))))
      (io.flush))
    (fn done []
      (when started?
        (if (and fence (fence.line:match "%S") (not (fence.line:match "^%s*```")))
            (io.write (code-line fence.line fence.lang fence.state))
            (flush-pending))
        (emit-word)
        (io.write reset "\n")))
    (values write done)))

(fn footer [parts]
  (print (.. (c :faint "└  ") (table.concat parts (c :faint " · ")))))

(fn notice [kind text]
  "Notices outside the panel (tool reload)."
  (let [[sym color] (case kind :reload ["↺" :peach] :error ["✗" :red] _ ["·" :faint])]
    (print (.. (c color sym) "  " (c :subtext text)))))

(fn prompt [?label]
  "The REPL prompt; with a label (e.g. \"dev\") in another color, so the mode stands out."
  (io.write "\n" (if ?label (c :peach (.. ?label "❯ ")) (c :mauve "❯ ")))
  (io.flush))

(fn emphasize [text]
  "Bold in the normal text color."
  (.. bold (rgb th.text) text reset))

{: c : pad : vwidth : gutter : truncate : begin : now : secs : guide : header : start : finish
 : body : stream : footer : notice : prompt :bold emphasize}
