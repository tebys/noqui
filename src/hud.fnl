;; Little boxes pinned to the top right of the terminal, stacked (:claude on top,
;; :watch below), drawn over the content with absolute positions (the cursor is
;; saved and restored, so it does not disturb what you type). They are redrawn only
;; when something changes, when the prompt comes back (scrolling shifts them) and
;; when the terminal is resized. It only draws if `enable` (the interactive REPL).
(local ui (require :src.ui))

(local min-cols 70)                  ; with less width nothing is drawn
(local order [:claude :watch])       ; from top to bottom

(local esc "\27")
(local cursor-save (.. esc "7"))
(local cursor-restore (.. esc "8"))

(local state {:enabled? false :paused? false :boxes {} :cols nil :drawn []})

;; NOQUI_HUD_LOG=/tmp/hud.log: keeps what is written to the terminal (with the escapes
;; visible), to debug drawing problems
(local log-path (os.getenv "NOQUI_HUD_LOG"))
;; NOQUI_HUD_STATIC=1: the boxes are only drawn when the prompt comes back, never
;; in the middle of the wait (to see if the async repaints are what breaks things)
(local static? (os.getenv "NOQUI_HUD_STATIC"))

;; Under rlwrap, everything that goes out on stdout is taken as part of the prompt and
;; rlwrap redraws it (with the cursor miscalculated). The boxes go straight to the tty of
;; the real terminal: rlwrap's, the parent process.
(local uv (require :luv))

(fn parent-tty []
  (let [ppid (string.format "%d" (uv.os_getppid))
        p (io.popen (.. "ps -o comm= -p " ppid " 2>/dev/null; ps -o tty= -p " ppid " 2>/dev/null"))
        comm (or (p:read :l) "")
        tty (: (or (p:read :l) "") :match "%S+")]
    (p:close)
    (when (and (comm:find "rlwrap" 1 true) tty (not= tty "??") (not= tty "?"))
      (.. "/dev/" tty))))

(var tty-path nil)
(var tty-file nil)

(fn write-out [text]
  (if tty-file
      (do (tty-file:write text) (tty-file:flush))
      (do (io.write text) (io.flush))))

(fn emit [out what]
  (when log-path
    (with-open [f (io.open log-path :a)]
      (f:write (os.date "%H:%M:%S ") what " " (: (table.concat out) :gsub "\27" "<ESC>") "\n")))
  (write-out (table.concat out)))

(fn term-cols []
  (let [f (io.popen (.. "(stty size < " (or tty-path "/dev/tty") ") 2>/dev/null"))
        cols (tonumber (or (: (f:read :a) :match "%d+%s+(%d+)") ""))]
    (f:close)
    cols))

(local max-inner (- ui.gutter 6)) ; max width of a box content

(fn row [color main ?sub]
  "A box row: colored circle + text (`?sub` is truncated so it fits)."
  (let [?sub (when ?sub (ui.truncate ?sub (math.max 1 (- max-inner 3 (ui.vwidth main)))))
        plain (.. "● " main (if ?sub (.. " " ?sub) ""))]
    {: plain
     :styled (.. (ui.c color "●") " " (ui.c :text main) (if ?sub (.. " " (ui.c :subtext ?sub)) ""))}))

(fn note [text]
  "A row without a circle (e.g. \"+2 more\")."
  {:plain text :styled (ui.c :faint text)})

(fn layout [cols]
  "Position of each visible box: [{:name :col :width :height :top :box}]."
  (let [geoms []]
    (var top 1)
    (each [_ name (ipairs order)]
      (let [box (. state.boxes name)]
        (when (and box (> (length box.rows) 0))
          (let [inner (accumulate [w (+ 2 (ui.vwidth box.title)) _ r (ipairs box.rows)]
                        (math.max w (ui.vwidth r.plain)))
                width (+ inner 4)
                height (+ (length box.rows) 2)]
            (table.insert geoms {: name :col (- cols width) : width : height : top : box : inner})
            (set top (+ top height))))))
    geoms))

(fn same? [a b]
  (and a b (= a.name b.name) (= a.col b.col) (= a.width b.width) (= a.height b.height) (= a.top b.top)))

(fn erase [out g]
  (for [i 0 (- g.height 1)]
    (table.insert out (string.format "%s[%d;%dH%s" esc (+ g.top i) g.col (string.rep " " g.width)))))

(fn paint [out g]
  (let [border (ui.c :faint "│")
        title (.. " " g.box.title " ")]
    (table.insert out (string.format "%s[%d;%dH%s" esc g.top g.col
                                     (ui.c :faint (.. "╭─" title (string.rep "─" (- g.width 3 (ui.vwidth title))) "╮"))))
    (each [i r (ipairs g.box.rows)]
      (table.insert out (string.format "%s[%d;%dH%s %s%s %s" esc (+ g.top i) g.col border r.styled
                                       (string.rep " " (- g.inner (ui.vwidth r.plain))) border)))
    (table.insert out (string.format "%s[%d;%dH%s" esc (+ g.top g.height -1) g.col
                                     (ui.c :faint (.. "╰" (string.rep "─" (- g.width 2)) "╯"))))))

(fn draw [?force]
  "Draws the boxes. Without `?force` it does nothing if what is visible did not change:
the one-second tick must not repaint (the cursor blinks and moves)."
  (when (and state.enabled? (not state.paused?) (or ?force (not static?)))
    (when (not state.cols) (set state.cols (or (term-cols) 0)))
    (let [out [cursor-save (.. esc "[?25l" esc "[?7l")]
          geoms (if (>= state.cols min-cols) (layout state.cols) [])]
      ;; whatever is no longer the same is erased; everything current is painted again
      (each [i old (ipairs state.drawn)]
        (when (not (same? old (. geoms i)))
          (erase out old)))
      ;; a forced repaint (the prompt comes back) can come after a scroll: erase
      ;; the whole rectangle, with one extra row, in case something got shifted
      (when (and ?force (> (length geoms) 0))
        (let [left (accumulate [c state.cols _ g (ipairs geoms)] (math.min c g.col))
              bottom (accumulate [b 0 _ g (ipairs geoms)] (math.max b (+ g.top g.height)))]
          (erase out {:col left :top 1 :width (+ (- state.cols left) 1) :height bottom})))
      (each [_ g (ipairs geoms)]
        (paint out g))
      (set state.drawn geoms)
      (table.insert out cursor-restore)
      (table.insert out (.. esc "[?7h" esc "[?25h"))
      (let [shown (table.concat out)]
        (when (or ?force (not= shown state.last))
          (set state.last shown)
          (emit out :draw))))))

(fn set-box [name title rows]
  "Sets (or removes, with empty or nil rows) the box `name`, and redraws."
  (tset state.boxes name (when (and rows (> (length rows) 0)) {: title : rows}))
  (draw))

(fn clear []
  "Erases all the boxes from the screen and does not draw until `resume` (while text
is being output the screen scrolls and would drag the boxes along)."
  (set state.paused? true)
  (when (and state.enabled? (> (length state.drawn) 0))
    (let [out [cursor-save]]
      ;; Enter already scrolled one line: what was drawn is now one row higher (and each
      ;; box in its column), so the whole rectangle they occupy is erased, with one
      ;; extra row, not box by box
      (let [left (accumulate [c state.cols _ g (ipairs state.drawn)] (math.min c g.col))
            bottom (accumulate [b 0 _ g (ipairs state.drawn)] (math.max b (+ g.top g.height)))]
        (erase out {:col left :top 1 :width (+ (- state.cols left) 1) :height bottom}))
      (table.insert out cursor-restore)
      (emit out :clear)))
  (set state.drawn []))

(fn resume []
  "The boxes are drawn again. With rlwrap the program output reaches the terminal
a bit later than what we write directly to the tty: we wait so as not to draw
before the text scrolls."
  (set state.paused? false)
  (if tty-file
      ;; two passes: the second one fixes things if the scroll arrived after the first
      (each [_ ms (ipairs [60 400])]
        (let [t (uv.new_timer)]
          (t:start ms 0 (fn []
                          (t:close)
                          (draw true)))))
      (draw true)))

(fn relayout []
  "The terminal was resized: the old boxes are no longer where they were drawn."
  (set state.cols nil)
  (set state.drawn [])
  (draw true))

(fn enable []
  (set tty-path (parent-tty))
  (set tty-file (and tty-path (io.open tty-path :w)))
  (when (not tty-file) (set tty-path nil))
  (set state.enabled? true))

(fn disable []
  (clear)
  (set state.enabled? false)
  (set state.boxes {})
  (when tty-file (tty-file:close))
  (set tty-file nil)
  (set tty-path nil))

{: row : note : set-box : draw : clear : resume : relayout : enable : disable :enabled? (fn [] state.enabled?)}
