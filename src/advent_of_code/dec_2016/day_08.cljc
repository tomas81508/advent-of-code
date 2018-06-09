(ns advent-of-code.dec-2016.day-08
  (:require [ysera.test :refer [deftest is is-not is=]]
            [ysera.collections :refer [seq-contains?]]
            [clojure.string :refer [split starts-with?]]))

; --- Day 8: Two-Factor Authentication ---
;
; You come across a door implementing what you can only assume is an implementation of two-factor authentication after a
; long game of requirements telephone.
;
; To get past the door, you first swipe a keycard (no problem; there was one on a nearby desk). Then, it displays a code
; on a little screen, and you type that code on a keypad. Then, presumably, the door unlocks.
;
; Unfortunately, the screen has been smashed. After a few minutes, you've taken everything apart and figured out how it
; works. Now you just have to work out what the screen would have displayed.
;
; The magnetic strip on the card you swiped encodes a series of instructions for the screen; these instructions are your
; puzzle input. The screen is 50 pixels wide and 6 pixels tall, all of which start off, and is capable of three somewhat
; peculiar operations:
;
; - rect AxB turns on all of the pixels in a rectangle at the top-left of the screen which is A wide and B tall.
; - rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B pixels. Pixels that would fall
;   off the right end appear at the left end of the row.
; - rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down by B pixels. Pixels that
;   would fall off the bottom appear at the top of the column.
;
; For example, here is a simple sequence on a smaller screen:
;
; rect 3x2 creates a small rectangle in the top-left corner: ###....
;                                                            ###....
;                                                            .......
;
; rotate column x=1 by 1 rotates the second column down by one pixel: #.#....
;                                                                     ###....
;                                                                     .#.....
;
; rotate row y=0 by 4 rotates the top row right by four pixels: ....#.#
;                                                               ###....
;                                                               .#.....
;
; rotate column x=1 by 1 again rotates the second column down by one pixel, causing the bottom pixel to wrap back to the
; top: .#..#.#
;      #.#....
;      .#.....
;
; As you can see, this display technology is extremely powerful, and will soon dominate the tiny-code-displaying-screen
; market. That's what the advertisement on the back of the display tries to convince you, anyway.
;
; There seems to be an intermediate check of the voltage used by the display: after you swipe your card, if the screen
; did work, how many pixels should be lit?

(defn create-display
  [columns rows]
  {:boundaries {:rows rows :columns columns}
   :lit-pixels #{}})

(defn rect
  {:test (fn []
           (is= (rect (create-display 7 3) 3 2)
                {:boundaries {:rows    3
                              :columns 7}
                 :lit-pixels #{[0 0] [1 0] [2 0]
                               [0 1] [1 1] [2 1]}}))}
  [display columns rows]
  (update display
          :lit-pixels
          (fn [pixels]
            (set (concat pixels
                         (set (for [column (range columns)
                                    row (range rows)]
                                [column row])))))))

(defn display-to-string
  {:test (fn []
           (is= (-> (create-display 7 3)
                    (rect 3 2)
                    (display-to-string))
                (str "###....\n"
                     "###....\n"
                     ".......")))}
  [display]
  (->> (map (fn [row-index]
              (map (fn [column-index]
                     (if (seq-contains? (:lit-pixels display) [column-index row-index])
                       "#"
                       "."))
                   (range (get-in display [:boundaries :columns]))))
            (range (get-in display [:boundaries :rows])))
       (interpose "\n")
       (flatten)
       (clojure.string/join)))

(defn rotate-row
  {:test (fn []
           (is= (-> (create-display 7 3)
                    (rect 3 2)
                    (rotate-row 1 2)
                    (display-to-string))
                (str "###....\n"
                     "..###..\n"
                     "......."))
           (is= (-> (create-display 2 2)
                    (rect 1 1)
                    (rotate-row 0 2)
                    (display-to-string))
                (str "#.\n"
                     "..")))}
  [display index amplitude]
  (update display :lit-pixels (fn [pixels]
                                (set (map (fn [pixel]
                                            (if (not= (second pixel) index)
                                              pixel
                                              [(rem (+ (first pixel) amplitude) (get-in display [:boundaries :columns]))
                                               (second pixel)]))
                                          pixels)))))


(defn rotate-column
  {:test (fn []
           (is= (-> (create-display 7 3)
                    (rect 3 2)
                    (rotate-column 1 2)
                    (display-to-string))
                (str "###....\n"
                     "#.#....\n"
                     ".#....."))
           (is= (-> (create-display 2 2)
                    (rect 1 1)
                    (rotate-column 0 2)
                    (display-to-string))
                (str "#.\n"
                     "..")))}
  [display index amplitude]
  (update display :lit-pixels (fn [pixels]
                                (set (map (fn [pixel]
                                            (if (not= (first pixel) index)
                                              pixel
                                              [(first pixel)
                                               (rem (+ (second pixel) amplitude) (get-in display [:boundaries :rows]))]))
                                          pixels)))))

(defmulti rotate
          (fn [display direction index amplitude]
            direction))

(defmethod rotate :row
  [display direction index amplitude]
  (rotate-row display index amplitude))

(defmethod rotate :column
  [display direction index amplitude]
  (rotate-column display index amplitude))

(deftest combined-test
         (is= (-> (create-display 7 3)
                  (rect 3 2)
                  (rotate-column 1 1)
                  (rotate-row 0 4)
                  (rotate-column 1 1)
                  (display-to-string))
              (str ".#..#.#\n"
                   "#.#....\n"
                   ".#.....")))

(deftest combined-test-with-multimethod
         (is= (-> (create-display 7 3)
                  (rect 3 2)
                  (rotate :column 1 1)
                  (rotate :row 0 4)
                  (rotate :column 1 1)
                  (display-to-string))
              (str ".#..#.#\n"
                   "#.#....\n"
                   ".#.....")))

(defn apply-text-commands
  {:test (fn []
           (is= (apply-text-commands (create-display 2 2)
                                     ["rect 1x1"])
                (rect (create-display 2 2) 1 1))
           (is= (apply-text-commands (create-display 7 3)
                                     ["rect 1x1"
                                      "rotate row y=0 by 5"])
                (-> (create-display 7 3)
                    (rect 1 1)
                    (rotate :row 0 5))))}
  [display texts]
  (reduce (fn [display text]
            (cond (starts-with? text "rect")
                  (let [[columns rows] (split (subs text 5) #"x")]
                    (rect display (read-string columns) (read-string rows)))

                  (starts-with? text "rotate")
                  (let [[_ direction index amplitude] (re-find #"(\w+) \w=(\d+) by (\d+)" (subs text 7))]
                    (rotate display (keyword direction) (read-string index) (read-string amplitude)))))
          display
          texts))

(deftest puzzle-a
         (is= (as-> (slurp "src/advent_of_code/dec_2016/day_08_input.txt") $
                    (split $ #"\n")
                    (apply-text-commands (create-display 50 6) $)
                    (:lit-pixels $)
                    (count $))
              115))

(deftest puzzle-b
         (is= (as-> (slurp "src/advent_of_code/dec_2016/day_08_input.txt") $
                    (split $ #"\n")
                    (apply-text-commands (create-display 50 6) $)
                    (display-to-string $))
              (str "####.####.####.#...##..#.####.###..####..###...##.\n"
                   "#....#....#....#...##.#..#....#..#.#......#.....#.\n"
                   "###..###..###...#.#.##...###..#..#.###....#.....#.\n"
                   "#....#....#......#..#.#..#....###..#......#.....#.\n"
                   "#....#....#......#..#.#..#....#.#..#......#..#..#.\n"
                   "####.#....####...#..#..#.#....#..#.#.....###..##..")))

