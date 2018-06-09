(ns advent-of-code.dec-2016.day-02
  (:require [ysera.test :refer [deftest is is-not is=]]
            [advent-of-code.dec-2016.day-01 :refer [distance abs]]
            [clojure.string :as string]))

; --- Day 2: Bathroom Security ---
;
; You arrive at Easter Bunny Headquarters under cover of darkness. However, you left in such a rush that you forgot to
; use the bathroom! Fancy office buildings like this one usually have keypad locks on their bathrooms, so you search the
; front desk for the code.
;
; "In order to improve security," the document you find says, "bathroom codes will no longer be written down. Instead,
; please memorize and follow the procedure below to access the bathrooms."
;
; The document goes on to explain that each button to be pressed can be found by starting on the previous button and
; moving to adjacent buttons on the keypad: U moves up, D moves down, L moves left, and R moves right. Each line of
; instructions corresponds to one button, starting at the previous button (or, for the first line, the "5" button);
; press whatever button you're on at the end of each line. If a move doesn't lead to a button, ignore it.
;
; You can't hold it much longer, so you decide to figure out the code as you walk to the bathroom. You picture a keypad
; like this:
;
; 1 2 3
; 4 5 6
; 7 8 9
;
; Suppose your instructions are:
;
; ULL
; RRDDD
; LURDL
; UUUUD
; You start at "5" and move up (to "2"), left (to "1"), and left (you can't, and stay on "1"), so the first button is 1.
; Starting from the previous button ("1"), you move right twice (to "3") and then down three times (stopping at "9"
; after two moves and ignoring the third), ending up with 9.
; Continuing from "9", you move left, up, right, down, and left, ending with 8.
; Finally, you move up four times (stopping at "2"), then down once, ending with 5.
; So, in this example, the bathroom code is 1985.
;
; Your puzzle input is the instructions from the document you found at the front desk. What is the bathroom code?

(defn on-square-board?
  {:test (fn []
           (is (on-square-board? [-1 0]))
           (is (on-square-board? [1 1]))
           (is-not (on-square-board? [2 -1]))
           (is-not (on-square-board? [-2 0])))}
  [coordinates]
  (reduce (fn [a coordinate]
            (and a (<= -1 coordinate 1)))
          true
          coordinates))


(def square-coordinate->input-map
  {[-1 1]  "1"
   [0 1]   "2"
   [1 1]   "3"
   [-1 0]  "4"
   [0 0]   "5"
   [1 0]   "6"
   [-1 -1] "7"
   [0 -1]  "8"
   [1 -1]  "9"})


(defn get-direction
  {:test (fn []
           (is= (get-direction \U) [0 1]))}
  [letter-char]
  (condp = letter-char
    \U [0 1]
    \D [0 -1]
    \L [-1 0]
    \R [1 0]))


(defn move-along-instruction
  {:test (fn []
           (is= (move-along-instruction "ULL" [0 0] on-square-board?) [-1 1])
           (is= (move-along-instruction "RRDDD" [-1 1] on-square-board?) [1 -1])
           (is= (move-along-instruction "LURDL" [1 -1] on-square-board?) [0 -1])
           (is= (move-along-instruction "UUUUD" [0 -1] on-square-board?) [0 0]))}
  [instruction start-coordinate on-board?-strategy]
  (reduce (fn [current-coordinate move]
            (let [potential-new-coordinate (map + (get-direction move) current-coordinate)]
              (if (on-board?-strategy potential-new-coordinate)
                potential-new-coordinate
                current-coordinate)))
          start-coordinate
          instruction))

(defn reveal-bathroom-code
  {:test (fn []
           (is= (reveal-bathroom-code [0 0]
                                      on-square-board?
                                      square-coordinate->input-map
                                      "ULL" "RRDDD" "LURDL" "UUUUD")
                "1985"))}
  [start-coordinate on-board?-strategy coordinate->input-map & instructions]
  (->> (reduce (fn [[code current-coordinate] instruction]
                 (let [new-coordinate (move-along-instruction instruction current-coordinate on-board?-strategy)]
                   [(str code (get coordinate->input-map new-coordinate)) new-coordinate]))
               ["" start-coordinate]
               instructions)
       (first)))


(def input-data
  (-> (slurp "src/advent_of_code/dec_2016/day_02_input.txt")
      (string/split #"\n")))

(deftest puzzle-a
         (is= (apply reveal-bathroom-code
                     [0 0]
                     on-square-board?
                     square-coordinate->input-map
                     input-data)
              "97289"))

; --- Part Two ---
;
; You finally arrive at the bathroom (it's a several minute walk from the lobby so visitors can behold the many fancy
; conference rooms and water coolers on this floor) and go to punch in the code. Much to your bladder's dismay, the
; keypad is not at all like you imagined it. Instead, you are confronted with the result of hundreds of man-hours of
; bathroom-keypad-design meetings:
;
;     1
;   2 3 4
; 5 6 7 8 9
;   A B C
;     D
;
; You still start at "5" and stop when you're at an edge, but given the same instructions as above, the outcome is very
; different:
;
; You start at "5" and don't move at all (up and left are both edges), ending at 5.
; Continuing from "5", you move right twice and down three times (through "6", "7", "B", "D", "D"), ending at D.
; Then, from "D", you move five more times (through "D", "B", "C", "C", "B"), ending at B.
; Finally, after five more moves, you end at 3.
; So, given the actual keypad layout, the code would be 5DB3.
;
; Using the same instructions in your puzzle input, what is the correct bathroom code?

(defn on-diamond-board?
  {:test (fn []
           (is (on-diamond-board? [-2 0]))
           (is (on-diamond-board? [1 1]))
           (is-not (on-diamond-board? [2 -1]))
           (is-not (on-diamond-board? [-3 0])))}
  [coordinate]
  (<= (apply distance coordinate) 2))


(def diamond-coordinate->input-map
  {[0 2]   "1"
   [-1 1]  "2"
   [0 1]   "3"
   [1 1]   "4"
   [-2 0]  "5"
   [-1 0]  "6"
   [0 0]   "7"
   [1 0]   "8"
   [2 0]   "9"
   [-1 -1] "A"
   [0 -1]  "B"
   [1 -1]  "C"
   [0 -2]  "D"})

(deftest puzzle-b
  (is= (apply reveal-bathroom-code
                [-2 0]
                on-diamond-board?
                diamond-coordinate->input-map input-data)
       "9A7DC"))

























