(ns advent-of-code.dec-2016.day-09
  (:require [advent-of-code.test :refer [is=]]
            [advent-of-code.collections :refer [seq-contains?]]
            [clojure.test :refer [deftest]]
            [clojure.string :refer [join]]))

(defn get-puzzle-input []
  (-> (slurp "src/advent_of_code/dec_2016/day_09_input.txt")))

(def pattern (re-pattern "([^\\(]*)\\(([\\d]+)x([\\d]+)\\)(.*)"))

(defn decompress
  {:test (fn []
           (is= (decompress "ADVENT") "ADVENT")
           (is= (decompress "A(1x5)BC") "ABBBBBC")
           (is= (decompress "(3x3)XYZ") "XYZXYZXYZ")
           (is= (decompress "A(2x2)BCD(2x2)EFG") "ABCBCDEFEFG")
           (is= (decompress "(6x1)(1x3)A") "(1x3)A")
           (is= (decompress "X(8x2)(3x3)ABCY") "X(3x3)ABC(3x3)ABCY")
           )}
  [input]
  (loop [s input
         result ""]
    (if-not (seq-contains? s \()
      (str result s)
      (let [[_ before number-of-characters-as-string repeat-number-as-string after]
            (re-find pattern s)
            number-of-characters (read-string number-of-characters-as-string)
            repeat-number (read-string repeat-number-as-string)
            repeat-characters (subs after 0 number-of-characters)
            after-characters (subs after number-of-characters)]
        (recur after-characters
               (str result before (join (repeat repeat-number repeat-characters))))))))

(deftest puzzle-a
  (is= (-> (get-puzzle-input)
           (decompress)
           (count))
       74532))

(defn decompress-count-2
  {:test (fn []
           (is= (decompress-count-2 "(3x3)XYZ") 9)
           (is= (decompress-count-2 "X(8x2)(3x3)ABCY") 20)
           (is= (decompress-count-2 "(27x12)(20x12)(13x14)(7x10)(1x12)A")
                241920)
           (is= (decompress-count-2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")
                445))}
  [input]
  (loop [s input
         result 0]
    (if (seq-contains? s \()
      (let [[_ before number-of-characters-as-string repeat-number-as-string after]
            (re-find pattern s)
            number-of-characters (read-string number-of-characters-as-string)
            repeat-number (read-string repeat-number-as-string)
            repeat-characters (subs after 0 number-of-characters)
            after-characters (subs after number-of-characters)
            length-of-an-inner-part (decompress-count-2 repeat-characters)
            inner-part-count (* repeat-number length-of-an-inner-part)]
        (recur after-characters
               (+ result (count before) inner-part-count)))
      (+ result (count s)))))

(deftest puzzle-b
  (is= (time (-> (get-puzzle-input)
                 (decompress-count-2)))
       11558231665))