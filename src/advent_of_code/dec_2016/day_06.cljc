(ns advent-of-code.dec-2016.day-06
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]))

; --- Day 6: Signals and Noise ---
;
; Something is jamming your communications with Santa. Fortunately, your signal is only partially jammed, and protocol
; in situations like this is to switch to a simple repetition code to get the message through.
;
; In this model, the same message is sent repeatedly. You've recorded the repeating message signal (your puzzle input),
; but the data seems quite corrupted - almost too badly to recover. Almost.
;
; All you need to do is figure out which character is most frequent for each position. For example, suppose you had
; recorded the following messages:
;
; eedadn
; drvtee
; eandsr
; raavrd
; atevrs
; tsrnev
; sdttsa
; rasrtv
; nssdts
; ntnada
; svetve
; tesnvt
; vntsnd
; vrdear
; dvrsen
; enarar
;
; The most common character in the first column is e; in the second, a; in the third, s, and so on. Combining these
; characters returns the error-corrected message, easter.
;
; Given the recording in your puzzle input, what is the error-corrected version of the message being sent?

(defn error-correction
  {:test (fn []
           (is= (error-correction ["eedadn"
                                   "drvtee"
                                   "eandsr"
                                   "raavrd"
                                   "atevrs"
                                   "tsrnev"
                                   "sdttsa"
                                   "rasrtv"
                                   "nssdts"
                                   "ntnada"
                                   "svetve"
                                   "tesnvt"
                                   "vntsnd"
                                   "vrdear"
                                   "dvrsen"
                                   "enarar"])
                "easter"))}
  [input]
  (->> (apply map list input)
       (map (fn [letters] (->> (frequencies letters)
                               (sort-by second)
                               (last)
                               (first))))
       (clojure.string/join)))

(deftest puzzle-a
  (is= (-> (slurp "src/advent_of_code/dec_2016/day_06_input.txt")
           (clojure.string/split #"\n")
           (error-correction))
       "bjosfbce"))

(defn error-correction-least-common
  {:test (fn []
           (is= (error-correction-least-common ["eedadn"
                                                "drvtee"
                                                "eandsr"
                                                "raavrd"
                                                "atevrs"
                                                "tsrnev"
                                                "sdttsa"
                                                "rasrtv"
                                                "nssdts"
                                                "ntnada"
                                                "svetve"
                                                "tesnvt"
                                                "vntsnd"
                                                "vrdear"
                                                "dvrsen"
                                                "enarar"])
                "advent"))}
  [input]
  (->> (apply map list input)
       (map (fn [letters] (->> (frequencies letters)
                               (sort-by second)
                               (ffirst))))
       (clojure.string/join)))

(deftest puzzle-b
  (is= (-> (slurp "src/advent_of_code/dec_2016/day_06_input.txt")
           (clojure.string/split #"\n")
           (error-correction-least-common))
       "veqfxzfx"))