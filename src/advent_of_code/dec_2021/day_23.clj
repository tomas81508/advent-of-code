(ns advent-of-code.dec-2021.day-23
  (:require [advent-of-code.test :refer [is=]]
            [clojure.math.combinatorics :refer [combinations]]
            [advent-of-code.math :refer [floor ceil]]))

(def puzzle-input ["#############"
                   "#...........#"
                   "###D#A#D#C###"
                   "  #B#C#B#A#"
                   "  #########"])


