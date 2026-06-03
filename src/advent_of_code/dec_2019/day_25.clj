(ns advent-of-code.dec-2019.day-25
  (:require [advent-of-code.test :refer [is=]]
            [advent-of-code.dec-2019.day-09 :as day-09]
            [clojure.test :refer [deftest]]
            [clojure.string :as string]
            [clojure.edn :as edn]))

; With Daniel Gullberg

(def input (slurp "src/advent_of_code/dec_2019/day_25.txt"))

(def program (day-09/parse-program input))

(defn get-output-string
  [output]
  (->> output
       (map char)
       (apply str)))

(defn print-output!
  [output]
  (-> (get-output-string output)
      (println)))

(def inputs (as-> ["north"
                   ;; Gets stuck
                   ; "take giant electromagnet"
                   "west"
                   ; It is suddenly completely dark! You are eaten by a Grue!
                   ; "take photons"
                   "east"
                   "south"

                   "east"
                   ;"take manifold"
                   "south"
                   ; too heavy
                   ;"take whirled peas"
                   "north"
                   "west"

                   "south"
                   "take space heater"
                   "south"
                   ;"take dark matter"
                   "west"
                   ; You're launched into space! Bye!
                   ; "take escape pod"
                   "east"
                   "north"
                   "east"
                   "north"
                   ; The molten lava is way too hot! You melt!
                   ; "take molten lava"
                   "west"
                   "north"
                   ; program never ends
                   ;"take infinite loop"
                   "south"
                   "south"
                   "take antenna"
                   "south"
                   "north"
                   "north"
                   "east"
                   "south"
                   "east"
                   ;"take bowl of rice"
                   "north"
                   "take klein bottle"
                   "north"
                   "take spool of cat6"
                   "west"
                   "north"

                   ]
                  $
                  (clojure.string/join "\n" $)
                  (str $ "\n")
                  (map int $)))

(deftest puzzle
  (->> (day-09/run program inputs)
       (:outputs)
       (get-output-string)
       (print-output!))
  ; 8462464
  )

