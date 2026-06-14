(ns advent-of-code.dec-2021.day-22
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combinatorics]))

(def test-input-raw ["on x=10..12,y=10..12,z=10..12"
                     "on x=11..13,y=11..13,z=11..13"
                     "off x=9..11,y=9..11,z=9..11"
                     "on x=10..10,y=10..10,z=10..10"])

(defn parse-input
  [input-raw]
  (->> input-raw
       (fn [line]
         {:switch  (if (string/starts-with? line "on") :on :off)
          :corners (->> (re-seq #"-?\d+" line)
                        (combinatorics/))})))


(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {}))}
  [input]
  )


