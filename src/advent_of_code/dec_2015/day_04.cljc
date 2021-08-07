(ns advent-of-code.dec-2015.day-04
  (:require [ysera.test :refer [is= deftest]]
            [digest :refer [md5]]))

(def puzzle-input "ckczppom")

(deftest puzzle-a
         (is= (time (->> (range)
                         (map-indexed (fn [index n] [index (md5 (str puzzle-input n))]))
                         (filter (fn [[_ x]] (clojure.string/starts-with? x "00000")))
                         (ffirst)))
              ; "Elapsed time: 202.904226 msecs"
              117946))

(deftest puzzle-b
         (is= (time (->> (range)
                         (map-indexed (fn [index n] [index (md5 (str puzzle-input n))]))
                         (filter (fn [[_ x]] (clojure.string/starts-with? x "000000")))
                         (ffirst)))
              ; "Elapsed time: 5900.810003 msecs"
              3938038))




