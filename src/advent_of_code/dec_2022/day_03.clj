(ns advent-of-code.dec-2022.day-03
  (:require [ysera.test :refer [is= deftest]]))

(def input-data (->> (slurp "src/advent_of_code/dec_2022/day_03_input.txt")
                     (clojure.string/split-lines)))

(def things "vJrwpWtwJgWrhcsFMMfFFhFp")

(defn split-in-half
  [rucksack]
  (let [total-number-of-things (count rucksack)
        half-number-of-things (/ total-number-of-things 2)]
    [(subs rucksack 0 half-number-of-things)
     (subs rucksack half-number-of-things)]))

(defn find-similar-thing
  [parts]
  (let [parts-as-set (map set parts)]
    (-> (apply clojure.set/intersection parts-as-set)
        (first))))

(split-in-half things)

(->> (split-in-half things)
     (find-similar-thing))

(defn get-priority
  {:test (fn []
           (is= (get-priority \a) 1)
           (is= (get-priority \z) 26)
           (is= (get-priority \A) 27)
           (is= (get-priority \Z) 52))}
  [thing]
  (if (< 96 (int thing))
    (- (int thing) 96)
    (- (int thing) 38)))


(->> input-data
     (map split-in-half)
     (map find-similar-thing)
     (map get-priority)
     (apply +))


(def trio ["nJLgNcQDNMlQHMvCbv"
           "zphFpmTszmwhGGFhhtppNfffVlvZvHCCVZzbfzvS"
           "mTTsmTRGstsFhWwtWjPRdjnJdjJnLjcLNd"])

(find-similar-thing trio)

(->> input-data
     (partition 3)
     (map find-similar-thing)
     (map get-priority)
     (apply +))
