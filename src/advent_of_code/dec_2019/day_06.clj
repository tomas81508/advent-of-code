(ns advent-of-code.dec-2019.day-06
  (:require [ysera.test :refer [is is-not is= deftest]]))

(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2019/day_06.txt") $
        (clojure.string/split-lines $)))

(def test-data ["COM)B"
                "B)C"
                "C)D"
                "D)E"
                "E)F"
                "B)G"
                "G)H"
                "D)I"
                "E)J"
                "J)K"
                "K)L"])

(defn create-state
  {:test (fn []
           (is= (create-state test-data)
                {"COM" 0
                 "B"   1
                 "G"   2
                 "H"   3
                 "C"   2
                 "D"   3
                 "I"   4
                 "E"   4
                 "J"   5
                 "K"   6
                 "L"   7
                 "F"   5})
           (is= (create-state test-data)
                (create-state (vec (reverse test-data)))))}
  [data]
  (let [pattern #"([^\)]+)\)(.*)"]
    (loop [[x & xs] data
           result {"COM" 0}]
      result
      (if-not x
        result
        (let [[_ parent child] (re-find pattern x)]
          (if (contains? result parent)
            (recur xs (assoc result child (inc (get result parent))))
            (recur (conj (vec xs) x) result)))))))

(defn number-of-orbits
  {:test (fn []
           (is= (->> (create-state test-data)
                     (number-of-orbits))
                42))}
  [state]
  (apply + (vals state)))

(defonce state (->> (get-puzzle-input)
                    (create-state)))

(deftest puzzle-a
         (is= (->> state
                   (number-of-orbits))
              402879))

(defn create-state-2
  {:test (fn []
           (is= (create-state-2 test-data)
                {"COM" {:d 0 :p nil}
                 "B"   {:d 1 :p "COM"}
                 "G"   {:d 2 :p "B"}
                 "H"   {:d 3 :p "G"}
                 "C"   {:d 2 :p "B"}
                 "D"   {:d 3 :p "C"}
                 "I"   {:d 4 :p "D"}
                 "E"   {:d 4 :p "D"}
                 "J"   {:d 5 :p "E"}
                 "K"   {:d 6 :p "J"}
                 "L"   {:d 7 :p "K"}
                 "F"   {:d 5 :p "E"}})
           (is= (create-state test-data)
                (create-state (vec (reverse test-data)))))}
  [data]
  (let [pattern #"([^\)]+)\)(.*)"]
    (loop [[x & xs] data
           result {"COM" {:d 0 :p nil}}]
      result
      (if-not x
        result
        (let [[_ parent child] (re-find pattern x)]
          (if (contains? result parent)
            (recur xs (assoc result
                        child
                        {:d (inc (get-in result [parent :d]))
                         :p parent}))
            (recur (conj (vec xs) x) result)))))))

(defn get-path
  {:test (fn []
           (is= (get-path (create-state-2 test-data) "K")
                (list "COM" "B" "C" "D" "E" "J" "K")))}
  [state x]
  (loop [result (list)
         x x]
    (if (= x "COM")
      (conj result x)
      (recur (conj result x)
             (get-in state [x :p])))))

(defn orbit-distance
  {:test (fn []
           (let [state (create-state-2 test-data)]
             (is= (orbit-distance
                    (get-path state "L")
                    (get-path state "I"))
                  3)))}
  [path-1 path-2]
  (let [common-path-length (loop [[x & xs] path-1
                                  [y & ys] path-2
                                  result 0]
                             (if (not= x y)
                               result
                               (recur xs ys (inc result))))]
    (+ (count path-1)
       (count path-2)
       (- (* 2 common-path-length))
       (- 2))))

(defonce state-2 (create-state-2 (get-puzzle-input)))

(deftest puzzle-b
         (is= (let [you-path (get-path state-2 "YOU")
                    san-path (get-path state-2 "SAN")]
                (orbit-distance you-path san-path))
              484))