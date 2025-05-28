(ns advent-of-code.dec-2015.day-14
  (:require [advent-of-code.test :refer [is=]]
            [clojure.math.combinatorics :as combinatorics]
            [clojure.edn :as edn]))

(def input (-> (slurp "src/advent_of_code/dec_2015/day_14_input.txt")
               (clojure.string/split-lines)))

(def test-input ["Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
                 "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."])

(def line-pattern #"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.")

(defn parse-line
  {:test (fn []
           (is= (parse-line "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.")
                ["Comet" 14 10 127])
           (is= (parse-line "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.")
                ["Dancer" 16 11 162]))}
  [text]
  (let [[_ n speed length rest-length] (re-find line-pattern text)]
    [n (edn/read-string speed) (edn/read-string length) (edn/read-string rest-length)]))

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {"Comet"  {:speed 14 :length 10 :rest-length 127}
                 "Dancer" {:speed 16 :length 11 :rest-length 162}}))}
  [input]
  (->> input
       (reduce (fn [a line]
                 (let [[name speed length rest-length] (parse-line line)]
                   (assoc a name {:speed speed :length length :rest-length rest-length})))
               {})))

(defn race
  {:test (fn []
           (is= (race (create-state test-input) 1000)
                {"Comet" 1120 "Dancer" 1056}))}
  [state time]
  (let [distances (for [[_ {:keys [speed length rest-length]}] state]
                    (let [cycle-length (+ length rest-length)
                          full-cycles (quot time cycle-length)
                          remaining-time (mod time cycle-length)
                          active-time (min remaining-time length)]
                      (+ (* full-cycles speed length) (* active-time speed))))]
    (zipmap (keys state) distances)))

(comment
  (->> (race (create-state input) 2503)
       (vals)
       (apply max))
  )

; part 2

(defn add-point-to-leader
  {:test (fn []
           (is= (add-point-to-leader {"Comet"  {:distance 10 :points 10}
                                      "Dancer" {:distance 20 :points 10}})
                {"Comet"  {:distance 10 :points 10}
                 "Dancer" {:distance 20 :points 11}}))}
  [race-state]
  (let [leaders (reduce-kv (fn [a k v]
                             (cond (> (:distance v) (:distance a))
                                   {:names [k] :distance (:distance v)}

                                   (= (:distance v) (:distance a))
                                   (update a :names conj k)
                                   :else a))
                           {:names    []
                            :distance 0}
                           race-state)]
    (->> (:names leaders)
         (reduce (fn [a leader]
                   (update-in a [leader :points] inc))
                 race-state))))

(defn race-with-bonus
  {:test (fn []
           (is= (race-with-bonus (create-state test-input) 1000)
                {"Comet"  {:distance 1120, :points 312}
                 "Dancer" {:distance 1056, :points 689}}))}
  [state time]
  (loop [race-state (zipmap (keys state) (repeat {:distance 0 :points 0}))
         t 0]
    (if (= t time)
      race-state
      (recur (-> (reduce (fn [a k]
                           (let [active-length (get-in state [k :length])
                                 cycle-length (+ active-length (get-in state [k :rest-length]))
                                 active (< (rem t cycle-length) active-length)]
                             (if-not active
                               a
                               (update-in a [k :distance] + (get-in state [k :speed])))))
                         race-state
                         (keys race-state))
                 (add-point-to-leader))
             (inc t)))))

(comment
  (->> (race-with-bonus (create-state input) 2503)
       (vals)
       (map :points)
       (apply max))
  )
