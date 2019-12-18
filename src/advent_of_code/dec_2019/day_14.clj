(ns advent-of-code.dec-2019.day-14
  (:require [ysera.test :refer [is is-not is= deftest]]))

(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2019/day_14.txt") $
        (clojure.string/split-lines $)))


(defn reaction->data
  {:test (fn []
           (is= (reaction->data "7 A, 1 C => 1 D")
                {"D" {:amount      1
                      :ingredients [{:item "A" :amount 7}
                                    {:item "C" :amount 1}]}}))}
  [reaction]
  (let [string->data (fn [s]
                       (let [[_ amount item] (re-find #"(\d+) ([A-Z]+)" s)]
                         {:amount (read-string amount) :item item}))
        index (clojure.string/index-of reaction "=>")
        inputs (as-> (subs reaction 0 (dec index)) $
                     (clojure.string/split $ #", ")
                     (map string->data $))
        output (-> (subs reaction (+ index 3))
                   (string->data))]
    {(:item output) {:amount      (:amount output)
                     :ingredients inputs}}))

(defn create-state
  {:test (fn []
           (is= (create-state ["10 ORE => 10 A"
                               "1 ORE => 1 B"
                               "7 A, 1 B => 1 C"
                               "7 A, 1 C => 1 D"
                               "7 A, 1 D => 1 E"
                               "7 A, 1 E => 1 FUEL"])
                {"A"    {:amount      10
                         :ingredients [{:item "ORE" :amount 10}]}
                 "B"    {:amount      1
                         :ingredients [{:item "ORE" :amount 1}]}
                 "C"    {:amount      1
                         :ingredients [{:item "A" :amount 7} {:item "B" :amount 1}]}
                 "D"    {:amount      1
                         :ingredients [{:item "A" :amount 7} {:item "C" :amount 1}]}
                 "E"    {:amount      1
                         :ingredients [{:item "A" :amount 7} {:item "D" :amount 1}]}
                 "FUEL" {:amount      1
                         :ingredients [{:item "A" :amount 7} {:item "E" :amount 1}]}}))}
  [reactions]
  (->> reactions
       (map reaction->data)
       (reduce merge
               {})))

(defn reduce-step
  {:test (fn []
           (let [state {"A"    {:amount      10
                                :ingredients [{:item "ORE" :amount 10}]}
                        "B"    {:amount      1
                                :ingredients [{:item "ORE" :amount 1}]}
                        "C"    {:amount      1
                                :ingredients [{:item "A" :amount 7} {:item "B" :amount 1}]}
                        "D"    {:amount      1
                                :ingredients [{:item "A" :amount 7} {:item "C" :amount 1}]}
                        "E"    {:amount      1
                                :ingredients [{:item "A" :amount 7} {:item "D" :amount 1}]}
                        "FUEL" {:amount      1
                                :ingredients [{:item "A" :amount 7} {:item "E" :amount 1}]}}]
             (is= (reduce-step state {:needed-items [{:item "FUEL" :amount 1}]
                                      :extra-items  {}
                                      :ORE          0})
                  {:needed-items [{:item "A" :amount 7} {:item "E" :amount 1}]
                   :extra-items  {}
                   :ORE          0})
             (is= (reduce-step state {:needed-items [{:item "A" :amount 7} {:item "E" :amount 1}]
                                      :extra-items  {}
                                      :ORE          0})
                  {:needed-items [{:item "E" :amount 1} {:item "ORE" :amount 10}]
                   :extra-items  {"A" 3}
                   :ORE          0})
             (is= (reduce-step state {:needed-items [{:item "E" :amount 1} {:item "ORE" :amount 10}]
                                      :extra-items  {"A" 3}
                                      :ORE          0})
                  {:needed-items [{:item "ORE" :amount 10} {:item "A" :amount 7} {:item "D" :amount 1}]
                   :extra-items  {"A" 3}
                   :ORE          0})
             (is= (reduce-step state {:needed-items [{:item "ORE" :amount 10} {:item "A" :amount 7} {:item "D" :amount 1}]
                                      :extra-items  {"A" 3}
                                      :ORE          0})
                  {:needed-items [{:item "A" :amount 7} {:item "D" :amount 1}]
                   :extra-items  {"A" 3}
                   :ORE          10})
             (is= (reduce-step state {:needed-items [{:item "A" :amount 7} {:item "D" :amount 1}]
                                      :extra-items  {"A" 3}
                                      :ORE          10})
                  {:needed-items [{:item "D" :amount 1} {:item "ORE" :amount 10}]
                   :extra-items  {"A" 6}
                   :ORE          10})
             (is= (reduce-step state {:needed-items [{:item "D" :amount 1} {:item "ORE" :amount 10}]
                                      :extra-items  {"A" 6}
                                      :ORE          10})
                  {:needed-items [{:item "ORE" :amount 10} {:item "A" :amount 7} {:item "C" :amount 1}]
                   :extra-items  {"A" 6}
                   :ORE          10})
             (is= (reduce-step state {:needed-items [{:item "ORE" :amount 10} {:item "A" :amount 7} {:item "C" :amount 1}]
                                      :extra-items  {"A" 6}
                                      :ORE          10})
                  {:needed-items [{:item "A" :amount 7} {:item "C" :amount 1}]
                   :extra-items  {"A" 6}
                   :ORE          20})
             (is= (reduce-step state {:needed-items [{:item "A" :amount 7} {:item "C" :amount 1}]
                                      :extra-items  {"A" 6}
                                      :ORE          20})
                  {:needed-items [{:item "C" :amount 1} {:item "ORE" :amount 10}]
                   :extra-items  {"A" 9}
                   :ORE          20})
             (is= (reduce-step state {:needed-items [{:item "C" :amount 1} {:item "ORE" :amount 10}]
                                      :extra-items  {"A" 9}
                                      :ORE          20})
                  {:needed-items [{:item "ORE" :amount 10} {:item "A" :amount 7} {:item "B" :amount 1}]
                   :extra-items  {"A" 9}
                   :ORE          20})
             (is= (reduce-step state {:needed-items [{:item "ORE" :amount 10} {:item "A" :amount 7} {:item "B" :amount 1}]
                                      :extra-items  {"A" 9}
                                      :ORE          20})
                  {:needed-items [{:item "A" :amount 7} {:item "B" :amount 1}]
                   :extra-items  {"A" 9}
                   :ORE          30})
             (is= (reduce-step state {:needed-items [{:item "A" :amount 7} {:item "B" :amount 1}]
                                      :extra-items  {"A" 9}
                                      :ORE          30})
                  {:needed-items [{:item "B" :amount 1}]
                   :extra-items  {"A" 2}
                   :ORE          30})
             (is= (reduce-step state {:needed-items [{:item "B" :amount 1}]
                                      :extra-items  {"A" 2}
                                      :ORE          30})
                  {:needed-items [{:item "ORE" :amount 1}]
                   :extra-items  {"A" 2}
                   :ORE          30})
             (is= (reduce-step state {:needed-items [{:item "ORE" :amount 1}]
                                      :extra-items  {"A" 2}
                                      :ORE          30})
                  {:needed-items []
                   :extra-items  {"A" 2}
                   :ORE          31})

             (let [state {"A"    {:amount 2 :ingredients [{:amount 9 :item "ORE"}]}
                          "B"    {:amount 3 :ingredients [{:amount 8 :item "ORE"}]}
                          "C"    {:amount 5 :ingredients [{:amount 7 :item "ORE"}]}
                          "AB"   {:amount 1 :ingredients [{:amount 3 :item "A"} {:amount 4 :item "B"}]}
                          "BC"   {:amount 1 :ingredients [{:amount 5 :item "B"} {:amount 7 :item "C"}]}
                          "CA"   {:amount 1 :ingredients [{:amount 4 :item "C"} {:amount 1 :item "A"}]}
                          "FUEL" {:amount 1 :ingredients [{:amount 2 :item "AB"} {:amount 3 :item "BC"} {:amount 4 :item "CA"}]}}]
               (is= (reduce-step state
                                 {:needed-items [{:amount 2 :item "AB"} {:amount 3 :item "BC"} {:amount 4 :item "CA"}]
                                  :extra-items  {}
                                  :ORE          0})
                    {:needed-items [{:amount 3 :item "BC"} {:amount 4 :item "CA"} {:amount 6 :item "A"} {:amount 8 :item "B"}]
                     :extra-items  {}
                     :ORE          0})
               (is= (reduce-step state
                                 {:needed-items [{:amount 8 :item "B"} {:amount 15 :item "B"} {:amount 21 :item "C"} {:amount 16 :item "C"} {:amount 4 :item "A"} {:amount 27 :item "ORE"}]
                                  :extra-items  {}
                                  :ORE          0})
                    {:needed-items [{:amount 15 :item "B"} {:amount 21 :item "C"} {:amount 16 :item "C"} {:amount 4 :item "A"} {:amount 27 :item "ORE"} {:amount 24 :item "ORE"}]
                     :extra-items  {"B" 1}
                     :ORE          0})
               (is= (reduce-step state
                                 {:needed-items [{:amount 16 :item "C"} {:amount 4 :item "A"} {:amount 27 :item "ORE"} {:amount 24 :item "ORE"} {:amount 40 :item "ORE"} {:amount 35 :item "ORE"}]
                                  :extra-items  {"B" 1 "C" 4}
                                  :ORE          0})
                    {:needed-items [{:amount 4 :item "A"} {:amount 27 :item "ORE"} {:amount 24 :item "ORE"} {:amount 40 :item "ORE"} {:amount 35 :item "ORE"} {:amount 21 :item "ORE"}]
                     :extra-items  {"B" 1 "C" 3}
                     :ORE          0}))))}
  [state data]
  (let [[{item :item amount :amount} & needed-items] (:needed-items data)]
    (cond (= item "ORE")
          (-> data
              (assoc :needed-items (or needed-items []))
              (update :ORE (fn [x] (+' x amount))))

          ; If we already have the stuff from previous reactions
          (and (contains? (:extra-items data) item)
               (>= (get-in data [:extra-items item])
                   amount))
          (as-> data $
                (assoc $ :needed-items needed-items)
                (update $ :extra-items (fn [ei] (if (= (get ei item) amount)
                                                  (dissoc ei item)
                                                  (update ei item (fn [x] (- x amount)))))))

          :else
          (let [minimum-amount (get-in state [item :amount])
                old-extra-amount (get-in data [:extra-items item] 0)
                needed-amount (- amount old-extra-amount)
                ceil (fn [x] (bigint (Math/ceil (double x))))
                total-amount (* (ceil (/ needed-amount minimum-amount)) minimum-amount)
                new-extra-amount (- total-amount needed-amount)
                amount-factor (/ total-amount minimum-amount)]
            (as-> data $
                  (cond (pos? new-extra-amount)
                        (update data :extra-items (fn [ei] (assoc ei item new-extra-amount)))

                        (pos? old-extra-amount)
                        (update data :extra-items (fn [ei] (dissoc ei item)))

                        :else
                        $)
                  (assoc $ :needed-items (concat needed-items
                                                 (map (fn [ingredient]
                                                        (update ingredient :amount (fn [x] (* x amount-factor))))
                                                      (get-in state [item :ingredients])))))))))

(defn calculate-ore
  {:test (fn []
           (is= (-> (create-state ["10 ORE => 10 A"
                                   "1 ORE => 1 B"
                                   "7 A, 1 B => 1 C"
                                   "7 A, 1 C => 1 D"
                                   "7 A, 1 D => 1 E"
                                   "7 A, 1 E => 1 FUEL"])
                    (calculate-ore 1))
                31))}
  [state fuel]
  (loop [data {:needed-items [{:item "FUEL" :amount fuel}]
               :extra-items  {}
               :ORE          0}]
    (let [data (reduce-step state data)]
      ; (println data)
      (if (empty? (:needed-items data))
        (:ORE data)
        (recur data)))))

(deftest test-1
         (is= (-> ["9 ORE => 2 A"
                   "8 ORE => 3 B"
                   "7 ORE => 5 C"
                   "3 A, 4 B => 1 AB"
                   "5 B, 7 C => 1 BC"
                   "4 C, 1 A => 1 CA"
                   "2 AB, 3 BC, 4 CA => 1 FUEL"]
                  (create-state)
                  (calculate-ore 1))
              165))

(deftest test-2
         (is= (-> ["157 ORE => 5 NZVS"
                   "165 ORE => 6 DCFZ"
                   "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
                   "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
                   "179 ORE => 7 PSHF"
                   "177 ORE => 5 HKGWZ"
                   "7 DCFZ, 7 PSHF => 2 XJWVT"
                   "165 ORE => 2 GPVTF"
                   "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"]
                  (create-state)
                  (calculate-ore 1))
              13312))

(deftest puzzle-a
         (is= (time (-> (get-puzzle-input)
                        (create-state)
                        (calculate-ore 1)))
              ; "Elapsed time: 14.474084 msecs"
              612880))

(deftest test-2b
         (let [state (create-state ["157 ORE => 5 NZVS"
                                    "165 ORE => 6 DCFZ"
                                    "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
                                    "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
                                    "179 ORE => 7 PSHF"
                                    "177 ORE => 5 HKGWZ"
                                    "7 DCFZ, 7 PSHF => 2 XJWVT"
                                    "165 ORE => 2 GPVTF"
                                    "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"])]
           (is (< (calculate-ore state 82892753)
                  1000000000000
                  (calculate-ore state 82892754)))))

(deftest puzzle-b
         ; This can easily be solved by binary search
         (let [state (create-state (get-puzzle-input))]
           (is= (calculate-ore state 2509120)
                999999564112N)
           (is= (calculate-ore state 2509121)
                1000000084167N)))
