(ns advent-of-code.dec-2020.day-23
  (:require [ysera.test :refer [is is-not is= deftest]]
            [ysera.collections :refer [seq-contains?]]))

(def puzzle-input "157623984")

(def test-input "389125467")

(defn get-index-of
  [coll v]
  (->> coll
       (map-indexed (fn [index n] [index n]))
       (filter (fn [[_ n]] (= n v)))
       (ffirst)))

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:circle  [3 8 9 1 2 5 4 6 7]
                 :current 3}))}
  [input]
  {:circle  (->> input
                 (map (fn [n] (read-string (str n))))
                 (vec))
   :current (read-string (str (first input)))})

(defn pick-up-three-cups-after-current
  {:test (fn []
           (is= (-> (create-state test-input)
                    (pick-up-three-cups-after-current))
                [[3 2 5 4 6 7] [8 9 1]])
           (is= (-> {:circle  [3 8 9 1 2 5 4 6 7]
                     :current 4}
                    (pick-up-three-cups-after-current))
                [[8 9 1 2 5 4] [6 7 3]]))}
  [state]
  (let [circle (:circle state)]
    (let [current-index (get-index-of (:circle state) (:current state))
          double-circle (vec (apply concat (repeat 2 circle)))
          the-three-cups (subvec double-circle (inc current-index) (+ current-index 4))
          the-three-cups-set (set the-three-cups)]
      [(->> (concat (subvec double-circle 0 (inc current-index))
                    (subvec double-circle (+ current-index 4)))
            (remove (fn [x] (contains? the-three-cups-set x)))
            (take 6))
       the-three-cups])))

(defn get-destination-cup
  {:test (fn []
           (is= (get-destination-cup [8 9 1 7 5 4] 3) 1)
           (is= (get-destination-cup [8 9 6 7 5 4] 3) 9))}
  [cups current-cup]
  (let [maybe-destination-cup (if (= current-cup 1) 9 (dec current-cup))]
    (if (seq-contains? cups maybe-destination-cup)
      maybe-destination-cup
      (recur cups maybe-destination-cup))))

(defn place-cups
  {:test (fn []
           (is= (place-cups [3 2 5 4 6 7] [8 9 1] 2)
                [3 2 8 9 1 5 4 6 7]))}
  [rest-of-cups pick-up-cups destination-cup]
  (let [destination-index (get-index-of rest-of-cups destination-cup)
        [before after] (split-at (inc destination-index) rest-of-cups)]
    (concat before pick-up-cups after)))

(defn update-current
  {:test (fn []
           (is= (update-current [3 2 8 9 1 5 4 6 7] 3)
                2)
           (is= (update-current [3 2 8 9 1 5 4 6 7] 7)
                3))}
  [cups current]
  (let [current-index (get-index-of cups current)]
    (nth cups (if (= current-index 8) 0 (inc current-index)))))

(defn move
  {:test (fn []
           (is= (-> (create-state test-input)
                    (move))
                {:circle  [3 2 8 9 1 5 4 6 7]
                 :current 2})
           (is= (-> (create-state test-input)
                    (move)
                    (move))
                {:circle  [3 2 5 4 6 7 8 9 1]
                 :current 5})
           (is= (as-> (create-state test-input) $
                      ((apply comp (repeat 10 move)) $))
                {:circle  [5 8 3 7 4 1 9 2 6]
                 :current 8})
           )}
  [state]
  (let [[rest-of-cups pick-up-cups] (pick-up-three-cups-after-current state)
        destination-cup (get-destination-cup rest-of-cups (:current state))
        cups (place-cups rest-of-cups pick-up-cups destination-cup)]
    {:circle  cups
     :current (update-current cups (:current state))}))

(defn collect-cup-labels
  {:test (fn []
           (is= (collect-cup-labels [5 8 3 7 4 1 9 2 6])
                "92658374"))}
  [cups]
  (let [index (get-index-of cups 1)
        [before after] (split-at (inc index) cups)]
    (->> (concat after (drop-last before))
         (clojure.string/join))))

(deftest test-cup-labels-after-100-moves
         (is= (as-> (create-state test-input) $
                    ((apply comp (repeat 100 move)) $)
                    (:circle $)
                    (collect-cup-labels $))
              "67384529"))

(deftest puzzle-a
         (is= (time (as-> (create-state puzzle-input) $
                          ((apply comp (repeat 100 move)) $)
                          (:circle $)
                          (collect-cup-labels $)))
              ; "Elapsed time: 4.054698 msecs"
              "58427369"))