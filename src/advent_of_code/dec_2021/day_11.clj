(ns advent-of-code.dec-2021.day-11
  (:require [advent-of-code.test :refer [is= is is-not]]))

(def puzzle-input "5723573158\n3154748563\n4783514878\n3848142375\n3637724151\n8583172484\n7747444184\n1613367882\n6228614227\n4732225334")

(def test-input "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526")

(defn create-state
  [input]
  (->> input
       (clojure.string/split-lines)
       (reduce-kv (fn [a y row]
                    (reduce-kv (fn [a x n-as-char]
                                 (assoc a [x y] (read-string (str n-as-char))))
                               a
                               (into [] (seq row))))
                  {})))

(def puzzle-state (create-state puzzle-input))

(def test-state (create-state test-input))

(defn step
  {:test (fn []
           (is= (step [test-state 0])
                [(create-state "6594254334\n3856965822\n6375667284\n7252447257\n7468496589\n5278635756\n3287952832\n7993992245\n5957959665\n6394862637")
                 0])
           (is= ((apply comp (repeat 2 step)) [test-state 0])
                [(create-state "8807476555\n5089087054\n8597889608\n8485769600\n8700908800\n6600088989\n6800005943\n0000007456\n9000000876\n8700006848")
                 35])
           (is= ((apply comp (repeat 10 step)) [test-state 0])
                [(create-state "0481112976\n0031112009\n0041112504\n0081111406\n0099111306\n0093511233\n0442361130\n5532252350\n0532250600\n0032240000")
                 204])
           (is= (second ((apply comp (repeat 100 step)) [test-state 0]))
                1656))}
  [[state flash-count]]
  (let [state (reduce-kv (fn [a k v]
                           (assoc a k (inc v)))
                         {}
                         state)
        get-new-flashes (fn [state flashes]
                          (clojure.set/difference (->> (seq state)
                                                       (filter (fn [[_ v]] (> v 9)))
                                                       (map first)
                                                       (set))
                                                  flashes))
        handle-flashs (fn [state flashs]
                        (reduce (fn [state flash]
                                  (->> (for [x (range -1 2) y (range -1 2) :when (not= [x y] [0 0])] [x y])
                                       (map (fn [d] (map + flash d)))
                                       (filter (fn [[x y]] (and (<= 0 x 9) (<= 0 y 9))))
                                       (reduce (fn [state coordinate]
                                                 (update state coordinate inc))
                                               state)))
                                state
                                flashs))
        [state new-flash-count] (loop [state state
                                       flashes #{}
                                       new-flashes (get-new-flashes state flashes)]
                                  (if (empty? new-flashes)
                                    [state (count flashes)]
                                    (let [state (handle-flashs state new-flashes)
                                          flashs (clojure.set/union flashes new-flashes)]
                                      (recur state
                                             flashs
                                             (get-new-flashes state flashs)))))]
    [(reduce-kv (fn [state k v]
                  (if (>= v 10)
                    (assoc state k 0)
                    state))
                state
                state)
     (+ flash-count new-flash-count)]))

(comment
  (time ((apply comp (repeat 100 step)) [puzzle-state 0]))
  ; "Elapsed time: 64.908363 msecs"
  1785
  )

(comment
  (time (loop [state puzzle-state
               index 1]
          (let [[state flash-count] (step [state 0])]
            (if (= 100 flash-count)
              index
              (recur state (inc index))))))
  ; "Elapsed time: 121.67678 msecs"
  354)




