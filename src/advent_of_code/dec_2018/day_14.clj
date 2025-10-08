(ns advent-of-code.dec-2018.day-14
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(def input 323081)

(def state {:recipes [3 7] :elves [0 1] :size 2})

(defn process
  {:test (fn []
           (is= (process state)
                {:recipes [3 7 1 0] :elves [0 1] :size 4}))}
  [{recipes :recipes elves :elves size :size}]
  (let [elv1-score (get recipes (first elves))
        elv2-score (get recipes (second elves))
        elves-score (+ elv1-score elv2-score)
        new-recipes (if (< elves-score 10)
                      [elves-score]
                      [1 (rem elves-score 10)])
        recipes (apply conj recipes new-recipes)
        number-of-recipes (count recipes)
        elv1-recipe (rem (inc (+ elv1-score (first elves))) number-of-recipes)
        elv2-recipe (rem (inc (+ elv2-score (second elves))) number-of-recipes)]
    {:recipes recipes
     :elves   [elv1-recipe elv2-recipe]
     :size    (+ size (count new-recipes))}))

(defn score-of-ten-recipes-after
  {:test (fn []
           (is= (score-of-ten-recipes-after state 9) 5158916779)
           (is= (score-of-ten-recipes-after state 2018) 5941429882))}
  [state n]
  (loop [state state]
    (if (> (count (:recipes state)) (+ 9 n))
      (->> (take-last 10 (:recipes state))
           (apply str)
           (edn/read-string))
      (recur (process state)))))

(defn puzzle-1
  []
  (time (score-of-ten-recipes-after state input))
  ; "Elapsed time: 432.794041 msecs"
  ; => 7162937112
  )

(defn check-last-recipes
  {:test (fn []
           (is (check-last-recipes [5 1 5 8 9 1 6 7 7 9] [1 6 7 7 9] 10))
           (is (check-last-recipes [3 7 1 0 1 0 1 2 4 5 1 5 8 9] [5 1 5 8 9] 14))
           (is (check-last-recipes [3 7 1 0 1 0 1 2 4 5 1 5 8 9 2] [5 1 5 8 9] 14))
           (is-not (check-last-recipes [5 1 5 8 9 1 6 7 7 9] [2 6 7 7 9] 10)))}
  ([recipes end-recipes size]
   (or (check-last-recipes recipes end-recipes size 0)
       (check-last-recipes recipes end-recipes size 1)))
  ([recipes end-recipes size mod]
   (let [length-of-end-recipes (count end-recipes)]
     (loop [index 1]
       (cond (> index length-of-end-recipes)
             true

             (= (get recipes (- size index mod)) (get end-recipes (- length-of-end-recipes index)))
             (recur (inc index))

             :else
             false)))))

(defn first-appears
  {:test (fn []
           (is= (first-appears state 51589) 9)
           (is= (first-appears state 92510) 18)
           (is= (first-appears state 59414) 2018))}
  [state n]
  (let [end-recipes (as-> (str n) $
                          (string/split $ #"")
                          (map edn/read-string $)
                          (vec $))]
    (loop [state state]
      (let [recipes (:recipes state)]
        (if (check-last-recipes recipes end-recipes (:size state))
          (let [m (if (check-last-recipes recipes end-recipes (:size state) 0) 0 1)]
            (- (count recipes) (count end-recipes) m))
          (recur (process state)))))))

(defn puzzle-2
  []
  (time (first-appears state input))
  ; "Elapsed time: 4768.565083 msecs"
  ; => 20195890
  )
