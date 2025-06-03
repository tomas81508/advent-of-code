(ns advent-of-code.dec-2015.day-21
  (:require [advent-of-code.test :refer [is=]]))

(def boss {:hit-points 103
            :damage     9
            :armor      2})

(def shop {:weapons {:dagger     {:cost 8 :damage 4 :armor 0}
                     :shortsword {:cost 10 :damage 5 :armor 0}
                     :warhammer  {:cost 25 :damage 6 :armor 0}
                     :longsword  {:cost 40 :damage 7 :armor 0}
                     :greataxe   {:cost 74 :damage 8 :armor 0}}

           :armor   {:leather    {:cost 13 :damage 0 :armor 1}
                     :chainmail  {:cost 31 :damage 0 :armor 2}
                     :splintmail {:cost 53 :damage 0 :armor 3}
                     :bandedmail {:cost 75 :damage 0 :armor 4}
                     :platemail  {:cost 102 :damage 0 :armor 5}}

           :rings   {:damage+1  {:cost 25 :damage 1 :armor 0}
                     :damage+2  {:cost 50 :damage 2 :armor 0}
                     :damage+3  {:cost 100 :damage 3 :armor 0}
                     :defense+1 {:cost 20 :damage 0 :armor 1}
                     :defense+2 {:cost 40 :damage 0 :armor 2}
                     :defense+3 {:cost 80 :damage 0 :armor 3}}})

(defn dead? [{hit-points :hit-points}] (not (pos? hit-points)))

(defn fight
  {:test (fn []
           (is= (fight {:hit-points 8
                        :damage     5
                        :armor      5}
                       {:hit-points 12
                        :damage     7
                        :armor      2})
                2))}
  [player boss]
  (let [boss (update boss :hit-points - (max (- (:damage player) (:armor boss)) 1))]
    (if (dead? boss)
      (:hit-points player)
      (let [player (update player :hit-points - (max (- (:damage boss) (:armor player)) 1))]
        (if (dead? player)
          (:hit-points player)
          (recur player boss))))))

(defn create-states
  []
  (for [weapon (seq (:weapons shop))
        armor (seq (:armor shop))
        ring-1 (conj (seq (:rings shop)) nil)
        ring-2 (conj (seq (:rings shop)) nil)
        :when (or (nil? ring-1)
                  (not= ring-1 ring-2))]
    {:damage (+ (:damage (second weapon)) (get-in ring-1 [1 :damage] 0) (get-in ring-2 [1 :damage] 0))
     :armor (+ (:armor (second armor)) (get-in ring-1 [1 :armor] 0) (get-in ring-2 [1 :armor] 0))
     :cost (+ (get-in weapon [1 :cost]) (get-in armor [1 :cost]) (get-in ring-1 [1 :cost] 0) (get-in ring-2 [1 :cost] 0))}))

(defn part-1 []
  (->> (create-states)
       (sort-by :cost)
       (distinct)
       (some (fn [state]
               (let [result (fight (assoc state :hit-points 100) boss)]
                 (when (pos? result)
                   (:cost state)))))))

(comment
  (time (part-1))
  ; "Elapsed time: 5.297333 msecs"
  ; => 121
  )

(defn part-2 []
  (->> (create-states)
       (sort-by :cost)
       (distinct)
       (reverse)
       (some (fn [state]
               (let [result (fight (assoc state :hit-points 100) boss)]
                 (when-not (pos? result)
                   (:cost state)))))))

(comment
  (time (part-2))
  ; "Elapsed time: 1.9775 msecs"
  ; => 201
  )

