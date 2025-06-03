(ns advent-of-code.dec-2015.day-22
  (:require [advent-of-code.test :refer [is=]]))

(def boss {:hit-points 51
           :damage     9})

(def spells [{:name :magic-missile :cost 53 :damage 4}
             {:name :drain :cost 73 :damage 2 :heal 2}
             {:name :shield :cost 113 :duration 6 :armor 7}
             {:name :poison :cost 173 :duration 6 :damage 3}
             {:name :recharge :cost 229 :duration 5 :mana 101}])

(defn dead? [{hit-points :hit-points}] (not (pos? hit-points)))

(def player {:hit-points 50 :mana 500})

(defn cost-to-much?
  [{mana :mana} {cost :cost}]
  (> cost mana))

(defn already-active?
  [player {duration :duration name :name}]
  (and duration
       (case name
         :shield (:shield player)
         :poison (:poison player)
         :recharge (:recharge player))))

(defn apply-spell
  [fight spell]
  (case (:name spell)
    :magic-missile (-> fight
                       (update-in [:boss :hit-points] - 4)
                       (update-in [:player :mana] - 53)
                       (update :mana-spent + 53))
    :drain (-> fight
               (update-in [:boss :hit-points] - 2)
               (update-in [:player :hit-points] + 2)
               (update-in [:player :mana] - 73)
               (update :mana-spent + 73))
    :shield (-> fight
                (assoc-in [:player :shield] {:amount 7 :duration 6})
                (update-in [:player :mana] - 113)
                (update :mana-spent + 113))
    :poison (-> fight
                (assoc-in [:player :poison] {:amount 3 :duration 6})
                (update-in [:player :mana] - 173)
                (update :mana-spent + 173))
    :recharge (-> fight
                  (assoc-in [:player :recharge] {:amount 101 :duration 5})
                  (update-in [:player :mana] - 229)
                  (update :mana-spent + 229))))

(defn apply-all-combinations
  [fight]
  (->> spells
       (remove (fn [a] (cost-to-much? (:player fight) a)))
       (remove (fn [a] (already-active? (:player fight) a)))
       (map (fn [a] (apply-spell fight a)))))

(defn decrease-or-remove-effect
  [player effect]
  (if (= 1 (get-in player [effect :duration]))
    (dissoc player effect)
    (update-in player [effect :duration] dec)))

(defn maybe-apply-poison
  [fight]
  (let [poison (get-in fight [:player :poison])]
    (if-not poison
      fight
      (-> fight
          (update-in [:boss :hit-points] - (:amount poison))
          (update :player decrease-or-remove-effect :poison)))))

(defn maybe-apply-recharge
  [fight]
  (let [recharge (get-in fight [:player :recharge])]
    (if-not recharge
      fight
      (-> fight
          (update-in [:player :mana] + (:amount recharge))
          (update :player decrease-or-remove-effect :recharge)))))

(defn maybe-remove-shield
  [fight]
  (let [shield (get-in fight [:player :shield])]
    (if-not shield
      fight
      (-> fight
          (update :player decrease-or-remove-effect :shield)))))

(defn apply-effects
  [fight]
  (-> fight
      (maybe-apply-poison)
      (maybe-apply-recharge)
      (maybe-remove-shield)))

(defn win?
  [fight]
  (not (pos? (get-in fight [:boss :hit-points]))))

(defn apply-player-actions
  [fight]
  {:pre [fight]}
  (->> fight
       (apply-effects)
       (apply-all-combinations)))

(defn apply-boss-actions
  [fight]
  (let [fight (-> fight
                  (apply-effects))]
    (if (win? fight)
      fight
      (update-in fight [:player :hit-points] - (- (get-in fight [:boss :damage])
                                                  (get-in fight [:player :shield :amount] 0))))))

(defn fight
  ([player boss] (fight player boss 0))
  ([player boss mana-spent]
   (loop [fights [{:player player :boss boss :mana-spent mana-spent}]]
     (if (empty? fights)
       :error
       (let [[best-score-fight & r] fights
             new-fights (apply-player-actions best-score-fight)
             winning-fight (->> new-fights
                                (filter win?)
                                (sort-by :mana-spent)
                                (first))]
         (if winning-fight
           winning-fight
           (let [new-fights (->> new-fights
                                 (map apply-boss-actions)
                                 (remove (fn [f] (dead? (:player f)))))
                 winning-fight (->> new-fights
                                    (filter win?)
                                    (sort-by :mana-spent)
                                    (first))]
             (if winning-fight
               winning-fight
               (recur (->> (concat new-fights r)
                           (distinct)
                           (sort-by :mana-spent)))))))))))

(defn part-1 []
  (:mana-spent (fight player boss))
  )

(comment
  (time (part-1))
  ; "Elapsed time: 102.565292 msecs"
  ; => 900
  )

(defn apply-player-actions-2
  [fight]
  {:pre [fight]}
  (let [fight (update-in fight [:player :hit-points] dec)]
    (if (dead? (:player fight))
      []
      (-> fight
          (apply-effects)
          (apply-all-combinations)))))

(defn fight-2
  ([player boss] (fight-2 player boss 0))
  ([player boss mana-spent]
   (loop [fights [{:player player :boss boss :mana-spent mana-spent}]]
     (if (empty? fights)
       :error
       (let [[best-score-fight & r] fights
             new-fights (apply-player-actions-2 best-score-fight)
             winning-fight (->> new-fights
                                (filter win?)
                                (sort-by :mana-spent)
                                (first))]
         (if winning-fight
           winning-fight
           (let [new-fights (->> new-fights
                                 (map apply-boss-actions)
                                 (remove nil?)
                                 (remove (fn [f] (dead? (:player f)))))
                 winning-fight (->> new-fights
                                    (filter win?)
                                    (sort-by :mana-spent)
                                    (first))]
             (if winning-fight
               winning-fight
               (recur (->> (concat new-fights r)
                           (distinct)
                           (sort-by :mana-spent)))))))))))

(comment
  (time (:mana-spent (fight-2 player boss)))
  ; "Elapsed time: 163.78375 msecs"
  ; => 1216
  )