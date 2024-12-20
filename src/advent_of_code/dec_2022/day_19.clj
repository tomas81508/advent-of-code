(ns advent-of-code.dec-2022.day-19
  (:require [advent-of-code.test :refer [is-not is=]]))

(def input (->> (slurp "src/advent_of_code/dec_2022/day_19_input.txt")
                (clojure.string/split-lines)))

(def test-input-1 "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.")
(def test-input-2 "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")

(defn create-blueprint-data
  {:test (fn []
           (is= (create-blueprint-data test-input-1)
                {:id       1
                 :ore      {:ore 4}
                 :clay     {:ore 2}
                 :obsidian {:ore 3 :clay 14}
                 :geode    {:ore 2 :obsidian 7}
                 :max      {:ore 4, :clay 14, :obsidian 7}}))}
  [blueprint-input]
  (let [pattern #"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."
        [id ore-robot-ore clay-robot-ore obsidian-roboy-ore obsidian-roboy-clay geode-robot-ore geode-robot-obsidian]
        (->> (re-matches pattern blueprint-input)
             (drop 1)
             (map read-string))
        blueprint {:id       id
                   :ore      {:ore ore-robot-ore}
                   :clay     {:ore clay-robot-ore}
                   :obsidian {:ore obsidian-roboy-ore :clay obsidian-roboy-clay}
                   :geode    {:ore geode-robot-ore :obsidian geode-robot-obsidian}}
        max-stuff (->> (select-keys blueprint [:ore :clay :obsidian :geode])
                       (vals)
                       (reduce (fn [a v]
                                 (reduce-kv (fn [a k v]
                                              (if (> v (get a k))
                                                (assoc a k v)
                                                a))
                                            a
                                            v))
                               {:ore      0
                                :clay     0
                                :obsidian 0}))]
    (assoc blueprint :max max-stuff)))

(def test-blueprint-1 (create-blueprint-data test-input-1))
(def test-blueprint-2 (create-blueprint-data test-input-2))
(def blueprints (->> input
                     (map create-blueprint-data)))

(defn buy-robot
  {:test (fn []
           (is= (buy-robot test-blueprint-1 {:ore 4 :clay 20 :robots {:obsidian 0}} :obsidian)
                {:ore 1 :clay 6 :robots {:obsidian 1}})
           (is-not (buy-robot test-blueprint-1 {:ore 4 :robots {:ore 4}} :ore))
           (is-not (buy-robot test-blueprint-1 {:ore 1 :clay 20 :robots {:obsidian 0}} :obsidian)))}
  [blueprint state robot-type]
  (when (or (= robot-type :geode)
            (< (get-in state [:robots robot-type]) (get-in blueprint [:max robot-type])))
    (let [cost (get blueprint robot-type)]
      (when-let [state (reduce-kv (fn [state type amount]
                                    (if (<= amount (get state type))
                                      ; we have enough
                                      (update state type - amount)
                                      ; we do not have the material
                                      (reduced nil)))
                                  state
                                  cost)]
        (update-in state [:robots robot-type] (fn [n] (if n (inc n) 1)))))))

(defn send-robots-to-collect
  [state]
  (assoc state :collecting (:robots state)))

(def state {:robots   {:ore 1 :clay 0 :obsidian 0 :geode 0}
            :ore      0
            :clay     0
            :obsidian 0
            :geode    0})

(defn get-max-number-of-geodes
  {:test (fn []
           (is= (get-max-number-of-geodes #{{:robots {:ore 1 :clay 0 :obsidian 0 :geode 0} :ore 1 :clay 0 :obsidian 0 :geode 0}})
                0))}
  [states]
  (->> states
       (map :geode)
       (apply max)))

(defn work-a-minute
  {:test (fn []
           (is= (work-a-minute test-blueprint-1 #{state} 10)
                #{{:robots {:ore 1 :clay 0 :obsidian 0 :geode 0} :ore 1 :clay 0 :obsidian 0 :geode 0}}))}
  [blueprint states time-left]
  (let [result-states (reduce (fn [a state]
                                (apply conj a
                                       (as-> (send-robots-to-collect state) $
                                             [$
                                              (buy-robot blueprint $ :ore)
                                              (buy-robot blueprint $ :clay)
                                              (buy-robot blueprint $ :obsidian)
                                              (buy-robot blueprint $ :geode)]
                                             (remove nil? $)
                                             (map (fn [state]
                                                    (-> (reduce-kv (fn [state k v]
                                                                     (update state k + v))
                                                                   state
                                                                   (:collecting state))
                                                        (dissoc :collecting)))
                                                  $)
                                             (into #{} $))))
                              #{}
                              states)
        max-geodes (get-max-number-of-geodes result-states)]
    (->> result-states
         (remove (fn [state]
                   ; This is not an optimal criteria
                   (<= (+ time-left (:geode state)) max-geodes)))
         (set))))

(defn work-on-blueprint
  [blueprint stop-time]
  (loop [states #{state}
         time 0]
    (println time (count states))
    (if (= time stop-time)
      states
      (recur (work-a-minute blueprint states (- stop-time time)) (inc time)))))

(defn work-on-blueprints
  [blueprints stop-time]
  (reduce (fn [a blueprint]
            (let [result (->> (work-on-blueprint blueprint stop-time)
                              (get-max-number-of-geodes))]
              (assoc a (:id blueprint) result)))
          {}
          blueprints))

(comment
  (->> (work-on-blueprint test-blueprint-2 24)
       (get-max-number-of-geodes))

  (work-on-blueprints [test-blueprint-1 test-blueprint-2] 24)

  (time (def puzzle-a (work-on-blueprints blueprints 24)))
  ; "Elapsed time: 193090.068945 msecs"

  (->> puzzle-a
       (reduce-kv (fn [a k v]
                    (+ a (* k v)))
                  0))

  (time (def puzzle-b (work-on-blueprints (take 3 blueprints) 32)))
  ; "Elapsed time: 229748.193372 msecs"

  (->> puzzle-b
       (vals)
       (apply *))

  )


