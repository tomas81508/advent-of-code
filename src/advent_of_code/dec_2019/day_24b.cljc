(ns advent-of-code.dec-2019.day-24b
  (:require [ysera.test :refer [is is= is-not]]
            [clojure.math :refer [pow]]
            [clojure.set :refer [union]]))

(def input ["..##."
            "..#.."
            "##..."
            "#...."
            "...##"])

(def test-input ["....#"
                 "#..#."
                 "#..##"
                 "..#.."
                 "#...."])

(defn create-state
  [input]
  {0 (->> input
          (reduce-kv (fn [a y row]
                       (->> (into [] row)
                            (reduce-kv (fn [a x c]
                                         (if (= c \.) a (conj a [x y])))
                                       a)))
                     #{}))})

(def test-state (create-state test-input))
(def state (create-state input))

(def directions [[-1 0] [0 -1] [1 0] [0 1]])

(defn values->sets
  [m]
  (reduce-kv (fn [a k v] (assoc a k (into #{} v))) {} m))

(defn get-neighbors
  {:test (fn []
           (is= (get-neighbors 0 [0 0])
                {0 #{[1 0] [0 1]}, -1 #{[2 1] [1 2]}})
           (is= (get-neighbors 0 [3 2])
                {0 #{[3 3] [4 2] [3 1]}
                 1 #{[4 3] [4 2] [4 1] [4 4] [4 0]}}))}
  [tile position]
  (let [neighbors-same-tile {tile (->> directions
                                       (map (fn [d] (mapv + d position)))
                                       (filter (fn [[x y]] (and (<= 0 x 4) (<= 0 y 4))))
                                       (remove #{[2 2]}))}]
    (->> (cond-> neighbors-same-tile
                 (= (first position) 0) (update (dec tile) conj [1 2])
                 (= (first position) 4) (update (dec tile) conj [3 2])
                 (= (second position) 0) (update (dec tile) conj [2 1])
                 (= (second position) 4) (update (dec tile) conj [2 3])
                 (= position [1 2]) (assoc (inc tile) #{[0 0] [0 1] [0 2] [0 3] [0 4]})
                 (= position [3 2]) (assoc (inc tile) #{[4 0] [4 1] [4 2] [4 3] [4 4]})
                 (= position [2 1]) (assoc (inc tile) #{[0 0] [1 0] [2 0] [3 0] [4 0]})
                 (= position [2 3]) (assoc (inc tile) #{[0 4] [1 4] [2 4] [3 4] [4 4]}))
         (values->sets))))

(defn bug?
  [state tile position]
  (contains? (get state tile) position))

(defn get-neighbor-bugs
  {:test (fn []
           (is= (-> test-state
                    (get-neighbor-bugs 0 [3 2]))
                {0 #{[4 2] [3 1]}}))}
  [state tile position]
  (->> (get-neighbors tile position)
       (reduce-kv (fn [a t ps]
                    (reduce (fn [a p]
                              (if (bug? state t p)
                                (update a t conj p)
                                a))
                            a
                            ps))
                  {})
       (values->sets)))

(defn bug-in-the-next-generation?
  {:test (fn []
           ; A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
           (is (-> (create-state ["##"])
                   (bug-in-the-next-generation? 0 [1 0])))
           (is-not (-> (create-state [".#"])
                       (bug-in-the-next-generation? 0 [1 0])))
           (is-not (-> (create-state ["###"])
                       (bug-in-the-next-generation? 0 [1 0])))
           ; An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
           (is (-> (create-state ["#"])
                   (bug-in-the-next-generation? 0 [1 0])))
           (is (-> (create-state ["#.#"])
                   (bug-in-the-next-generation? 0 [1 0])))
           (is-not (-> (create-state ["#.#"
                                      ".#"])
                       (bug-in-the-next-generation? 0 [1 0]))))}
  [state tile position]
  (let [neighbor-bugs (get-neighbor-bugs state tile position)
        number-of-neighbor-bugs (->> (vals neighbor-bugs)
                                     (map count)
                                     (reduce +))]
    (or (and (bug? state tile position)
             (= 1 number-of-neighbor-bugs))
        (and (not (bug? state tile position))
             (<= 1 number-of-neighbor-bugs 2)))))

(defn tick-a-minute
  {:test (fn []
           (is= (-> ((apply comp (repeat 10 tick-a-minute)) test-state)
                    (get -5))
                #{[3 3] [1 1] [4 2] [1 3] [2 4] [2 0] [3 1]}))}
  [state]
  (let [interesting-positions (->> state
                                   (reduce-kv (fn [a tile positions]
                                                (->> positions
                                                     (reduce (fn [a position]
                                                               (let [neighbors (get-neighbors tile position)]
                                                                 (->> neighbors
                                                                      (reduce-kv (fn [a tile positions]
                                                                                   (update a tile union positions))
                                                                                 a))))
                                                             a)))
                                              state))]
    (->> interesting-positions
         (reduce-kv (fn [a tile positions]
                      (->> positions
                           (reduce (fn [a position]
                                     (if (bug-in-the-next-generation? state tile position)
                                       (update a tile conj position)
                                       a))
                                   a)))
                    {})
         (values->sets))))

(defn count-bugs
  {:test (fn []
           (is= (count-bugs ((apply comp (repeat 10 tick-a-minute)) test-state))
                99))}
  [state]
  (->> (vals state)
       (map count)
       (reduce +)))

(comment
  (time (count-bugs ((apply comp (repeat 200 tick-a-minute)) state)))
  ; "Elapsed time: 4938.982521 msecs"
  ; => 1896
  )

