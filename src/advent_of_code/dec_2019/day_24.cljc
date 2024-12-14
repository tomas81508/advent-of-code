(ns advent-of-code.dec-2019.day-24
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
  {:bugs (->> input
              (reduce-kv (fn [a y row]
                           (->> (into [] row)
                                (reduce-kv (fn [a x c]
                                             (if (= c \.) a (conj a [x y])))
                                           a)))
                         #{}))})

(def test-state (create-state test-input))
(def state (create-state input))

(defn distance
  {:test (fn []
           (is= (distance [4 4] [4 4]) 0)
           (is= (distance [0 0] [1 0]) 1)
           (is= (distance [1 1] [3 3]) 2)
           (is= (distance [-1 0] [1 1]) 2)
           (is= (distance [4 4] [5 -4]) 8))}
  [p1 p2]
  (->> (map - p1 p2)
       (map abs)
       (apply max)))

(defn neighbors
  {:test (fn []
           (is (neighbors [4 4] [5 4]))
           (is (neighbors [0 0] [1 0]))
           (is-not (neighbors [4 4] [4 4]))
           (is-not (neighbors [1 1] [3 3]))
           (is-not (neighbors [-1 0] [1 1])))}
  [p1 p2]
  (= (distance p1 p2) 1))

(defn bug?
  {:test (fn []
           (is (-> test-state
                   (bug? [0 4])))
           (is-not (-> test-state
                       (bug? [0 0]))))}
  [state position]
  (contains? (:bugs state) position))

(def directions [[-1 0] [0 -1] [1 0] [0 1]])

(defn get-neighbors
  {:test (fn []
           (is= (get-neighbors [2 0])
                #{[1 0] [3 0] [2 1]}))}
  [position]
  (->> directions
       (map (fn [d] (map + d position)))
       (filter (fn [[x y]] (and (<= 0 x 4) (<= 0 y 4))))
       (set)))

(defn get-neighbor-bugs
  {:test (fn []
           (is= (-> test-state
                    (get-neighbor-bugs [3 2]))
                #{[4 2] [3 1]}))}
  [state position]
  (->> (get-neighbors position)
       (filter (fn [c] (bug? state c)))
       (set)))

(defn bug-in-the-next-generation?
  {:test (fn []
           ; A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
           (is (-> (create-state ["##"])
                   (bug-in-the-next-generation? [1 0])))
           (is-not (-> (create-state [".#"])
                       (bug-in-the-next-generation? [1 0])))
           (is-not (-> (create-state ["###"])
                       (bug-in-the-next-generation? [1 0])))
           ; An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
           (is (-> (create-state ["#"])
                   (bug-in-the-next-generation? [1 0])))
           (is (-> (create-state ["#.#"])
                   (bug-in-the-next-generation? [1 0])))
           (is-not (-> (create-state ["#.#"
                                      ".#"])
                       (bug-in-the-next-generation? [1 0]))))}
  [state position]
  (let [neighbor-bugs (get-neighbor-bugs state position)
        number-of-neighbor-bugs (count neighbor-bugs)]
    (or (and (bug? state position)
             (= 1 number-of-neighbor-bugs))
        (and (not (bug? state position))
             (<= 1 number-of-neighbor-bugs 2)))))


(defn tick
  {:test (fn []
           (is= (tick test-state)
                (create-state ["#..#." "####." "###.#" "##.##" ".##.."])))}
  [state]
  (let [interesting-positions (->> (union (:bugs state)
                                          (->> (:bugs state)
                                               (map get-neighbors)
                                               (apply union))))]
    (assoc state :bugs (->> interesting-positions
                            (filter (fn [p] (bug-in-the-next-generation? state p)))
                            (set)))))

(defn get-recurring-state
  {:test (fn []
           (is= (get-recurring-state test-state)
                {:bugs #{[0 3] [1 4]}}))}
  [state]
  (loop [state state
         states #{state}]
    (let [state (tick state)]
      (if (contains? states state)
        state
        (recur state (conj states state))))))

(def recurring-state (get-recurring-state state))

(defn biodiversity-rating
  {:test (fn []
           (is= (biodiversity-rating {:bugs #{[0 3] [1 4]}})
                2129920))}
  [state]
  (->> (for [y (range 5) x (range 5)] [x y])
       (map-indexed (fn [index x] [index x]))
       (filter (fn [[_ p]] (bug? state p)))
       (map (fn [[i _]] (int (pow 2 i))))
       (reduce +)))

(comment
  (biodiversity-rating recurring-state)
  )




