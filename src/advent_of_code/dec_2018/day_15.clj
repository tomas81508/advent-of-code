(ns advent-of-code.dec-2018.day-15
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]))

(def test-input-1 ["#######"
                   "#E..G.#"
                   "#...#.#"
                   "#.G.#G#"
                   "#######"])

(defn create-state
  {:test (fn []
           (is= (create-state test-input-1)
                {:open    #{[2 2] [2 3] [3 3] [1 1] [5 3] [4 1] [5 2] [1 3] [5 1] [3 1] [2 1] [1 2] [3 2]},
                 :goblins #{[2 3] [5 3] [4 1]},
                 :elves   #{[1 1]}}))}
  [input]
  (->> input
       (reduce-kv (fn [state y row]
                    (->> (vec row)
                         (reduce-kv (fn [state x c]
                                      (cond-> state
                                              (#{\G} c) (update :goblins conj [x y])
                                              (#{\E} c) (update :elves conj [x y])
                                              (#{\G \E \.} c) (update :open conj [x y])))
                                    state)))
                  {:open    #{}
                   :goblins #{}
                   :elves   #{}})))

(def test-state-1 (create-state test-input-1))

(defn elf?
  {:test (fn []
           (is (elf? test-state-1 [1 1]))
           (is-not (elf? test-state-1 [1 2])))}
  [state position]
  (contains? (:elves state) position))

(defn goblin?
  {:test (fn []
           (is (goblin? test-state-1 [2 3]))
           (is-not (goblin? test-state-1 [1 2])))}
  [state position]
  (contains? (:goblins state) position))

(def directions [[0 -1] [-1 0] [1 0] [0 1]])

(defn open? [state pos]
  (contains? (get state :open) pos))

(defn extend-path [state path direction type-of-warrior visited-path]
  (let [new-position (mapv + (first path) direction)]
    (when (and (open? state new-position)
               (not (contains? visited-path new-position))
               (or (= type-of-warrior :elf)
                   (not (goblin? state new-position)))
               (or (= type-of-warrior :goblin)
                   (not (elf? state new-position)))
               (not (contains? (into #{} path) new-position)))
      (conj path new-position))))

(defn filter-enemy-paths
  [state paths type-of-warrior]
  (let [enemy? (if (= type-of-warrior :elf) goblin? elf?)]
    (->> paths
         (filter (fn [[position]] (enemy? state position))))))

(def test-input-2 ["#########"
                   "#G..G..G#"
                   "#.......#"
                   "#.......#"
                   "#G..E..G#"
                   "#.......#"
                   "#.......#"
                   "#G..G..G#"
                   "#########"])

(def test-state-2 (create-state test-input-2))

; 1234567
(def test-input-2-1 ["#########"
                     "#.G...G.#"                            ;1
                     "#...G...#"                            ;2
                     "#...E..G#"                            ;3
                     "#.G.....#"                            ;4
                     "#.......#"                            ;5
                     "#G..G..G#"                            ;6
                     "#.......#"
                     "#########"])

(def test-state-2-1 (create-state test-input-2-1))

(def test-input-2-2 ["#########"
                     "#..G.G..#"
                     "#...G...#"
                     "#.G.E.G.#"
                     "#.......#"
                     "#G..G..G#"
                     "#.......#"
                     "#.......#"
                     "#########"])

(def test-state-2-2 (create-state test-input-2-2))

(defn move
  {:test (fn []
           (is (-> (move test-state-2-2 [3 1])
                   (move [5 1])
                   (move [4 2])
                   (move [2 3])
                   (move [4 3])
                   (move [6 3])
                   (move [1 5])
                   (move [4 5])
                   (move [7 5]))))}
  [state position]
  (let [type-of-warrior (if (elf? state position) :elf :goblin)]
    (loop [paths (vector (list position))]
      (let [visited-paths (->> paths
                               (reduce (fn [a path]
                                         (apply conj a path))
                                       #{}))
            extended-paths (->> paths
                                (mapcat (fn [path]
                                          (->> directions
                                               (keep (fn [d]
                                                       (extend-path state path d type-of-warrior visited-paths)))))))]
        (if (empty? extended-paths)
          state
          (let [enemy-paths (filter-enemy-paths state extended-paths type-of-warrior)]
            (if (empty? enemy-paths)
              (recur extended-paths)
              (let [first-path (first enemy-paths)]
                (if (= (count first-path) 2)
                  state
                  (let [next-position (first (take-last 2 first-path))
                        key (if (= type-of-warrior :elf) :elves :goblins)]
                    (update state key (fn [friends] (-> friends
                                                        (disj position)
                                                        (conj next-position))))))))))))))

(defn move-all-warriors
  {:test (fn []
           (is= (move-all-warriors test-state-2)
                test-state-2-1)
           (is= (move-all-warriors (create-state ["GE"]))
                (create-state ["GE"])))}
  [state]
  (let [warriors (concat (:elves state) (:goblins state))
        sorted-warriors (sort-by (comp vec reverse) warriors)]
    (reduce move state sorted-warriors)))

((apply comp (repeat 2 move-all-warriors)) test-state-2)


(defn state->string
  [state]
  (let [max-y (->> (:open state)
                   (map second)
                   (reduce max 0))
        max-x (->> (:open state)
                   (map first)
                   (reduce max 0))]
    (->> (for [y (range (+ 2 max-y))
               x (range (+ 2 max-x))]
           [x y])
         (partition (+ 2 max-x))
         (map (fn [row]
                (reduce (fn [a p]
                          (str a
                               (cond (goblin? state p) "G"
                                     (elf? state p) "E"
                                     (contains? (:open state) p) "."
                                     :else "#")))
                        ""
                        row)))
         (string/join "\n"))))

(loop [state test-state-2
       i 4]
  (println (state->string state))
  (if (zero? i)
    state
    (recur (move-all-warriors state) (dec i))))