(ns advent-of-code.dec-2023.day-02
  (:require [clojure.test :refer [deftest]]
            [clojure.tools.reader.edn :refer [read-string]]
            [advent-of-code.test :refer [is= is is-not]]))

(def input (->> (slurp "src/advent_of_code/dec_2023/day_02_input.txt")
                (clojure.string/split-lines)))

(def test-input ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
                 "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
                 "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
                 "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
                 "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"])

(def bag {:red 12 :green 13 :blue 14})


(defn get-subset-info
  {:test (fn []
           (is= (get-subset-info " 1 green, 2 red, 6 blue")
                {:green 1 :red 2 :blue 6}))}
  [s]
  (->> (clojure.string/split s #",")
       (reduce (fn [a v]
                 (let [[_ n color] (re-find #"[ ]*([\d]+) (green|red|blue)" v)]
                   (assoc a (keyword color) (read-string n))))
               {})))

(defn get-game-info
  [game]
  (let [[_ game-id game-info] (re-find #"Game (\d+):(.*)" game)
        game-subsets (->> (clojure.string/split game-info #";")
                          (map get-subset-info))]
    {:id      game-id
     :subsets game-subsets}))

(defn possible-game?
  "If game is possible then the game id is returned."
  {:test (fn []
           (is (possible-game? bag "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))
           (is-not (possible-game? bag "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")))}
  [bag game]
  (let [{game-id :id subsets :subsets} (get-game-info game)]
    (when (->> subsets
               (some (fn [game-subset]
                       (->> game-subset
                            (reduce-kv (fn [a k v] (or a (> v (get bag k))))
                                       false))))
               (not))
      game-id)))

(deftest puzzle-a
  (is= (->> input
            (map (fn [l] (possible-game? bag l)))
            (remove nil?)
            (map read-string)
            (reduce +))
       2265))

; part 2

(defn calculate-fewest-cubes
  {:test (fn []
           (is= (calculate-fewest-cubes "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
                {:blue 6 :green 2 :red 4}))}
  [game]
  (let [{subsets :subsets} (get-game-info game)]
    (->> subsets
         (reduce (fn [a subset]
                   (reduce-kv (fn [a k v]
                                (update a k max v))
                              a
                              subset))
                 {:blue 0 :green 0 :red 0}))))

(deftest puzzle-b
  (is= (->> input
            (map calculate-fewest-cubes)
            (map (fn [cubes] (reduce * (vals cubes))))
            (reduce +))
       64097))


