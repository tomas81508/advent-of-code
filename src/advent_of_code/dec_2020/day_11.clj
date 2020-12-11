(ns advent-of-code.dec-2020.day-11
  (:require [ysera.test :refer [is is-not is= deftest]]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2020/day_11.txt")
       (clojure.string/split-lines)))

(def test-input ["L.LL.LL.LL"
                 "LLLLLLL.LL"
                 "L.L.L..L.."
                 "LLLL.LL.LL"
                 "L.LL.LL.LL"
                 "L.LLLLL.LL"
                 "..L.L....."
                 "LLLLLLLLLL"
                 "L.LLLLLL.L"
                 "L.LLLLL.LL"])

(def test-input-after-two-rounds ["#.LL.L#.##"
                                  "#LLLLLL.L#"
                                  "L.L.L..L.."
                                  "#LLL.LL.L#"
                                  "#.LL.LL.LL"
                                  "#.LLLL#.##"
                                  "..L.L....."
                                  "#LLLLLLLL#"
                                  "#.LLLLLL.L"
                                  "#.#LLLL.##"])

(defn create-state
  {:test (fn []
           (is= (create-state [".L#" "L.L"])
                {:seats {[1 0] :empty
                         [2 0] :taken
                         [0 1] :empty
                         [2 1] :empty}}))}
  [input]
  (->> input
       (map-indexed (fn [y row] [y row]))
       (reduce (fn [a [y row]]
                 (reduce (fn [a [x l]]
                           (if (or (= l \L) (= l \#))
                             (update a :seats
                                     (fn [seats]
                                       (assoc seats [x y] ({\L :empty \# :taken} l))))

                             a))
                         a
                         (map-indexed (fn [x l] [x l]) row)))
               {:seats {}})))

(defn get-all-seats
  {:test (fn []
           (is= (-> (create-state [".L#" "L.L"])
                    (get-all-seats))
                #{[1 0] [0 1] [2 1] [2 0]}))}
  [state]
  (set (keys (:seats state))))

(defn seat?
  [state seat]
  (contains? (:seats state) seat))

(def directions (for [x (range -1 2)
                      y (range -1 2)
                      :when (not= [x y] [0 0])]
                  [x y]))

(defn get-adjacent-seats
  {:test (fn []
           (is= (-> (create-state [".L#" "LL."])
                    (get-adjacent-seats [2 0]))
                #{[1 0] [1 1]}))}
  [state seat]
  (->> directions
       (map (fn [d] (map + seat d)))
       (filter (partial seat? state))
       (set)))

(defn seat-empty?
  [state seat]
  (= (get-in state [:seats seat]) :empty))

(defn seat-occupied?
  [state seat]
  (= (get-in state [:seats seat]) :taken))

(defn get-occupied-adjacent-seats
  [state seat]
  (->> (get-adjacent-seats state seat)
       (filter (partial seat-occupied? state))))

(defn take-seat
  [state seat]
  (assoc-in state [:seats seat] :taken))

(defn free-seat
  [state seat]
  (assoc-in state [:seats seat] :empty))

(defn tick-a-round
  {:test (fn []
           (is= (-> (create-state test-input)
                    (tick-a-round)
                    (tick-a-round))
                (create-state test-input-after-two-rounds)))}
  [state]
  (reduce (fn [new-state seat]
            (let [occupied-adjacent-seats (get-occupied-adjacent-seats state seat)]
              (cond
                ; If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
                (and (seat-empty? state seat)
                     (zero? (count occupied-adjacent-seats)))
                (take-seat new-state seat)

                ; If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
                (and (seat-occupied? state seat)
                     (>= (count occupied-adjacent-seats) 4))
                (free-seat new-state seat)

                :else
                new-state)))
          state
          (get-all-seats state)))

(defn get-occupied-seats
  [state]
  (reduce-kv (fn [a k v]
               (if (= v :taken)
                 (conj a k)
                 a))
             []
             (:seats state)))

(deftest puzzle-test
         (is= (loop [state (create-state test-input)]
                (let [next-state (tick-a-round state)]
                  (if (= state next-state)
                    (count (get-occupied-seats state))
                    (recur next-state))))
              37))

(comment
  "This test takes about 7s so i don't want to run it as often."
  (deftest puzzle-a
           (is= (time (loop [state (create-state (get-puzzle-input))]
                        (let [next-state (tick-a-round state)]
                          (if (= state next-state)
                            (count (get-occupied-seats state))
                            (recur next-state)))))
                ; "Elapsed time: 7518.975366 msecs"
                2321))
  )

(defn create-state-2
  {:test (fn []
           (is= (create-state-2 [".L#" "L.L"])
                {:seats  {[1 0] :empty
                          [2 0] :taken
                          [0 1] :empty
                          [2 1] :empty}
                 :width  2
                 :height 1}))}
  [input]
  (-> (create-state input)
      (assoc :width (dec (count (first input))))
      (assoc :height (dec (count input)))))

(defn inside-area?
  {:test (fn []
           (let [state (create-state-2 [".L#" "L.L"])]
             (is-not (inside-area? state [4 4]))
             (is-not (inside-area? state [-1 1]))
             (is (inside-area? state [1 1]))))}
  [state [x y]]
  (and (<= 0 x (:width state))
       (<= 0 y (:height state))))

(defn occupied-seat-in-direction?
  {:test (fn []
           (let [state (create-state-2 [".......#."
                                        "...#....."
                                        ".#.L....."
                                        "........."
                                        "..#L....."])]
             (is-not (occupied-seat-in-direction? state [3 4] [0 -1]))
             (is (occupied-seat-in-direction? state [3 4] [-1 -1]))
             (is-not (occupied-seat-in-direction? state [3 4] [1 0]))))}
  [state seat direction]
  (loop [seat seat]
    (let [seat (map + seat direction)]
      (cond (not (inside-area? state seat))
            false

            (seat-occupied? state seat)
            true

            (seat-empty? state seat)
            false

            :else
            (recur seat)))))

(defn get-occupied-directions
  {:test (fn []
           (is= (-> (create-state-2 [".......#."
                                     "...#....."
                                     ".#......."
                                     "........."
                                     "..#L....#"
                                     "....#...."
                                     "........."
                                     "#........"
                                     "...#....."])
                    (get-occupied-directions [3 4])
                    (count))
                8)
           (is= (-> (create-state-2 [".##.##."
                                     "#.#.#.#"
                                     "##...##"
                                     "...L..."
                                     "##...##"
                                     "#.#.#.#"
                                     ".##.##."])
                    (get-occupied-directions [3 3])
                    (count))
                0))}
  [state seat]
  (reduce (fn [a d]
            (if (occupied-seat-in-direction? state seat d)
              (conj a d)
              a))
          #{}
          directions))

(def test-input-2 ["L.LL.LL.LL"
                   "LLLLLLL.LL"
                   "L.L.L..L.."
                   "LLLL.LL.LL"
                   "L.LL.LL.LL"
                   "L.LLLLL.LL"
                   "..L.L....."
                   "LLLLLLLLLL"
                   "L.LLLLLL.L"
                   "L.LLLLL.LL"])

(def test-input-2-after-2-rounds ["#.LL.LL.L#"
                                  "#LLLLLL.LL"
                                  "L.L.L..L.."
                                  "LLLL.LL.LL"
                                  "L.LL.LL.LL"
                                  "L.LLLLL.LL"
                                  "..L.L....."
                                  "LLLLLLLLL#"
                                  "#.LLLLLL.L"
                                  "#.LLLLL.L#"])

(def test-input-2-after-3-rounds ["#.L#.##.L#"
                                  "#L#####.LL"
                                  "L.#.#..#.."
                                  "##L#.##.##"
                                  "#.##.#L.##"
                                  "#.#####.#L"
                                  "..#.#....."
                                  "LLL####LL#"
                                  "#.L#####.L"
                                  "#.L####.L#"])

(def test-input-2-after-4-rounds ["#.L#.L#.L#"
                                  "#LLLLLL.LL"
                                  "L.L.L..#.."
                                  "##LL.LL.L#"
                                  "L.LL.LL.L#"
                                  "#.LLLLL.LL"
                                  "..L.L....."
                                  "LLLLLLLLL#"
                                  "#.LLLLL#.L"
                                  "#.L#LL#.L#"])

(def test-input-2-after-6-rounds ["#.L#.L#.L#"
                                  "#LLLLLL.LL"
                                  "L.L.L..#.."
                                  "##L#.#L.L#"
                                  "L.L#.LL.L#"
                                  "#.LLLL#.LL"
                                  "..#.L....."
                                  "LLL###LLL#"
                                  "#.LLLLL#.L"
                                  "#.L#LL#.L#"])

(defn tick-a-round-with-directions
  {:test (fn []
           (is= (-> (create-state-2 test-input-2)
                    ((apply comp (repeat 2 tick-a-round-with-directions))))
                (create-state-2 test-input-2-after-2-rounds))
           (is= (-> (create-state-2 test-input-2)
                    ((apply comp (repeat 4 tick-a-round-with-directions))))
                (create-state-2 test-input-2-after-4-rounds))
           (is= (-> (create-state-2 test-input-2-after-2-rounds)
                    (tick-a-round-with-directions))
                (create-state-2 test-input-2-after-3-rounds))
           (is= (-> (create-state-2 test-input-2)
                    ((apply comp (repeat 6 tick-a-round-with-directions))))
                (create-state-2 test-input-2-after-6-rounds))
           )}
  [state]
  (reduce (fn [new-state seat]

            (cond
              ; If a seat is empty (L) and there are no visible occupied seats, the seat becomes occupied.
              (and (seat-empty? state seat)
                   (zero? (count (get-occupied-directions state seat))))
              (take-seat new-state seat)

              ; If a seat is occupied (#) and five or more visible occupied seats for an occupied seat to become empty.
              (and (seat-occupied? state seat)
                   (>= (count (get-occupied-directions state seat)) 5))
              (free-seat new-state seat)

              :else
              new-state))
          state
          (get-all-seats state)))


(comment
  (deftest puzzle-b
           (is= (time (loop [state (create-state-2 (get-puzzle-input))
                             counter 0]
                        (println counter)
                        (let [next-state (tick-a-round-with-directions state)]
                          (if (= state next-state)
                            (count (get-occupied-seats state))
                            (recur next-state (inc counter))))))
                ; "Elapsed time: 8759.0915 msecs"
                2102))

  )






