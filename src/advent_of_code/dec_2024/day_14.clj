(ns advent-of-code.dec-2024.day-14
  (:require [advent-of-code.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/dec_2024/day_14_input.txt"))

(def test-input "p=0,4 v=3,-3\np=6,3 v=-1,-3\np=10,3 v=-1,2\np=2,0 v=2,-1\np=0,0 v=1,3\np=3,0 v=-2,-2\np=7,6 v=-1,-3\np=3,0 v=-1,-2\np=9,3 v=2,3\np=7,3 v=-1,2\np=2,4 v=2,-3\np=9,5 v=-3,-3")

(defn parse-input
  [input]
  (->> input
       (re-seq #"[\d-]+")
       (map read-string)
       (partition 4)
       (map (fn [[x y vx vy]] [[x y] [vx vy]]))))

(defn create-state
  [input width height]
  {:width  width
   :height height
   :guards (parse-input input)})

(def test-state (create-state test-input 11 7))

(defn move-guard
  {:test (fn []
           (is= (move-guard [[0 4] [3 -3]] 11 7)
                [[3 1] [3 -3]])
           (is= (move-guard [[3 1] [3 -3]] 11 7)
                [[6 5] [3 -3]]))}
  [guard width height]
  (let [[vx vy] (second guard)]
    (update guard 0 (fn [[x y]] [(mod (+ x vx) width) (mod (+ y vy) height)]))))

(defn one-second
  [state]
  (update state :guards (fn [guards]
                          (->> guards
                               (map (fn [guard] (move-guard guard (:width state) (:height state))))))))

(defn n-seconds
  [state n]
  (loop [state state
         n n]
    (if (zero? n)
      state
      (recur (one-second state) (dec n)))))

(defn get-quadrants
  {:test (fn []
           (is= (get-quadrants 11 7)
                [[[0 4] [0 2]]
                 [[6 10] [0 2]]
                 [[0 4] [4 6]]
                 [[6 10] [4 6]]]))}
  [width height]
  ; [[x-min x-max] [y-min y-max]]
  [[[0 (dec (/ (dec width) 2))] [0 (dec (/ (dec height) 2))]]
   [[(/ (inc width) 2) (dec width)] [0 (dec (/ (dec height) 2))]]
   [[0 (dec (/ (dec width) 2))] [(/ (inc height) 2) (dec height)]]
   [[(/ (inc width) 2) (dec width)] [(/ (inc height) 2) (dec height)]]])

(defn get-safety-factor
  [state]
  (->> (get-quadrants (:width state) (:height state))
       (map (fn [[[x-min x-max] [y-min y-max]]]
              (->> (:guards state)
                   (filter (fn [[[x y] _]]
                             (and (<= x-min x x-max)
                                  (<= y-min y y-max))))
                   (count))))
       (reduce *)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input 11 7) 12))}
  [input width height]
  (-> (create-state input width height)
      (n-seconds 100)
      (get-safety-factor)))

(defn draw
  [state]
  (let [guard-positions (->> (:guards state)
                             (map first)
                             (into #{}))]
    (->> (for [y (range (:height state))
               x (range (:width state))]
           [x y])
         (partition (:width state))
         (map (fn [row] (->> row
                             (map (fn [position] (if (contains? guard-positions position) "#" "."))))))
         (map (fn [row] (apply str row)))
         (clojure.string/join "\n"))))

(def state (create-state input 101 103))

(defn part-2
  [state start-n steps]
  (loop [state (n-seconds state start-n)
         n start-n
         result ""]
    (if (< n 10000)
      (recur (n-seconds state steps)
             (+ n steps)
             (str result n "\n" (draw state) "\n"))
      (spit (str "src/advent_of_code/dec_2024/day_14b-result-" start-n "-" steps ".txt") result))))

(comment
  ; "Elapsed time: 62.778273 msecs"
  ; => 211692000
  (time (part-1 input 101 103))

  (time (part-2 state 22 101))
  )