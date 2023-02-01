(ns advent-of-code.dec-2022.day-17b
  (:require [ysera.test :refer [is is-not is= deftest]]
            [clojure.set :refer [intersection union]]))

(def input (slurp "src/advent_of_code/dec_2022/day_17_input.txt"))
(def test-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def test-stream (seq test-input))
(def stream (seq input))

(def parts [#{[2 0] [3 0] [4 0] [5 0]}
            #{[2 1] [3 1] [4 1] [3 2] [3 0]}
            #{[2 0] [3 0] [4 0] [4 1] [4 2]}
            #{[2 0] [2 1] [2 2] [2 3]}
            #{[2 0] [3 0] [2 1] [3 1]}])

(def state {:falling-rock    nil
            :rocks           #{[1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0]}
            :next-part       0
            :number-of-rocks 0})

(defn allowed-positions?
  {:test (fn []
           (is (allowed-positions? {} #{[4 4] [5 4] [6 4] [7 4]}))
           (is-not (allowed-positions? {} #{[5 4] [6 4] [7 4] [8 4]}))
           (is-not (allowed-positions? {:rocks #{[5 4]}} #{[5 4]})))}
  [state positions]
  (let [[min-x max-x] (->> positions
                           (map first)
                           (apply (juxt min max)))]
    (and (<= 1 min-x max-x 7)
         (empty? (intersection (:rocks state) positions)))))

(defn apply-jet-stream-part
  {:test (fn []
           ; can move right
           (is= (apply-jet-stream-part {:falling-rock #{[3 4] [4 4] [5 4] [6 4]}}
                                       \>)
                {:falling-rock #{[4 4] [5 4] [6 4] [7 4]}})
           ; hitting the wall to the right
           (is= (apply-jet-stream-part {:falling-rock #{[4 4] [5 4] [6 4] [7 4]}}
                                       \>)
                {:falling-rock #{[4 4] [5 4] [6 4] [7 4]}}))}
  [state stream-part]
  (let [operation (if (= stream-part \<) - +)
        maybe-new-position (->> (:falling-rock state)
                                (map (fn [p] (map operation p [1 0])))
                                (set))]
    (if (allowed-positions? state maybe-new-position)
      (assoc state :falling-rock (set maybe-new-position))
      state)))

(defn move-a-step
  {:test (fn []
           (is= (move-a-step (assoc state :falling-rock #{[3 4] [4 4] [5 4] [6 4]})
                             \>)
                {:falling-rock    #{[4 3] [6 3] [7 3] [5 3]}
                 :rocks           #{[1 0] [3 0] [7 0] [2 0] [5 0] [6 0] [4 0]}
                 :next-part       0
                 :number-of-rocks 0})
           (is= (move-a-step (assoc state :falling-rock #{[3 1] [4 1] [5 1] [6 1]})
                             \>)
                {:rocks           #{[1 0] [3 0] [7 0] [2 0] [5 0] [6 0] [4 0] [4 1] [5 1] [6 1] [7 1]}
                 :next-part       0
                 :number-of-rocks 1}))}
  [state stream-part]
  (let [state (apply-jet-stream-part state stream-part)
        maybe-new-falling-rock-positions (->> (:falling-rock state)
                                              (map (fn [p] (map + p [0 -1])))
                                              (set))]
    (if (empty? (intersection (:rocks state) maybe-new-falling-rock-positions))
      ; The rock can fall down
      (assoc state :falling-rock maybe-new-falling-rock-positions)
      ; The falling rock hit the ground
      (-> state
          (update :rocks union (:falling-rock state))
          (dissoc :falling-rock)
          (update :number-of-rocks inc)))))

(defn get-height
  [state]
  (->> (union (:rocks state) (:falling-rock state))
       (map second)
       (apply max)))

(defn print-chamber
  [state]
  (let [height (get-height state)]
    (str "|.......|\n"
         (->> (for [y (->> (range height)
                           (map (fn [x] (- height x))))
                    x (range 1 8)]
                [x y])
              (partition 7)
              (reduce (fn [a row]
                        (str a
                             "|"
                             (reduce (fn [a p]
                                       (str a (cond (contains? (:rocks state) p) "#"
                                                    (contains? (:falling-rock state) p) "@"
                                                    :else ".")))
                                     ""
                                     row)
                             "|\n"))
                      ""))
         "+-------+\n")))

(defn maybe-add-falling-rock
  {:test (fn []
           (is= (-> (maybe-add-falling-rock state)
                    (:falling-rock))
                #{[6 4] [3 4] [4 4] [5 4]}))}
  [state]
  (if (:falling-rock state)
    state
    (let [height (get-height state)
          new-rock (->> (get parts (:next-part state))
                        (map (fn [p] (map + p [1 (+ height 4)])))
                        (set))]
      (-> state
          (assoc :falling-rock new-rock)
          (update :next-part (fn [n] (rem (inc n) 5)))))))

(defn move
  {:test (fn []
           (is= (-> (move state (cycle test-stream) 2022)
                    (get-height))
                3068))}
  [state stream number-of-rocks]
  (loop [state state
         [stream-part & stream] stream]
    (if (= (:number-of-rocks state) number-of-rocks)
      state
      (let [next-state (-> (maybe-add-falling-rock state)
                           ;((fn [state]
                           ;   ;(println state)
                           ;   (println stream-part)
                           ;   (println (print-chamber state))
                           ;   state))
                           (move-a-step stream-part))]
        (recur next-state stream)))))



(comment
  (time
    (-> (move state (cycle stream) 2022)
        ;(print-chamber)
        ;(println)
        (get-height)))
  ; "Elapsed time: 1730.162056 msecs"
  ; => 3117

  )
