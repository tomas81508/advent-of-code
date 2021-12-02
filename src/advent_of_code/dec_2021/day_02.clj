(ns advent-of-code.dec-2021.day-02
  (:require [ysera.test :refer [is= deftest]]))

(def input-data (->> (slurp "src/advent_of_code/dec_2021/day_02_input.txt")
                     (clojure.string/split-lines)))

(def test-data ["forward 5" "down 5" "forward 8" "up 3" "down 8" "forward 2"])

(defn move
  {:test (fn []
           (is= (move test-data)
                {:horizontal 15 :depth 10}))}
  [instructions]
  (reduce (fn [position instruction]
            (let [[direction amount-as-string] (clojure.string/split instruction #" ")]
              (let [amount (read-string amount-as-string)]
                (cond (= direction "forward")
                      (update position :horizontal + amount)

                      (= direction "down")
                      (update position :depth + amount)

                      (= direction "up")
                      (update position :depth - amount)))))
          {:horizontal 0
           :depth      0}
          instructions))

(deftest puzzle-a
         (is= (time (->> (move input-data)
                         (vals)
                         (apply *)))
              ; "Elapsed time: 1.182675 msecs"
              2215080))



(defn move-2
  {:test (fn []
           (is= (move-2 test-data)
                {:horizontal 15 :depth 60 :aim 10}))}
  [instructions]
  (reduce (fn [position instruction]
            (let [[direction amount-as-string] (clojure.string/split instruction #" ")]
              (let [amount (read-string amount-as-string)]
                (cond (= direction "forward")
                      (-> position
                          (update :horizontal + amount)
                          (update :depth + (* (:aim position) amount)))

                      (= direction "down")
                      (update position :aim + amount)

                      (= direction "up")
                      (update position :aim - amount)))))
          {:horizontal 0
           :depth      0
           :aim        0}
          instructions))

(deftest puzzle-b
         (is= (time (let [{h :horizontal d :depth} (move-2 input-data)]
                      (* h d)))
              ; "Elapsed time: 1.874467 msecs"
              2215080))
