(ns advent-of-code.dec-2016.day-19
  (:require [advent-of-code.test :refer [is= is is-not]]))

(def input 3005290)

(defn steal-presents
  {:test (fn []
           (is= (steal-presents 5) 3))}
  [number-of-elves]
  (let [elves (range 1 (inc number-of-elves))]
    (loop [elves elves
           number-of-elves number-of-elves]
      (cond (even? number-of-elves)
            (recur (->> elves
                        (partition 2)
                        (map first))
                   (/ number-of-elves 2))

            (= number-of-elves 1)
            (first elves)

            :else
            (recur (cons (last elves)
                         (->> elves
                              (partition 2)
                              (map first)))
                   (/ (inc number-of-elves) 2))))))

(comment
  (time (steal-presents input))
  (steal-presents 100) 73
  (steal-presents 200) 145
  (steal-presents 300)
  ; "Elapsed time: 754.811792 msecs"
  ; => 1816277
  )

(defn steal-across-presents
  {:test (fn []
           (is= (steal-across-presents 5) 2)
           (is= (steal-across-presents 9) 9)
           (is= (steal-across-presents 10) 1)
           )}
  [number-of-elves]
  (let [elves (into [] (range 1 (inc number-of-elves)))]
    (loop [elves elves
           current-index 0
           number-of-elves number-of-elves]
      ;(println elves (get elves current-index))
      (if (= number-of-elves 1)
        (first elves)
        (let [elf-index-to-remove (rem (+ current-index (quot number-of-elves 2))
                                       number-of-elves)]
          ;(println "Remove" (get elves elf-index-to-remove))
          (recur (into (subvec elves 0 elf-index-to-remove)
                       (subvec elves (inc elf-index-to-remove)))
                 (let [naive-next-index (if (< elf-index-to-remove current-index)
                                          current-index
                                          (inc current-index))]
                   (if (< naive-next-index (dec number-of-elves))
                     naive-next-index
                     0))
                 (dec number-of-elves)))))))

(defn find-power
  {:test (fn []
           (is= (find-power 1) 0)
           (is= (find-power 3) 1)
           (is= (find-power 9) 2)
           (is= (find-power 11) 2)
           (is= (find-power 26) 2)
           (is= (find-power 27) 3)
           (is= (find-power 28) 3))}
  [n]
  (loop [n n p 0]
    (let [result (/ n 3)]
      (if (< result 1) p (recur result (inc p))))))

(defn power [p] (apply * (repeat p 3)))

(defn find-interval
  {:test (fn []
           (is= (find-interval 0) 2)
           (is= (find-interval 1) 6)
           (is= (find-interval 2) 18))}
  [p]
  (- (power (inc p)) (power p)))

(defn solution
  {:test (fn []
           (is= (solution 18) 9)
           (is= (solution 19) 11)
           (is= (solution 26) 25)
           (is= (solution 27) 27)
           (is= (solution 28) 1)
           (is= (solution 29) 2)
           (is= (solution 30) 3))}
  [n]
  (let [p (find-power n)
        interval (find-interval p)
        part-of-interval (- n (power p))
        half-interval (/ interval 2)]
    (cond (> part-of-interval half-interval)
          (+ half-interval (* 2 (- part-of-interval half-interval)))
          (zero? part-of-interval)
          (power p)
          :else
          part-of-interval)))

(comment
  (->> (range 1 82)
       (map (fn [n] [n (steal-across-presents n)])))
  ; see the pattern from here

  (solution input)



  )







