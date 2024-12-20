(ns advent-of-code.dec-2024.day-11
  (:require [advent-of-code.test :refer [is=]]))

(def input "6571 0 5851763 526746 23 69822 9 989")
(def test-input "125 17")

(defn blink-of-stone
  {:test (fn []
           (is= (blink-of-stone 0) 1)
           (is= (blink-of-stone 1) 2024)
           (is= (blink-of-stone 2024) [20 24])
           (is= (blink-of-stone 1000) [10 0])
           (is= (blink-of-stone 512072) [512 72]))}
  [stone]
  (if (zero? stone)
    1
    (let [stone-str (str stone)
          stone-str-count (count stone-str)]
      (if (even? stone-str-count)
        [(read-string (subs stone-str 0 (/ stone-str-count 2)))
         ;; specify base 10 with 10r because (read-string "072") -> 58 (treated as octal)
         (read-string (str "10r" (subs stone-str (/ stone-str-count 2))))]
        (* 2024 stone)))))

(defn blink-times
  {:test (fn []
           (is= (blink-times [125 17] 1)
                [253000 1 7])
           (is= (blink-times [125 17] 2)
                [253 0 2024 14168])
           (is= (blink-times [125 17] 3)
                [512072 1 20 24 28676032])
           (is= (blink-times [125 17] 4)
                [512 72 2024 2 0 2 4 2867 6032])
           (is= (blink-times [125 17] 5)
                [1036288 7 2 20 24 4048 1 4048 8096 28 67 60 32])
           (is= (blink-times [125 17] 6)
                [2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2]))}
  [stones n]
  (reduce (fn [stones _]
            (->> stones
                 (map blink-of-stone)
                 (flatten)))
          stones
          (range n)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 55312))}
  [input]
  (-> (->> (re-seq #"\d+" input)
           (map read-string))
      (blink-times 25)
      (count)))

(def blink-of-stone-times-2
  (memoize
    (fn [stone n]
      (if (zero? n)
        1
        (let [next-stone (blink-of-stone stone)]
          (if (number? next-stone)
            (blink-of-stone-times-2 next-stone (dec n))
            (+ (blink-of-stone-times-2 (first next-stone) (dec n))
               (blink-of-stone-times-2 (second next-stone) (dec n)))))))))

(defn blink-times-2
  {:test (fn []
           (is= (blink-times-2 [125 17] 1)
                3)
           (is= (blink-times-2 [125 17] 2)
                4)
           (is= (blink-times-2 [125 17] 3)
                5)
           (is= (blink-times-2 [125 17] 4)
                9)
           (is= (blink-times-2 [125 17] 5)
                13)
           (is= (blink-times-2 [125 17] 6)
                22))}
  [stones n]
  (->> stones
       (map (fn [s]
              (blink-of-stone-times-2 s n)))
       (reduce +)))

(defn part-2
  [input]
  (-> (blink-times-2 (->> (re-seq #"\d+" input)
                          (map read-string))
                     75)))

(comment
  ;; "Elapsed time: 400.387292 msecs"
  ;=> 183248
  (time (part-1 input))

  ;; "Elapsed time: 198.11 msecs"
  ;=> 218811774248729
  (time (part-2 input))

  ;; Part 1 with solution from part 2
  ;; "Elapsed time: 0.629584 msecs"
  ;=> 183248
  (time (-> (blink-times-2 (->> (re-seq #"\d+" input)
                                (map read-string))
                           25)))
  )

(defn blink-times-3
  {:test (fn []
           (is= (blink-times-3 {125 1 17 1} 1)
                {253000 1
                 1      1
                 7      1})
           (is= (blink-times-3 {125 1 17 1} 2)
                {253   1
                 0     1
                 2024  1
                 14168 1})
           (is= (blink-times-3 {125 1 17 1} 3)
                {512072   1
                 1        1
                 20       1
                 24       1
                 28676032 1})
           (is= (blink-times-3 {125 1 17 1} 6)
                {0          2
                 14168      1
                 7          1
                 4          1
                 48         2
                 40         2
                 4048       1
                 2097446912 1
                 6          2
                 3          1
                 2          4
                 2024       1
                 96         1
                 80         1
                 8          1}))}
  [stones n]
  (reduce (fn [stones _]
            (reduce-kv (fn [a s old-count]
                         (let [next-stone (blink-of-stone s)]
                           (if (number? next-stone)
                             (update a next-stone (fn [new-count]
                                                    (+ old-count (or new-count 0))))
                             (-> a
                                 (update (first next-stone) (fn [new-count]
                                                              (+ old-count (or new-count 0))))
                                 (update (second next-stone) (fn [new-count]
                                                               (+ old-count (or new-count 0))))))))
                       {}
                       stones))
          stones
          (range n)))

(comment
  ;; "Elapsed time: 4.610667 msecs"
  ;=> 183248
  (time (->> (blink-times-3 (->> (re-seq #"\d+" input)
                                 (map read-string)
                                 (frequencies))
                            25)
             (vals)
             (reduce +)))

  ;; "Elapsed time: 150.699958 msecs"
  ;=> 218811774248729
  (time (->> (blink-times-3 (->> (re-seq #"\d+" input)
                                 (map read-string)
                                 (frequencies))
                            75)
             (vals)
             (reduce +)))

  )

