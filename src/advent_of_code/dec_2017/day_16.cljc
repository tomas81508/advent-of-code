(ns advent-of-code.dec-2017.day-16
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :refer [starts-with? split]]))

(def input (-> (slurp "src/advent_of_code/dec_2017/day_16_input.txt")
               (clojure.string/split #",")))

(defn dance
  {:test (fn []
           (is= (dance ["a" "b" "c" "d" "e"] "s1" 5)
                ["e" "a" "b" "c" "d"])
           (is= (dance ["a" "b" "c" "d" "e"] "s3" 5)
                ["c" "d" "e" "a" "b"])
           (is= (dance ["e" "a" "b" "c" "d"] "x3/4" 5)
                ["e" "a" "b" "d" "c"])
           (is= (dance ["e" "a" "b" "d" "c"] "pe/b" 5)
                ["b" "a" "e" "d" "c"])
           (is= (dance [\a \b \c \d \e \f \g \h \i \n \k \l \m \j \o \p] "s8" 16)
                [\i \n \k \l \m \j \o \p \a \b \c \d \e \f \g \h]))}
  [program move size]
  (cond (starts-with? move "s")
        (let [n (read-string (subs move 1))]
          (->> program
               (cycle)
               (drop (- size n))
               (take size)))

        (starts-with? move "x")
        (let [[f l] (->> (re-seq #"\d+" move)
                         (map read-string))
              vector-program (vec program)]
          (-> vector-program
              (assoc f (get vector-program l))
              (assoc l (get vector-program f))))

        :else
        (let [[p1 p2] (-> (subs move 1)
                          (split #"/"))]
          (->> program
               (map (fn [x] (condp = x p1 p2 p2 p1 x)))))))

(defn total-dance
  [program input]
  (reduce (fn [program move]
            (dance program move 16))
          program
          input))

(comment
  (->> (total-dance (split "abcdefghijklmnop" #"") input)
       (apply str))
  )

; part 2

(defn find-cycle
  []
  (loop [program (split "abcdefghijklmnop" #"")
         configs #{program}
         index 0]
    (let [new-program (total-dance program input)
          index (inc index)]
      (if (contains? configs new-program)
        index
        (recur new-program
               (conj configs new-program)
               index)))))

(def cycle-dance (find-cycle))
(def puzzle-2-dances (rem 100000000 cycle-dance))

(comment

  (->> (reduce (fn [program _]
                 (total-dance program input))
               (split "abcdefghijklmnop" #"")
               (range puzzle-2-dances))
       (apply str))

  "fbmcgdnjakpioelh"

  )




