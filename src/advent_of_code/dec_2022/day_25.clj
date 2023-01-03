(ns advent-of-code.dec-2022.day-25
  (:require [ysera.test :refer [is is-not is= deftest]]))

(def input (->> (slurp "src/advent_of_code/dec_2022/day_25_input.txt")
                (clojure.string/split-lines)))

(def snafu-digit->decimal-digit {\2 2
                                 \1 1
                                 \0 0
                                 \- -1
                                 \= -2})

(defn snafu->decimal
  {:test (fn []
           (is= (snafu->decimal "1") 1)
           (is= (snafu->decimal "2") 2)
           (is= (snafu->decimal "1=") 3)
           (is= (snafu->decimal "1-") 4)
           (is= (snafu->decimal "10") 5)
           (is= (snafu->decimal "11") 6)
           (is= (snafu->decimal "12") 7)
           (is= (snafu->decimal "2=") 8)
           (is= (snafu->decimal "2-") 9)
           (is= (snafu->decimal "20") 10)
           (is= (snafu->decimal "1=0") 15)
           (is= (snafu->decimal "1-0") 20)
           (is= (snafu->decimal "1=11-2") 2022)
           (is= (snafu->decimal "1-0---0") 12345)
           (is= (snafu->decimal "1121-1110-1=0") 314159265)
           )}
  [x]
  (->> (reverse x)
       (map-indexed (fn [index d]
                      (* (long (Math/pow 5 index))
                         (snafu-digit->decimal-digit d))))
       (reduce +)))

(def test-input ["1=-0-2" "12111" "2=0=" "21" "2=01" "111" "20012" "112" "1=-1=" "1-12" "12" "1=" "122"])

(comment
  (->> test-input
       (map snafu->decimal)
       (reduce +))
  ; 4890

  (time
    (->> input
         (map snafu->decimal)
         (reduce +)))
  ; "Elapsed time: 67.698126 msecs"
  ; => 27210103880867
  )

(defn decimal->snafu
  {:test (fn []
           (is= (decimal->snafu 1) "1")
           (is= (decimal->snafu 2) "2")
           (is= (decimal->snafu 3) "1=")
           (is= (decimal->snafu 4) "1-")
           (is= (decimal->snafu 5) "10")
           (is= (decimal->snafu 6) "11")
           (is= (decimal->snafu 7) "12")
           (is= (decimal->snafu 8) "2=")
           (is= (decimal->snafu 1747) "1=-0-2")
           (is= (decimal->snafu 906) "12111")
           (is= (decimal->snafu 198) "2=0=")
           (is= (decimal->snafu 11) "21")
           (is= (decimal->snafu 201) "2=01")
           (is= (decimal->snafu 31) "111")
           (is= (decimal->snafu 1257) "20012")
           (is= (decimal->snafu 32) "112")
           (is= (decimal->snafu 353) "1=-1=")
           (is= (decimal->snafu 107) "1-12")
           (is= (decimal->snafu 7) "12")
           (is= (decimal->snafu 3) "1=")
           (is= (decimal->snafu 37) "122"))}
  [x]
  (loop [x x
         result ""]
    (if (zero? x)
      result
      (let [r (mod x 5)]
        (condp = r
          3 (recur (/ (+ x 2) 5)
                   (str "=" result))
          4 (recur (/ (+ x 1) 5)
                   (str "-" result))
          (recur (/ (- x r) 5)
                 (str r result)))))))

(comment
  (time (decimal->snafu 27210103880867))
  ; "Elapsed time: 0.094882 msecs"
  ; => "121=2=1==0=10=2-20=2"
  )




