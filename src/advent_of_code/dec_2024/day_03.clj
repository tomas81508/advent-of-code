(ns advent-of-code.dec-2024.day-03)

(def input (slurp "src/advent_of_code/dec_2024/day_03_input.txt"))

(def test-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn multiply
  [input]
  (->> (re-seq #"mul\(\d{1,3},\d{1,3}\)" input)
       (map (fn [x] (map read-string (re-seq #"\d+" x))))
       (map (fn [numbers] (apply * numbers)))
       (reduce +)))

(comment
  (multiply test-input)
  (multiply input)
  )

; part 2

(def test-input-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn do-dont-multiply
  [input]
  (->> (re-seq #"mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)" input)
       (reduce (fn [a s]
                 (cond (= s "do()")
                       (assoc a :active true)

                       (= s "don't()")
                       (assoc a :active false)

                       (not (:active a))
                       a

                       :else
                       (update a :sum + (multiply s))))
               {:active true
                :sum    0})))

(comment
  (do-dont-multiply test-input-2)
  (do-dont-multiply input)

  (apply * 2 [3 4 5])
  (* 2 3 4 5)

  (reduce * [2 3 4 5])
  (* (* (* 2 3) 4) 5)

  (reduce * 1 [2 3 4 5])
  (* (* (* (* 1 2) 3) 4) 5)

  (update {:sum 42} :sum + (multiply "mul(3,4)"))
  (update {:sum 42} :sum (fn [old-sum] (+ old-sum  (multiply "mul(3,4)"))))
  )

