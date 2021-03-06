(ns advent-of-code.dec-2016.day-12
  (:require [ysera.test :refer [deftest is= is is-not]]
            [ysera.collections :refer [seq-contains?]]
            [clojure.string :refer [starts-with? split]]))


(def puzzle-input ["cpy 1 a"
                   "cpy 1 b"
                   "cpy 26 d"
                   "jnz c 2"
                   "jnz 1 5"
                   "cpy 7 c"
                   "inc d"
                   "dec c"
                   "jnz c -2"
                   "cpy a c"
                   "inc a"
                   "dec b"
                   "jnz b -2"
                   "cpy c b"
                   "dec d"
                   "jnz d -6"
                   "cpy 14 c"
                   "cpy 14 d"
                   "inc a"
                   "dec d"
                   "jnz d -2"
                   "dec c"
                   "jnz c -5"])

(def test-input ["cpy 41 a"
                 "inc a"
                 "inc a"
                 "dec a"
                 "jnz a 2"
                 "dec a"])

(defn get-value
  [memory value-or-variable]
  (if (re-matches #"[\d]+" value-or-variable)
    (read-string value-or-variable)
    (get memory value-or-variable)))

(defn run
  {:test (fn []
           (is= (-> (run test-input)
                    (get "a"))
                42))}
  ([instructions] (run instructions {}))
  ([instructions memory]
   (loop [current 0
          memory (merge {"a" 0 "b" 0 "c" 0 "d" 0} memory)]
     (let [i (get instructions current)]
       (cond (nil? i)
             memory

             (starts-with? i "cpy")
             (let [[_ value-or-variable variable] (split i #" ")
                   value (get-value memory value-or-variable)]
               (recur (inc current) (assoc memory variable value)))

             (starts-with? i "inc")
             (let [[_ variable] (split i #" ")]
               (recur (inc current) (update memory variable inc)))

             (starts-with? i "dec")
             (let [[_ variable] (split i #" ")]
               (recur (inc current) (update memory variable dec)))

             (starts-with? i "jnz")
             (let [[_ value-or-variable steps] (split i #" ")
                   value (get-value memory value-or-variable)]
               (if (zero? value)
                 (recur (inc current) memory)
                 (recur (+ current (read-string steps)) memory))))))))

(deftest puzzle-a
         (is= (time (-> (run puzzle-input)
                        (get "a")))
              ; "Elapsed time: 477.288251 msecs"
              318007))

(deftest puzzle-b
         (is= (time (-> (run puzzle-input {"c" 1})
                        (get "a")))
              ; "Elapsed time: 11451.04182 msecs"
              9227661))