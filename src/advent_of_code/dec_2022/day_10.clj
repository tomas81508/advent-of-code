(ns advent-of-code.dec-2022.day-10
  (:require [ysera.test :refer [is is-not is=]]))

(def input (->> (slurp "src/advent_of_code/dec_2022/day_10_input.txt")
                (clojure.string/split-lines)))

(def test-input ["addx 15" "addx -11" "addx 6" "addx -3" "addx 5" "addx -1" "addx -8" "addx 13" "addx 4" "noop" "addx -1" "addx 5" "addx -1" "addx 5" "addx -1" "addx 5" "addx -1" "addx 5" "addx -1" "addx -35" "addx 1" "addx 24" "addx -19" "addx 1" "addx 16" "addx -11" "noop" "noop" "addx 21" "addx -15" "noop" "noop" "addx -3" "addx 9" "addx 1" "addx -3" "addx 8" "addx 1" "addx 5" "noop" "noop" "noop" "noop" "noop" "addx -36" "noop" "addx 1" "addx 7" "noop" "noop" "noop" "addx 2" "addx 6" "noop" "noop" "noop" "noop" "noop" "addx 1" "noop" "noop" "addx 7" "addx 1" "noop" "addx -13" "addx 13" "addx 7" "noop" "addx 1" "addx -33" "noop" "noop" "noop" "addx 2" "noop" "noop" "noop" "addx 8" "noop" "addx -1" "addx 2" "addx 1" "noop" "addx 17" "addx -9" "addx 1" "addx 1" "addx -3" "addx 11" "noop" "noop" "addx 1" "noop" "addx 1" "noop" "noop" "addx -13" "addx -19" "addx 1" "addx 3" "addx 26" "addx -30" "addx 12" "addx -1" "addx 3" "addx 1" "noop" "noop" "noop" "addx -9" "addx 18" "addx 1" "addx 2" "noop" "noop" "addx 9" "noop" "noop" "noop" "addx -1" "addx 2" "addx -37" "addx 1" "addx 3" "noop" "addx 15" "addx -21" "addx 22" "addx -6" "addx 1" "noop" "addx 2" "addx 1" "noop" "addx -10" "noop" "noop" "addx 20" "addx 1" "addx 2" "addx 2" "addx -6" "addx -11" "noop" "noop" "noop"])

(defn handle-commands
  [commands]
  (loop [register 1
         commands commands
         signal-strengths []
         value nil
         cycle 0
         crt ["" "" "" "" "" ""]]
    (let [signal-strength (when (zero? (rem (- cycle 20) 40))
                            (* register cycle))
          signal-strengths (if signal-strength
                             (conj signal-strengths signal-strength)
                             signal-strengths)
          command (first commands)
          crt-row (quot cycle 40)
          crt (if (contains? #{(dec register) register (inc register)} (rem cycle 40))
                (update crt crt-row str "#")
                (update crt crt-row str "."))]
      (cond (and (not value) (not command))
            [signal-strengths crt]

            value
            (recur (+ register value)
                   commands
                   signal-strengths
                   nil
                   (inc cycle)
                   crt)

            (= command "noop")
            (recur register
                   (drop 1 commands)
                   signal-strengths
                   nil
                   (inc cycle)
                   crt)

            :else
            (let [value (read-string (subs command 5))]
              (recur register
                     (drop 1 commands)
                     signal-strengths
                     value
                     (inc cycle)
                     crt))))))

(comment
  (handle-commands test-input)

  (time (->> (handle-commands input)
             (first)
             (apply +)))
  (second (handle-commands input))
  ; PGHFGLUG
  )

