(ns advent-of-code.dec-2017.day-23
  (:require [ysera.test :refer [is=]]))

(def input ["set b 93"
            "set c b"
            "jnz a 2"
            "jnz 1 5"
            "mul b 100"
            "sub b -100000"
            "set c b"
            "sub c -17000"
            "set f 1"
            "set d 2"
            "set e 2"
            "set g d"
            "mul g e"
            "sub g b"
            "jnz g 2"
            "set f 0"
            "sub e -1"
            "set g e"
            "sub g b"
            "jnz g -8"
            "sub d -1"
            "set g d"
            "sub g b"
            "jnz g -13"
            "jnz f 2"
            "sub h -1"
            "set g b"
            "sub g c"
            "jnz g 2"
            "jnz 1 3"
            "sub b -17"
            "jnz 1 -23"])

(def state {:variables {"a" 0 "b" 0 "c" 0 "d" 0 "e" 0 "f" 0 "g" 0 "h" 0}
            :line      0
            :counter   0})

(def program (->> input
                  (map (fn [command] (clojure.string/split command #" ")))
                  (map (fn [things] (map (fn [thing] (if (re-find #"\d+" thing) (read-string thing) thing)) things)))
                  (into [])))

(defn variable? [s] (string? s))

(defn get-value
  [state x]
  (if (variable? x) (get-in state [:variables x]) x))

(defn run-command
  [state [command arg1 arg2]]
  (let [arg2-value (get-value state arg2)]
    (condp = command
      "set" (-> state
                (assoc-in [:variables arg1] arg2-value)
                (update :line inc))
      "mul" (-> state
                (update-in [:variables arg1] * arg2-value)
                (update :line inc)
                (update :counter inc))
      "sub" (-> state
                (update-in [:variables arg1] - arg2-value)
                (update :line inc))
      "jnz" (if (= (get-value state arg1) 0)
              (update state :line inc)
              (update state :line + arg2-value)))))

(defn run-program
  [state n]
  (if (pos? n)
    (let [command (get program (:line state))
          state (run-command state command)]
      (if (number? state)
        state
        (recur state (dec n))))
    state))

(def program-size (count program))

(defn run-program-until-halt
  [state]
  (if-not (and (>= (:line state) 0)
               (< (:line state) program-size))
    (:counter state)
    (let [command (get program (:line state))
          state (run-command state command)]
      (if (number? state)
        state
        (recur state)))))


(comment
  (run-program-until-halt state)
  8281
  )
