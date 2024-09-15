(ns advent-of-code.dec-2017.day-18
  (:require [ysera.test :refer [is=]]))

(def input ["set i 31" "set a 1" "mul p 17" "jgz p p" "mul a 2" "add i -1" "jgz i -2" "add a -1" "set i 127" "set p 826" "mul p 8505" "mod p a" "mul p 129749" "add p 12345" "mod p a" "set b p" "mod b 10000" "snd b" "add i -1" "jgz i -9" "jgz a 3" "rcv b" "jgz b -1" "set f 0" "set i 126" "rcv a" "rcv b" "set p a" "mul p -1" "add p b" "jgz p 4" "snd a" "set a b" "jgz 1 3" "snd b" "set f 1" "add i -1" "jgz i -11" "snd a" "jgz f -16" "jgz a -19"])

(def state {:variables (zipmap (->> input
                                    (map (fn [l] (second (clojure.string/split l #" "))))
                                    (filter (fn [x] (re-find #"[a-z]" x))))
                               (repeat 0))
            :line      0
            :sound     nil
            :recover   nil})

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
  (let [arg2-value (when arg2 (get-value state arg2))]
    (condp = command
      "snd" (-> state
                (assoc :sound (get-value state arg1))
                (update :line inc))
      "set" (-> state
                (assoc-in [:variables arg1] arg2-value)
                (update :line inc))
      "add" (-> state
                (update-in [:variables arg1] + arg2-value)
                (update :line inc))
      "mul" (-> state
                (update-in [:variables arg1] * arg2-value)
                (update :line inc))
      "mod" (-> state
                (update-in [:variables arg1] rem arg2-value)
                (update :line inc))
      "rcv" (if (= (get-value state arg1) 0)
              (-> state
                  (update :line inc))
              (-> state
                  (assoc :recover (:sound state))
                  (update :line inc)))
      "jgz" (if (pos? (get-value state arg1))
              (update state :line + arg2-value)
              (update state :line inc)))))

(defn run-program-until-halt
  [state]
  (if (:recover state)
    (:recover state)
    (let [command (get program (:line state))
          state (run-command state command)]
      (if (number? state)
        state
        (recur state)))))


(comment
  (run-program-until-halt state)
  8281
  )