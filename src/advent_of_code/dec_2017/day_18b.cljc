(ns advent-of-code.dec-2017.day-18b
  (:require [ysera.test :refer [is=]]))

(def input ["set i 31" "set a 1" "mul p 17" "jgz p p" "mul a 2" "add i -1" "jgz i -2" "add a -1" "set i 127" "set p 826" "mul p 8505" "mod p a" "mul p 129749" "add p 12345" "mod p a" "set b p" "mod b 10000" "snd b" "add i -1" "jgz i -9" "jgz a 3" "rcv b" "jgz b -1" "set f 0" "set i 126" "rcv a" "rcv b" "set p a" "mul p -1" "add p b" "jgz p 4" "snd a" "set a b" "jgz 1 3" "snd b" "set f 1" "add i -1" "jgz i -11" "snd a" "jgz f -16" "jgz a -19"])

(def state (let [program {:variables (zipmap (->> input
                                                  (map (fn [l] (second (clojure.string/split l #" "))))
                                                  (filter (fn [x] (re-find #"[a-z]" x))))
                                             (repeat 0))
                          :line      0
                          :blocked   false
                          :queue     []}]
             {"0" (-> program
                      (assoc-in [:variables "p"] 0)
                      (assoc :queue []))
              "1" (-> program
                      (assoc-in [:variables "p"] 1)
                      (assoc :queue [])
                      (assoc :counter 0))}))

(def instructions (->> input
                       (map (fn [command] (clojure.string/split command #" ")))
                       (map (fn [things] (map (fn [thing] (if (re-find #"\d+" thing) (read-string thing) thing)) things)))
                       (into [])))

(defn variable? [s] (string? s))

(defn get-value
  [state program-id x]
  (if (variable? x) (get-in state [program-id :variables x]) x))

(def get-other-program-id {"0" "1" "1" "0"})

(defn run-command
  [state program-id [command arg1 arg2]]
  (let [arg2-value (when arg2 (get-value state program-id arg2))]
    (condp = command
      "snd" (let [other-program-id (get-other-program-id program-id)
                  value (get-value state program-id arg1)]
              ;(println "Sending" value "to the program" other-program-id)
              (-> (if (= program-id "1")
                    (update-in state ["1" :counter] inc)
                    state)
                  (update-in [other-program-id :queue] (fn [q] (conj q (get-value state program-id value))))
                  (update-in [program-id :line] inc)))
      "set" (-> state
                (assoc-in [program-id :variables arg1] arg2-value)
                (update-in [program-id :line] inc))
      "add" (-> state
                (update-in [program-id :variables arg1] + arg2-value)
                (update-in [program-id :line] inc))
      "mul" (-> state
                (update-in [program-id :variables arg1] * arg2-value)
                (update-in [program-id :line] inc))
      "mod" (-> state
                (update-in [program-id :variables arg1] rem arg2-value)
                (update-in [program-id :line] inc))
      "rcv" (let [value (first (get-in state [program-id :queue]))]
              ;(println "Receiving" arg1 value)
              (if value
                (-> state
                    (assoc-in [program-id :variables arg1] value)
                    (update-in [program-id :queue] (fn [q] (into [] (drop 1 q))))
                    (update-in [program-id :line] inc))
                (-> state
                    (assoc-in [program-id :blocked] true))))
      "jgz" (if (pos? (get-value state program-id arg1))
              (update-in state [program-id :line] + arg2-value)
              (update-in state [program-id :line] inc)))))

(defn program-blocked? [state program-id]
  (and (get-in state [program-id :blocked])
       (empty? (get-in state [program-id :queue]))))

(defn run-program-until-blocked
  [state program-id]
  (if (program-blocked? state program-id)
    state
    (let [command (get instructions (get-in state [program-id :line]))
          state (if (get-in state [program-id :blocked])
                  (assoc-in state [program-id :blocked] false)
                  state)]
      (recur (run-command state program-id command) program-id))))


(defn run-programs-until-deadlock
  [state n]
  ;(println n)
  (if (> n 100000)
    state
    (cond (not (program-blocked? state "0"))
          (do ;(println "Running program 0")
              (recur (run-program-until-blocked state "0") (inc n)))

          (not (program-blocked? state "1"))
          (do ;(println "Running program 1")
              (recur (run-program-until-blocked state "1") (inc n)))

          :else
          state)))


(comment
  (run-programs-until-deadlock state 0)
  ; Too high: 6551587
  )