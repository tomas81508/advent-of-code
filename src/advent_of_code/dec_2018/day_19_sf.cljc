(ns advent-of-code.dec-2018.day-19-sf
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [advent-of-code.dec-2018.day-16 :as day-16]))

(def get-operation {:addr day-16/addr
                    :addi day-16/addi
                    :mulr day-16/mulr
                    :muli day-16/muli
                    :banr day-16/banr
                    :bani day-16/bani
                    :borr day-16/borr
                    :bori day-16/bori
                    :setr day-16/setr
                    :seti day-16/seti
                    :gtir day-16/gtir
                    :gtri day-16/gtri
                    :gtrr day-16/gtrr
                    :eqir day-16/eqir
                    :eqri day-16/eqri
                    :eqrr day-16/eqrr})



(defn read-program
  [filepath]
  (let [text (-> (slurp filepath)
                 (str/split-lines))
        ip-binding (-> (first text)
                       (str/split #" ")
                       (second)
                       (read-string))
        instructions (->> (rest text)
                          (map (fn [line]
                                 (let [[instr a b c] (str/split line #" ")]
                                   {:op (keyword instr)
                                    :a  (read-string a)
                                    :b  (read-string b)
                                    :c  (read-string c)}))))]
    {:ip-binding   ip-binding
     :instructions instructions
     :length       (count instructions)}))


(defn run-program
  [program start-registers]
  (loop [ip 0
         registers start-registers
         tick 0]
    (cond
      (>= ip (:length program))
      {:registers registers :tick tick :status :normal}

      (> tick 5) ; 50000000)
      {:registers registers :tick tick :status :stopped}

      :else
      (let [{op :op a :a b :b c :c} (nth (:instructions program) ip)
            operation (get-operation op)
            post-registers (-> registers
                               (assoc (:ip-binding program) ip)
                               (operation [a b c]))
            new-ip (inc (nth post-registers (:ip-binding program)))]
        ; (when (= 0 (mod tick 10000000))
          (println "post-registers" post-registers "ip" new-ip "tick" tick)
          ; )
        (recur new-ip post-registers (inc tick))))))

(defn get-first-answer
  []
  (let [program (read-program "src/advent_of_code/dec_2018/day_19.txt")
        post-registers (first (run-program program [1 0 0 0 0 0]))]
    (nth post-registers 0)))
