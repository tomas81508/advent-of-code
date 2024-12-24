(ns advent-of-code.dec-2024.day-24b
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :as string]
            [clojure.math :refer [pow]]))

(def input-2 (->> (slurp "src/advent_of_code/dec_2024/day_24_input-2.txt")
                  (string/split-lines)))

(def test-input-2 ["ntg XOR fgs -> mjb" "y02 OR x01 -> tnw" "kwq OR kpj -> z05" "x00 OR x03 -> fst" "tgd XOR rvg -> z01" "vdt OR tnw -> bfw" "bfw AND frj -> z10" "ffh OR nrd -> bqk" "y00 AND y03 -> djm" "y03 OR y00 -> psh" "bqk OR frj -> z08" "tnw OR fst -> frj" "gnj AND tgd -> z11" "bfw XOR mjb -> z00" "x03 OR x00 -> vdt" "gnj AND wpb -> z02" "x04 AND y00 -> kjc" "djm OR pbm -> qhw" "nrd AND vdt -> hwm" "kjc AND fst -> rvg" "y04 OR y02 -> fgs" "y01 AND x02 -> pbm" "ntg OR kjc -> kwq" "psh XOR fgs -> tgd" "qhw XOR tgd -> z09" "pbm OR djm -> kpj" "x03 XOR y03 -> ffh" "x00 XOR y04 -> ntg" "bfw OR bqk -> z06" "nrd XOR fgs -> wpb" "frj XOR qhw -> z04" "bqk OR frj -> z07" "y03 OR x01 -> nrd" "hwm AND bqk -> z03" "tgd XOR rvg -> z12" "tnw OR pbm -> gnj"])

(defn create-number-map
  {:test (fn []
           (is= (create-number-map "x" 5 "1100")
                {"x00" {:output 0} "x01" {:output 0} "x02" {:output 1} "x03" {:output 1} "x04" {:output 0}}))}
  [prefix length input]
  (->> (str (apply str (repeat length "0")) input)
       (reverse)
       (take length)
       (map-indexed (fn [index v] [index v]))
       (reduce (fn [a [index v]]
                 (let [key (str prefix (if (< index 10) "0" "") index)]
                   (assoc a key {:output (read-string (str v))})))
               {})))

(defn create-number-values
  {:test (fn []
           (is= (create-number-values 5 "1100" "0101")
                {"x00" {:output 0} "x01" {:output 0} "x02" {:output 1} "x03" {:output 1} "x04" {:output 0}
                 "y00" {:output 1} "y01" {:output 0} "y02" {:output 1} "y03" {:output 0} "y04" {:output 0}}))}
  [length input-x input-y]
  (merge (create-number-map "x" length input-x) (create-number-map "y" length input-y)))

(defn create-gates
  [input-gates]
  (reduce (fn [state i]
            (let [[b a] (string/split i #" -> ")
                  [i1 gate i2] (string/split b #" ")]
              (assoc state a {:output nil
                              :gate   gate
                              :inputs [i1 i2]})))
          {}
          input-gates))

(def gates (create-gates input-2))

(defn get-input-values
  [state wire]
  (if (get-in state [wire :output])
    :done
    (->> (get-in state [wire :inputs])
         (reduce (fn [input-values input]
                   (if-let [input-value (get-in state [input :output])]
                     (conj input-values input-value)
                     (reduced :fail)))
                 []))))

(defn apply-gate
  [gate input-values]
  (case gate
    "XOR" (apply bit-xor input-values)
    "OR" (apply max input-values)
    "AND" (apply min input-values)))

(defn pulse
  [state]
  (->> state
       (reduce-kv (fn [state wire v]
                    (let [update-values (get-input-values state wire)]
                      (if (keyword? update-values)
                        state
                        (let [output-value (apply-gate (:gate v) update-values)]
                          ;(when (= output-value 1)
                          ;  (println wire "got 1" (:gate v) (get-in state [wire :inputs])))
                          (assoc-in state [wire :output] output-value)))))
                  state)))

(defn pulse-until-the-end
  [state]
  (loop [state state]
    (let [new-state (pulse state)]
      (if (= state new-state)
        state
        (recur new-state)))))

(defn find-decimal
  [state]
  (as-> state $
        (seq $)
        (filter (fn [[k _]] (string/starts-with? k "z")) $)
        (sort-by first $)
        (reverse $)
        (map (fn [[_ v]] (:output v)) $)
        (apply str $)
        (new BigInteger $ 2)))

(defn swap-outputs
  [gates output-1 output-2]
  (let [value-1 (get gates output-1)
        value-2 (get gates output-2)]
    (-> gates
        (assoc output-1 value-2)
        (assoc output-2 value-1))))

(defn add
  {:test (fn []
           (is= (add gates 45 "0" "0") 0)
           (is= (add gates 45 "1" "0") 1)
           (is= (add gates 45 "0" "1") 1)
           (is= (add gates 45 "1" "1") 2)
           (is= (add gates 45 "10" "11") 5)
           (is= (add gates 45 "1000" "1000") 16)
           (is= (add gates 45 "10000" "10000") 32)
           (is= (add gates 45 "100000" "100000") 64)
           (is= (add gates 45 "1000000" "1000000") 128)
           (is= (add gates 45 "10000000" "10000000") 256)
           (is= (add gates 45 "100000000" "100000000") 512)
           (is= (add gates 45 "1000000000" "1000000000") 1024)
           (is= (add gates 45 "10000000000" "10000000000") 2048)
           (is= (add gates 45 "100000000000" "100000000000") 4096)
           (is= (add gates 45 "1000000000000" "1000000000000") 8192)
           (is= (add gates 45 (apply str "1" (repeat 13 "0")) (apply str "1" (repeat 13 "0"))) 16384)
           (is= (add gates 45 (apply str "1" (repeat 14 "0")) (apply str "1" (repeat 14 "0"))) 32768)
           (is= (add gates 45 (apply str "1" (repeat 15 "0")) (apply str "1" (repeat 15 "0"))) 65536)
           (is= (add gates 45 (apply str "1" (repeat 16 "0")) (apply str "1" (repeat 16 "0"))) 131072)
           (is= (add gates 45 (apply str "1" (repeat 17 "0")) (apply str "1" (repeat 17 "0"))) 262144)
           (is= (add gates 45 (apply str "1" (repeat 18 "0")) (apply str "1" (repeat 18 "0"))) 524288)
           (is= (add gates 45 (apply str "1" (repeat 19 "0")) (apply str "1" (repeat 19 "0"))) (long (pow 2 20)))
           (is= (add gates 45 (apply str "1" (repeat 20 "0")) (apply str "1" (repeat 20 "0"))) (long (pow 2 21)))
           (is= (add gates 45 (apply str "1" (repeat 21 "0")) (apply str "1" (repeat 21 "0"))) (long (pow 2 22)))
           (is= (add gates 45 (apply str "1" (repeat 22 "0")) (apply str "1" (repeat 22 "0"))) (long (pow 2 23)))
           (is= (add gates 45 (apply str "1" (repeat 23 "0")) (apply str "1" (repeat 23 "0"))) (long (pow 2 24)))
           (is= (add gates 45 (apply str "1" (repeat 24 "0")) (apply str "1" (repeat 24 "0"))) (long (pow 2 25)))
           (is= (add gates 45 (apply str "1" (repeat 25 "0")) (apply str "1" (repeat 25 "0"))) (long (pow 2 26)))
           (is= (add gates 45 (apply str "1" (repeat 26 "0")) (apply str "1" (repeat 26 "0"))) (long (pow 2 27)))
           (is= (add gates 45 (apply str "1" (repeat 27 "0")) (apply str "1" (repeat 27 "0"))) (long (pow 2 28)))
           (is= (add gates 45 (apply str "1" (repeat 28 "0")) (apply str "1" (repeat 28 "0"))) (long (pow 2 29)))
           (is= (add gates 45 (apply str "1" (repeat 29 "0")) (apply str "1" (repeat 29 "0"))) (long (pow 2 30)))
           (is= (add gates 45 (apply str "1" (repeat 30 "0")) (apply str "1" (repeat 30 "0"))) (long (pow 2 31)))
           (is= (add gates 45 (apply str "1" (repeat 31 "0")) (apply str "1" (repeat 31 "0"))) (long (pow 2 32)))
           (is= (add gates 45 (apply str "1" (repeat 32 "0")) (apply str "1" (repeat 32 "0"))) (long (pow 2 33)))
           (is= (add gates 45 (apply str "1" (repeat 33 "0")) (apply str "1" (repeat 33 "0"))) (long (pow 2 34)))
           (is= (add gates 45 (apply str "1" (repeat 34 "0")) (apply str "1" (repeat 34 "0"))) (long (pow 2 35)))
           (is= (add gates 45 (apply str "1" (repeat 35 "0")) (apply str "1" (repeat 35 "0"))) (long (pow 2 36)))
           (is= (add gates 45 (apply str "1" (repeat 36 "0")) (apply str "1" (repeat 36 "0"))) (long (pow 2 37)))
           (is= (add gates 45 (apply str "1" (repeat 37 "0")) (apply str "1" (repeat 37 "0"))) (long (pow 2 38)))
           (is= (add gates 45 (apply str "1" (repeat 38 "0")) (apply str "1" (repeat 38 "0"))) (long (pow 2 39)))
           (is= (add gates 45 (apply str "1" (repeat 39 "0")) (apply str "1" (repeat 39 "0"))) (long (pow 2 40)))
           (is= (add gates 45 (apply str "1" (repeat 40 "0")) (apply str "1" (repeat 40 "0"))) (long (pow 2 41)))
           (is= (add gates 45 (apply str "1" (repeat 41 "0")) (apply str "1" (repeat 41 "0"))) (long (pow 2 42)))
           (is= (add gates 45 (apply str "1" (repeat 42 "0")) (apply str "1" (repeat 42 "0"))) (long (pow 2 43)))
           (is= (add gates 45 (apply str "1" (repeat 43 "0")) (apply str "1" (repeat 43 "0"))) (long (pow 2 44)))
           (is= (add gates 45 (apply str "1" (repeat 44 "0")) (apply str "1" (repeat 44 "0"))) (long (pow 2 45)))

           (is= (add gates 45 (apply str (repeat 1 "1")) "0") (dec (long (pow 2 1))))
           (is= (add gates 45 (apply str (repeat 2 "1")) "0") (dec (long (pow 2 2))))
           (is= (add gates 45 (apply str (repeat 3 "1")) "0") (dec (long (pow 2 3))))
           (is= (add gates 45 (apply str (repeat 4 "1")) "0") (dec (long (pow 2 4))))
           (is= (add gates 45 (apply str (repeat 5 "1")) "0") (dec (long (pow 2 5))))
           (is= (add gates 45 (apply str (repeat 6 "1")) "0") (dec (long (pow 2 6))))
           (is= (add gates 45 (apply str (repeat 7 "1")) "0") (dec (long (pow 2 7))))
           (is= (add gates 45 (apply str (repeat 8 "1")) "0") (dec (long (pow 2 8))))
           (is= (add gates 45 (apply str (repeat 9 "1")) "0") (dec (long (pow 2 9))))
           (is= (add gates 45 (apply str (repeat 10 "1")) "0") (dec (long (pow 2 10))))
           (is= (add gates 45 (apply str (repeat 11 "1")) "0") (dec (long (pow 2 11))))
           (is= (add gates 45 (apply str (repeat 12 "1")) "0") (dec (long (pow 2 12))))
           (is= (add gates 45 (apply str (repeat 13 "1")) "0") (dec (long (pow 2 13))))
           (is= (add gates 45 (apply str (repeat 14 "1")) "0") (dec (long (pow 2 14))))
           (is= (add gates 45 (apply str (repeat 15 "1")) "0") (dec (long (pow 2 15))))
           (is= (add gates 45 (apply str (repeat 16 "1")) "0") (dec (long (pow 2 16))))
           (is= (add gates 45 (apply str (repeat 17 "1")) "0") (dec (long (pow 2 17))))
           (is= (add gates 45 (apply str (repeat 18 "1")) "0") (dec (long (pow 2 18))))
           (is= (add gates 45 (apply str (repeat 19 "1")) "0") (dec (long (pow 2 19))))
           (is= (add gates 45 (apply str (repeat 20 "1")) "0") (dec (long (pow 2 20))))
           (is= (add gates 45 (apply str (repeat 21 "1")) "0") (dec (long (pow 2 21))))
           (is= (add gates 45 (apply str (repeat 22 "1")) "0") (dec (long (pow 2 22))))
           (is= (add gates 45 (apply str (repeat 23 "1")) "0") (dec (long (pow 2 23))))
           (is= (add gates 45 (apply str (repeat 24 "1")) "0") (dec (long (pow 2 24))))
           (is= (add gates 45 (apply str (repeat 25 "1")) "0") (dec (long (pow 2 25))))
           (is= (add gates 45 (apply str (repeat 26 "1")) "0") (dec (long (pow 2 26))))
           (is= (add gates 45 (apply str (repeat 27 "1")) "0") (dec (long (pow 2 27))))
           (is= (add gates 45 (apply str (repeat 28 "1")) "0") (dec (long (pow 2 28))))
           (is= (add gates 45 (apply str (repeat 29 "1")) "0") (dec (long (pow 2 29))))
           (is= (add gates 45 (apply str (repeat 30 "1")) "0") (dec (long (pow 2 30))))
           (is= (add gates 45 (apply str (repeat 31 "1")) "0") (dec (long (pow 2 31))))
           (is= (add gates 45 (apply str (repeat 32 "1")) "0") (dec (long (pow 2 32))))
           (is= (add gates 45 (apply str (repeat 33 "1")) "0") (dec (long (pow 2 33))))
           (is= (add gates 45 (apply str (repeat 34 "1")) "0") (dec (long (pow 2 34))))
           (is= (add gates 45 (apply str (repeat 35 "1")) "0") (dec (long (pow 2 35))))
           (is= (add gates 45 (apply str (repeat 36 "1")) "0") (dec (long (pow 2 36))))
           (is= (add gates 45 (apply str (repeat 37 "1")) "0") (dec (long (pow 2 37))))
           (is= (add gates 45 (apply str (repeat 38 "1")) "0") (dec (long (pow 2 38))))
           (is= (add gates 45 (apply str (repeat 39 "1")) "0") (dec (long (pow 2 39))))
           (is= (add gates 45 (apply str (repeat 40 "1")) "0") (dec (long (pow 2 40))))
           (is= (add gates 45 (apply str (repeat 41 "1")) "0") (dec (long (pow 2 41))))
           (is= (add gates 45 (apply str (repeat 42 "1")) "0") (dec (long (pow 2 42))))
           (is= (add gates 45 (apply str (repeat 43 "1")) "0") (dec (long (pow 2 43))))
           (is= (add gates 45 (apply str (repeat 44 "1")) "0") (dec (long (pow 2 44))))
           )}
  [gates length x y]
  (let [state (-> (merge gates (create-number-values length x y))
                  (swap-outputs "z14" "hbk")
                  (swap-outputs "z18" "kvn")
                  (swap-outputs "z23" "dbb")
                  (swap-outputs "tfn" "cvh")
                  )]
    (-> (pulse-until-the-end state)
        (find-decimal))))

(comment
  (->> (range 1 43)
       (some (fn [m]
               (when-not (= (let [n-string (apply str "111" (repeat m "0"))]
                              (add gates 45 n-string n-string))
                            (long (* 2 (+ (pow 2 m) (pow 2 (inc m)) (pow 2 (+ 2 m))))))
                 m))))


  (->> (for [x (range 45)]
         [(apply str "1" (repeat x "0")) (apply str "1" (repeat x "0")) (long (pow 2 (inc x)))])
       (some (fn [[x y r]] (when-not (= r (add gates 45 x y)) [x y]))))

  (->> (for [n (range 1 46)]
         (let [x (apply str (repeat n "1"))
               r (BigInteger. x 2)]
           [x (* 2 r)]))
       (some (fn [[x r]] (when-not (= r (add gates 45 x x)) x))))

  (->> (for [n (range 1 46)]
         (let [x (apply str (repeat n "1"))
               r (BigInteger. x 2)]
           [x r]))
       (some (fn [[x r]] (when-not (= r (add gates 45 x "0")) x))))

  )


(get gates "z34")
(get gates "hbk")

(def l ["z14" "hbk" "z18" "kvn" "z23" "dbb" "cvh" "tfn"])
(string/join "," (sort l))

; NOT cvh,dbb,jss,z14,z15,z18,z19,z23
; NOT cvh,dbb,hbk,jss,z14,z18,z19,z23
; NOT cvh,dbb,hbk,tfn,z14,z18,z19,z23

(defn get-wires-having-input
  [gates input]
  (->> gates
       (seq)
       (filter (fn [[_ v]]
                 (contains? (into #{} (:inputs v)) input)))))

(get-wires-having-input gates "x18")
(get-wires-having-input gates "grp")
(get gates "cjb")
(get gates "ffb")

(comment
  (->> gates
       (seq)
       (filter (fn [[k v]]
                 (and (string/starts-with? k "z")
                      (not= (:gate v) "XOR")))))
  (["z23" {:output nil, :gate "AND", :inputs ["dvw" "rpg"]}]
   ["z45" {:output nil, :gate "OR", :inputs ["cjf" "pfk"]}]
   ["z18" {:output nil, :gate "AND", :inputs ["y18" "x18"]}]
   ["z14" {:output nil, :gate "OR", :inputs ["sjr" "tck"]}])

  (->> gates
       (seq)
       (filter (fn [[k v]]
                 (and (not= k "z00")
                      (not= k "z18")
                      (string/starts-with? k "z")
                      (let [inputs (:inputs v)]
                        (->> inputs
                             (some (fn [input]
                                     (let [[i1 i2] (sort (get-in gates [input :inputs]))]
                                       (if-not (and (string/starts-with? i1 "x")
                                                    (string/starts-with? i2 "y"))
                                         [k v]))))))))))

  )




