(ns advent-of-code.dec-2018.day-17
  (:require [clojure.string]))

(defn get-puzzle-input []
  (-> (slurp "src/advent_of_code/dec_2018/day_17.txt")
      (clojure.string/split-lines)))

(defn get-map
  [input]
  (->
    (reduce (fn [result line]
              (if (= (first line) \x)
                (let [regex (re-pattern "x=(\\d+), y=(\\d+)\\.\\.(\\d+)")
                      [_ x-str y1-str y2-str] (re-find regex line)
                      [x y1 y2] (map read-string [x-str y1-str y2-str])]
                  (reduce (fn [result y]
                            (assoc result [x y] \#))
                          result
                          (range y1 (inc y2))))
                (let [regex (re-pattern "y=(\\d+), x=(\\d+)\\.\\.(\\d+)")
                      [_ y-str x1-str x2-str] (re-find regex line)
                      [y x1 x2] (map read-string [y-str x1-str x2-str])]
                  (reduce (fn [result x]
                            (assoc result [x y] \#))
                          result
                          (range x1 (inc x2)))))
              ) {} input)
    (assoc [500 0] \+)))

(defn get-max-coordinates
  [ground]
  (let [xmax (->> (keys ground)
                  (map first)
                  (apply max))
        xmin (->> (keys ground)
                  (map first)
                  (apply min))
        ymax (->> (keys ground)
                  (map second)
                  (apply max))
        ymin (->> (keys ground)
                  (map second)
                  (apply min))]
    [xmax xmin ymax ymin]))

(defn draw-map
  [ground]
  (let [[xmax xmin ymax ymin] (get-max-coordinates ground)]
    (->> (range ymin (inc ymax))
         (map (fn [y]
                (->> (range (dec xmin) (+ xmax 2))
                     (map (fn [x]
                            (get ground [x y] \.)))
                     (apply str)))))))

(defn get-water-passed
  [ground]
  (->
    (reduce-kv (fn [result pos v]
                 (if (= v \|)
                   result
                   (dissoc result pos))
                 ) ground ground)
    (assoc [500 0] \+)))

(defn get-element
  [ground pos]
  (get ground pos \.))


(defn fill
  [ground pos element]
  (assoc ground pos element))

(defn below
  [ground [x y]]
  (get-element ground [x (inc y)]))


(defn clay-wall-or-fall-right
  [ground [x y]]
  (loop [current-x (inc x)]
    (cond (= (get-element ground [current-x y]) \#)
          [:clay current-x]

          (or (= (below ground [current-x y]) \.)
              (= (below ground [current-x y]) \|))
          [:fall current-x]

          :else
          (recur (inc current-x)))))

(defn clay-wall-or-fall-left
  [ground [x y]]
  (loop [current-x (dec x)]
    (cond (= (get-element ground [current-x y]) \#)
          [:clay current-x]

          (or (= (below ground [current-x y]) \.)
              (= (below ground [current-x y]) \|))
          [:fall current-x]

          :else
          (recur (dec current-x)))))


(defn water-fall
  [ground [x y]]
  (loop [current-y (dec y)]
    (let [element (get-element ground [x current-y])]
      (if (= element \.)
        (recur (dec current-y))
        current-y))))


(defn flow-once
  [ground water-passed-positions]
  (reduce (fn [result [x y]]
            (let [below-pos [x (inc y)]
                  below (get-element result below-pos)]
              (cond (= below \|)
                    [result water-passed-positions]

                    (= below \.)
                    (reduce (fn [result y]
                              [(fill result [x y] \|) (conj water-passed-positions [x y])])
                            [result water-passed-positions]
                            (range y (water-fall ground [x y])))


                    (or (= below \#) (= below \~))
                    (let [[element-left x-left] (clay-wall-or-fall-left ground [x y])
                          [element-right x-right] (clay-wall-or-fall-right ground [x y])]
                      ;(println y x-left element-left x-right element-right)
                      (if (= element-left element-right :clay)
                        (reduce (fn [result x]
                                  [(fill result [x y] \~) (remove #(= % [x y]) water-passed-positions)])
                                result
                                (range (inc x-left) x-right))
                        (reduce (fn [result x]
                                  (if (= (get-element result [x y]) \#)
                                    [result water-passed-positions]
                                    [(fill result [x y] \|) (conj water-passed-positions [x y])]))
                                [result water-passed-positions]
                                (range x-left (inc x-right))))))))
          ground
          water-passed-positions))


(defn get-first-answer
  [ground]
  (let [[xmax xmin global-y-max ymin] (get-max-coordinates ground)]
    (let [finished
          (loop [[ground water-passed-positions] [ground (get-water-passed ground)]
                 tick 0]
            (when (= 0 (mod tick 1000))
              (println tick)
              (clojure.pprint/pprint (draw-map ground)))
            (let [y-max (->> (keys ground)
                             (map second)
                             (apply max))]
              (if (> y-max global-y-max)
                ground
                (recur (time (flow-once ground water-passed-positions)) (inc tick)))))]
      (clojure.pprint/pprint (draw-map finished))
      (reduce-kv (fn [result [x y] v]
                   (if (and
                         (>= y ymin)
                         (<= y global-y-max)
                         (or (= v \~) (= v \|)))
                     (inc result)
                     result)) 0 finished))))




(comment (get-first-answer (get-map (read-input))))