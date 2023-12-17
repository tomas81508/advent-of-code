(ns advent-of-code.dec-2023.day-17
  (:require [advent-of-code.test :refer [is= is is-not]]))

; solved with Daniel Gullberg

(def input (slurp "src/advent_of_code/dec_2023/day_17_input.txt"))
(def test-input "2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533")

(defn get-heat-loss-map
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [a i]
              (let [line (nth lines i)]
                (reduce (fn [[a max-i max-j] j]
                          [(assoc a [i j] (read-string (str (nth line j)))) (max max-i i) (max max-j j)])
                        a
                        (range (count line)))))
            [{} 0 0]
            (range (count lines)))))

(defn get-neighbours
  [heat-loss-map {position :position direction :direction straight :straight}]
  (reduce (fn [a d]
            (let [pos (mapv + position d)]
              (if (contains? heat-loss-map pos)
                (conj a {:position pos :direction d :straight (if (= direction d) (inc straight) 1)})
                a)))
          #{}
          (remove (fn [d]
                    (or (and (= d direction) (= straight 3))
                        (= d (mapv * direction [-1 -1]))))
                  [[1 0] [-1 0] [0 1] [0 -1]])))

(defn manhattan-distance
  [p1 p2]
  (+ (abs (- (first p1) (first p2)))
     (abs (- (second p1) (second p2)))))


;; https://en.wikipedia.org/wiki/A*_search_algorithm
(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 102))}
  [input]
  (let [[heat-loss-map max-i max-j] (get-heat-loss-map input)
        end-position [max-i max-j]]
    (loop [unvisited {{:position [0 0] :direction [0 1] :straight 0} 0}
           visited {}]
      (let [next-to-process (first (reduce-kv (fn [[best-k best-estimate] k v]
                                                (let [estimated-cost (+ v (manhattan-distance (:position k) end-position))]
                                                  (if (< estimated-cost best-estimate)
                                                    [k estimated-cost]
                                                    [best-k best-estimate])))
                                              [nil ##Inf]
                                              unvisited))
            cost-so-far (get unvisited next-to-process)
            unvisited (dissoc unvisited next-to-process)]
        (println (count unvisited) (count visited))
        (if (= (:position next-to-process) end-position)
          cost-so-far
          (let [neighbours (->> (get-neighbours heat-loss-map next-to-process)
                                (remove (fn [n]
                                          (contains? visited n))))]
            (recur (reduce (fn [unvisited neighbour]
                             (let [old-cost (get unvisited neighbour ##Inf)
                                   best-cost-less-straight (->> (range 1 (:straight neighbour))
                                                                (reduce (fn [a s]
                                                                          (min a (get unvisited (assoc neighbour :straight s) ##Inf)))
                                                                        old-cost))
                                   new-cost (+ cost-so-far (get heat-loss-map (:position neighbour)))]
                               (if (< new-cost best-cost-less-straight)
                                 (assoc unvisited neighbour new-cost)
                                 unvisited)))
                           unvisited
                           neighbours)
                   (assoc visited next-to-process cost-so-far))))))))

(defn get-neighbours-b
  [heat-loss-map {position :position direction :direction straight :straight}]
  (if (< straight 4)
    (let [new-position (mapv + position direction)]
      (if (contains? heat-loss-map new-position)
        #{{:position new-position :direction direction :straight (inc straight)}}
        #{}))
    (reduce (fn [a d]
              (let [pos (mapv + position d)]
                (if (contains? heat-loss-map pos)
                  (conj a {:position pos :direction d :straight (if (= direction d) (inc straight) 1)})
                  a)))
            #{}
            (remove (fn [d]
                      (or (and (= d direction) (= straight 10))
                          (= d (mapv * direction [-1 -1]))))
                    [[1 0] [-1 0] [0 1] [0 -1]]))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 94))}
  [input]
  (let [[heat-loss-map max-i max-j] (get-heat-loss-map input)
        end-position [max-i max-j]]
    (loop [unvisited {{:position [0 0] :direction [0 1] :straight 0} 0}
           visited {}]
      (let [next-to-process (first (reduce-kv (fn [[best-k best-estimate] k v]
                                                (let [estimated-cost (+ v (manhattan-distance (:position k) end-position))]
                                                  (if (< estimated-cost best-estimate)
                                                    [k estimated-cost]
                                                    [best-k best-estimate])))
                                              [nil ##Inf]
                                              unvisited))
            cost-so-far (get unvisited next-to-process)
            unvisited (dissoc unvisited next-to-process)]
        (println (count unvisited) (count visited))
        (if (= (:position next-to-process) end-position)
          cost-so-far
          (let [neighbours (->> (get-neighbours-b heat-loss-map next-to-process)
                                (remove (fn [n]
                                          (contains? visited n))))]
            (recur (reduce (fn [unvisited neighbour]
                             (let [old-cost (get unvisited neighbour ##Inf)
                                   best-cost-less-straight (->> (range 4 (:straight neighbour))
                                                                (reduce (fn [a s]
                                                                          (min a (get unvisited (assoc neighbour :straight s) ##Inf)))
                                                                        old-cost))
                                   new-cost (+ cost-so-far (get heat-loss-map (:position neighbour)))]
                               (if (< new-cost best-cost-less-straight)
                                 (assoc unvisited neighbour new-cost)
                                 unvisited)))
                           unvisited
                           neighbours)
                   (assoc visited next-to-process cost-so-far))))))))

(comment
  (time (solve-a input))
  ;; 870
  ;; "Elapsed time: 119354.273065 msecs"

  (time (solve-b input))
  ;; 1063
  ;; "Elapsed time: 1100008.036926 msecs"
  )