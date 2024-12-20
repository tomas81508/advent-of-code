(ns advent-of-code.dec-2019.day-11
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]
            [advent-of-code.dec-2019.day-09 :refer [run]]))

(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2019/day_11.txt") $
        (clojure.string/split $ #",")
        (map read-string $)
        (vec $)))

(def white 1)
(def black 0)

(defn get-new-direction
  [current-direction turn]
  (cond (= current-direction [0 -1])
        (if (zero? turn) [-1 0] [1 0])

        (= current-direction [1 0])
        (if (zero? turn) [0 -1] [0 1])

        (= current-direction [0 1])
        (if (zero? turn) [1 0] [-1 0])

        (= current-direction [-1 0])
        (if (zero? turn) [0 1] [0 -1])))

(defn paint-all
  [int-code starting-color]
  (loop [program {:int-code      int-code
                  :index         0
                  :relative-base 0
                  :inputs        [0]
                  :outputs       []}
         painted-parts {[0 0] starting-color}
         position [0 0]
         direction [0 -1]]
    (let [color-at-position (if (and (contains? painted-parts position)
                                     (= (get painted-parts position) white))
                              white
                              black)
          program (-> program
                      (assoc :inputs [color-at-position])
                      (run))]
      (if (:halted program)
        painted-parts
        (let [[new-color turn] (:outputs program)
              paint-job (when (not= color-at-position new-color)
                          new-color)
              direction (get-new-direction direction turn)]
          (recur (assoc program :outputs [])
                 (if-not paint-job
                   painted-parts
                   (assoc painted-parts position paint-job))
                 (map + position direction)
                 direction))))))

(deftest puzzle-a
  (is= (time (-> (get-puzzle-input)
                 (paint-all black)
                 (keys)
                 (count)))
       ; "Elapsed time: 346.623486 msecs"
       1771))


(deftest puzzle-b
  (is= (time (let [painting (as-> (get-puzzle-input) $
                                  (paint-all $ white))]
               (->> (range 6)
                    (map (fn [y]
                           (->> (range 1 40)
                                (map (fn [x]
                                       (if (= 1 (get painting [x y]))
                                         "#"
                                         " ")))
                                (clojure.string/join ""))))
                    (clojure.string/join "\n"))))
       (str "#  #  ##  #### #  #   ## #  # #  # ####\n"
            "#  # #  # #    #  #    # #  # #  #    #\n"
            "#### #    ###  ####    # #### #  #   # \n"
            "#  # # ## #    #  #    # #  # #  #  #  \n"
            "#  # #  # #    #  # #  # #  # #  # #   \n"
            "#  #  ### #### #  #  ##  #  #  ##  ####")))