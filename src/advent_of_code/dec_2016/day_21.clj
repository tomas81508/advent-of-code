(ns advent-of-code.dec-2016.day-21
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.test :refer [deftest]]
            [clojure.string :as string]))

(def input (->> (slurp "src/advent_of_code/dec_2016/day_21_input.txt")
                (string/split-lines)))

(defn swap-position-x-with-position-y
  "swap position X with position Y means that the letters at indexes X and Y (counting from 0) should be swapped."
  {:test (fn []
           (is= (swap-position-x-with-position-y [\a \b \c \d \e] 4 0)
                [\e \b \c \d \a]))}
  [state x y]
  (assoc state x (state y)
               y (state x)))

(defn swap-letter-x-with-letter-y
  "swap letter X with letter Y means that the letters X and Y should be swapped (regardless of where they appear in the string)."
  {:test (fn []
           (is= (swap-letter-x-with-letter-y [\e \b \c \d \a] \b \d)
                [\e \d \c \b \a]))}
  [state x y]
  (->> state
       (map (fn [c] (cond (= c x) y
                          (= c y) x
                          :else c)))
       (into [])))

(defn rotate-left-x-steps
  "rotate left/right X steps means that the whole string should be rotated; for example, one right rotation would turn abcd into dabc."
  {:test (fn []
           (is= (rotate-left-x-steps [\a \b \c \d \e] 2)
                [\c \d \e \a \b]))}
  [state x]
  (let [rotation (rem x (count state))]
    (into [] (concat (drop rotation state) (take rotation state)))))

(defn rotate-right-x-steps
  "rotate left/right X steps means that the whole string should be rotated; for example, one right rotation would turn abcd into dabc."
  {:test (fn []
           (is= (rotate-right-x-steps [\a \b \c \d \e] 2)
                [\d \e \a \b \c]))}
  [state x]
  (let [rotation (rem x (count state))]
    (into [] (concat (take-last rotation state) (drop-last rotation state)))))

(defn rotate-based-on-position
  "rotate based on the position of letter X means that the whole string should be rotated to the right based on the index of letter X (counting from 0) as determined before this instruction does any rotations. Once the index is determined, rotate the string to the right one time, plus a number of times equal to that index, plus one additional time if the index was at least 4."
  {:test (fn []
           (is= (rotate-based-on-position [\e \c \a \b \d] \d)
                [\d \e \c \a \b])
           (is= (rotate-based-on-position [\e \c \a \b \d] \a)
                [\a \b \d \e \c])
           (is= (rotate-based-on-position [\b \d \e \c \a] \a)
                [\a \b \d \e \c]))}
  [state c]
  (let [index (->> state
                   (keep-indexed (fn [index item] (when (= item c) index)))
                   (first))
        x (+ 1 index (if (>= index 4) 1 0))]
    (rotate-right-x-steps state x)))

(defn reverse-positions-x-through-y
  "reverse positions X through Y means that the span of letters at indexes X through Y (including the letters at X and Y) should be reversed in order."
  {:test (fn []
           (is= (reverse-positions-x-through-y [\a \b \c \d \e] 1 3)
                [\a \d \c \b \e]))}
  [state x y]
  (let [[before r] (split-at x state)
        [middle last] (split-at (inc (- y x)) r)]
    (into [] (concat before (reverse middle) last))))

(defn move-position-x-to-position-y
  "move position X to position Y means that the letter which is at index X should be removed from the string, then inserted such that it ends up at index Y."
  {:test (fn []
           (is= (move-position-x-to-position-y [\b \c \d \e \a] 1 4)
                [\b \d \e \a \c])
           (is= (move-position-x-to-position-y [\b \d \e \a \c] 3 0)
                [\a \b \d \e \c]))}
  [state x y]
  (let [[before r] (split-at x state)
        element-at-x (first r)
        after (rest r)]
    (into [] (if (< y x)
               (let [[before-y after-y] (split-at y before)]
                 (concat before-y
                         [element-at-x]
                         after-y
                         after))
               (let [[before-y after-y] (split-at (- y x) after)]
                 (concat before
                         before-y
                         [element-at-x]
                         after-y))))))

(comment
  (->> input
       (reduce (fn [state row]
                 (let [digits (->> (re-seq #"\d" row)
                                   (map read-string))]
                   (cond (string/starts-with? row "rotate right")
                         (apply rotate-right-x-steps state digits)

                         (string/starts-with? row "rotate left")
                         (apply rotate-left-x-steps state digits)

                         (string/starts-with? row "swap position")
                         (apply swap-position-x-with-position-y state digits)

                         (string/starts-with? row "reverse positions")
                         (apply reverse-positions-x-through-y state digits)

                         (string/starts-with? row "move position")
                         (apply move-position-x-to-position-y state digits)

                         (string/starts-with? row "rotate based")
                         (apply rotate-based-on-position
                                state
                                (->> (drop 1 (re-find #"rotate based on position of letter (\w)" row))
                                     (map (fn [l] (first (seq l))))))

                         (string/starts-with? row "swap letter")
                         (apply swap-letter-x-with-letter-y
                                state
                                (->> (drop 1 (re-find #"swap letter (\w) with letter (\w)" row))
                                     (map (fn [l] (first (seq l))))))
                         )))
               [\a \b \c \d \e \f \g \h])
       (string/join))
  )

; part 2

(def scrambled-password [\f \b \g \d \c \e \a \h])

(defn inverse-rotate-based-on-position
  "rotate based on the position of letter X means that the whole string should be rotated to the right based on the index of letter X (counting from 0) as determined before this instruction does any rotations. Once the index is determined, rotate the string to the right one time, plus a number of times equal to that index, plus one additional time if the index was at least 4."
  {:test (fn []
           (is= (inverse-rotate-based-on-position [\d \e \c \a \b] \d)
                [\e \c \a \b \d])
           (is= (inverse-rotate-based-on-position [\a \b \d \e \c] \a)
                [\b \d \e \c \a]))}
  [state c]
  (let [alternatives (->> (iterate (fn [coll] (take (count state) (drop 1 (cycle coll))))
                                   state)
                          (take (count state)))]
    (->> alternatives
         (some (fn [alt]
                 (when (= (rotate-based-on-position alt c) state)
                   alt)))
         (into []))))

(deftest invariants
  (let [state [\a \b \c \d \e \f \g \h]]
    (is= (-> state
             (rotate-right-x-steps 3)
             (rotate-left-x-steps 3))
         state)
    (is= (-> state
             (swap-position-x-with-position-y 2 5)
             (swap-position-x-with-position-y 2 5))
         state)
    (is= (-> state
             (reverse-positions-x-through-y 2 5)
             (reverse-positions-x-through-y 2 5))
         state)
    (is= (-> state
             (move-position-x-to-position-y 2 5)
             (move-position-x-to-position-y 5 2))
         state)
    (is= (-> state
             (swap-letter-x-with-letter-y \c \f)
             (swap-letter-x-with-letter-y \c \f))
         state)
    (is= (-> state
             (rotate-based-on-position \d)
             (inverse-rotate-based-on-position \d))
         state)))

(comment
  (->> input
       (reverse)
       (reduce (fn [state row]
                 (let [digits (->> (re-seq #"\d" row)
                                   (map read-string))]
                   (println row state digits)
                   (cond (string/starts-with? row "rotate right")
                         (apply rotate-left-x-steps state digits)

                         (string/starts-with? row "rotate left")
                         (apply rotate-right-x-steps state digits)

                         (string/starts-with? row "swap position")
                         (apply swap-position-x-with-position-y state digits)

                         (string/starts-with? row "reverse positions")
                         (apply reverse-positions-x-through-y state digits)

                         (string/starts-with? row "move position")
                         (apply move-position-x-to-position-y state (reverse digits))

                         (string/starts-with? row "rotate based")
                         (apply inverse-rotate-based-on-position
                                state
                                (->> (drop 1 (re-find #"rotate based on position of letter (\w)" row))
                                     (map (fn [l] (first (seq l))))))

                         (string/starts-with? row "swap letter")
                         (apply swap-letter-x-with-letter-y
                                state
                                (->> (drop 1 (re-find #"swap letter (\w) with letter (\w)" row))
                                     (map (fn [l] (first (seq l))))))
                         )))
               scrambled-password)
       (string/join))
  ; not efacbdhg
  )

