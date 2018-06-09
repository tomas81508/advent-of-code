(ns advent-of-code.dec-2016.day-07
  (:require [ysera.test :refer [deftest is is-not is=]]
            [ysera.collections :refer [seq-contains?]]
            [clojure.string :refer [split]]))

; --- Day 7: Internet Protocol Version 7 ---
;
; While snooping around the local network of EBHQ, you compile a list of IP addresses (they're IPv7, of course; IPv6 is
; much too limited). You'd like to figure out which IPs support TLS (transport-layer snooping).
;
; An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA. An ABBA is any four-character sequence
; which consists of a pair of two different characters followed by the reverse of that pair, such as xyyx or abba.
; However, the IP also must not have an ABBA within any hypernet sequences, which are contained by square brackets.
;
; For example:
;
; abba[mnop]qrst supports TLS (abba outside square brackets).
; abcd[bddb]xyyx does not support TLS (bddb is within square brackets, even though xyyx is outside square brackets).
; aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior characters must be different).
; ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even though it's within a larger string).
;
; How many IPs in your puzzle input support TLS?

(defn ABBA?
  {:test (fn []
           (is (ABBA? "abba"))
           (is (ABBA? "ioxxoj"))
           (is (ABBA? "sabba"))
           (is-not (ABBA? "ab"))
           (is-not (ABBA? "aaaa"))
           (is-not (ABBA? ""))
           (is-not (ABBA? "abb")))}
  [text]
  (reduce (fn [a v]
            (let [part-text (subs text v (+ v 4))]
              (or a
                  (and (= (first part-text) (nth part-text 3))
                       (= (second part-text) (nth part-text 2))
                       (not= (first part-text) (second part-text))))))
          false
          (range (inc (- (count text) 4)))))

(defn support-TLS?
  {:test (fn []
           (is (support-TLS? "abba[mnop]qrst"))
           (is (support-TLS? "abcd[bddc]xyyx"))
           (is (support-TLS? "ab[acs]cv[scsc]abba"))
           (is (support-TLS? "ioxxoj[asdfgh]zxcvbn"))
           (is-not (support-TLS? "abcd[bddb]xyyx"))
           (is-not (support-TLS? "aaaa[qwer]tyui")))}
  [IP-address]
  (let [fragments (split IP-address #"\[|\]")
        outers (->> fragments
                    (map-indexed (fn [index text]
                                   (when (even? index) text)))
                    (remove nil?))
        inners (->> fragments
                    (map-indexed (fn [index text]
                                   (when (odd? index) text)))
                    (remove nil?))]
    (and (reduce (fn [a outer]
                   (or a (ABBA? outer)))
                 false
                 outers)
         (reduce (fn [a inner]
                   (and a (not (ABBA? inner))))
                 true
                 inners))))


(deftest puzzle-a
         (is= (as-> (slurp "src/advent_of_code/dec_2016/day_07_input.txt") $
                    (clojure.string/split $ #"\n")
                    (filter support-TLS? $)
                    (count $))
              110))

; --- Part Two ---
;
; You would also like to know which IPs support SSL (super-secret listening).
;
; An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in the supernet sequences (outside any
; square bracketed sections), and a corresponding Byte Allocation Block, or BAB, anywhere in the hypernet sequences. An
; ABA is any three-character sequence which consists of the same character twice with a different character between
; them, such as xyx or aba. A corresponding BAB is the same characters but in reversed positions: yxy and bab,
; respectively.
;
; For example:
;
; aba[bab]xyz supports SSL (aba outside square brackets with corresponding bab within square brackets).
; xyx[xyx]xyx does not support SSL (xyx, but no corresponding yxy).
; aaa[kek]eke supports SSL (eke in supernet with corresponding kek in hypernet; the aaa sequence is not related, because
; the interior character must be different).
; zazbz[bzb]cdb supports SSL (zaz has no corresponding aza, but zbz has a corresponding bzb, even though zaz and zbz
; overlap).
; How many IPs in your puzzle input support SSL?

(defn ABA->BAB
  {:test (fn []
           (is= (ABA->BAB "xyx") "yxy"))}
  [ABA]
  (let [[A B _] ABA]
    (str B A B)))


(defn support-SSL?
  {:test (fn []
           (is (support-SSL? "aba[bab]xyz"))
           (is (support-SSL? "aaa[kek]eke"))
           (is (support-SSL? "zazbz[bzb]cdb"))
           (is-not (support-SSL? "xyx[xyx]xyx")))}
  [IP-address]
  (let [fragments (split IP-address #"\[|\]")
        ABAs (->> fragments
                  (map-indexed (fn [index text]
                                 (when (even? index) text)))
                  (remove nil?)
                  (reduce (fn [ABAs outer]
                            (concat ABAs
                                    (reduce (fn [ABAs index]
                                              (if (and (= (nth outer index) (nth outer (+ 2 index)))
                                                       (not= (nth outer index) (nth outer (inc index))))
                                                (conj ABAs (subs outer index (+ 3 index)))
                                                ABAs))
                                            []
                                            (range (inc (- (count outer) 3))))))
                          [])
                  (flatten))
        BABs (->> fragments
                  (map-indexed (fn [index text]
                                 (when (odd? index) text)))
                  (remove nil?)
                  (reduce (fn [BABs outer]
                            (concat BABs
                                    (reduce (fn [BABs index]
                                              (if (and (= (nth outer index) (nth outer (+ 2 index)))
                                                       (not= (nth outer index) (nth outer (inc index))))
                                                (conj BABs (subs outer index (+ 3 index)))
                                                BABs))
                                            []
                                            (range (inc (- (count outer) 3))))))
                          [])
                  (flatten))]
    (reduce (fn [a ABA]
              (or a (let [BAB (ABA->BAB ABA)]
                      (ysera.collections/seq-contains? BABs BAB))))
            false
            ABAs)))

(deftest puzzle-b
         (is= (as-> (slurp "src/advent_of_code/dec_2016/day_07_input.txt") $
                    (clojure.string/split $ #"\n")
                    (filter support-SSL? $)
                    (count $))
              242))