(ns advent-of-code.dec-2016.day-04
  (:require [ysera.test :refer [deftest is is-not is=]]
            [clojure.string :as string]))

; --- Day 4: Security Through Obscurity ---
;
; Finally, you come across an information kiosk with a list of rooms. Of course, the list is encrypted and full of decoy
; data, but the instructions to decode the list are barely hidden nearby. Better remove the decoy data first.
;
; Each room consists of an encrypted name (lowercase letters separated by dashes) followed by a dash, a sector ID, and a
; checksum in square brackets.
;
; A room is real (not a decoy) if the checksum is the five most common letters in the encrypted name, in order, with
; ties broken by alphabetization. For example:
;
; aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a (5), b (3), and then a tie between x,
; y, and z, which are listed alphabetically.
; a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied (1 of each), the first five are
; listed alphabetically.
; not-a-real-room-404[oarel] is a real room.
; totally-real-room-200[decoy] is not.
; Of the real rooms from the list above, the sum of their sector IDs is 1514.
;
; What is the sum of the sector IDs of the real rooms?

(defn get-letters-sector-id-and-checksum
  {:test (fn []
           (is= (get-letters-sector-id-and-checksum "aaaaa-bbb-z-y-x-123[abxyz]")
                ["aaaaa-bbb-z-y-x" "123" "abxyz"])
           (is= (get-letters-sector-id-and-checksum "a-b-c-d-e-f-g-h-987[abcde]")
                ["a-b-c-d-e-f-g-h" "987" "abcde"])
           (is= (get-letters-sector-id-and-checksum "not-a-real-room-404[oarel]")
                ["not-a-real-room" "404" "oarel"])
           (is= (get-letters-sector-id-and-checksum "totally-real-room-200[decoy]")
                ["totally-real-room" "200" "decoy"])
           (is= (get-letters-sector-id-and-checksum "qzmt-zixmtkozy-ivhz-343")
                ["qzmt-zixmtkozy-ivhz" "343"]))}
  [code]
  (->> (drop 1 (re-find #"([\w|-]+)-(\d+)\[?(\w+)?\]?" code))
       (remove nil?)))


(defn create-checksum
  {:test (fn []
           (is= (create-checksum {\a 4 \b 2 \c 1 \d 2 \x 1 \y 1})
                "abdcx"))}
  [frequencies-map]
  (->> (vec frequencies-map)
       (sort-by (juxt (fn [[_ number]] (- number))
                      (fn [[letter _]] letter)))
       (map first)
       (take 5)
       (string/join)))


(defn a-real-room?
  {:test (fn []
           (is (a-real-room? "aaaaa-bbb-z-y-x-123[abxyz]"))
           (is (a-real-room? "a-b-c-d-e-f-g-h-987[abcde]"))
           (is (a-real-room? "not-a-real-room-404[oarel]"))
           (is-not (a-real-room? "totally-real-room-200[decoy]")))}
  [code]
  (let [[letters _ checksum] (get-letters-sector-id-and-checksum code)
        frequencies-map (frequencies (string/replace letters "-" ""))]
    (= (create-checksum frequencies-map) checksum)))


(defn get-sector-id
  {:test (fn []
           (is= (get-sector-id "aaaaa-bbb-z-y-x-123[abxyz]") 123)
           (is= (get-sector-id "a-b-c-d-e-f-g-h-987[abcde]") 987)
           (is= (get-sector-id "not-a-real-room-404[oarel]") 404)
           (is-not (get-sector-id "totally-real-room-200[decoy]")))}
  [code]
  (when (a-real-room? code)
    (let [[_ sector-id _] (get-letters-sector-id-and-checksum code)]
      (read-string sector-id))))


(defn sum-sector-ids
  {:test (fn []
           (is= (sum-sector-ids ["aaaaa-bbb-z-y-x-123[abxyz]"
                                 "a-b-c-d-e-f-g-h-987[abcde]"
                                 "not-a-real-room-404[oarel]"
                                 "totally-real-room-200[decoy]"])
                1514))}
  [input]
  (->> input
       (map get-sector-id)
       (remove nil?)
       (apply +)))


(def input-data
  (-> (slurp "src/advent_of_code/dec_2016/day_04_input.txt")
      (string/split #"\n")))


(deftest puzzle-a
         (is= (sum-sector-ids input-data)
              173787))


; --- Part Two ---
;
; With all the decoy data out of the way, it's time to decrypt this list and get moving.
;
; The room names are encrypted by a state-of-the-art shift cipher, which is nearly unbreakable without the right
; software. However, the information kiosk designers at Easter Bunny HQ were not expecting to deal with a master
; cryptographer like yourself.
;
; To decrypt a room name, rotate each letter forward through the alphabet a number of times equal to the room's sector
; ID. A becomes B, B becomes C, Z becomes A, and so on. Dashes become spaces.
;
; For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.
;
; What is the sector ID of the room where North Pole objects are stored?


(defn rotate-letter
  {:test (fn []
           (is= (rotate-letter \a 1) \b)
           (is= (rotate-letter \a 2) \c)
           (is= (rotate-letter \a 26) \a)
           (is= (rotate-letter \a 105) \b)
           (is= (rotate-letter \z 1) \a))}
  [letter number]
  (let [mod-number (inc (- (int \z) (int \a)))
        equivalent-number (mod number mod-number)
        translated-number-before-rotation (- (int letter) (int \a))
        translated-number-after-rotation (mod (+ translated-number-before-rotation equivalent-number) mod-number)]
    (-> (+ translated-number-after-rotation (int \a))
        (char))))


(defn reveal-name
  {:test (fn []
           (is= (reveal-name "qzmt-zixmtkozy-ivhz-343")
                "very encrypted name")
           (is= (reveal-name "lmprfnmjc-mzhcar-qrmpyec-548[mcrpa]")
                "northpole object storage")
           (is= (reveal-name "qzmt-zixmtkozy-ivhz-343[zimtk]")
                "very encrypted name"))}
  [code]
  (let [[coded-name sector-id _] (get-letters-sector-id-and-checksum code)
        name (->> coded-name
                  (map (fn [letter]
                         (if (= letter \-)
                           \space
                           (rotate-letter letter (read-string sector-id)))))
                  (string/join ""))]
    name))


(deftest puzzle-b
         (is= (->> input-data
                   (filter (fn [code]
                             (and (a-real-room? code)
                                  (string/includes? (reveal-name code) "north")
                                  (string/includes? (reveal-name code) "pole"))))
                   (first)
                   (get-letters-sector-id-and-checksum)
                   (second))
              "548"))



















