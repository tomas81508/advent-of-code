(ns advent-of-code.dec-2020.day-04
  (:require [ysera.test :refer [is is-not is= deftest]]
            [clojure.spec.alpha :as spec]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2020/day_04.txt")
       (clojure.string/split-lines)))

(def test-input ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
                 "byr:1937 iyr:2017 cid:147 hgt:183cm"
                 ""
                 "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
                 "hcl:#cfa07d byr:1929"
                 ""
                 "hcl:#ae17e1 iyr:2013"
                 "eyr:2024"
                 "ecl:brn pid:760753108 byr:1931"
                 "hgt:179cm"
                 ""
                 "hcl:#cfa07d eyr:2025 pid:166559648"
                 "iyr:2011 ecl:brn hgt:59in"])

(defn create-map
  {:test (fn []
           (is= (create-map "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929")
                {"iyr" "2013"
                 "ecl" "amb"
                 "cid" "350"
                 "eyr" "2023"
                 "pid" "028048884"
                 "hcl" "#cfa07d"
                 "byr" "1929"}))}
  [string]
  (->> (clojure.string/split string #" ")
       (map (fn [word] (clojure.string/split word #":")))
       (into {})))

(defn format-input-data
  {:test (fn []
           (is= (format-input-data test-input)
                [{:ecl "gry" :pid "860033327" :eyr "2020" :hcl "#fffffd" :byr "1937" :iyr "2017" :cid "147" :hgt "183cm"}
                 {:iyr "2013" :ecl "amb" :cid "350" :eyr "2023" :pid "028048884" :hcl "#cfa07d" :byr "1929"}
                 {:hcl "#ae17e1" :iyr "2013" :eyr "2024" :ecl "brn" :pid "760753108" :byr "1931" :hgt "179cm"}
                 {:hcl "#cfa07d" :eyr "2025" :pid "166559648" :iyr "2011" :ecl "brn" :hgt "59in"}]))}
  [input-data]
  (as-> input-data $
        (reduce (fn [a l]
                  (cond (= l "")
                        (-> a
                            (update :passport-data conj (:current a))
                            (assoc :current ""))

                        (= (:current a) "")
                        (assoc a :current l)

                        :else
                        (update a :current str " " l)))
                {:current       ""
                 :passport-data []}
                $)
        (update $ :passport-data conj (:current $))
        (:passport-data $)
        (map create-map $)
        (clojure.walk/keywordize-keys $)))

(defn valid-passport?
  {:test (fn []
           (is (valid-passport? {:ecl "gry" :pid "860033327" :eyr "2020" :hcl "#fffffd" :byr "1937" :iyr "2017" :cid "147" :hgt "183cm"}))
           (is-not (valid-passport? {:iyr "2013" :ecl "amb" :cid "350" :eyr "2023" :pid "028048884" :hcl "#cfa07d" :byr "1929"}))
           (is (valid-passport? {:hcl "#ae17e1" :iyr "2013" :eyr "2024" :ecl "brn" :pid "760753108" :byr "1931" :hgt "179cm"}))
           (is-not (valid-passport? {:hcl "#cfa07d" :eyr "2025" :pid "166559648" :iyr "2011" :ecl "brn" :hgt "59in"})))}
  [passport]
  (>= (count (select-keys passport [:ecl :pid :eyr :hcl :byr :iyr :hgt])) 7))

(deftest puzzle-a
         (is= (->> (get-puzzle-input)
                   (format-input-data)
                   (map valid-passport?)
                   (filter true?)
                   (count))
              245))

(defn four-digits?
  [x]
  (re-matches (re-pattern "[\\d]{4}") x))

;byr (Birth Year) - four digits; at least 1920 and at most 2002.
(spec/def ::byr (spec/and four-digits?
                          (fn [n] (<= 1920 (read-string n) 2002))))

(deftest byr
         (is (spec/valid? ::byr "2002"))
         (is-not (spec/valid? ::byr "2003")))

;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
(spec/def ::iyr (spec/and four-digits?
                          (fn [n] (<= 2010 (read-string n) 2020))))

;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
(spec/def ::eyr (spec/and four-digits?
                          (fn [n] (<= 2020 (read-string n) 2030))))

;hgt (Height) - a number followed by either cm or in:
;If cm, the number must be at least 150 and at most 193.
;If in, the number must be at least 59 and at most 76.
(spec/def ::hgt (fn [x]
                  (when-let [[_ n-as-string unit] (re-matches (re-pattern "([\\d]+)(cm|in)") x)]
                    (let [n (read-string n-as-string)]
                      (if (= unit "cm")
                        (<= 150 n 193)
                        (<= 59 n 76))))))

(deftest hgt
         (is (spec/valid? ::hgt "60in"))
         (is (spec/valid? ::hgt "190cm"))
         (is-not (spec/valid? ::hgt "190in"))
         (is-not (spec/valid? ::hgt "190")))

;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
(spec/def ::hcl (fn [x] (re-matches (re-pattern "#[0-9|a-f]{6}") x)))

(deftest hcl
         (is (spec/valid? ::hcl "#123abc"))
         (is-not (spec/valid? ::hcl "#123abz"))
         (is-not (spec/valid? ::hcl "123abc")))

;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
(spec/def ::ecl (fn [x] (re-matches (re-pattern "(amb|blu|brn|gry|grn|hzl|oth)") x)))

(deftest ecl
         (is (spec/valid? ::ecl "brn"))
         (is-not (spec/valid? ::ecl "wat")))

;pid (Passport ID) - a nine-digit number, including leading zeroes.
(spec/def ::pid (fn [x] (re-matches (re-pattern "[\\d]{9}") x)))

(deftest pid
         (is (spec/valid? ::pid "000000001"))
         (is-not (spec/valid? ::pid "0123456789")))

;cid (Country ID) - ignored, missing or not.

(spec/def ::passport (spec/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]))

(def invalid-passport-input ["eyr:1972 cid:100"
                             "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
                             ""
                             "iyr:2019"
                             "hcl:#602927 eyr:1967 hgt:170cm"
                             "ecl:grn pid:012533040 byr:1946"
                             ""
                             "hcl:dab227 iyr:2012"
                             "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
                             ""
                             "hgt:59cm ecl:zzz"
                             "eyr:2038 hcl:74454a iyr:2023"
                             "pid:3556412378 byr:2007"])

(def valid-passport-input ["pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
                           "hcl:#623a2f"
                           ""
                           "eyr:2029 ecl:blu cid:129 byr:1989"
                           "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
                           ""
                           "hcl:#888785"
                           "hgt:164cm byr:2001 iyr:2015 cid:88"
                           "pid:545766238 ecl:hzl"
                           "eyr:2022"
                           ""
                           "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"])

(defn really-valid-passport?
  {:test (fn []
           (let [[p1 p2 p3 p4] (->> invalid-passport-input
                                    (format-input-data))]
             (is-not (really-valid-passport? p1))
             (is-not (really-valid-passport? p2))
             (is-not (really-valid-passport? p3))
             (is-not (really-valid-passport? p4)))
           (let [[p1 p2 p3 p4] (->> valid-passport-input
                                    (format-input-data))]
             (is (really-valid-passport? p1))
             (is (really-valid-passport? p2))
             (is (really-valid-passport? p3))
             (is (really-valid-passport? p4))))}
  [x]
  (spec/valid? ::passport x))

(deftest puzzle-b
         (is= (->> (get-puzzle-input)
                   (format-input-data)
                   (map really-valid-passport?)
                   (filter true?)
                   (count))
              133))





