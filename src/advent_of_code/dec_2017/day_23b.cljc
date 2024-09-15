(ns advent-of-code.dec-2017.day-23b
  (:require [ysera.test :refer [is is-not is=]]))

;        b = 109300, c = 126300, d = 0, e = 0, f = 0, g = 0, h = 0;
;
;        do {
;            f = 1;

; ---- BEGIN ----- This part checks if d * e = b for any integers d and e less than b, ie checking if b is a prime
;            d = 2;
;            do {
;                e = 2;
;                do {
;                    if (d * e != b) {
;                        f = 0;
;                    }
;                    e++;
;                } while (e != b);
;                d++;
;            } while (b != d);
; ---- END -----

;            if (f != 0) {
;                h++;
;            }
;            if (b == c) {
;                break;
;            }
;            b = b + 17;
;
;        } while (true);

(defn prime?
  {:test (fn []
           (is (prime? 3))
           (is (prime? 5))
           (is (prime? 7))
           (is-not (prime? 4))
           (is-not (prime? 6))
           (is-not (prime? 8)))}
  [n]
  (->> (range 2 (inc (Math/sqrt n)))
       (some (fn [x] (zero? (rem n x))))
       (not)))

(->> (range 109300 (inc 126300) 17)
     (remove prime?)
     (count))