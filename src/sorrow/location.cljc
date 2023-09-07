(ns sorrow.location
  "Namespace containing functionality for locating errors in encoded strings"
  (:require [clojure.set :as s]
            [sorrow.numeric :as n]))

(defmulti error-locator
  "Returns a function that accepts a pair of checksums for a word with errors
  and returns a vector containing:
   2 integers indicating the position of the error
   1 integer indicating the magnitude of the error (zero for transposition errors)."
  :method)

(defmethod error-locator 1
  [{:keys [p n a b]}]
  (let [inv (n/inverses-mod-p p)]
    (fn [s1 s2]
      (let [ep1 (mod (- (* s2 (inv s1)) b) p)
            ep2 (/ (- ep1 a 1) 2)
            e (if (<= 1 ep1 n)
                (mod (* s1 (inv (mod (+ ep1 a) (dec p)))) p)
                0)]
        [(dec ep1) (dec ep2) e]))))

(defmethod error-locator 2
  [{:keys [p n a b]}]
  ;; hairy modular arithmetic to find parameters r, t
  (let [inv (n/inverses-mod-p p)
        pwrs (n/powers-of-n-mod-p p 2)
        lgs (s/map-invert pwrs)
        numer (mod (dec (pwrs b)) p)
        denom (mod (dec (pwrs a)) p)
        t' (- (lgs (mod (* numer (inv denom)) p)))
        r (n/mod-inverse 36 (- b a))
        t (mod (* r t') (- (dec p)))]
    (fn [s1 s2]
      (let [k (lgs (mod (* s2 (inv s1)) p))
            ep1 (mod (* r k) (dec p))
            ep2 (mod (+ (* r k) t) (dec p))
            e (if (<= 1 ep1 n)
                (mod (* s1 (inv (pwrs (mod (* a ep1) p)))) p)
                0)]
        [(dec ep1) (dec ep2) e]))))
