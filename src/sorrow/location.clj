(ns sorrow.location
  (:require [clojure.set :as s]
            [sorrow.numeric :as n]))

(defmulti error-locator
  "Returns a function that accepts a pair of checksums for a word with errors
  and returns two integers indicating the position of the error and its
  magnitude (zero for a transcription error)."
  :method)

(defmethod error-locator 1
  [{:keys [p n a b]}]
  (let [inv (n/inverses-mod-p p)]
    (fn [s1 s2]
      (let [ep1 (dec (mod (- (* s2 (inv s1)) b) p))
            ep2 (dec (mod (/ (- ep1 a) 2) p))
            e (if (<= 0 ep1 (dec n))
                (mod (* s1 (inv (mod (+ ep1 a) p))) p)
                0)]
        [ep1 ep2 e]))))

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
            e (if (<= 0 ep1 (dec n))
                (mod (* s1 (inv (pwrs (mod (* a ep1) p)))) p)
                0)]
        [ep1 ep2 e]))))
