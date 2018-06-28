(ns sorrow.location
  (:require [clojure.set :as s]
            [sorrow.numeric :as n]))

(defn- method1-locator
  "Error locator for method 1."
  [{:keys [p n a b]}]
  (let [inv (n/inverses-mod-p p)]
    (fn [s1 s2]
      (let [ep1 (dec (mod (- (* s2 (inv s1)) b) p))
            ep2 (dec (/ (- ep1 a) 2))
            e (if (<= 0 ep1 (dec n))
                (mod (* s1 (inv (mod (+ ep1 a) p))) p)
                0)]
        [ep1 ep2 e]))))

(defn method2-locator
  "Error locator for method 2."
  [{:keys [p n a b]}]
  (let [inv (n/inverses-mod-p p)
        pwrs (n/powers-of-n-mod-p p 2)
        lgs (s/map-invert pwrs)
        r (n/mod-inverse 36 (- b a))
        t (mod (* r (- (lgs (mod (* (mod (dec (pwrs b)) p) (inv (mod (dec (pwrs a)) p))) p)))) (- (dec p)))]
    (fn [s1 s2]
      (let [k (lgs (mod (* s2 (inv s1)) p))
            ep1 (mod (* r k) (dec p))
            ep2 (mod (+ (* r k) t) (dec p))
            e (if (<= 0 ep1 (dec n))
                (mod (* s1 (inv (pwrs (mod (* a ep1) p)))) p)
                0)]
        [ep1 ep2 e]))))

(defn error-locator
  "Returns a function that accepts a pair of checksums for a word with errors
  and returns two integers indicating the position of the error and its
  magnitude (zero for a transcription error)."
  [{:keys [method] :as ws}]
  (if (= method 1)
    (method1-locator ws)
    (method2-locator ws)))
