(ns sorrow.location
  (:require [clojure.set :as s]
            [sorrow.numeric :as n]))

(defn- method1-locator
  "Returns a function that accepts a pair of checksums for a word with errors
  and returns two integers indicating the position of the error and its
  magnitude (zero for a transcription error)."
  [{:keys [p a b]}]
  (let [inv (n/inverses-mod-p p)]
    (fn [s1 s2]
      (let [ep1 (dec (mod (- (* s2 (inv s1)) b) p))
            ep2 (dec (/ (- ep1 a) 2))]
        [ep1 ep2]))))

(defn- method2-locator
  "Returns a function that accepts a pair of checksums for a word with errors
  and returns two integers indicating the position of the error and its
  magnitude (zero for a transcription error)."
  [{:keys [p a b]}]
  (let [inv (n/inverses-mod-p p)
        kmap (s/map-invert (n/powers-of-n-mod-p p 2))]
    (fn [s1 s2]
      (let [k (kmap (mod (* s2 (inv s1)) p))
            ep1 (rem k (dec p))
            ep2 (dec (/ (- ep1 a) 2))]
        [ep1 ep2]))))

(defn error-locator
  "Returns a function that accepts a pair of checksums for a word with errors
  and returns two integers indicating the position of the error and its
  magnitude (zero for a transcription error)."
  [{:keys [method] :as ws}]
  (if (= method 1)
    (method1-locator ws)
    (method2-locator ws)))
