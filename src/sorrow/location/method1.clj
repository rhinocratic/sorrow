(ns sorrow.location.method1
  (:require [sorrow.numeric :as n]))

(defn locator
  "Returns a function that accepts a pair of checksums for a word with errors
  and returns two integers indicating the position of the error and its
  magnitude (zero for a transcription error)."
  [{:keys [p a b]}]
  (let [inv (n/inverses-mod-p p)]
    (fn [s1 s2]
      (let [ep1 (dec (mod (- (* s2 (inv s1)) b) p))
            ep2 (dec (/ (- ep1 a) 2))]
        [ep1 ep2]))))
