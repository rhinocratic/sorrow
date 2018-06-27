(ns sorrow.detection.core
   (:require [sorrow.numeric :as n]))

(defn- checksum-calculator
  "Returns a function that accepts a vector of integers of length n and returns
   a vector of two checksums calculated from the given weight sequences w, w'."
  [p {:keys [w w']}]
  (fn [nums]
    (mapv #(n/weighted-sum p nums %) [w w'])))

(defn- classify-checksums
  "Return :correct, :uncorrectable or :correctable depending upon the values of
   checksums s1, s2."
  [[s1 s2]]
  (condp = (count (filter zero? [s1 s2]))
    2 :correct
    1 :uncorrectable
    0 :correctable))
