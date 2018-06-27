(ns sorrow.weights.core
  (:require [sorrow.weights.method1 :as wm1]
            [sorrow.weights.method2 :as wm2]
            [sorrow.numeric :as n]))

(defn weight-scheme
  "Choose the method of calculating a weight scheme for encoding/correcting
  words, based upon the cardinality of alphabet alpha and desired (encoded) word
  length n.  Where possible, the simpler method 1 will be preferred, only using
  method 2 when longer words are required."
  [alpha n]
  {:pre [(string? alpha)
         (apply distinct? alpha)
         (n/prime? (count alpha))
         (int? n)
         (<= 5 n (/ (dec (count alpha)) 2))]}
  (let [p (count alpha)
        method1-max (int (Math/floor (/ (+ p 2) 3)))]
    (cond
      (<= n method1-max) (wm1/weight-scheme alpha n)
      :else              (wm2/weight-scheme alpha n))))
