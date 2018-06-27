(ns sorrow.weights.core
  (:require [sorrow.weights.method1 :as wm1]
            [sorrow.weights.method2 :as wm2]))

(defn weight-scheme
  "Choose the method of calculating a weight scheme for encoding/correcting words,
   based upon size of alphabet a and desired (encoded) word length n.  Where possible,
   the simpler method 1 will be preferred, only using method 2 when longer words
   are required."
  [a n]
  (let [p (count a)
        method1-max (int (Math/floor (/ (+ p 2) 3)))]
    (cond
      (<= n method1-max) (wm1/weight-scheme p n)
      :else (wm2/weight-scheme p n))))
