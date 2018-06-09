(ns sorrow.sethi
  ; https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data
  (:require [clojure.string :as str]))

(def possible-primes
  "A list of integers starting with 2, 3 and followed by all integers of the form 6k±1.
   The list includes all primes, and may be used for a quick primality check."
  (concat [2 3]
    (->> (iterate #(+ 6 %) 5)
      (mapcat #(vector % (+ 2 %))))))

(defn prime?
  "Predicate that returns true if n is prime, false otherwise."
  [n]
  {:pre [(pos-int? n)]}
  (let [chk (take-while #(<= % (Math/sqrt n)) possible-primes)]
    (and
      (not= 1 n)
      (every? #(not= 0 (mod n %)) chk))))
