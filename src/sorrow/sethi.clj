(ns sorrow.sethi
  ; https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data
  (:require [clojure.string :as str]))

(def ^{:private true} possible-primes
  "A list of integers starting with 2, 3 and followed by all integers of the form 6k±1.
   The list includes all primes, and may be used for a quick primality check."
  (concat [2 3]
    (->> (iterate #(+ 6 %) 5)
      (mapcat #(vector % (+ 2 %))))))

(defn- prime?
  "Predicate that returns true if n is prime, false otherwise."
  [n]
  {:pre [(pos-int? n)]}
  (let [chk (take-while #(<= % (Math/sqrt n)) possible-primes)]
    (and
      (not= 1 n)
      (every? #(not= 0 (mod n %)) chk))))

(defn gcd
  "Calculates the greatest common divisor gdc(m, n), and the Bezout coefficients
   x, y such that xm + yn = gcd(m, n), using the extended Euclidean algorithm.
   Returns a vector [x y gcd(m, n)]"
  [m n]
  {:pre [(pos-int? m) (pos-int? n)]}
  (loop [x [0 1]
         y [1 0]
         r [n m]]
    (if (zero? (fnext r))
      (vec (map first [x y r]))
      (let [nxt (fn [[a b]] [b (- a (* (quot (first r) (last r)) b))])
            [x' y' r'] (map nxt [x y r])]
        (recur x' y' r')))))

(defn- mod-inverse
  "Find the multiplicative inverse of n mod p, where p is prime"
  [n p]
  {:pre [(prime? p) (pos-int? n)]}
  n)
