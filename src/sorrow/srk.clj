(ns sorrow.srk
  ; https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

(def ^{:private true} possible-primes
  "A list of integers starting with 2, 3 and followed by all integers of the form 6k±1.
   The list includes all primes, and may be used for a quick primality check."
  (concat [2 3]
    (->> (iterate #(+ 6 %) 5)
      (mapcat #(vector % (+ 2 %))))))

(defn- prime?
  "Predicate that returns true if n is prime, false otherwise."
  [n]
  (let [chk (take-while #(<= % (Math/sqrt n)) possible-primes)]
    (and
      (not= 1 n)
      (every? #(not= 0 (mod n %)) chk))))

(defn- gcd
  "Calculates the greatest common divisor gcd(m, n), and the Bezout coefficients
   x, y such that xm + yn = gcd(m, n), using the extended Euclidean algorithm.
   Returns a vector [x y gcd(m, n)]"
  [m n]
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
  (-> (gcd n p)
    (first)
    (mod p)))

(defn- code-length
  "For alphabet size p (prime) and arbitrary b >= 0, find the length of an encoded
   word (including check characters) for an encoding scheme (Method 1).
   This will be a maximum when b = 0."
  [p b]
  (int (Math/floor (/ (- (+ p 2) b) 3))))

(defn- weight-parameters
  "For a desired encoded word length n and alphabet size p (prime), calculate the
   two weight parameters a, b that will be used in generating weight sequences for
   encoding and decoding.  Returns a vector [a b]"
  [n p]
  [(- n 2) (first (filter #(= n (code-length p %)) (range p)))])

(defn- weight-sequences
  "For a desired encoded word length n and alphabet size p (prime), calculate the
   two sequences of weights that will be used in encoding and decoding.  Returns a
   vector of the weight sequences"
  [n p]
  (let [[a b] (weight-parameters n p)
        r (range 1 (inc n))
        w (partial + a)
        w' (fn [x] (mod (* (+ a x) (+ b x)) p))]
    [(mapv w r) (mapv w' r)]))
