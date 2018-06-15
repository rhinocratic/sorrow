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

(defn lcm
  "Calculate the least common multiple of m and n"
  [m n]
  (/ (* m n) (last (gcd m n))))

(defn- mod-inverse
  "Find the multiplicative inverse of n mod p, where p is prime"
  [p n]
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
  [p n]
  [(- n 2) (first (filter #(= n (code-length p %)) (range p)))])

(defn- weight-sequences
  "For a desired encoded word length n and alphabet size p (prime), calculate the
   two sequences of weights that will be used in encoding and decoding.  Returns a
   vector of the weight sequences"
  [p n]
  (let [[a b] (weight-parameters p n)
        r (range 1 (inc n))
        w (partial + a)
        w' (fn [x] (mod (* (+ a x) (+ b x)) p))]
    [(mapv w r) (mapv w' r)]))

(defn- weighted-sum
  "Given a sequence of integers is and a sequence of weights ws,
   return the weighted sum of the integers modulo p."
  [p is ws]
  (let [dot-product (apply + (map * is ws))]
    (mod dot-product p)))

(defn- scalev
  "Multiply a vector v by the given scale s and reduce the entries modulo p"
  [p s v]
  (map #(mod (* s %) p) v))

(defn- minusv
  "Subract v2 from v1, reducing the entries modulo p"
  [p v1 v2]
  (vec (map #(mod (- %1 %2) p) v1 v2)))

(defn- eliminate-ith-term
  "Eliminate the ith term from the simultaneous congruences modulo p represented by a and b:
  a0.x0 + a1.x1 + ... + an ≡ 0 (mod p)
  b0.x0 + b1.x1 + ... + bn ≡ 0 (mod p)"
  [p a b i]
  (let [lcm-of-ith-term (lcm (nth a i) (nth b i))
        scale (fn [v] (scalev p (/ lcm-of-ith-term (nth v i)) v))
        [sa sb] (map scale [a b])
        difference (minusv p sa sb)]
    (concat
      (subvec difference 0 i)
      (subvec difference (inc i)))))

(defn- solve-linear-congruence
  "Find a solution x of the congruence r.x + s ≡ 0 (mod p)"
  [p r s]
  (let [t (mod (- p s) p)]
    (mod (* t (mod-inverse p r)) p)))
;
; (defn- solve-simultaneous-congruences
;   "Solve the following simultaneous congruences for x and y:
;   a0.x + a1.y + a2 ≡ 0 (mod p)
;   b0.x + b1.y + b2 ≡ 0 (mod p)"
;   [a b p]
;   (let [v1 (eliminate-ith-term a b 0 p)
;         v2 (eliminate-ith-term a b 1 p)
;         x (solve-linear-congruence v2 p)
;         y (solve-linear-congruence v1 p)]
;     [x y]))
