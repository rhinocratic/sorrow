(ns sorrow.srk
  "Implements the error-correcting coding scheme for alphanumeric data described by
   A.S. Sethi, V. Rajaraman and P.S. Kenjale in their paper:
   https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data"
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
  "For alphabet size p (prime) and arbitrary b (0 <= b < p), find the length of an encoded
   word (including check characters) for an encoding scheme (Method 1 from the paper).
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
  (mapv #(mod (- %1 %2) p) v1 v2))

(defn- eliminate-ith-term
  "Eliminate the ith term from the simultaneous congruences modulo p represented
   by vectors a and b:
   a0.x0 + a1.x1 + ... + an ≡ 0 (mod p)
   b0.x0 + b1.x1 + ... + bn ≡ 0 (mod p)"
  [p a b i]
  (let [[ai bi] (map #(nth % i) [a b])
        [sa sb] (map #(/ (lcm ai bi) %) [ai bi])
        diff (minusv p (scalev p sa a) (scalev p sb b))]
    (-> []
      (into (take i diff))
      (into (drop (inc i) diff)))))

(defn- linear-congruence-solver
  "Returns a function that accepts parameters r, s and finds a solution x for
   the linear congruence r.x + s ≡ 0 (mod p)"
  [p]
  (let [inverses (into {} (map #(vector % (mod-inverse p %)) (range p)))]
    (fn [[r s]]
      (let [t (mod (- p s) p)]
        (mod (* t (inverses r)) p)))))

(defn- simultaneous-congruence-solver
  "Returns a function that accepts vectors a, b and solves the following simultaneous
   congruences for x and y:
   a0.x + a1.y + a2 ≡ 0 (mod p)
   b0.x + b1.y + b2 ≡ 0 (mod p),
   returning a vector [x y] of the solutions."
  [p]
  (let [solve (linear-congruence-solver p)]
    (fn [a b]
      (let [vs (map #(eliminate-ith-term p a b %) [1 0])]
        (mapv solve vs)))))

(defn- checksum-appender
  "Returns a function that accepts a sequence of integers and appends two check digits
   calculated from the weight sequences w and w'"
  [p w w']
  (let [solve (simultaneous-congruence-solver p)]
    (fn [nums]
      (let [[a b] (map #(conj (vec (take-last 2 %)) (weighted-sum p nums %)) [w w'])
            [x y] (solve a b)]
        (conj nums x y)))))

(defn- chars->integers
  "Returns a function that maps words formed from characters of the alphabet to
   sequences of integers."
  [alphabet]
  (fn [word]
    (let [m (zipmap alphabet (range))]
      (mapv m word))))

(defn- integers->chars
  "Returns a function that maps sequences of integers to words formed from characters
   of the alphabet"
  [alphabet]
  (fn [nums]
    (let [m (zipmap (range) alphabet)]
      (apply str (map m nums)))))

(defn- valid-params?
  "Determine whether or not the cardinality of the alphabet a is prime, and the
   desired word length n is within the bounds imposed by the alphabet size"
  [a n]
  (let [p (count a)]
    (and (prime? p) (<= n (Math/floor (/ (+ 2 p) 3))))))

(defn encoder
  "Returns a function that encodes words of unencoded length n (formed from characters
   of the given alphabet a) by appending 2 checksum characters."
  [a n]
  {:pre [(valid-params? a n)]}
  (let [p (count a)
        [w w'] (weight-sequences p n)]
    (fn [word]
      (-> word
        ((chars->integers a))
        ((checksum-appender p w w'))
        ((integers->chars a))))))

(defn error-position-finder
  "Given an alphabet size p, encoded word length n and a map of inverses modulo p,
   returns a function that accepts a pair of checksums for a word with errors and
   returns the error position along with a category of :transcription, :transposition
   or :uncorrectable depending upon the type of error."
  [p n inv]
  (let [[wp1 wp1] (weight-parameters p n)]
    (fn [s1 s2]
      (let [ep1 (dec (mod (- (* s2 (inv s1)) wp2) p))
            ep2 (dec (/ (- ep1 wp1) 2))]
        (cond
          (<= 0 ep1 (dec n)) {:transcription ep1}
          (and (int? ep2) (<= 0 ep2 (- n 2))) {:transposition ep2}
          :else {:uncorrectable -1})))))

(defn checksum-calculator
  "Returns a function that calculates the checksums modulo p of a given sequence of integers
   using the weight sequences supplied.  The function returns a vector of the checksums
   along with a category of :correctable :uncorrectable or :correct according to the
   number of checksums (0, 1 or 2) that were zero."
   [p w w']
   (fn [nums]
     (let [checksums (mapv #(weight-sum p nums %) [w w'])
           num-zeros (count (filter zero? checksums))
           category (condp = num-zeros
                      2 :correct
                      1 :uncorrectable
                      0 :correctable)]

       (conj checksums category))))

(defn decoder
  "Returns a function that decodes words of encoded length n+2, returning words of
   length n and correcting single character transcription errors or transpositions
   of adjacent characters."
  [a n]
  (let [p (count a)
        [wp1 wp1] (weight-parameters p n)
        [w w'] (weight-sequences p n)]
    (fn [word]
      (-> word
        ((chars->integers))
        ((decode))
        ((integers->chars))))))

(def alphanumeric-upper-case
  "An alphabet containing Digits and upper case letters, plus '*' to make the cardinality prime"
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*")

(def alphanumeric-mixed-case
  "An alphabet containing Digits, upper and lower case letters, minus capital 'O' to make the cardinality prime"
  "0123456789ABCDEFGHIJKLMNPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
