(ns sorrow.weights.method2
  "Implementation of the second, more complex scheme for deriving weight sequences.
   This scheme permits words of encoded length up to (p - 1)/2 characters, where
   p (prime) is the cardinality of the alphabet."
  (:require [sorrow.numeric :as n]))

(defn- diff-coprime?
  "Predicate returning true if the greatest common divisor of x and (b - a) is 1."
  [x [a b]]
  (= 1 (last (n/gcd x (- b a)))))

(defn- gcd-lte-2?
  "Predicate returning true if the greatest common divisor of x, y is no greater than 2."
  [x y]
  (<= (last (n/gcd x y)) 2))

(defn- distinct-pairs
  "Find all distinct pairs (when considered as sets) of unequal elements from a
  collection coll of distinct items.  The elements of each pair will retain the
  same order they had in the original collection."
  [coll]
  (loop [c coll res []]
    (if (= 1 (count c))
      res
      (let [pairs (mapv #(vector (first c) %) (rest c))]
        (recur (rest c) (concat res pairs))))))

(defn- solution-predicate
  "For prime p, returns a predicate that accepts a vector of integers [a b] and is true when
  2^a + 2^b ≡ 2 (mod p) or β^a - β^b ≡ 2 (mod p) where β = 2^((p - 3) / 2) with a even, b odd."
  [p]
  (let [alphas (n/powers-of-n-mod-p p 2)
        beta (apply * (repeat (/ (- p 3) 2) 2))
        betas (n/powers-of-n-mod-p p beta)]
    (fn [[a b]]
      (or
        (= 2 (mod (+ (alphas a) (alphas b)) p))
        (let [[a' b'] (if (even? a) [a b] [b a])]
          (= 2 (mod (- (betas a') (betas b')) p)))))))

(defn- candidate-pairs
  "Find distinct pairs of integers a, b satisfying the following criteria:
   - gcd(a, p-1) <= 2
   - gcd(b, p-1) <= 2
   - gcd(b-a, p-1) = 1
   - 2^a + 2^b ≡ 2 (mod p) or β^a - β^b ≡ 2 (mod p) where β = 2^((p - 3) / 2) with a even, b odd."
  [p]
  (let [soln? (solution-predicate p)]
    (->> (range 1 p)
      (filter (partial gcd-lte-2? (dec p)))
      (distinct-pairs)
      (filter (partial diff-coprime? (dec p)))
      (filter soln?))))

(defn- weight-parameters
  "Choose the parameters used to generate weight sequences for an alphabet of size p (prime)"
  [p]
  (first (candidate-pairs p)))

(defn- gen-sequence
  "Generate a weight sequence for words of length n formed from an alphabet of
  cardinality p (prime), containing every mth power of 2 modulo p"
  [p n m]
  (let [alphas (n/powers-of-n-mod-p p 2)]
    (->> (iterate #(mod (+ m %) (dec p)) m)
      (take n)
      (replace {0 (dec p)})
      (map alphas))))

(defn- weight-sequences
  "Calculate weight sequences from the parameters [a b] for words with encoded length n
  formed from an alphabet of cardinality p (prime)."
  [p n [a b]]
  (mapv (partial gen-sequence p n) [a b]))

(defn weight-scheme
  "For a desired encoded word length n and alphabet a of prime cardinality,
  returns a map containing the weight parameters and sequences that will be used
  in encoding and correction."
  [alpha n]
  (let [p (count alpha)
        [a b] (weight-parameters p)
        [w w'] (weight-sequences p n [a b])]
    {:p p :n n :a a :b b :w w :w' w' :alphabet alpha :method 2}))
