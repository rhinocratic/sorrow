(ns sorrow.numeric)

(def ^{:private true} possible-primes
  "A list of integers starting with 2, 3 and followed by all integers of the form 6k±1.
   The list includes all primes, and may be used for a quick primality check."
  (concat [2 3]
    (->> (iterate #(+ 6 %) 5)
      (mapcat #(vector % (+ 2 %))))))

(defn prime?
  "Predicate that returns true if n is prime, false otherwise."
  [n]
  (let [chk (take-while #(<= % (Math/sqrt n)) possible-primes)]
    (and
      (not= 1 n)
      (every? #(not= 0 (mod n %)) chk))))

(defn gcd
  "Calculates the greatest common divisor gcd(m, n), and the Bezout coefficients
   x, y such that xm + yn = gcd(m, n), using the extended Euclidean algorithm.
   Returns a vector [x y gcd(m, n)]"
  [m n]
  (loop [x [0 1]
         y [1 0]
         r [n m]]
    (if (zero? (fnext r))
      (let [sgn (if (pos? (first r)) 1 -1)]
        (vec (map #(* sgn (first %)) [x y r])))
      (let [nxt (fn [[a b]] [b (- a (* (quot (first r) (last r)) b))])
            [x' y' r'] (map nxt [x y r])]
        (recur x' y' r')))))

(defn lcm
  "Calculate the least common multiple of m and n"
  [m n]
  (/ (* m n) (last (gcd m n))))

(defn mod-inverse
  "Find the multiplicative inverse of n mod p"
  [p n]
  (-> (gcd n p)
    (first)
    (mod p)))

(defn inverses-mod-p
  "Returns a map of inverses mod p of the integers 1, ..., p-1"
  [p]
  (into {} (map #(vector % (mod-inverse p %)) (range 1 p))))

(defn powers-of-n-mod-p
  "For p (prime), returns a map of the p-1 distinct powers of n indexed by their exponent.
   The values of the map will contain all of the non-zero elements of the finite field GF(p)."
  [p n]
  (->> (iterate #(mod (* % n) p) n)
    (take (dec p))
    (map-indexed #(vector (inc %1) %2))
    (into {})))

(defn weighted-sum
  "Given a sequence of integers is and a sequence of weights ws,
   return the weighted sum of the integers modulo p."
  [p is ws]
  (let [dot-product (apply + (map * is ws))]
    (mod dot-product p)))

(defn scalev
  "Multiply a vector v by the given scale s and reduce the entries modulo p"
  [p s v]
  (map #(mod (* s %) p) v))

(defn minusv
  "Subract vector v2 from v1, reducing the entries modulo p"
  [p v1 v2]
  (mapv #(mod (- %1 %2) p) v1 v2))

(defn eliminate-ith-term
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

(defn linear-congruence-solver
  "Returns a function that accepts parameters r, s and finds a solution x for
   the linear congruence r.x + s ≡ 0 (mod p)"
  [p]
  (let [inverses (inverses-mod-p p)]
    (fn [[r s]]
      (let [t (mod (- p s) p)]
        (mod (* t (inverses r)) p)))))

(defn simultaneous-congruence-solver
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
