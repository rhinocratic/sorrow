(ns sorrow.srk.weights.method1
  "Implementation of the first (simpler) method described in the paper for
   generating weight sequences.  This will suffice if the desired length of
   an encoded word is not more than ⌊(p + 2)/3⌋ where p (prime) is the cardinality
   of the alphabet")

(defn code-length
  "For alphabet size p (prime) and arbitrary b (0 <= b < p), find the length of an encoded
   word (including check characters) for an encoding scheme (Method 1 from the paper).
   This will be a maximum when b = 0."
  [p b]
  (int (Math/floor (/ (- (+ p 2) b) 3))))

(defn weight-parameters
  "For a desired encoded word length n and alphabet size p (prime), calculate the
   two weight parameters a, b that will be used in generating weight sequences for
   encoding and decoding.  Returns a vector [a b]"
  [p n]
  [(- n 2) (first (filter #(= n (code-length p %)) (range p)))])

(defn weight-sequences
  "For a desired encoded word length n and alphabet size p (prime), calculate the
   two sequences of weights that will be used in encoding and decoding.  Returns a
   vector of the weight sequences"
  [p n]
  (let [[a b] (weight-parameters p n)
        r (range 1 (inc n))
        w (partial + a)
        w' (fn [x] (mod (* (+ a x) (+ b x)) p))]
    [(mapv w r) (mapv w' r)]))

; (defn valid-params?
;   "Determine whether or not the cardinality of the alphabet a is prime, and the
;    desired word length n is within the bounds imposed by the alphabet size"
;   [a n]
;   (let [p (count a)]
;     (and (n/prime? p) (<= n (Math/floor (/ (+ 2 p) 3))))))
