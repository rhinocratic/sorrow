(ns sorrow.srk.weights.method1
  "Implementation of the first (simpler) method described in the paper for
   generating weight sequences.  This will suffice if the desired length of
   an encoded word is not more than ⌊(p + 2)/3⌋ where p (prime) is the cardinality
   of the alphabet")

(defn- code-length
  "For alphabet size p (prime) and arbitrary b (0 <= b < p), find the length of an encoded
   word (including check characters) for an encoding scheme. This will be maximal when b = 0."
  [p b]
  (int (Math/floor (/ (- (+ p 2) b) 3))))

(defn weight-scheme
  "For a desired encoded word length n and alphabet size p (prime), calculate the
   weight parameters and sequences that will be used in encoding and correcting.
   Returns a map of the weight parameters :a, :b and the weight sequences :w, :w'."
  [p n]
  (let [a (- n 2)
        b (->> (range p)
            (filter #(= n (code-length p %)))
            (first))
        r (range 1 (inc n))
        w (map (partial + a) r)
        w' (map #(mod (* (+ a %) (+ b %)) p) r)]
    {:a a :b b :w w :w' w'}))
