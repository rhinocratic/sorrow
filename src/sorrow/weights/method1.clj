(ns sorrow.weights.method1
  "Implementation of the first (simpler) method for deriving weight sequences
   described in the paper.  This will suffice if the desired length of
   an encoded word is not more than ⌊(p + 2)/3⌋ where p (prime) is the cardinality
   of the alphabet")

(defn weight-scheme
  "For a desired encoded word length n and alphabet a of prime cardinality,
  returns a map containing the weight parameters and sequences that will be used
  in encoding and correction."
  [alpha n]
  (let [p (count alpha)
        N (int (Math/floor (/ (+ p 2) 3)))
        a (- N 2)
        r (range 1 (inc n))
        w (map (partial + a) r)
        w' (map #(mod (* (+ a %) %) p) r)]
    {:p p :n n :a a :b 0 :w w :w' w' :alphabet alpha :method 1}))
