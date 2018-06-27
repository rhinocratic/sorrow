(ns sorrow.weights.method1
  "Implementation of the first (simpler) method for deriving weight sequences
   described in the paper.  This will suffice if the desired length of
   an encoded word is not more than ⌊(p + 2)/3⌋ where p (prime) is the cardinality
   of the alphabet")

(defn weight-scheme
  "For a desired encoded word length n and alphabet size p (prime), calculate the
   weight parameters and sequences that will be used in encoding and correcting.
   Returns a map of the weight parameters :a and :b, the weight sequences :w and :w,'
   the alphabet size and word length :p, :n and the :method (= 1)."
  [p n]
  (let [N (int (Math/floor (/ (+ p 2) 3)))
        a (- N 2)
        r (range 1 (inc n))
        w (map (partial + (- n 2)) r)
        w' (map #(mod (* (+ a %) %) p) r)]
    {:p p :n n :a a :b 0 :w w :w' w' :method 1}))
