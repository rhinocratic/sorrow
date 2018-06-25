(ns sorrow.srk.decode
  (:require [sorrow.srk.numeric :as n]
            [sorrow.srk.weights.method1 :as w]))

(defn error-position-finder
  "Given an alphabet size p, encoded word length n and a map of inverses modulo p,
   returns a function that accepts a pair of checksums for a word with errors and
   returns the error position along with a category of :transcription, :transposition
   or :uncorrectable depending upon the type of error."
  [p n inv]
  (let [[wp1 wp2] (w/weight-parameters p n)]
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
     (let [checksums (mapv #(n/weighted-sum p nums %) [w w'])
           num-zeros (count (filter zero? checksums))
           category (condp = num-zeros
                      2 :correct
                      1 :uncorrectable
                      0 :correctable)]

       (conj checksums category))))


; (defn decoder
;   "Returns a function that decodes words of encoded length n+2, returning words of
;    length n and correcting single character transcription errors or transpositions
;    of adjacent characters."
;   [a n]
;   (let [p (count a)
;         [wp1 wp1] (w/weight-parameters p n)
;         [w w'] (w/weight-sequences p n)]
;     (fn [word]
;       (-> word
;         ((chars->integers))
;         ((decode))
;         ((integers->chars))))))
