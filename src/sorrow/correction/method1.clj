(ns sorrow.correction.method1
  (:require [sorrow.numeric :as n]
            [sorrow.weights.method1 :as w]))

(defn error-position-finder
  "Given a weight scheme, returns a function that accepts a pair of checksums
   for a word with errors and returns two integers indicating the position of the error."
  [{:keys [p a b]}]
  (let [inv (n/inverses-mod-p p)]
    (fn [s1 s2]
      (let [ep1 (dec (mod (- (* s2 (inv s1)) b) p))
            ep2 (dec (/ (- ep1 a) 2))]
        [ep1 ep2]))))

(defn classify-error
  "Classifies an error as :transcription, :transposition or :uncorrectable based
   upon the values of error position indicators ep1, ep2."
  [n [ep1 ep2]]
  (cond
    (<= 0 ep1 (dec n)) :transcription
    (and (int? ep2) (<= 0 ep2 (- n 2))) :transposition
    :else :uncorrectable))

(defn error-corrector
  [p n])

(defn error-position-finder
  "Given an alphabet size p, encoded word length n and a map of inverses modulo p,
   returns a function that accepts a pair of checksums for a word with errors and
   returns the error position along with a category of :transcription, :transposition
   or :uncorrectable depending upon the type of error."
  [p n inv]
  (let [{:keys [a b]} (w/weight-scheme p n)]
    (fn [s1 s2]
      (let [ep1 (dec (mod (- (* s2 (inv s1)) b) p))
            ep2 (dec (/ (- ep1 a) 2))]
        (cond
          (<= 0 ep1 (dec n)) {:transcription ep1}
          (and (int? ep2) (<= 0 ep2 (- n 2))) {:transposition ep2}
          :else {:uncorrectable -1})))))
