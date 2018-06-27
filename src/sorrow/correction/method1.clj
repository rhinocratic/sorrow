(ns sorrow.correction.method1
  (:require [sorrow.numeric :as n]))

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
    (<= 0 ep1 (dec n))       :transcription
    (and (int? ep2)
         (<= 0 ep2 (- n 2))) :transposition
    :else                    :uncorrectable))

(defn error-corrector
  "Returns an error corrector function for the given weight scheme, accepting
   integer vectors of length n and returning a map containing:
   - :corrected - the corrected word (omitted if uncorrectable)
   - :uncorrected - the uncorrected word
   - :error-type - :transcription, :transposition or :uncorrectable
   - :error-position - the position at which the error occurs (omitted if uncorrectable)"
  [ws]
  (let [pos (error-position-finder ws)]
    (fn [])))
