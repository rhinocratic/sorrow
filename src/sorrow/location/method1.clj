(ns sorrow.location.method1
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

; (defn classify-error
;   "Classifies an error as :transcription, :transposition or :uncorrectable based
;    upon the values of error position indicators ep1, ep2."
;   [n [ep1 ep2]]
;   (cond
;     (<= 0 ep1 (dec n))       :transcription
;     (and (int? ep2)
;          (<= 0 ep2 (- n 2))) :transposition
;     :else                    :uncorrectable))

(defn locator
  "Returns an error location function for the given weight scheme, accepting
   a pair of checksums and returning a vector of 2 integers describing the
   location of the error."
  [ws]
  (let [pos (error-position-finder ws)]
    (fn [])))
