(ns sorrow.srk.encode
  "Implements the error-correcting coding scheme for alphanumeric data described by
   A.S. Sethi, V. Rajaraman and P.S. Kenjale in their paper:
   https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data"
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [sorrow.srk.numeric :as n]
            [sorrow.srk.weights.method1 :as w]))

(defn chars->integers
  "Returns a function that maps words formed from characters of the alphabet to
   sequences of integers."
  [alphabet]
  (fn [word]
    (let [m (zipmap alphabet (range))]
      (mapv m word))))

(defn integers->chars
  "Returns a function that maps sequences of integers to words formed from characters
   of the alphabet"
  [alphabet]
  (fn [nums]
    (let [m (zipmap (range) alphabet)]
      (apply str (map m nums)))))

(defn valid-params?
  "Determine whether or not the cardinality of the alphabet a is prime, and the
   desired word length n is within the bounds imposed by the alphabet size"
  [a n]
  (let [p (count a)]
    (and (n/prime? p) (<= n (Math/floor (/ (+ 2 p) 3))))))

(defn coding-scheme
  "For the given alphabet and desired encoded word length n, returns a map containing:
   :p - the cardinality of the alphabet (must be prime)
   :
   :a, :b - parameters used in deriving the weight sequences used for encoding/decoding
   :w, :w' - weight sequences used in encoding/decoding"
  [alphabet n]
  (let [[a b] (w/weight-parameters)]))

(defn encoder
  "Returns a function that encodes words of unencoded length n (formed from characters
   of the given alphabet a) by appending 2 checksum characters."
  [a n]
  {:pre [(valid-params? a n)]}
  (let [p (count a)
        [w w'] (w/weight-sequences p n)]
    (fn [word]
      (-> word
        ((chars->integers a))
        ((n/checksum-appender p w w'))
        ((integers->chars a))))))

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

(def alphanumeric-upper-case
  "An alphabet containing Digits and upper case letters, plus '*' to make the cardinality prime"
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*")

(def alphanumeric-mixed-case
  "An alphabet containing Digits, upper and lower case letters, minus capital 'O' to make the cardinality prime"
  "0123456789ABCDEFGHIJKLMNPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
