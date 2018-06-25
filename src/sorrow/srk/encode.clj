(ns sorrow.srk.encode
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [sorrow.srk.numeric :as n]
            [sorrow.srk.weights.method1 :as w]))

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

; (defn encoder
;   "Returns a function that encodes words of unencoded length n (formed from characters
;    of the given alphabet a) by appending 2 checksum characters."
;   [a n]
;   {:pre [(valid-params? a n)]}
;   (let [p (count a)
;         [w w'] (w/weight-sequences p n)]
;     (fn [word]
;       (-> word
;         ((chars->integers a))
;         ((n/checksum-appender p w w'))
;         ((integers->chars a))))))
