(ns sorrow.srk.core
  "Implements the error-correcting coding scheme for alphanumeric data described by
   A.S. Sethi, V. Rajaraman and P.S. Kenjale in their paper:
   https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data"
   (:require [sorrow.srk.numeric :as n]
             [sorrow.srk.weights.method1 :as wm1]
             [sorrow.srk.weights.method2 :as wm2]))

(def alphanumeric-upper-case
  "An alphabet containing digits and upper case letters, plus '*' to make the cardinality prime"
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*")

(def alphanumeric-mixed-case
  "An alphabet containing digits, upper and lower case letters, minus capital 'O' to make the cardinality prime"
  "0123456789ABCDEFGHIJKLMNPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defn alphabet?
  "Predicate that returns true if its argument is a string of prime length with distinct characters."
  [a]
  (and
    (string? a)
    (apply distinct? a)
    (n/prime? (count a))))

(defn str->ints
  "Returns a function that converts words formed from characters of the alphabet a to
   sequences of integers."
  [a]
  (fn [word]
    (let [m (zipmap a (range))]
      (mapv m word))))

(defn ints->str
  "Returns a function that converts sequences of integers to words formed from characters
   of the given alphabet a"
  [a]
  (fn [nums]
    (let [m (zipmap (range) a)]
      (apply str (map m nums)))))

(defn choose-method
  "Choose the method of calculating a weight scheme for encoding/correcting words,
   based upon the alphabet size and desired (encoded) word length.  Where possible,
   the simpler method 1 will be preferred, only using method 2 when longer words
   are required."
  [p n]
  (let [method1-max (int (Math/floor (/ (+ p 2) 3)))
        method2-max (/ (dec p) 2)]
    (cond
      (<= 4 n method1-max) :method1
      (<= 5 n method2-max) :method2
      :default :error)))

; (defmulti weight-scheme choose-scheme)
; (defmethod weight-scheme :method1 wm1/weight-scheme)
; (defmethod weight-scheme :method2 wm2/weight-scheme)
; (defmethod weight-scheme :error (fn [p n] {:error "Unable to find encoding scheme for requested alphabet and word length"}))

(defn encoder
  "Returns an encoder for words of encoded length n formed from letters of the alphabet a."
  [a n])
