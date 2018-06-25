(ns sorrow.srk.core
  "Implements the error-correcting coding scheme for alphanumeric data described by
   A.S. Sethi, V. Rajaraman and P.S. Kenjale in their paper:
   https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data"
   (:require [sorrow.srk.numeric :as n]))

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
