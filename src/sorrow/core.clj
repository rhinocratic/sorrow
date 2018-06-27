(ns sorrow.core
  "Implements the error-correcting coding scheme for alphanumeric data described
  by A.S. Sethi, V. Rajaraman and P.S. Kenjale in their paper:
  https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data"
   (:require [clojure.spec.alpha :as s]
             [sorrow.weights.core :as w]
             [sorrow.encoding :as e]
             [sorrow.correction.core :as c]))

(def alphanumeric-upper-case
  "An alphabet containing digits and upper case letters, plus '*' to make the
  cardinality prime (= 37)"
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*")

(def alphanumeric-mixed-case
  "An alphabet containing digits, upper and lower case letters, minus capital
  'O' to make the cardinality prime (= 61)"
  "0123456789ABCDEFGHIJKLMNPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defn encoder
  "Returns an encoder for words of encoded length n formed from letters of
  alphabet alpha (a string).  The cardinality of the alphabet should be prime,
  and n should be chosen so that 5 <= n <= (p - 1)/2, where p is the
  cardinality of the alphabet"
  [alpha n]
  (-> (w/weight-scheme alpha n)
    (e/encoder-for-weight-scheme)))

(defn validator
  "Returns a validator for words of encoded length n formed from letters of alphabet alpha."
  [alpha n]
  (-> (w/weight-scheme alpha n)))
