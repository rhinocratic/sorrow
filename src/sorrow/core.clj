(ns sorrow.core
  "Implements the error-correcting coding scheme for alphanumeric data described by
   A.S. Sethi, V. Rajaraman and P.S. Kenjale in their paper:
   https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data"
   (:require [clojure.spec.alpha :as s]
             [sorrow.weights.core :as w]
             [sorrow.encoding :as e]
             [sorrow.correction :as c]))

(def alphanumeric-upper-case
  "An alphabet containing digits and upper case letters, plus '*' to make the cardinality prime"
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*")

(def alphanumeric-mixed-case
  "An alphabet containing digits, upper and lower case letters, minus capital 'O' to make the cardinality prime"
  "0123456789ABCDEFGHIJKLMNPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defn encoder
  "Returns an encoder for words of encoded length n formed from letters of alphabet alpha."
  [alpha n]
  (-> (w/weight-scheme alpha n)
    (e/encoder-for-weight-scheme)))

(defn validator
  "Returns a validator for words of encoded length n formed from letters of alphabet alpha."
  [alpha n]
  (-> (w/weight-scheme alpha n)))
