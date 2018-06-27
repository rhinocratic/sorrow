(ns sorrow.core
  "Implements the error-correcting coding scheme for alphanumeric data described by
   A.S. Sethi, V. Rajaraman and P.S. Kenjale in their paper:
   https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data"
   (:require [clojure.spec.alpha :as s]
             [sorrow.weights.core :as w]
             [sorrow.detection.core :as d]
             [sorrow.numeric :as n]))

(def alphanumeric-upper-case
  "An alphabet containing digits and upper case letters, plus '*' to make the cardinality prime"
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*")

(def alphanumeric-mixed-case
  "An alphabet containing digits, upper and lower case letters, minus capital 'O' to make the cardinality prime"
  "0123456789ABCDEFGHIJKLMNPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

; An alphabet is valid if its argument is a string of prime length with distinct characters.
(s/def ::alphabet
  (s/and
    string?
    #(apply distinct? %)
    #(n/prime? (count %))))

; A scheme is valid if it consists of a valid alphabet and an (encoded) word length
; that is achievable given the alphabet size.
(s/def ::scheme
  (s/and
    #(int? (last %))
    #(<= 5 (last %) (/ (dec (first %)) 2))))


; (defn- corrector-for-weight-scheme
;   [a n {:keys [w w' method]}]
;   (if (= 1 method)))
;

; (defn validator
;   "Returns a validator for words of encoded length n formed from letters of the alphabet a."
;   [a n]
;   {:pre [(s/valid? ::alphabet a) (s/valid? ::scheme [(count a) n])]}
;   (let [ws (weight-scheme a n)]))
