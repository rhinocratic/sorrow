(ns sorrow.core
  "Implements the error-correcting coding scheme for alphanumeric data described
  by A.S. Sethi, V. Rajaraman and P.S. Kenjale in their paper:
  https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data"
   (:require [sorrow.weights.method1 :as wm1]
             [sorrow.weights.method2 :as wm2]
             [sorrow.numeric :as n]
             [sorrow.encoding :as e]
             [sorrow.correction :as c]))

(def alphanumeric-upper-case
  "An alphabet containing digits and upper case letters, plus '*' to make the
  cardinality prime (= 37)"
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*")

(def alphanumeric-mixed-case
  "An alphabet containing digits, upper and lower case letters, minus capital
  'O' to make the cardinality prime (= 61)"
  "0123456789ABCDEFGHIJKLMNPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defn- weight-scheme
  "Choose the method of calculating a weight scheme for encoding/correcting
  words, based upon the cardinality of alphabet alpha and desired (encoded) word
  length n.  Where possible, the simpler method 1 will be preferred, only using
  method 2 when longer words are required."
  [alpha n]
  {:pre [(string? alpha)
         (apply distinct? alpha)
         (n/prime? (count alpha))
         (int? n)
         (<= 5 n (/ (dec (count alpha)) 2))]}
  (let [p (count alpha)
        method1-max (int (/ (+ p 2) 3))]
    (cond
      (<= n method1-max) (wm1/weight-scheme alpha n)
      :else              (wm2/weight-scheme alpha n))))

(defn encoder
  "Returns an encoder for words formed from letters of alphabet alpha
  (a string).

  The encoder accepts unencoded words of length n-2, and returns encoded
  words of length n formed by appending two check characters.

  The cardinality of the alphabet should be prime,
  and n should be chosen so that 5 <= n <= (p - 1)/2, where p is the
  cardinality of the alphabet."
  [alpha n]
  (-> (weight-scheme alpha n)
    (e/encoder)))

(defn corrector
  "Returns a validator/corrector for encoded words of length n formed from
  letters of alphabet alpha.

  The corrector returns a map containing the following entries:
    :status      - :correct, :corrected or :uncorrectable
    :original    - the uncorrected word, as supplied to the corrector
    :correct     - the corrected word (absent if :status = :uncorrectable,
                   same as :original if :status = :correct)
    :error-type  - :transcription or :transposition if :status = :corrected
    :error-pos   - position of the error in the word if :status = :corrected"
  [alpha n]
  (-> (weight-scheme alpha n)
    (c/corrector)))
