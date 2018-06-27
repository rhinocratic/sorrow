(ns sorrow.core
  "Implements the error-correcting coding scheme for alphanumeric data described
  by A.S. Sethi, V. Rajaraman and P.S. Kenjale in their paper:
  https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data"
   (:require [sorrow.weights.core :as w]
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
  "Returns an encoder for words formed from letters of alphabet alpha
  (a string).

  The encoder accepts unencoded words of length n-2, and returns encoded
  words of length n formed by appending two check characters.

  The cardinality of the alphabet should be prime,
  and n should be chosen so that 5 <= n <= (p - 1)/2, where p is the
  cardinality of the alphabet."
  [alpha n]
  (-> (w/weight-scheme alpha n)
    (e/encoder)))

(defn corrector
  "Returns a corrector/validator for encoded words of length n formed from
  letters of alphabet alpha.

  The corrector returns a map containing the following entries:
    :original    - the uncorrected word, as supplied to the corrector
    :status      - :correct, :corrected or :uncorrectable
    :correct     - the corrected word (absent if status is :uncorrectable)
    :error-type  - :transcription or :transposition if :status is :corrected
    :error-pos   - position of the error in the word if :status is :corrected"

  [alpha n]
  (-> (w/weight-scheme alpha n)
    (c/corrector)))
