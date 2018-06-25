(ns sorrow.srk.core
  "Implements the error-correcting coding scheme for alphanumeric data described by
   A.S. Sethi, V. Rajaraman and P.S. Kenjale in their paper:
   https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data")

(def alphanumeric-upper-case
  "An alphabet containing Digits and upper case letters, plus '*' to make the cardinality prime"
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*")

(def alphanumeric-mixed-case
  "An alphabet containing Digits, upper and lower case letters, minus capital 'O' to make the cardinality prime"
  "0123456789ABCDEFGHIJKLMNPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

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
