(ns sorrow.srk.core
  "Implements the error-correcting coding scheme for alphanumeric data described by
   A.S. Sethi, V. Rajaraman and P.S. Kenjale in their paper:
   https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data"
   (:require [clojure.spec.alpha :as s]
             [sorrow.srk.numeric :as n]
             [sorrow.srk.weights.method1 :as wm1]
             [sorrow.srk.weights.method2 :as wm2]))

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

(defn- str->ints
  "Returns a function that converts words formed from characters of the alphabet a to
   sequences of integers."
  [a]
  (fn [word]
    (let [m (zipmap a (range))]
      (mapv m word))))

(defn- ints->str
  "Returns a function that converts sequences of integers to words formed from characters
   of the given alphabet a"
  [a]
  (fn [nums]
    (let [m (zipmap (range) a)]
      (apply str (map m nums)))))

(defn- weight-scheme
  "Choose the method of calculating a weight scheme for encoding/correcting words,
   based upon size of alphabet a and desired (encoded) word length n.  Where possible,
   the simpler method 1 will be preferred, only using method 2 when longer words
   are required."
  [a n]
  (let [p (count a)
        method1-max (int (Math/floor (/ (+ p 2) 3)))]
    (cond
      (<= n method1-max) (wm1/weight-scheme p n)
      :else (wm2/weight-scheme p n))))

(defn- checksum-appender
  "Returns a function that accepts a sequence of integers and appends two check digits
   calculated from the weight sequences w and w'"
  [p w w']
  (let [solve (n/simultaneous-congruence-solver p)]
    (fn [nums]
      (let [[a b] (map #(conj (vec (take-last 2 %)) (n/weighted-sum p nums %)) [w w'])
            [x y] (solve a b)]
        (conj nums x y)))))

(defn- encoder-for-weight-scheme
  "Returns an encoder for the given alphabet, encoded word length and weight scheme."
  [a n {:keys [w w']}]
  (let [appender (checksum-appender (count a) w w')]
    (fn [w]
      {:pre [(every? (set a) w) (= (- n 2) (count w))]}
      (-> w
        ((str->ints a))
        appender
        ((ints->str a))))))

(defn encoder
  "Returns an encoder for words of encoded length n formed from letters of the alphabet a."
  [a n]
  {:pre [(s/valid? ::alphabet a) (s/valid? ::scheme [(count a) n])]}
  (let [ws (weight-scheme a n)]
    (encoder-for-weight-scheme a n ws)))

(defn- checksum-calculator
  "Returns a function that accepts a vector of integers of length n and returns a vector of
   two checksums calculated from the given weight sequences."
  [p {:keys [w w']}]
  (fn [nums]
    (mapv #(n/weighted-sum p nums %) [w w'])))

(defn- classify-checksums
  "Return :correct, :uncorrectable or :correctable depending upon the values of s1, s2."
  [[s1 s2]]
  (condp = (count (filter zero? [s1 s2]))
    2 :correct
    1 :uncorrectable
    0 :correctable))

(defn- validator-for-weight-scheme
  [a n {:keys [w w' method]}])

(defn validator
  "Returns a validator for words of encoded length n formed from letters of the alphabet a."
  [a n]
  {:pre [(s/valid? ::alphabet a) (s/valid? ::scheme [(count a) n])]}
  (let [ws (weight-scheme a n)]))
