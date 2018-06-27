(ns sorrow.encoding
  (:require [clojure.spec.alpha :as s]
            [sorrow.translation :as t]
            [sorrow.weights.core :as w]
            [sorrow.numeric :as n]))

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
  "Returns an encoder for the given alphabet and weight scheme."
  [a {:keys [n w w']}]
  (let [appender (checksum-appender (count a) w w')]
    (fn [w]
      {:pre [(every? (set a) w) (= (- n 2) (count w))]}
      (-> w
        ((t/str->ints a))
        appender
        ((t/ints->str a))))))

(defn encoder
  "Returns an encoder for words of encoded length n formed from letters of the alphabet a."
  [a n]
  {:pre [(s/valid? ::alphabet a) (s/valid? ::scheme [(count a) n])]}
  (let [ws (w/weight-scheme a n)]
    (encoder-for-weight-scheme a n ws)))
