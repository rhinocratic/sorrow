(ns sorrow.encoding
  (:require [clojure.spec.alpha :as s]
            [sorrow.translation :as t]
            [sorrow.weights.core :as w]
            [sorrow.numeric :as n]))

(defn- checksum-appender
  "Returns a function that accepts a sequence of integers and appends two check
   digits calculated from the given weight scheme."
  [{:keys [p w w']}]
  (let [solve (n/simultaneous-congruence-solver p)]
    (fn [nums]
      (let [[a b] (map #(conj (vec (take-last 2 %)) (n/weighted-sum p nums %)) [w w'])
            [x y] (solve a b)]
        (conj nums x y)))))

(defn- encoder-for-weight-scheme
  "Returns an encoder for the given weight scheme."
  [{:keys [n w w' alphabet] :as ws}]
  (let [appender (checksum-appender ws)]
    (fn [w]
      {:pre [(every? (set alphabet) w) (= (- n 2) (count w))]}
      (-> w
        ((t/str->ints alphabet))
        appender
        ((t/ints->str alphabet))))))

(defn encoder
  "Returns an encoder for words of encoded length n formed from letters of the alphabet a."
  [a n]
  {:pre [(s/valid? ::alphabet a) (s/valid? ::scheme [(count a) n])]}
  (-> (w/weight-scheme a n)
    (encoder-for-weight-scheme)))
