(ns sorrow.encoding
  (:require [sorrow.translation :as t]
            [sorrow.numeric :as n]))

(defn- checksum-appender
  "Returns a function that accepts a sequence of integers and appends two check
   digits calculated from the given weight scheme."
  [{:keys [p w w']}]
  (let [solve (n/simultaneous-congruence-solver p)]
    (fn [word]
      (let [[a b] (map #(conj (vec (take-last 2 %)) (n/weighted-sum p word %)) [w w'])
            [x y] (solve a b)]
        (conj word x y)))))

(defn encoder
  "Returns an encoder for the given weight scheme."
  [{:keys [n w w' alphabet] :as ws}]
  (let [add-checksum (checksum-appender ws)
        to-ints (t/str->ints alphabet)
        to-str (t/ints->str alphabet)]
    (fn [w]
      {:pre [(every? (set alphabet) w) (= (- n 2) (count w))]}
      (-> w
        to-ints
        add-checksum
        to-str))))
