(ns sorrow.correction
   (:require [sorrow.numeric :as n]
             [sorrow.translation :as t]
             [sorrow.location :as l]))

(defn- checksum-calculator
  "Returns a function that accepts a vector of integers and returns a vector of
   two checksums calculated from the given weight scheme."
  [{:keys [p w w']}]
  (fn [nums]
    (mapv #(n/weighted-sum p nums %) [w w'])))

(defn- checksum-status
  "Return :correct, :uncorrectable or :correctable depending upon
  the values of checksums s1, s2."
  [s1 s2]
  (condp = (count (filter zero? [s1 s2]))
    2 :correct
    1 :uncorrectable
    0 :correctable))

(defn- error-classifier
  "Returns a function that classifies a potentially correctable error as
  :transcription, :transposition or :uncorrectable based upon the values of
  error position indicators ep1, ep2."
  [{:keys [n]}]
  (fn [ep1 ep2]
    (cond
      (<= 0 ep1 (dec n))       :transcription
      (and (int? ep2)
           (<= 0 ep2 (- n 2))) :transposition
      :else                    :uncorrectable)))

(defmulti correct-error
  "Correct a single transcription error or transposition of adjacent characters."
  :error-type)

(defmethod correct-error :transcription
  [{:keys [nums error-pos error-size] :as m}]
  (let [cor (update nums error-pos #(- % error-size))]
    (assoc m :corrected cor)))

(defmethod correct-error :transposition
  [{:keys [nums error-pos] :as m}]
  (let [cor (concat
              (take error-pos nums)
              [(nums (inc error-pos)) (nums error-pos)]
              (drop (+ 2 error-pos) nums))]
    (assoc m :corrected cor)))

(defmethod correct-error :uncorrectable
  [m]
  (-> m
    (dissoc :error-pos :error-type)
    (assoc :status :uncorrectable)))


(defn word-classifier
  "Returns a function that accepts a word and returns a map containing:
    :original  - the original word
    :nums      - the original word as a vector of ints
    :status    - :correct, :uncorrectable or :correctable
    :checksums - a vector containing the 2 checksums"
  [{:keys [alphabet] :as ws}]
  (let [to-ints (t/str->ints alphabet)
        check (checksum-calculator ws)]
    (fn [w]
      (let [nums (to-ints w)
            [s1 s2] (check nums)]
        {:original w
         :nums nums
         :status (checksum-status s1 s2)
         :checksums [s1 s2]}))))

(defn word-corrector
  "Returns a function that accepts a word and returns a map containing details
  of any correction applied."
  [ws]
  (let [classify (word-classifier ws)
        locate (l/error-locator ws)]
    (fn [w]
      (let [m (classify w)]
        (condp = (:status m)
          :correct       (merge m {:correct (:original m)})
          :uncorrectable m
          :correctable   m)))))

(defn corrector
  "Returns a validator/corrector function for the given weight scheme.  The
  function accepts a vector of integers representing a word, and returns a map
  containing the following:
    :original    - the uncorrected word, as supplied to the corrector
    :status      - :correct, :corrected or :uncorrectable
    :correct     - the corrected word (absent if status is :uncorrectable)
    :error-type  - :transcription or :transposition if :status is :corrected
    :error-pos   - position of the error in the word if :status is :corrected"
  [{:keys [n alphabet] :as ws}]
  (let [correct (word-corrector ws)]
    (fn [w]
      {:pre [(every? (set alphabet) w) (= n (count w))]}
      (-> w
        correct
        (select-keys [:original :status :correct :error-type :error-pos])))))
