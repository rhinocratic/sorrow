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

(defn- error-locator
  "Add error location and type information to a validation record"
  [ws]
  (let [locate-error (l/error-locator ws)
        classify-error (error-classifier ws)]
    (fn [{:keys [checksums] :as m}]
      (let [[ep1 ep2 size p] (apply locate-error checksums)
            type (classify-error ep1 ep2)
            pos (condp = type
                 :transcription ep1
                 :transposition ep2
                 :uncorrectable 0)]
        (assoc m :error-pos pos :error-type type :error-size size)))))

(defmulti correct
  "Correct a single transcription error or transposition of adjacent characters."
  :error-type)

(defmethod correct :transcription
  [{:keys [p nums error-pos error-size] :as m}]
  (let [cor (update nums error-pos #(mod (- % error-size) p))]
    (assoc m :corrected cor :status :corrected)))

(defmethod correct :transposition
  [{:keys [nums error-pos] :as m}]
  (let [cor (concat
              (take error-pos nums)
              [(nums (inc error-pos)) (nums error-pos)]
              (drop (+ 2 error-pos) nums))]
    (assoc m :corrected cor :status :corrected)))

(defmethod correct :uncorrectable
  [m]
  (-> m
    (dissoc :error-pos :error-type)
    (assoc :status :uncorrectable)))

(defn- validator
  "Returns a function that accepts a word and returns a map containing:
    :status    - :correct, :uncorrectable or :correctable
    :original  - the original word
    :nums      - the original word as a vector of ints
    :checksums - a vector containing the 2 checksums
    :p         - the cardinality of the alphabet."
  [{:keys [p alphabet] :as ws}]
  (let [to-ints (t/str->ints alphabet)
        check (checksum-calculator ws)]
    (fn [w]
      (let [nums (to-ints w)
            [s1 s2] (check nums)]
        {:original w
         :nums nums
         :status (checksum-status s1 s2)
         :checksums [s1 s2]
         :p p}))))

(defn- update-if-exists
  "Update a map if the key k exists"
  [m k f]
  (if (contains? m k)
    (update m k f)
    m))

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
  (let [validate (validator ws)
        locate (error-locator ws)
        to-str (t/ints->str alphabet)]
    (fn [w]
      {:pre [(every? (set alphabet) w) (= n (count w))]}
      (let [m (validate w)]
        (-> (condp = (:status m)
              :correct       (merge m {:correct (:original m)})
              :correctable   (-> m locate correct)
              :uncorrectable m)
          (select-keys [:status :original :corrected :error-type :error-pos])
          (update-if-exists :corrected to-str))))))
