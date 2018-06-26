(ns sorrow.verhoeff.core
  "Generate and verify checksums for numeric strings using the Verhoeff algorithn
   (described at https://en.wikipedia.org/wiki/Verhoeff_algorithm)")

(defn- digits
  "Get an array of the digits of n, padded with leading zeros to len digits if necessary"
  [n len]
  (let [s (str n)
        padded (concat (repeat (- len (count s)) \0) s)]
    (vec (map #(- (int %) 48) padded))))

(defn- mult
  "Multiply two elements of the dihedral group D5"
  [i j]
  (let [table [[0 1 2 3 4 5 6 7 8 9]
               [1 2 3 4 0 6 7 8 9 5]
               [2 3 4 0 1 7 8 9 5 6]
               [3 4 0 1 2 8 9 5 6 7]
               [4 0 1 2 3 9 5 6 7 8]
               [5 9 8 7 6 0 4 3 2 1]
               [6 5 9 8 7 1 0 4 3 2]
               [7 6 5 9 8 2 1 0 4 3]
               [8 7 6 5 9 3 2 1 0 4]
               [9 8 7 6 5 4 3 2 1 0]]]
    (get-in table [i j])))

(defn- inv
  "Get the inverse of an element of the dihedral group D5"
  [i]
  (get [0 4 3 2 1 5 6 7 8 9] i))

(defn- perm
  "Apply a permutation to a number given its position in an array"
  [pos n]
  (let [table [[0 1 2 3 4 5 6 7 8 9]
               [1 5 7 6 2 8 3 0 9 4]
               [5 8 0 3 7 9 6 1 4 2]
               [8 9 1 6 0 4 3 5 2 7]
               [9 4 5 3 1 2 6 8 7 0]
               [4 2 8 6 5 7 3 9 0 1]
               [2 7 9 3 8 0 6 4 1 5]
               [7 0 4 6 9 1 3 2 5 8]]]
    (get-in table [pos n])))

(defn- checksum
  "Calculate the Verhoeff checksum for an array of digits"
  [digits]
  (let [reduce-fn (fn [c [i digit]] (mult c (perm (mod i 8) digit)))
        indexed-digits (map vector (iterate inc 0) (reverse digits))]
    (reduce reduce-fn 0 indexed-digits)))

(defn encode
  "Encode a number according to the Verhoeff scheme, padding with leading zeroes to l-1 digits and appending a checksum."
  [len n]
  {:pre [(int? n) (int? len) (< 0 n) (< 0 len) (> len (count (str n)))]}
  (let [digits (digits n (dec len))
        check-digit (inv (checksum (conj digits 0)))]
    (apply str (conj digits check-digit))))

(defn valid?
  "Determine whether or not the supplied string is of length len and represents a number with a valid Verhoeff checksum."
  [len s]
  {:pre [(int? len) (< 0 len) (string? s) (re-matches #"\d+" s)]}
  (and (= len (count s))
    (zero? (checksum (digits s len)))))
