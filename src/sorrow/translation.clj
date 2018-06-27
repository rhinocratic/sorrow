(ns sorrow.translation)

(defn str->ints
  "Returns a function that converts words formed from characters of the alphabet a to
   sequences of integers."
  [a]
  (fn [word]
    (let [m (zipmap a (range))]
      (mapv m word))))

(defn ints->str
  "Returns a function that converts sequences of integers to words formed from characters
   of the given alphabet a"
  [a]
  (fn [nums]
    (let [m (zipmap (range) a)]
      (apply str (map m nums)))))
