(ns bird-watcher)

(def last-week
  [0 2 5 3 7 8 4])

(defn today [birds]
  (last birds))

(defn inc-bird [birds]
  (let [last-index (dec (count birds))
        new-count (inc (get birds last-index))]
    (assoc birds last-index new-count)))

(defn day-without-birds? [birds]
  (if (some zero? birds)
    true
    false))

(defn n-days-count [birds n]
  (apply + (take n birds)))

(defn busy-days [birds]
  (count (for [b birds :when (>= b 5)] b)))

(defn matches-pattern? [data pattern]
  (if (empty? data)
    true
    (let [slice-length (min (count data) (count pattern))
          data-slice (take slice-length data)
          pattern-slice (take slice-length pattern)]
      (if (= data-slice pattern-slice)
        (recur (drop (count pattern) data) pattern)
        false))))

(defn odd-week? [birds]
  (matches-pattern? birds [1 0]))
