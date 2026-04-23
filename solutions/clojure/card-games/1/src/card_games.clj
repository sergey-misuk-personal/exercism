(ns card-games)

(defn rounds
  "Takes the current round number and returns 
   a `list` with that round and the _next two_."
  [n]
  (list n (+ n 1) (+ n 2)))


(defn concat-rounds
  "Takes two lists and returns a single `list` 
   consisting of all the rounds in the first `list`, 
   followed by all the rounds in the second `list`"
  [l1 l2]
  (concat l1 l2))


(defn contains-round?
  "Takes a list of rounds played and a round number.
   Returns `true` if the round is in the list, `false` if not."
  [l n]
  (= n
     (some #{n} l)))


(defn card-average
  "Returns the average value of a hand"
  [hand]
  (let [total (reduce + hand)
        number (count hand)]
    (double (/ total number))))


(defn approx-average?
  "Returns `true` if average is equal to either one of:
  - Take the average of the _first_ and _last_ number in the hand.
  - Using the median (middle card) of the hand."
  [hand]
  (let [first-last-average (double (/ (+ (first hand) (last hand)) 2))
        median (double (nth hand (/ (count hand) 2)))
        average (card-average hand)]
    (or (= first-last-average average) (= median average))))


(defn average-even-odd?
  "Returns true if the average of the cards at even indexes 
   is the same as the average of the cards at odd indexes."
  [hand]
  (let [indices (range (count hand))
        even-indices (filter even? indices)
        odd-indices (filter odd? indices)
        cards-at-even-indices (map (partial nth hand) even-indices)
        cards-at-odd-indices (map (partial nth hand) odd-indices)]
    (= (card-average cards-at-even-indices) (card-average cards-at-odd-indices))))


(defn maybe-double-last
  "If the last card is a Jack (11), doubles its value
   before returning the hand."
  [hand]
  (let [last-card (last hand)]
    (if (= last-card 11)
      (let [hand-vec (vec hand)
            last-index (dec (count hand-vec))]
        (apply list (assoc hand-vec last-index 22)))
      hand)))
