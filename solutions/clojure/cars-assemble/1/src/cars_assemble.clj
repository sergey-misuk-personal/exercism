(ns cars-assemble)

(defn cars-per-hour
  [speed]
  (* 221 speed))

(defn success-rate
  [speed]
  (cond
    (= speed 0) 0
    (<= speed 4) 100
    (<= speed 8) 90
    (= speed 9) 80
    (= speed 10) 77))

(defn production-rate
  "Returns the assembly line's production rate per hour,
   taking into account its success rate"
  [speed]
  (double (/ (* (cars-per-hour speed) (success-rate speed)) 100)))

(defn working-items
  "Calculates how many working cars are produced per minute"
  [speed]
  (int (/ (production-rate speed) 60)))
