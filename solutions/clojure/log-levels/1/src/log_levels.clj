(ns log-levels
  (:require [clojure.string :as str]))

(defn extract [log]
  (let [[level-part message-part] (str/split log #":" 2)
        level (str/lower-case (str/replace level-part #"[\[\]]" ""))
        message (str/trim message-part)]
    [level, message]))

(defn message
  "Takes a string representing a log line
   and returns its message with whitespace trimmed."
  [s]
  (let [[_, message] (extract s)]
    message))


(defn log-level
  "Takes a string representing a log line
   and returns its level in lower-case."
  [s]
  (let [[level] (extract s)]
    level))



(defn reformat
  "Takes a string representing a log line and formats it
   with the message first and the log level in parentheses."
  [s]
  (let [[level message] (extract s)]
    (str message " (" level ")")))



