(ns exfn.helpers
  (:require [clojure.string :as str]))

(defn get-source-line-numbers [source]
  (loop [lines (str/split-lines source)
         no 0
         line-nos ""]
    (if (empty? lines)
      line-nos
      (if (or (= "" (first lines)) (clojure.string/starts-with? (first lines) ";"))
        (recur (rest lines) no (str line-nos "" "\n"))
        (recur (rest lines) (inc no) (str line-nos no "\n"))))))