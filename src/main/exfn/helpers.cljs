(ns exfn.helpers
  (:require [clojure.string :as str]))

(defn get-source-line-numbers [source]
  (:line-nos (reduce (fn [{:keys [cur line-nos] :as acc} i]
                       (if (or (= "" i) (clojure.string/starts-with? i ";"))
                         (assoc acc :line-nos (str line-nos "\n"))
                         (-> acc
                             (assoc :line-nos (str line-nos cur "\n"))
                             (update :cur inc))))
                     {:cur 0 :line-nos ""}
                     (str/split-lines source))))