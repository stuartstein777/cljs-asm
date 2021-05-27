(ns main.exfn.scratch
  (:require [clojure.string :as str]
            [exfn.validators :as vdt]))

(def matchers-and-formatters
  [[#"^:\w+"                                (fn [a] {:original a :formatted (keyword (subs a 1))})]
   [#"^(:\w+)\[(\d+|:\w+)?\]"               (fn [[_ reg idx]] {:original (str reg idx) :formatted {:register reg :index idx}})]
   [#"^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)" (fn [[n _]]  {:original n :formatted (js/Number n)})]
   [#"^(['`])(?:(?=(\\?))\2.)*?\1"         (fn [[el _]] {:original el :formatted (subs el 1 (dec (count el)))})]
   [#"^\[(.*?)\]" (fn [[o a]] {:original o :formatted a :array true})]])

(defn format-element [el]
  (some (fn [[rx fmt]]
          (some-> (re-find rx el) fmt))
        matchers-and-formatters))

(defn format-array 
  ([arr] (format-array arr []))
  ([arr items]
   (prn arr)
   (if (= "" arr)
     (remove nil? items)
     (let [{:keys [original formatted array]} (format-element arr)]
       (recur (subs arr (inc (count original)))
              (if array
                (conj items (format-array formatted))
                (conj items formatted)))))))

(format-array "`abc 'def' ghi` `foo 'bar' [quax]` 25.9585 -.8484 'king' `queen` [1 2 :b 3 `foo`] :a")