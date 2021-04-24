(ns exfn.parser
  (:require [clojure.string :as str]))

(defn is-register? [x]
  (nil? (re-find #"^[\+\-]?\d*\.?[Ee]?[\+\-]?\d*$" x)))

(defn get-value [x]
  (if (is-register? x)
    (keyword x)
    (js/Number x)))

(defn parse-msg [instruction]
  (into [(keyword (subs instruction 0 3))]
        (let [input (map identity (subs instruction 4))]
          (loop [to-parse input
                 res []
                 in-quote? false
                 current-string ""]
            (let [i (first to-parse)]
              (if (empty? to-parse)
                (conj res (get-value current-string))
                (cond
                  (and in-quote? (= i \'))
                  (recur (rest to-parse) (conj res current-string) false "")

                  (and in-quote? (not= i \'))
                  (recur (rest to-parse) res in-quote? (str current-string (str i)))

                  (and (not in-quote?) (= i \'))
                  (recur (rest to-parse) (if (= "" current-string)
                                           res
                                           (conj res (get-value current-string))) true "")

                  (and (not in-quote?) (= i \space))
                  (recur (rest to-parse) res in-quote? current-string)

                  (and (not in-quote?) (= i \;))
                  (conj res (get-value current-string))

                  :else
                  (recur (rest to-parse) res in-quote? (str current-string (str i))))))))))

(defn- drop-last-char [s]
  (subs s 0 (dec (count s))))

(defn to-keywords [instructions]
  (let [[instruction op1 op2] (str/split instructions #" ")]
    (cond (= "msg" instruction)
          (parse-msg instructions)

          (and (nil? op1) (nil? op2) (str/ends-with? instruction ":"))
          [:label (keyword (drop-last-char instruction))]

          :else
          (cond-> [(keyword instruction)]
            (not (nil? op1)) (conj (get-value op1))
            (not (nil? op2)) (conj (get-value op2))))))

(defn scrub-comments [s]
  (if (and (not (str/starts-with? s "msg"))
           (str/includes? s ";"))
    (str/trimr (subs s 0 (str/index-of s ";")))
    s))

(defn parse [asm]
  (->> (str/split-lines asm)
       (map #(str/trimr (str/triml %)))
       (map scrub-comments)
       (remove #(= "" %))
       (remove #(str/starts-with? % ";"))
       (map to-keywords)))