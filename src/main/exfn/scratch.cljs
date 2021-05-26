(ns main.exfn.scratch
  (:require [clojure.string :as str]
            [exfn.validators :as vdt]))

(def code-regex 
  #"^(\w+:?)\s*('.+'|`.+`|:\w+\[:\w+\]|:\w+\[\d+\]|:\w+|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+)?\s*('.+'|`.+`|:\w+\[:\w+\]|:\w+\[\d+\]|:\w+|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+|\w+|\[.*\])?$")

(def matchers-and-formatters
  [[#"^:\w+$"                                (fn [a]           (keyword (subs a 1)))]
   [#"^(:\w+)\[(\d+|:\w+)?\]"                (fn [[_ reg idx]] {:register reg :index idx})]
   [#"^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$" (fn [[_ n]]       (js/Number n))]
   [#"^('.+')$"                              (fn [[el _]]      (subs el 1 (dec (count el))))]
   [#"^(`.+`)$"                              (fn [[el _]]      (subs el 1 (dec (count el))))]
   [#"\w+"                                    keyword]])

(defn format-element [el]
  (some (fn [[rx fmt]]
          (some-> (re-matches rx el) fmt))
        matchers-and-formatters))

(defn format-line [[instr & args]]
  (cond (seq? args)
        (concat [(keyword instr)] (map format-element args))

        (str/ends-with? instr ":")
        [:label (keyword (subs instr 0 (dec (count instr))))]

        :else
        [(keyword instr)]))

(defn parse-line-of-code [line]
  (->> line
       (re-matches code-regex)
       (rest)
       (remove nil?)
       (format-line)
       (vec)))

(defn scrub-comments [s]
  (if (and (not (str/starts-with? s "msg"))
           (str/includes? s ";"))
    (str/trimr (subs s 0 (str/index-of s ";")))
    s))

(defn get-macro-call [macro-names line]
  (first (filter #(str/starts-with? line %) macro-names)))

(defn build-symbol-table [asm]
  (reduce (fn [a [i ix]]
            (if (= (first ix) :label)
              (assoc a (second ix) (inc i))
              a))
          {}
          (map vector (range) asm)))

(defn get-args [line]
  (let [args (->> (str/split (->> (re-seq #"\((.*?)\)" line)
                                  (first)
                                  (rest)
                                  (first)) ",")
                  (remove #(= "" %))
                  (map str/trim))]
    (when (seq args)
      (zipmap (->> (range 1 (inc (count args)))
                   (map (fn [n] (str "%" n))))
              args))))

(defn replace-macro-args [args macro-line]
  (let [regex (re-pattern (str/join "|" (keys args)))]
    (str/replace macro-line regex args)))

(defn expand
  [line macro] ;
  (let [args (get-args line)]
    (if args
      (map (partial replace-macro-args args) macro)
      macro)))

(defn macro-expand-line [macros line]
  (let [macro-call (get-macro-call (keys macros) line)]
    (if macro-call
      (expand line (macros macro-call))
      (list line))))

(defn macro-expand-code [code macros]
  (->> (mapcat (partial macro-expand-line macros) code)))

(defn parse-data-entry [data]
  (let [[_ reg value] (re-find #"^(\w+) (.+)" data)]
    [(keyword reg) (format-element value)]))

(defn prepare-source [asm]
  (->> (str/split-lines asm)
       (map #(str/trimr (str/triml %)))
       (map scrub-comments)
       (remove #(= "" %))
       (remove #(str/starts-with? % ";"))))

(defn get-blocks [source]
  (let [blocks (->> source
                    (partition-by (fn [item] (str/starts-with? item ".")))
                    (partition 2))]
    (zipmap (map #(-> % ffirst (subs 1) keyword) blocks)
            (map second blocks))))

(defn parse [asm]
  (let [source (prepare-source asm)
        parse-errors (vdt/validate source)]
    (if (= "" parse-errors)
      (let [{:keys [code macros data]} (get-blocks source)
            parsed-code (->> (macro-expand-code code macros)
                             (mapv parse-line-of-code))
            symbol-table (build-symbol-table parsed-code)]
        {:code parsed-code
         :data (mapv parse-data-entry data)
         :errors ""
         :symbol-table symbol-table})
      {:code []
       :data []
       :errors parse-errors})))