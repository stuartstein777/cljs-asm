(ns main.exfn.scratch
  (:require [clojure.string :as str]))

(defn to-arr [arr]
  :array)

;; an element can be a instruction  (end)
;;                   a number (5)
;;                   a register (:a)
;;                   a register with an array index (:a[5] or :a[:b])
;;                   a string (`foo` or 'foo')
;;                   a label (foo:)
(def matchers-and-formatters
  [[#"^:\w+$"                                (fn [a]           (keyword (subs a 1)))]
   [#"^(:\w+)\[(\d+|:\w+)?\]"                (fn [[_ reg idx]] {:register reg :index idx})]
   [#"^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$" (fn [[_ n]]       (js/Number n))]
   [#"^('.+')$"                              (fn [[el _]]      (subs el 1 (dec (count el))))]
   [#"^(`.+`)$"                              (fn [[el _]]      (subs el 1 (dec (count el))))]
   [#"^\[\w+|\d+\]$"                         to-arr]
   [#".+"                                    keyword]])

(defn format-element [el]
  (prn "el: " el)
  (some (fn [[rx fmt]]
          (some-> (re-matches rx el) fmt))
        matchers-and-formatters))

(defn format-instruction [instr]
  (if (str/ends-with? instr ":")
    [:label (keyword (subs instr 0 (dec (count instr))))]
    (keyword instr)))

(defn format-line [[instr & args]]
  (if (seq? args)
    (concat [(keyword instr)] (map format-element args))
    (format-instruction instr)))

(defn parse-line-of-code [line]
  (->> line
       (re-matches
        #"^(\w+:?)\s*('.+'|`.+`|:\w+\[:\w+\]|:\w+\[\d+\]|:\w+|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+)?\s*('.+'|`.+`|:\w+\[:\w+\]|:\w+\[\d+\]|:\w+|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+|\w+)?$")
       (rest)
       (remove nil?)
       (format-line)))

;; (rest (re-matches
;;        #"^(\w+:?)\s*(:\w+\[(:\w+|\d+)*\]|:\w+|\w+:|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+|\w+|\[.*\])?\s*('.+'|`.+`|:\w+\[(:\w+|\d+)*\]|:\w+|\w+:|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+|\w+|\[.*\])?$"
;;        "mov :a[:abc] :d[:efg]"))

;; (rest (re-matches
;;        #"^(\w+:?)\s*(:\w+\[(:\w+|\d+)*\]|:\w+|\w+:|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+|\w+|\[.*\])?\s*('.+'|`.+`|:\w+\[(:\w+|\d+)*\]|:\w+|\w+:|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+|\w+|\[.*\])?$"
;;        "mov :a[:b] :c[:d]"))

;; (rest (re-matches
;;        #"^(\w+:?)\s*(:\w+\[(:\w+|\d+)*\]|:\w+|\w+:|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+|\w+|\[.*\])?\s*('.+'|`.+`|:\w+\[(:\w+|\d+)*\]|:\w+|\w+:|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+|\w+|\[.*\])?$"
;;        "mov :a[2] :c[3]"))
;; (defn to-register [a]
;;   (keyword (subs a 1)))

;; (defn to-reg-array [[_ reg idx]]
;;   {:register reg :index idx})

;; (defn to-number [[_ n]] (js/Number n))

;; (defn to-string [[el _]]
;;   (subs el 1 (dec (count el))))

(comment (rest (re-matches
                #"^(\w+:?)\s*('.+'|`.+`|:\w+\[:\w+\]|:\w+\[\d+\]|:\w+|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+)?\s*('.+'|`.+`|:\w+\[:\w+\]|:\w+\[\d+\]|:\w+|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+|\w+)?$"
                "mov :a[:cv] :b[:def]"))

         (rest (re-matches #"^(\w+:?)\s*(:\w+\[:\w+\]|:\w+\[:\d+\])\s*(:\w+\[:\w+\]|:\w+\[\d+\])" "foo :a[:bc] :d[55]"))
         (rest (re-matches #"^([a-zA-Z]+) (:[a-zA-Z]+\[:?[^\]]*])\s(:[a-zA-Z]+\[[^\]]*\])$" "mov :a[:bc] :d[445]"))
         (rest (re-matches #"^([a-zA-Z]+) (:[a-zA-Z]+\[[^\]]*])\s(:[a-zA-Z]+\[[^\]]*\])$" "mov :a[55] :d[66]"))
         (rest (re-matches #"^([a-zA-Z]+) (:[a-zA-Z]+\[[^\]]*])\s(:[a-zA-Z]+\[[^\]]*\])$" "mov :a[55] :d[66]"))

         (parse-line-of-code "mov :a :b")
         (parse-line-of-code "mov :a 55")
         (parse-line-of-code "inc :a")
         (parse-line-of-code "inc :a[2]")
         (parse-line-of-code "inc :a[20]")
         (parse-line-of-code "end")
         (parse-line-of-code "foo:")
         (parse-line-of-code "rep 20")
         (parse-line-of-code "mov :a -.56")
         (parse-line-of-code "mov :a 'foo'")
         (parse-line-of-code "mov :a `foo`")
         (parse-line-of-code "mov :a `foo 'bar' quax`")
         (parse-line-of-code "mov :a -5.56")
         (parse-line-of-code "call foo")
         (parse-line-of-code "mov :a [1 2 [3 4] 5]")
         (parse-line-of-code "mov :a[2] :b")
         (parse-line-of-code "mov :a[:v] :b[:d]")
         (parse-line-of-code "mov :a[:vv] :b[:dd]")
         (parse-line-of-code "mov :a[2] :b[2]")
         (parse-line-of-code "mov :a[:d] :b[0]")
         (parse-line-of-code "inc :a[2]")
         (parse-line-of-code "jnz :a 5"))