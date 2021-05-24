(ns main.exfn.scratch
  (:require [clojure.string :as str]))

(defn to-register [a]
  (keyword (subs a 1)))

(defn to-reg-array [[_ reg idx]]
  {:register reg :index idx})

(defn to-number [[_ n]]
  (js/Number n))

(defn to-string [[el _]]
  (subs el 1 (dec (count el))))

;; an element can be a instruction  (end)
;;                   a number (5)
;;                   a register (:a)
;;                   a register with an array index (:a[5] or :a[:b])
;;                   a string (`foo` or 'foo')
;;                   a label (foo:)
(def matchers-and-formatters
  [[#"^:\w+$" to-register]
   [#"^(:\w+)\[(\d+|\w+)?\]" to-reg-array]
   [#"^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$" to-number]
   [#"^('.+')$" to-string]
   [#"^(`.+`)$" to-string]])

(defn format-element [el]
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
        #"^(\w+:?)\s*('.+'|`.+`|:\w+\[:?[\w+|\d+]?\]|:\w+|\w+:|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+|\w+|\[.*\])?\s*('.+'|`.+`|:\w+\[:?[\w+|\d+]?\]|:\w+|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+|\w+|\[.*\])?$")
       (rest)
       (remove nil?)
       (format-line)))

(parse-line-of-code "mov :a :b")
(parse-line-of-code "mov :a 55")
(parse-line-of-code "inc :a")
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
(parse-line-of-code "mov :a[2] :b[2]")
(parse-line-of-code "mov :a[:d] :b[0]")
(parse-line-of-code "inc :a[2]")
(parse-line-of-code "jnz :a 5")