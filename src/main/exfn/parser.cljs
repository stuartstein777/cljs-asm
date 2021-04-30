(ns exfn.parser
  (:require [clojure.string :as str]))

(defn is-register? [x]
  (boolean (str/starts-with? x ":")))

(defn get-value [x]
  (prn x)
  (if (is-register? x)
    (keyword (subs x 1))
    x))

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

(defn format-arg [arg]
  (cond (is-register? arg)
        (keyword (subs arg 1))

        (re-find #"(\d+)" arg)
        (js/Number arg)

        (or (str/starts-with? arg "'") (str/starts-with? arg "`"))
        arg

        :else
        (keyword arg)))

(defn format-arguments [[instruction arg1 arg2]]
  (cond-> [(keyword instruction)]
    arg1 (conj (format-arg arg1))
    (and (some? arg2) (not= "" arg2)) (conj (format-arg arg2))))

;; ==============================================================================================
;; A line will look like this:
;;
;; instruction arg1 arg2
;; arg2 is optional.
;;
;; e.g.
;; mov :a :b
;; mov :a 'hello, world'
;; mov :a 555
;; call foo
;; prn 555
;; foo:
;; prn 'hello, world'
;; cat 'hello ' 'world'
;; ret
;; pop :x
;; push :x
;; push `hello, world 'bar', quax`
;; push `hello, world "bar", quax`
;; push 555
;;
;; if arg starts with a : it's a register.
;; if arg starts with a ' it's a string.
;; else arg is a number.
;; ==============================================================================================
(defn parse-line-of-code [line]
  (if (re-find #"\w+:$" line)
    [:label (keyword (subs line 0 (dec (count line))))]
    (let [instruction (first (re-find #"^(\w+)" line))
          args (subs line (inc (count instruction)))
          first-arg (cond
                      ; first argument is a register
                      (str/starts-with? args ":")
                      (->> args
                           (re-find #"^(\:\w+)")
                           (first))

                      ; first argument is a string
                      (str/starts-with? args "`")
                      (first (re-find #"([`])(?:(?=(\\?))\2.)*?\1" args))

                      ; first argument is a string
                      (str/starts-with? args "'")
                      (first (re-find #"(['])(?:(?=(\\?))\2.)*?\1" args))

                      ; first argument is a number.
                      (re-find #"^\d" args)
                      (first (re-find #"(\d+)" args))

                      ; first argument is a label
                      :else
                      (first (re-find #"(\w+)" args)))
          
          second-arg (str/trim (subs args (count first-arg)))]
      (-> (cond (and (nil? first-arg) (= second-arg ""))
                [instruction]
                (and first-arg (= second-arg ""))
                [instruction first-arg]
                :else
                [instruction first-arg second-arg])
          (format-arguments)))))

(comment (parse-line-of-code "prn 'hello world'")
         (parse-line-of-code "prn 555")
         (parse-line-of-code "call foo")
         (parse-line-of-code "foo:")
         (parse-line-of-code "mov :a :b")
         (parse-line-of-code "push `abc 'bar' quax`")
         (parse-line-of-code "cat 'hello ' 'world'")
         (parse-line-of-code "cat `foo 'bar' quax` 'world'")
         (parse-line-of-code "mov :a 555")
         (parse-line-of-code "mov :a 'foo, 'bar', quax'")
         (parse-line-of-code "ret"))

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
       (map parse-line-of-code)))

(comment (parse-line-of-code "mov :a 5"))