(ns exfn.parser
  (:require [clojure.string :as str]
            [exfn.validators :as vdt]))

(defn is-register? [x]
  (boolean (str/starts-with? x ":")))

(defn get-value [x]
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

;; ==============================================================================================
;; Takes the arguments for a line of source code and formats them depending on their
;; type.
;;
;; Valid argument types:
;;    keyword - meaning its a register.
;;    number  - meaning its a number.
;;    string  - starts with ` or '
;;    else return it as a keyword.
;; ==============================================================================================
(defn format-arg [arg]
  (cond (is-register? arg)
        (keyword (subs arg 1))

        (re-find #"^(\d+)$" arg)
        (js/Number arg)

        (or (str/starts-with? arg "'") (str/starts-with? arg "`"))
        (subs arg 1 (dec (count arg)))

        :else
        (keyword arg)))

;; ==============================================================================================
;; Formats the arguments, handles if their are 0, 1 or 2 args.
;; ==============================================================================================
(defn format-arguments [[instruction arg1 arg2]]
  (cond-> [(keyword instruction)]
    arg1 (conj (format-arg arg1))
    (and (some? arg2) (not= "" arg2)) (conj (format-arg arg2))))

(comment "format-argument takes instruction and its args as a vector. Returns them formatted for
          interpreter. Handles 0, 1 or 2 args. e.g:"
         (format-arguments ["mov" ":a" "5"])
         (format-arguments ["mov" ":a" ":b"])
         (format-arguments ["call" "foo"])
         (format-arguments ["ret"])
         (format-arguments ["rep 5"]))

(defn get-first-arg [args]
  (cond
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
    (first (re-find #"(\w+)" args))))

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
  (if (re-find #"\w+:$" line) ; if the line is foo: then its a label.
    [:label (keyword (subs line 0 (dec (count line))))]
    (let [instruction (first (re-find #"^(\w+)" line))
          args (subs line (inc (count instruction)))
          first-arg (get-first-arg args)
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

;; ==============================================================================================
;; Get rid of all comments, this can be either a line that starts with a ; or a line with a
;; trailing comment, e.g.
;;
;; mov :a :b ; this is a comment.
;; ==============================================================================================
(defn scrub-comments [s]
  (if (and (not (str/starts-with? s "msg"))
           (str/includes? s ";"))
    (str/trimr (subs s 0 (str/index-of s ";")))
    s))

;; ==============================================================================================
;; Get all the macros from the macro section.
;; The input is the source code, split by lines.
;; Return them in a map keyed on the macro name.
;; Do not include the % on the start of the name or %end
;; ==============================================================================================
(defn get-macros [source]
  (let [macro-start (.indexOf source ".macros")
        macro-end (.indexOf source ".code")
        macros (->> source
                    (drop (inc macro-start))
                    (take (- (dec macro-end) macro-start))
                    (partition-by (fn [n] (= n "%end")))
                    (remove #(= '("%end") %)))]
    (zipmap (->> (map first macros)
                 (map #(subs % 1)))
            (map rest macros))))

(comment "get-macros-test"
         (let [prepared-source (list ".macros" "%square-and-sum" "mul %1 %1" "mul %2 %2" "add %1 %2" "%end" "%add-ten" "add %1 10" "%end" ".code" "mov :a 2" "mov :b 5" "square-and-sum(:a, :b)" "add-ten (:a)")]
           (get-macros prepared-source)))

;; ==============================================================================================
;; Get the code from the .code section.
;; ==============================================================================================
(defn get-code [source]
  (let [code-start (.indexOf source ".code")
        data-start (.indexOf source ".data")
        code-end (if (= -1 data-start) (count source) data-start)]
    (->> source
         (drop (inc code-start))
         (take (- (dec code-end) code-start)))))

;; + macro expansion ============================================================================

;; ==============================================================================================
;; Gets the macro call (if it is a macro-call) from the collection of macro names for that line of
;; code. Returns nil if the line of code is not a macro call.
;; ==============================================================================================
(defn get-macro-call [macro-names line]
  (first (filter #(str/starts-with? line %) macro-names)))

(comment "get-macro-call examples"
         "returns sum-and-square, since the line starts with sum-and-square, which is in our macro list"
         (get-macro-call ["sum-and-square" "add-ten"] "sum-and-square(:a, :b)")
         "returns nill since mov :a :b is not a macro"
         (get-macro-call ["sum-and-square" "add-ten"] "mov :a :b")
         "returns nill, looks like macro but it's not in the list."
         (get-macro-call ["sum-and-square" "add-ten"] "foo(:a)")
         "returns add-ten"
         (get-macro-call ["sum-and-square" "add-ten"] "add-ten(:a)"))

;;=======================================================================================================
;; Builds the symbol table for jump targets
;; A jump target is a label of form foo:
;;=======================================================================================================
(defn build-symbol-table [asm]
  (reduce (fn [a [i ix]]
            (if (= (first ix) :label)
              (assoc a (second ix) i)
              a))
          {}
          (map vector (range) asm)))

;; ==============================================================================================
;; Get the arguments for the macro call.
;; Takes a line of source for a macro call, e.g,
;;     sum-and-square(:a, :b)
;; This method needs to extract the args :a and :b.
;; This then gets put into a sort of positional map, so the first the arg is :a, it will be keyed
;; on "%1", :b is the second arg so it is keyed on "%2".
;; Input: sum-and-square (:a, :b)
;; Output: {"%1" :a, "%2" :b}
;; This is used as the map input to str/replace for the macro expansion.
;; If no args, return nil.
;; ==============================================================================================
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

(comment "get-args examples:"
         (get-args "sum-and-square(:a, :b)")
         (get-args "foo()"))

;; ==============================================================================================
;; Replaces the %1, %2 etc arguments in macro code with the actual arguments from the source call.
;; e.g.
;; %sum-and-square
;;    mul %1 %1
;;    mul %2 %2
;;    add %1 %2
;; %end
;; .code
;;   sum-and-square(:a, :b)
;;
;; The first line will have to macro expand to:
;;    mul :a :a
;;
;; Takes in the args as keys, these are ordered as positional in the source call. e.g.
;; sum-and-square(:a, :b) args will be {"%1" :a, "%2" :b}
;; ==============================================================================================
(defn replace-macro-args [args macro-line]
  (let [regex (re-pattern (str/join "|" (keys args)))]
    (str/replace macro-line regex args)))

(comment "replacing macro argse example"
         (replace-macro-args {"%1" ":a", "%2", ":b"} "mul %1 %1")
         (replace-macro-args {"%1" ":a", "%2", ":b"} "add %1 %2"))

;; Need to handle nested macro calls. So bind current result.
;; call expand again and see if its any different, if its not. we are done.
(defn expand
  [line macro] ;
  (let [args (get-args line)]
    (if args
      (map (partial replace-macro-args args) macro)
      macro)))

(comment "expand example:"
         (expand "sum-and-square(:a, :b)" ["mul %1 %1" "mul %2 %2" "add %1 %2"])
         "if no arguments, just return the macro"
         (expand "foo()" ["foo :a 6" "foo :b 7" "add :a :b"]))

(defn macro-expand-line [macros line]
  (let [macro-call (get-macro-call (keys macros) line)]
    (if macro-call
      (expand line (macros macro-call))
      (list line))))

(comment "If the line is a macro, expand it, otherwise just return the line (as a list) as we
          use mapcat to concatenat all the results from macro-expand-line"
         (macro-expand-line {"square-and-sum" ["mul %1 %1" "mul %2 %2" "add %1 %2"], "add-ten" ["add %1 10"]}
                            "square-and-sum(:a, :b)")
         (macro-expand-line {"square-and-sum" ["mul %1 %1" "mul %2 %2" "add %1 %2"], "add-ten" ["add %1 10"]}
                            "mov :a 5"))

(defn macro-expand-code [code macros]
  (->> (mapcat (partial macro-expand-line macros) code)))

;; - macro expansion ====================================================================

(defn get-data [source]
  (let [data-start (.indexOf source ".data")
        data-end (count source)]
    (when (not= -1 data-start)
      (->> source
           (drop (inc data-start))
           (take (- (dec data-end) data-start))))))

(defn parse-data-entry [data]
  (let [[_ reg value] (re-find #"^(\w+) (.+)" data)]
    [(keyword reg) (format-arg value)]))

(comment "parse-data-entry"
     (parse-data-entry "foo 42")
     (parse-data-entry "bar `this is a string`")
     (parse-data-entry "quax 'another string'"))

(defn prepare-source [asm]
  (->> (str/split-lines asm)
       (map #(str/trimr (str/triml %)))
       (map scrub-comments)
       (remove #(= "" %))
       (remove #(str/starts-with? % ";"))))

;; This has to return either the parsed code or the errors from parsing.
(defn parse [asm]
  (let [source (prepare-source asm)
        parse-errors (vdt/validate source)]
    (if (= "" parse-errors)
      (let [code (get-code source)
            macros (get-macros source)
            data (get-data source)
            parsed-code (->> (macro-expand-code code macros)
                             (map parse-line-of-code))
            symbol-table (build-symbol-table parsed-code)]
        {:code parsed-code
         :data (mapv parse-data-entry data)
         :errors ""
         :symbol-table symbol-table})
      {:code []
       :data []
       :errors parse-errors})))