(ns exfn.parser
  (:require [clojure.string :as str]
            [exfn.validators :as vdt]))

(defn is-register? [x]
  (boolean (str/starts-with? x ":")))

(defn get-value [x]
  (if (is-register? x)
    (keyword (subs x 1))
    x))

(def code-regex #"^(\w+:?)\s*('.+'|`.+`|:\w+\[:\w+\]|:\w+\[\d+\]|:\w+|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+)?\s*('.+'|`.+`|:\w+\[:\w+\]|:\w+\[\d+\]|:\w+|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+|\w+|\[.*\])?$")

(def matchers-and-formatters
  [[#"^:\w+$"                                (fn [a]           (keyword (subs a 1)))]
   [#"^(:\w+)\[(\d+|:\w+)?\]"                (fn [[_ reg idx]] {:register reg :index idx})]
   [#"^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$" (fn [[n _]]       (js/Number n))]
   [#"^('.+')$"                              (fn [[el _]]      (subs el 1 (dec (count el))))]
   [#"^(`.+`)$"                              (fn [[el _]]      (subs el 1 (dec (count el))))]
   [#"\w+"                                    keyword]])

(defn format-element [el]
  (some (fn [[rx fmt]]
          (some-> (re-matches rx el) fmt))
        matchers-and-formatters))

(defn format-instruction [instr]
  (if (str/ends-with? instr ":")
    [:label (keyword (subs instr 0 (dec (count instr))))]
    [(keyword instr)]))

(defn format-line [[instr & args]]
  (if (seq? args)
    (concat [(keyword instr)] (map format-element args))
    (format-instruction instr)))

(defn parse-line-of-code [line]
  (->> line
       (re-matches code-regex)
       (rest)
       (remove nil?)
       (format-line)
       (vec)))

;; ==============================================================================================
;; Get rid of all comments, this can be either a line that starts with a ; or a line with a
;; trailing comment, e.g.
;;
;; Dont scrub comments inside strings. e.g.
;; mov :a `foo bar ; quax ` should remain untouched.
;; ==============================================================================================
(defn scrub-comments [s]
  (let [comment-start-loc
        (->> s
             (reduce (fn [{:keys [opened found-semi idx] :as acc} i]
                       (cond            
                         ; opening a string.
                         (and (or (= i \`) (= i \')) (not opened) (not found-semi))
                         (-> acc
                             (assoc :opened i)
                             (update :idx inc))
              
                         ; closing a string
                         (or (= i opened \') (= i opened \`))
                         (-> acc
                             (dissoc :opened)
                             (update :idx inc))
              
                         ; hit a ; and we are not in an open string, so note it's position,
                         ; and also we haven't already hit a ;
                         (and (= i \;) (not opened) (not found-semi))
                         (-> acc
                             (assoc :found-semi idx))
                         
                         :else
                         (update acc :idx inc)))
                     {:idx 0})
             :found-semi)]
    (if comment-start-loc
      (-> (subs s 0 comment-start-loc) str/trim)
      s)))

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
              (assoc a (second ix) (inc i))
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
    [(keyword reg) (format-element value)]))

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

(defn get-blocks [source]
  (let [blocks (->> source
                    (partition-by (fn [item] (str/starts-with? item ".")))
                    (partition 2))]
    (zipmap (map #(-> % ffirst (subs 1) keyword) blocks)
            (map second blocks))))

;; This has to return either the parsed code or the errors from parsing.
(defn parse [asm]
  (let [source (prepare-source asm)
        parse-errors (vdt/validate source)]
    (if (= "" parse-errors)
      (let [code (get-code source)
            macros (get-macros source)
            data (get-data source)
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