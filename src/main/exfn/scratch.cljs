(ns main.exfn.scratch
  (:require [clojure.string :as str]
            [exfn.validators :as vdt]))

(def code-regex
  #"^(\w+:?)\s*('.+'|`.+`|:\w+\[:\w+\]|:\w+\[\d+\]|:\w+|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+)?\s*('.+'|`.+`|:\w+\[:\w+\]|:\w+\[\d+\]|:\w+|%\w+|[+-]?[0-9]*[.][0-9]*?|[.][0-9]+|\w+|\[.*\])?$")

(def matchers-and-formatters
  [[#"^:\w+$"                                (fn [a]           (keyword (subs a 1)))]             ; register
   [#"^(:\w+)\[(\d+|:\w+)?\]"                (fn [[_ reg idx]] {:register reg :index idx})]       ; register with array
   [#"^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$" (fn [[_ n]]       (js/Number n))]                    ; number
   [#"^('.+')$"                              (fn [[el _]]      (subs el 1 (dec (count el))))]     ; ' ' string
   [#"^(`.+`)$"                              (fn [[el _]]      (subs el 1 (dec (count el))))]     ; ` ` string
   [#"\w+"                                    keyword]])                                         ; label.

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

(defn get-macros [source]
  (let [macro-start (.indexOf source ".macros")
        macro-end   (.indexOf source ".code")
        macros      (->> source
                         (drop (inc macro-start))
                         (take (- (dec macro-end) macro-start))
                         (partition-by (fn [n] (= n "%end")))
                         (remove #(= '("%end") %)))]
    (zipmap (map (comp #(subs %1 1) first) macros)
            (map rest macros))))

(let [code [".macros" "%sum-and-square" "mul %1 %1" "mul %2 %2" "add %1 %2" "%end" "%add-ten" "add %1 10" "%end" ".code"]]
  (get-macros code))

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

(->>
 ".macros
    %print-nth
       call setordinal
       mov :s :msg1
       cat :s %2
       cat :s :ord
       cat :s :msg2
       cat :s %1
       prn :s
    %end
    %next-fib
        push %1
        add %1 %2
        pop %2
    %end
     %restart
          inc %1
          jmp start
     %end
     %check123
          cmp %1 1
          je st
         cmp %1 2
         je nd
         cmp %1 3
         je rd
     %end
.code
   prn `How many fibonacci numbers to print?`
   inp :tgt
start:
   cmp :ctr :tgt
   jg finish
   print-nth(:b, :ctr)
   next-fib(:a, :b)
   restart(:ctr)

finish:
   end

setordinal:
   ; if ctr is 1 then return 'st', if 2 return 'nd',
   ; if 3 return 'rd'
   check123(:ctr)

   ; if we got here, we are not = 1, 2 or 3. so need to 
   ; check if we are greater than 20
   cmp :ctr 20
   jg gt20
   mov :ord `th `
   ret
   gt20:
      ; we are greater than 20 so divide by 10
      ; and check if remainder is 1, 2 or 3
      mov :n :ctr
      rem :n 10 ; integer division
      check123(:n)
      mov :ord `th `
      ret
      st:
         mov :ord `st `
         ret
      nd:
         mov :ord `nd `
         ret
      rd:
         mov :ord `rd `
         ret

.data
   a 1
   b 1
   ctr 1
   msg1 `The `
   msg2 ` fibonacci number is `"
   (prepare-source)
   (get-blocks)
   :macros)


{"print-nth" ("call setordinal" "mov :s :msg1" "cat :s %2" "cat :s :ord" "cat :s :msg2" "cat :s %1" "prn :s"), "next-fib" ("push %1" "add %1 %2" "pop %2"), "restart" ("inc %1" "jmp start"), "check123" ("cmp %1 1" "je st" "cmp %1 2" "je nd" "cmp %1 3" "je rd")}
