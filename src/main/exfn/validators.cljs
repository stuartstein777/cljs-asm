(ns exfn.validators
  (:require [clojure.string :as str]
            [exfn.helpers :as h]
            [exfn.errors :as err]))

;; this is the result of calling prepare-source
;; code is broken into lines.

(defn has-two-arguments? [line]
  (if (= (count line) 3)
    ""
    (str (first line) " requires 2 arguments.")))

(defn has-one-argument? [line]
  (= (count line) 2))

(defn has-no-arguments? [line]
  (= (count line) 1))

(defn first-argument-is-a-register? [line]
  (str/starts-with? (second line) ":"))

(def rules {:mov [has-two-arguments? first-argument-is-a-register?]})

(defn get-section [code]
  (condp = code
    ".data" :data
    ".code" :code
    ".macros" :macro))

(defn join [suggestions]
  (condp = (count suggestions)
    1 (first suggestions)
    2 (str (first suggestions) " or " (second suggestions))
    (str (str/join ", " (butlast suggestions)) " or " (last suggestions))))

(defn build-error [invalid-instruction line-no replacements is-macro?]
  (let [suggestions (h/get-suggestions-for-invalid-instruction invalid-instruction replacements)]
    (if (> (count suggestions) 4)
      (str line-no ": Invalid " (if is-macro? "macro call `" "instruction `") invalid-instruction)
      (str line-no ": Invalid " (if is-macro? "macro call `" "instruction `") invalid-instruction "`. Did you mean " (join suggestions) "?"))))

;;=================================================================================================================
;; Want to check the macro call is a valid macro-call. That is, it is in the list of macro-names.
;; If it isn't then we need to get suggestions for possible macro-names (if possible).
;; If errors, return new acc with conj'd error.
;;
;; macro call with be off the form
;;    sum-and-square(:a, :b)
;;    add-ten(:a)
;;    foobar()
;;
;; Need to strip out everything before bracket to just sum-and-square
;; 
;; Otherwise return acc.
;;=================================================================================================================
(defn validate-macro-call [acc macro-names macro-call line-no]
  (let [macro-name (apply str (take-while #(not= \( %) macro-call))]
    (if (macro-names macro-name)
      acc
      (update acc :errors conj (build-error macro-call line-no macro-names true)))))

;;=================================================================================================================
;; validate-line
;;
;; Return new accumulator for the reduce. Want to set errors if line doesnt' validate.
;;=================================================================================================================
(defn validate-line [{:keys [in-macro?] :as acc} line-no code in-macro? macro-names]
  (let [[instruction & args] (str/split code #" ")]

    (cond
      ;; it looks like a macro call:
      (re-find #"(\(\)|(\(.+?\)))" code)
      (validate-macro-call acc macro-names code line-no)

      ;; handle if it looks like a label:
      (re-find #"\w+:" code)
      acc

      :else
      (if (not (h/valid-instructions instruction))
        (update acc :errors conj (build-error instruction line-no h/valid-instructions false))
        acc))))

(defn verify [{:keys [current-section open-macro macro-names] :as acc} [line-no code]]
  #_(prn "current-section: " current-section ", open-macro: " open-macro ", line " line-no ": " code)
  (cond
    ; current line is the start of the macro definitions section. So mark us as in macro section.
    (or (= code ".macros") (= code ".code") (= code ".data"))
    (assoc acc :current-section (get-section code))

    ;; macro parsing --------------------------------------------------------------------------------------
    ;; TODO: Can move all this out to a macro parse function.
    ; if we are in a macro, and we hit a %end line, then close the current macro.
    (and (= :macro current-section) (= code "%end") open-macro)
    (assoc acc :open-macro false)

    ; if we are not in a macro, and we hit a %end line, then this is a syntax error, show error about how to start a macro definition..
    (and (= :macro current-section) (= code "%end") (not open-macro))
    (update acc :errors conj (str line-no err/invalid-macro-end))

    ; we are not in a macro definition and we hit a line that starts with %, so open a macro.
    (and (= :macro current-section) (str/starts-with? code "%") (not open-macro))
    (-> acc
        (assoc :open-macro true)
        (update :macro-names conj (subs code 1)))

    ; we are in a macro definition and hit a line that starts with %, assume its a new macro definition and show error about missing end.
    ; wont be an end, the earlier case would case catch that.
    (and (= :macro current-section) (str/starts-with? code "%") open-macro)
    (update acc :errors conj (str line-no err/invalid-macro-definition))

    ; we are in a macro section, but not an open macro and we hit a line that doesnt start with %, assume its an instruction and show
    ; error about adding a macro start definition.
    (and (= :macro current-section) (not (str/starts-with? code "%")) (not open-macro))
    (update acc :errors conj (str line-no ": Any instructions in a macro section should be between a macro definition. Macro definitions start with %macro-name and end with %end\n"
                                  "e.g.\n"
                                  "%square-and-sum\n"
                                  "   mul %1 %1\n"
                                  "   mul %2 %2\n"
                                  "   add %1 %2\n"
                                  "%end"))

    ;; in a macro, line doesnt start with % so its an instruction. Parse the instruction.
    (and (= :macro current-section) (not (str/starts-with? code "%")) open-macro)
    (validate-line acc line-no code true macro-names)

    (= :code current-section)
    (validate-line acc line-no code false macro-names)

    ;; code parsing ---------------------------------------------------------------------------------------

    :else
    acc))

(def sample-code '(".macros"
                   "%square-and-sum"
                   "mul %1 %1"
                   "muk %2 %2"
                   "add %1 %2"
                   "%end"
                   "%add-ten"
                   "add %1 10"
                   "%end"
                   ".code"
                   "mov :a 0"
                   "mov :b 1"
                   "squareandsub(:a, :b)"
                   "mov :c 2"
                   "prn :b"
                   "call foo"
                   "mul :c :b"
                   "cmp :a :b"
                   "ffne quax"
                   "mul :c 10"
                   "quax:"
                   "nop"
                   "call bar"
                   "pop :d"
                   "pop :e"
                   "prn :d"
                   "prn :e"
                   "xor :b :b"
                   "end" "foo:"
                   "inc :b"
                   "xxx"
                   "ret"
                   "bar:"
                   "add :a 7"
                   "sub :c 1"
                   "push 3"
                   "push 4"
                   "wet"
                   ".data"
                   "xyz 123"))

(defn validate [code]
  ;; want line numbers with each line.
  (->> (map vector (range (count sample-code)) sample-code)
       (reduce verify {:current-section nil
                       :errors          []
                       :macro-names     #{}
                       :open-macro      false})
       :errors))