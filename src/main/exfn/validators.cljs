(ns exfn.validators
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [exfn.helpers :as h]))

(def invalid-macro-end ": Invalid macro end definition. Found %end without an opening macro definition.")
(def invalid-macro-definition ": Invalid macro start definition. Previous macro was not closed. Are you missing %end ?")

;;=================================================================================================================
;; checks the intstruction has no arguments.
;; returns nil if the line is valid.
;;=================================================================================================================
(defn has-no-arguments? [line]
  (when (not (re-seq  #"^(\w+)\s*?$" line))
    "does not expect arguments"))

;;=================================================================================================================
;; Checks that the instruction has only one argument.
;; e.g `inc :a`
;; Should allow extraneous spaces between instruction and argument
;; e.g. `inc     :a` should be valid. 
;;
;; if line is valid, return nil.
;;=================================================================================================================
(defn has-one-argument? [line]
  (when (not (re-seq #"^\s*?(\w+)\s+[:%]?(\w+|'.+'|`.+`):?$" line))
    "should only have one argument."))

;;=================================================================================================================
;; Checks if the instruction has two arguments.
;; e.g. mov :a :b
;;    if we are in a macro, then we want the first argument to start with a %,
;;    if its not in a macro then we want the first argument to be a register, so starts with a :
;; Should allow extraneous spaces betewen instruction and argument or between arguments,
;; e.g. "mov     :a     :b   " should be valid.
;;
;; if line is valid, return nil.
;;=================================================================================================================
(defn has-two-arguments? [line]
  (when (not (re-seq #"^(\w+)\s+('.+'|`.+`|:\w+|%\w+|([0-9]+([.][0-9]*)?|[.][0-9]+)|\w+)\s+('.+'|`.+`|:\w+|%\w+|[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)|\w+)$" line))
    "should have two arguments."))

;;=================================================================================================================
;; Checks if the first argument to the line is a register:
;;    if we are in a macro, then we want the first argument to start with a %,
;;    if its not in a macro then we want the first argument to be a register, so starts with a :
;;
;; if line is valid, return nil.
;;=================================================================================================================
(defn first-argument-is-a-register? [is-macro? line]
  (cond (and is-macro? (not (re-seq #"^\s*(\w+)\s+(:(\w+)|%(\w+))" line)))
        "first argument in a macro can't be a constant (value or string). Should have form %1, %2 etc"

        (and (not is-macro?) (not (re-seq #"^\s*(\w+)\s+:(\w+)" line)))
        "first argument must be a register. Registers start with a :"

        :else
        nil))

;;=================================================================================================================
;; This checks that the line has the form:
;; 'instruction label'
;; Any extraneous spaces are removed. e.g.
;; 'instruction     label' should be valid.
;; Labels should not start with a digit, e.g.
;; 'instruction 9label' should be invalid.
;;
;; if line is valid, return nil.
;;=================================================================================================================
(defn one-argument-and-its-a-label [line]
  (when (not (re-seq #"^(\w+)\s+([a-zA-Z_][a-zA-Z0-9_]*)$" line))
    "expects one argument and it must be a label. Labels can not start with a digit."))

;;=================================================================================================================
;; The rules for each instruction.
;;=================================================================================================================
(def rules
  {"mov"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-two-arguments?]}
   "mul"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-two-arguments?]}
   "add"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-two-arguments?]}
   "rem"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-two-arguments?]}
   "sub"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-two-arguments?]}
   "xor"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-two-arguments?]}
   "div"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-two-arguments?]}
   "and"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-two-arguments?]}
   "or"   {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-two-arguments?]}
   "inc"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-one-argument?]}
   "dec"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-one-argument?]}
   "cat"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-two-arguments?]}
   "cer"  {:macro-applicable     []
           :non-macro-applicable [has-no-arguments?]}
   "not"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-one-argument?]}
   "len"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-two-arguments?]}
   "nop"  {:macro-applicable     []
           :non-macro-applicable [has-no-arguments?]}
   "prn"  {:macro-applicable     []
           :non-macro-applicable [has-one-argument?]}
   "jnz"  {:macro-applicable     []
           :non-macro-applicable [has-two-arguments?]}
   "jz"   {:macro-applicable     []
           :non-macro-applicable [has-two-arguments?]}
   "jmp"  {:macro-applicable     []
           :non-macro-applicable [one-argument-and-its-a-label]}
   "cmp"  {:macro-applicable     []
           :non-macro-applicable [has-two-arguments?]}
   "jg"   {:macro-applicable     []
           :non-macro-applicable [one-argument-and-its-a-label]}
   "jge"  {:macro-applicable     []
           :non-macro-applicable [one-argument-and-its-a-label]}
   "jle"  {:macro-applicable     []
           :non-macro-applicable [one-argument-and-its-a-label]}
   "jl"   {:macro-applicable     []
           :non-macro-applicable [one-argument-and-its-a-label]}
   "je"   {:macro-applicable     []
           :non-macro-applicable [one-argument-and-its-a-label]}
   "jne"  {:macro-applicable     []
           :non-macro-applicable [one-argument-and-its-a-label]}
   "call" {:macro-applicable     []
           :non-macro-applicable [has-one-argument?]}
   "ret"  {:macro-applicable     []
           :non-macro-applicable [has-no-arguments?]}
   "end"  {:macro-applicable     []
           :non-macro-applicable [has-no-arguments?]}
   "pop"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-one-argument?]}
   "push" {:macro-applicable     []
           :non-macro-applicable [has-one-argument?]}
   "rp"   {:macro-applicable     []
           :non-macro-applicable [has-no-arguments?]}
   "rz"   {:macro-applicable     []
           :non-macro-applicable [has-one-argument?]}
   "rnz"  {:macro-applicable     []
           :non-macro-applicable [has-one-argument?]}
   "rlz"  {:macro-applicable     []
           :non-macro-applicable [has-one-argument?]}
   "rlez" {:macro-applicable     []
           :non-macro-applicable [has-one-argument?]}
   "rgz"  {:macro-applicable     []
           :non-macro-applicable [has-one-argument?]}
   "rgez" {:macro-applicable     []
           :non-macro-applicable [has-one-argument?]}
   "inp" {:macro-applicable     [first-argument-is-a-register?]
          :non-macro-applicable [has-one-argument?]}})

;;=================================================================================================================
;; validate-instruction
;;
;; runs an instructions ruleset against the supplied args and collects the error(s).
;;=================================================================================================================
(defn validate-instruction [instr line is-macro?]
  (let [{:keys [macro-applicable non-macro-applicable]} (rules instr)
        applied  (map #(partial % is-macro?) macro-applicable)
        all-rules (apply conj applied non-macro-applicable)]
    (keep (fn [f]
            (let [error (f line)]
              (when error
                (str "Invalid `" instr "` call, `" instr "` " error)))) all-rules)))

;;=================================================================================================================
;; get-section
;;
;; maps the source . sections to a keyword.
;;=================================================================================================================
(defn get-section [code]
  (condp = code
    ".data" :data
    ".code" :code
    ".macros" :macro))

;;=================================================================================================================
;; join
;;
;; joins a number of strings in the form:
;;  foo, bar or quax
;;  foo or bar
;;  foo
;;=================================================================================================================
(defn join [suggestions]
  (condp = (count suggestions)
    1 (first suggestions)
    2 (str (first suggestions) " or " (second suggestions))
    (str (str/join ", " (butlast suggestions)) " or " (last suggestions))))

;;=================================================================================================================
;; build-error
;;
;; Takes an invalid instruction, the line number, is-macro? determining if this is a macro-call or not, and the
;; possible replacements, for regular instructions these will be the valid instructions. If it's a macro call then
;; the replacements would be parsed macros.
;; If we have more than 4 suggestions then don't suggest anything as this means we didn't really match on anything.
;;=================================================================================================================
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
;; Also want to check the arguments, e.g. foo(:a, :b) but foo macro expects %1 %2 %3 then this should be an error.
;; \w+(\(.*\))
;;\( (.*?) \)
;; Otherwise return acc.
;;=================================================================================================================
(defn validate-macro-call [acc macro-names macro-call line-no]
  (let [macro-name (str/trim (apply str (take-while #(not= \( %) macro-call)))]
    (if (macro-names macro-name)
      acc
      (update acc :errors conj (build-error macro-call line-no macro-names true)))))

;;=================================================================================================================
;;
;; Take a line of code that calls a macro, e.g.
;;    foo(:a, :b)
;; and from acc, get the number of expected args for that macro. This is the macro key in acc. 
;;    {"foo" #{%1 %2 %3}}
;; 
;; And checks the args (:a, :b) count is equal to the expected count in the definition.
;; In this case it would raise an error as foo expects 3, but is only called with 2.
;;=================================================================================================================
(defn validate-macro-call-args [acc code line-no]
  (let [whitespace-stripped (str/replace code #" " "")
        macro-name (str/trim (apply str (take-while #(not= \( %) whitespace-stripped)))
        expected-macro-args (-> acc :macros (get macro-name))
        expected-macro-call-count (count expected-macro-args)
        macro-call-arg-count (as-> whitespace-stripped _
                               (re-seq #"\((.*?)\)" _)
                               (first _)
                               (rest _)
                               (first _)
                               (str/split _ ",")
                               (remove #(= % "") _)
                               (count _))]
    (if (not= expected-macro-call-count macro-call-arg-count)
      (update acc :errors conj (str line-no ": Invalid call to macro " macro-name ". "
                                    macro-name " expects " expected-macro-call-count " [ "
                                    (str/join "," (-> acc
                                                      :macros
                                                      (get macro-name)
                                                      (sort)))
                                    " ] arguments. You have called it with " macro-call-arg-count))
      acc)))

;;=================================================================================================================
;; validate-line
;;
;; Return new accumulator for the reduce. Want to set errors if line doesnt' validate.
;;=================================================================================================================
(defn validate-line [{:keys [open-macro macros] :as acc} line-no code]
  (let [instruction (first (str/split code #" "))
        macro-names (set (keys macros))]
    (cond
      ;; it looks like a macro call:
      (re-find #"(\(\)|(\(.+?\)))" code)
      (-> acc
          (validate-macro-call macro-names code line-no)
          (validate-macro-call-args code line-no))

      ;; handle if it looks like a label:
      (re-find #"\w+:" code)
      acc

      ;; else its a regular instruction (but may be an instruction in a macro):
      :else
      (if (not (h/valid-instructions instruction))
        ; if its invalid instruction, then add error.
        (update acc :errors conj (build-error instruction line-no h/valid-instructions false))
        ;else, validate the instruction has the correct arguments and types.
        (let [errors (->> (validate-instruction instruction code open-macro)
                          (map #(str line-no ": " %)))]
          ; if we got any errors add them to the errors key. Otherwise just return the acc unchanged.
          (if (seq errors)
            (update acc :errors #(apply conj %1 %2) errors)
            acc))))))

;;=================================================================================================================
;; macros is a vector of macro:
;;   {:name "macro-name"
;;    :args #{%1 %2 %3}}
;; we want to add any argument that is %n to the macro
;;=================================================================================================================
(defn add-macro-arguments [acc code current-macro]
  (let [param-args (map first (re-seq #"(%\d+)" (-> code
                                                    (str/replace  #"`.*?`|'.*?'" " ")
                                                    (str/trim))))]
    (update-in acc [:macros current-macro] set/union (set param-args))))


(comment
  (set/union #{} (set (list)))
  (update-in {:macros {:foo #{} :bar #{:d :e}}}
          [:macros :bar] set/union (set '(:a :b :c))))

;;=================================================================================================================
;; verify
;;
;; This is the reducing function for the verifier.
;;
;; It accumulates a current parsing state.
;; If we hit a .macro, .code or .data  line then this means we are parsing macros, so the accumulator flag
;; :current-section is set.
;; If we hit a macro definition then we need to set the :open-macro flag to true.
;; If we hit an end macro definition then we need to se the :open-macro flag to false. etc
;; See the comments at each cond for more details.
;;=================================================================================================================
(defn verify [{:keys [current-section open-macro macros] :as acc} [line-no code]]
  (cond
    ; current line is the start of the macro definitions section. So mark us as in macro section.
    (or (= code ".macros") (= code ".code") (= code ".data"))
    (assoc acc :current-section (get-section code))

    ;; TODO: Can move all this out to a macro parse function.
    ; if we are in a macro, and we hit a %end line, then close the current macro.
    (and (= :macro current-section) (= code "%end") open-macro)
    (-> acc
        (assoc :open-macro false)
        (assoc :current-macro nil))

    ; if we are not in a macro, and we hit a %end line, then this is a syntax error, show error about how to start a macro definition..
    (and (= :macro current-section) (= code "%end") (not open-macro))
    (update acc :errors conj (str line-no invalid-macro-end))

    ; we are not in a macro definition and we hit a line that starts with %, so open a macro.
    (and (= :macro current-section) (str/starts-with? code "%") (not open-macro))
    (let [current-macro (subs code 1)]
      (-> acc
          (assoc :open-macro true)
          (assoc :current-macro current-macro)
          (assoc-in [:macros current-macro] #{})))

    ; we are in a macro definition and hit a line that starts with %, assume its a new macro definition and show error about missing end.
    ; wont be an end, the earlier case would case catch that.
    (and (= :macro current-section) (str/starts-with? code "%") open-macro)
    (update acc :errors conj (str line-no invalid-macro-definition))

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
    (-> acc
        (add-macro-arguments code (:current-macro acc))
        (validate-line line-no code))

    (= :code current-section)
    (validate-line acc line-no code)

    :else
    acc))

;;=================================================================================================================
;; validate
;; 
;; This function takes prepared source.
;;=================================================================================================================
(defn validate [code]
  (->> (map vector (iterate inc 0) code)
       (reduce verify
               {:current-section nil
                :errors          []
                :macros          {}
                :open-macro      false})
       :errors
       (str/join "\n")))

(comment (validate '(".macros"
                     "%square-and-sum"
                     "mul %1 %1"
                     "mul %2 %2"
                     "add %1 %2"
                     "%end"
                     "%add-ten"
                     "add %1 10"
                     "%end"
                     ".code"
                     "mov :a 0"
                     "mov :b 1"
                     "square-and-sum (:a, :b)"
                     "mov :c 'foo bar'"
                     "prn :b"
                     "call foo"
                     "mul :c 5"
                     "cmp :a :b"
                     "jne quax"
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
                     "ret"
                     "bar:"
                     "add :a 7"
                     "sub :c 1"
                     "push 3"
                     "push 4"
                     "ret"
                     ".data"
                     "xyz 123")))