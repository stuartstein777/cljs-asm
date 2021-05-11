(ns exfn.validators
  (:require [clojure.string :as str]))

;; this is the result of calling prepare-source
;; code is broken into lines.
(def sample-code '(".macros"
                   ;"mul %1 %1"
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
                   "square-and-sum(:a, :b)"
                   "mov :c 2"
                   "prn :b"
                   "call foo"
                   "mul :c :b"
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
                   "xyz 123"))

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

(defn parse-instruction [acc line-no code in-macro?]
  
  )

(defn verify [{:keys [current-section open-macro] :as acc} [line-no code]]
  #_(prn "current-section: " current-section ", open-macro: " open-macro ", line " line-no ": " code)
  (cond
    ; current line is the start of the macro definitions section. So mark us as in macro section.
    (or (= code ".macros") (= code ".code") (= code ".data"))
    (assoc acc :current-section (get-section code))

    ; current line is the start of the code definitions section. So mark us as in code section.
    #_(= code ".code")
    #_(assoc acc :current-section :code)

    ; current line is the start of the data definitions section. So mark us as in data section.
    #_(= code ".data")
    #_(assoc acc :current-section :data)

    ;; macro parsing --------------------------------------------------------------------------------------
    ;; TODO: Can move all this out to a macro parse function.
    ; if we are in a macro, and we hit a %end line, then close the current macro.
    (and (= :macro current-section) (= code "%end") open-macro)
    (assoc acc :open-macro false)

    ; if we are not in a macro, and we hit a %end line, then this is a syntax error, show error about how to start a macro definition..
    (and (= :macro current-section) (= code "%end") (not open-macro))
    (update acc :errors conj (str line-no ": Invalid macro end definition. Macros start definitions should be of the form %macro-name"))

    ; we are not in a macro definition and we hit a line that starts with %, so open a macro.
    (and (= :macro current-section) (str/starts-with? code "%") (not open-macro))
    (assoc acc :open-macro true)

    ; we are in a macro definition and hit a line that starts with %, assume its a new macro definition and show error about missing end.
    (and (= :macro current-section) (str/starts-with? code "%") (not open-macro))
    (update acc :errors conj (str line-no ": Invalid macro start definition. Previous macro was not closed. Are you missing %end ?"))

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
    #_(parse-instruction line-no code open-macro)
    acc

    ;; code parsing ---------------------------------------------------------------------------------------

    :else
    acc))

(defn validate [lines]
  ;; wwant line numbers with each line.
  (->> (map vector (range (count sample-code)) sample-code)
       (reduce verify {:current-section  nil
                       :open-macro  false
                       :errors           []})
       :errors))