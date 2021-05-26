(ns exfn.errors
  (:require [clojure.string :as str]))

(def invalid-macro-end ": Invalid macro end definition. Found %end without an opening macro definition.")
(def invalid-macro-definition ": Invalid macro start definition. Previous macro was not closed. Are you missing %end ?")

(defn has-no-arguments? [line]
  (when (not (re-seq  #"^(\w+)$" line))
    "does not expect arguments"))

(defn has-one-argument? [line]
  (when (not (re-seq #"^(\w+) [:%]?(\w+|'.+'|`.+`|\w+\[.*\]):?$" line))
    "should only have one argument."))

(comment (has-one-argument? "inc :a[2]"))

(defn has-two-arguments? [line]
  (when (not (re-seq #"^(\w+) ('.+'|`.+`|:\w+|%\w+|([0-9]+([.][0-9]*)?|[.][0-9]+)|\w+|:\w+\[.+\]) ('.+'|`.+`|:\w+|%\w+|[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)|\w+|\[.*\])$" line))
    "should have two arguments."))

;; if we are in a macro, then we want the first argument to start with a %,
;; if its not in a macro then we want the first argument to be a register.
(defn first-argument-is-a-register? [is-macro? line]
  (cond (and is-macro? (not (re-seq #"^(\w+)\s(:(\w+)|%(\w+))" line)))
        "first argument in a macro can't be a constant (value or string). Should have form %1, %2 etc"

        (and (not is-macro?) (not (re-seq #"^(\w+)\s:(\w+)" line)))
        "first argument must be a register. Registers start with a :"

        :else
        nil))

(defn one-argument-and-its-a-label [line]
  (when (not (re-seq #"^(\w+) ([a-zA-Z_][a-zA-Z0-9_]*)$" line))
    "expects one argument and it must be a label."))

(comment
  (one-argument-and-its-a-label "jne quax"))
(comment
  ; two arguments
  (re-seq #"^(\w+) (:\w+\s)('.+'|`.+`|:\w+|\d+)$" "mov :a :b")
  (re-seq #"^(\w+) (:\w+\s)('.+'|`.+`|:\w+|\d+)$" "mov :a")
  ; register followed by string:
  (rest (re-matches #"^(\w+) (:\w+\s)('.+'|`.+`)$" "mov :a 'foo bar'"))
  ; two registers.
  (rest (re-matches #"^(\w+) (:\w+)\s(:\w+)$" "mov :a :b"))
  ; one register
  (rest (re-matches #"^(\w+) (:\w+)$" "mov :a"))
  ; one argument and its a label, match things like jump foo:
  (rest (re-matches #"^(\w+) (\w+:)$" "jmp foo:"))
  ;one argument
  (rest (re-matches #"^(\w+) :?(\w+):?$" "jmp foo:"))
  ; no arguments
  (rest (re-matches #"^(\w+)$" "ret")))

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