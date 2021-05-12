(ns exfn.errors
  (:require [clojure.string :as str]))

(def invalid-macro-end ": Invalid macro end definition. Found %end without an opening macro definition.")
(def invalid-macro-definition ": Invalid macro start definition. Previous macro was not closed. Are you missing %end ?")

(defn has-no-arguments? [args]
  (prn "has-no-argumetns:: " args)
  (when (not (zero? (count args)))
    (str "should have no arguments.")))

(defn has-one-argument? [args]
  (when (not= (count args) 1)
    (str "should have no arguments.")))

(defn has-two-arguments? [args]
  (when (not= (count args) 2)
    (str "should have two arguments.")))

(defn first-argument-is-a-register? [is-macro? args]
  (cond (and is-macro? (not (re-find #"%\d+" (first args))))
        "first argument in a macro can't be a constant (value or string). Should have form %1, %2 etc"

        (and (not is-macro?) (not (str/starts-with? (first args) ":")))
        "first argument must be a register."

        :else
        nil))

(defn first-argument-is-a-label? [args]
  (when (not (str/starts-with? (first args) ":"))
    "first argument must be a jump label of form `foo`"))

(def rules
  {"mov"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-two-arguments?]}
   "mul"  {:macro-applicable     [first-argument-is-a-register?]
           :non-macro-applicable [has-two-arguments?]}
   "add"  {:macro-applicable     [first-argument-is-a-register?]
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
           :non-macro-applicable [has-one-argument?]}
   "jmp"  {:macro-applicable     []
           :non-macro-applicable [has-one-argument? first-argument-is-a-label?]}
   "cmp"  {:macro-applicable     []
           :non-macro-applicable [has-two-arguments?]}
   "jg"   {:macro-applicable     []
           :non-macro-applicable [has-one-argument? first-argument-is-a-label?]}
   "jge"  {:macro-applicable     []
           :non-macro-applicable [has-one-argument? first-argument-is-a-label?]}
   "jle"  {:macro-applicable     []
           :non-macro-applicable [has-one-argument? first-argument-is-a-label?]}
   "jl"   {:macro-applicable     []
           :non-macro-applicable [has-one-argument? first-argument-is-a-label?]}
   "je"   {:macro-applicable     []
           :non-macro-applicable [has-one-argument? first-argument-is-a-label?]}
   "jne"  {:macro-applicable     []
           :non-macro-applicable [has-one-argument? first-argument-is-a-label?]}
   "call" {:macro-applicable     []
           :non-macro-applicable [has-one-argument? ]}
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
   "rnz"   {:macro-applicable     []
           :non-macro-applicable [has-one-argument?]}
   "rlz"   {:macro-applicable     []
           :non-macro-applicable [has-one-argument?]}
   "rlez"   {:macro-applicable     []
            :non-macro-applicable [has-one-argument?]}
   "rgz"   {:macro-applicable     []
            :non-macro-applicable [has-one-argument?]}
   "rgez"   {:macro-applicable     []
             :non-macro-applicable [has-one-argument?]}})