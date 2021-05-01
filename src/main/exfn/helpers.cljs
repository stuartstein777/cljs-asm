(ns exfn.helpers
  (:require [clojure.string :as str]))

(defn get-source-line-numbers [source]
  (:line-nos (reduce (fn [{:keys [cur line-nos] :as acc} i]
                       (if (or (= "" i) (clojure.string/starts-with? i ";"))
                         (assoc acc :line-nos (str line-nos "\n"))
                         (-> acc
                             (assoc :line-nos (str line-nos cur "\n"))
                             (update :cur inc))))
                     {:cur 0 :line-nos ""}
                     (->> (str/split source #"\r?\n" -1)
                          (map str/trim)))))

(defn get-supported-instructions []
  [{:instruction "mov"
    :example      "mov :a :b"
    :description  "moves b (number or register) into register :a"}
   {:instruction "add"
    :example      "add :a :b"
    :description  "a + b (numbers or registers), result goes into :a"}
   {:instruction "sub"
    :example      "sub :a :b"
    :description  "a - b (numbers or registers), result goes into :a"}
   {:instruction "mul"
    :example      "mul :a :b"
    :description  "a * b (numbers or registers), result goes into :a"}
   {:instruction "div"
    :example      "div :a :b"
    :description  "a / b (numbers or registers), result goes into :a"}
   {:instruction "and"
    :example      "and :a :b"
    :description  "a ∧ b (numbers or registers), result goes into :a"}
   {:instruction "or"
    :example      "or :a :b"
    :description  "a ∨ b (numbers or registers), result goes into :a"}
   {:instruction "xor"
    :example      "xor :a :b"
    :description  "a ⊕ b (numbers or registers), result goes into :a"}
   {:instruction "dec"
    :example      "dec :a"
    :description  "Decrements the register :a by one"}
   {:instruction "inc"
    :example      "inc :a"
    :description  "Increments the register :a by one"}
   {:instruction "jnz"
    :example      "jnz :x :y"
    :description  "jumps y (number or register) instructions (positive or negative) if x (number or register) is not zero."}
   {:instruction "label"
    :example      "foo:"
    :description  "Creates a label foo: that can be used by jmp or call instructions. If encountered as an instruction it is ignored."}
   {:instruction "jmp"
    :example      "jmp foo"
    :description  "Moves the execution pointer to the label foo."}
   {:instruction "nop"
    :example      "nop"
    :description  "Does nothing."}
   {:instruction "cmp"
    :example      "cmp :x :y"
    :description  "compares x and y and stores the result in the internal register :cmp, result will either be x < y, x = y, x > y."}
   {:instruction  "jne"
    :example      "jne foo"
    :description  "jumps to the label foo if the result of the previous cmp call was that x /= y"}
   {:instruction  "jg"
    :example      "jg foo"
    :description  "jumps to the label foo if the result of the previous cmp call was that x > y"}
   {:instruction  "jge"
    :example      "jge foo"
    :description  "jumps to the label foo if the result of the previous cmp call was that x >= y"}
   {:instruction  "je"
    :example      "je foo"
    :description  "jumps to the label foo if the result of the previous cmp call was that x = y"}
   {:instruction  "jle"
    :example      "jle foo"
    :description  "jumps to the label foo if the result of the previous cmp call was that x <= y"}
   {:instruction  "jl"
    :example      "jl foo"
    :description  "jumps to the label foo if the result of the previous cmp call was that x < y"}
   {:instruction  "call"
    :example      "call foo"
    :description  "Moves the execution pointer to the label foo, pushes the current execution pointer onto the EIP stack so that it can be returned to by a ret instruction."}
   {:instruction "ret"
    :example      "ret"
    :description  "returns execution to the top execution pointer on the execution pointer stack. Results in popping eip stack."}
   {:instruction "end"
    :example      "end"
    :description  "terminates the program."}
   {:instruction "pop"
    :example      "pop :x"
    :description  "Pops the top value off the stack into register x"}
   {:instruction "push"
    :example      "push :x"
    :description  "Pushes x (value or register) onto the stack"}
   {:instruction "cat"
    :example      "cat :x y"
    :description  "Concatents the string in register x with the string y (where y is a register or literal string)"}
   {:instruction "comments"
    :example      "; foo"
    :description  "Comments are ignored, can be on own line or trailing, e.g. mov a b ; moves b into a"}])