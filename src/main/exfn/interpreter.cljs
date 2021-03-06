(ns exfn.interpreter)

;;=======================================================================================================
;; Returns 1 if n has an even number of 1s in its binary representation, otherwise 0.
;;=======================================================================================================
(defn get-parity [n]
  (if (even? (-> (filter #(= "1" %) (.toString n 2)) count)) 1 0))

;;=======================================================================================================
;; if x is a register, returns the value from the registers.
;; Otherwise return x (as it's a value not a register).
;;=======================================================================================================
(defn get-value [registers x]
  (if (keyword? x)
    (get registers x)
    x))

;;=======================================================================================================
;; An error is an error number and an error message.
;; This methods add both to the memory.
;;=======================================================================================================
(defn add-error [memory err-no err-msg]
  (-> memory
      (update-in [:internal-registers] assoc :err err-no)
      (update-in [:internal-registers] assoc :err-msg err-msg)))

;;=======================================================================================================
;; Return the predicate for cmp jumps that we want the jump check to satisfy.
;;
;; If jump is jge (jump if greater than or equal), then valid predicates for cmp are :eq (equal to) or :gt (greater than)
;; If jump is jge (jump if greater than), then valid predicates for cmp are :gt (greater than)
;; If jump is jne (jump if not equal), then valid predicates for cmp are :gt (greater than) or :lt (less than)
;; If jump is je (jump if equal), then valid predicates for cmp are :eq (equal to)
;; If jump is jle (jump if less than or equal), then valid predicates for cmp are :eq (equal to) or :lt (less than)
;; If jump is jl (jump if less than), then valid predicates for cmp are :lt (less than)
;;=======================================================================================================
(defn cmp-jump-predicates [jump-instruction]
  (condp = jump-instruction
    :jge #{:eq :gt}
    :jg  #{:gt}
    :jne #{:lt :gt}
    :je  #{:eq}
    :jle #{:eq :lt}
    :jl  #{:lt}))

;;=======================================================================================================
;; Update the existing output with the new line of output.
;;=======================================================================================================
(defn append-output [existing new]
  (cond (and existing new)
        (str existing "\n" new)

        (and existing (not new))
        existing

        :else
        new))

;;=======================================================================================================
;; MOV instruction
;;
;; Syntax:
;; mov a b
;;
;; Moves the contents of `b` (value or register) into register `a`
;; Increments eip to next instruction.
;;=======================================================================================================
(defn mov [{:keys [registers] :as memory} [a b]]
  (if (keyword a)
    (-> memory
        (update :registers assoc a (get-value registers b)))
    (add-error memory 101 "First argument to mov must be a register.")))

;;=======================================================================================================
;; PRN instruction
;;
;; Syntax:
;; prn a
;;
;; Appends the contents of a (string / number / register) to output
;; Increments eip to next instruction.
;;=======================================================================================================
(defn prnout [{:keys [output registers] :as memory} args]
  (-> memory
      (assoc :output (append-output output (get-value registers (first args))))))

;;=======================================================================================================
;; Gets the function to apply for the given instruction f.
;;=======================================================================================================
(defn get-math-fun [f]
  (condp = f
    :add +
    :sub -
    :div quot
    :mul *
    :xor bit-xor
    :rem rem
    :and bit-and
    :or bit-or))

;;=======================================================================================================
;; math instruction, covers add, sub, div, mul, xor, and, or
;;
;; Syntax:
;; instruction a b
;;
;; Performs the math operation for instruction passing `a` and `b` as args. Stores the result in `a`.
;; Sets the parity flag if result in `a` has even number of bits in its binary representation.
;;=======================================================================================================
(defn math-func [instruction {:keys [registers] :as memory} [a b]]
  (let [av (get-value registers a)
        bv (get-value registers b)]
    (cond
      (not (keyword? a))
      (add-error memory 202 (str "First argument to math operation " instruction " must be a register. Invalid {" a "}"))

      (or (not (number? av)) (not (number? bv)))
      (add-error memory 201 (str "Math operation " instruction " performed on non number arguments." a " and " b))

      :else
      (let [result ((get-math-fun instruction) av bv)]
        (-> memory
            (update-in [:registers] assoc a result)
            (update-in [:internal-registers] assoc :par (get-parity result)))))))

;;=======================================================================================================
;; cat instruction
;;
;; Syntax:
;; cat a b
;;
;; Concatentates two strings `a` (registers) and `b` (registers or string literal), stores the result in `a`
;; e.g.
;; 
;;     mov :a 'hello '
;;     mov :b 'world'
;;     cat :a :b
;; Will leave :a 'hello world'
;;
;; Increments eip to next instruction.
;;=======================================================================================================
(defn str-cat [{:keys [registers] :as memory} [a b]]
  (-> memory
      (update-in [:registers] assoc a (str (get-value registers a) (get-value registers b)))))

;;=======================================================================================================
;; inc instruction
;;
;; Syntax:
;; inc a
;;
;; Increments the value in registers `a`, stores the incremented value in `a`
;; e.g.
;; 
;;     mov :a 5
;;     inc :a
;; Will leave :a = 6
;;
;; Increments eip to next instruction.
;;=======================================================================================================
(defn increment [{:keys [registers] :as memory} [a]]
  (let [target (get-value registers a)]
    (if (number? target)
      (-> memory
          (update-in [:registers] assoc a (inc target))
          (update-in [:internal-registers] assoc :par (get-parity (inc target))))
      (add-error memory 203 "Can only increment numbers."))))

;;=======================================================================================================
;; dec instruction
;;
;; Syntax:
;; dec a
;;
;; Decrements the value in registers `a`, stores the decremented value in `a`
;; e.g.
;; 
;;     mov :a 5
;;     dec :a
;; Will leave :a = 4
;;
;; Increments eip to next instruction.
;;=======================================================================================================
(defn decrement [{:keys [registers] :as memory} [a]]
  (let [target (get-value registers a)]
    (if (number? target)
      (-> memory
          (update-in [:registers] assoc a (dec target))
          (update-in [:internal-registers] assoc :par (get-parity (dec target))))
      (add-error memory 204 "Can only decrement numbers."))))

;;=======================================================================================================
;; not instruction
;;
;; Syntax:
;; not a
;;
;; Performs bit-not on 'a', stores the result in 'a'
;; e.g.
;; 
;;     mov :a 5
;;     not :a
;; Will leave :a = -6
;;=======================================================================================================
(defn bitnot [{:keys [registers] :as memory} [a]]
  (if (number? (get-value registers a))
    (let [result (bit-not (get-value registers a))]
      (-> memory
          (update-in [:registers] assoc a result)
          (update-in [:internal-registers] assoc :par (get-parity result))
          (assoc :last-edit-register a)))
    (add-error memory 205 "not instruction argument not a number.")))

;;=======================================================================================================
;; strlen
;;
;; Syntax:
;; strlen a b
;;
;; Stores the length of string `b` in `a`
;;=======================================================================================================
(defn strlen [{:keys [registers] :as memory} [a b]]
  (let [v (get-value registers b)]
    (if (string? v)
      (-> memory
          (update-in [:registers] assoc a (count v)))
      (add-error memory 301 "len passed non-string argument."))))

;;=======================================================================================================
;; jnz instruction
;;
;; Syntax:
;; jnz a b
;;
;; Jumps `b` (number or register) instructions (positive or negative) if `a` (number or register) is not
;; zero.
;; If it is zero, then increments the eip.
;;=======================================================================================================
(defn jnz [{:keys [eip registers] :as memory} [a b]]
  (let [a (get-value registers a)
        b (get-value registers b)
        jmp (if (zero? a) 1 b)]
    (assoc memory :eip (+ eip jmp))))

;;=======================================================================================================
;; jz instruction
;;
;; Syntax:
;; jz a b
;;
;; Jumps `b` (number or register) instructions (positive or negative) if `a` (number or register) is
;; zero.
;; If it is not zero, then increments the eip.
;;=======================================================================================================
(defn jz [{:keys [eip registers] :as memory} [a b]]
  (let [a (get-value registers a)
        b (get-value registers b)
        jmp (if (not (zero? a)) 1 b)]
    (assoc memory :eip (+ eip jmp))))

;;=======================================================================================================
;; jmp instruction
;;
;; Syntax:
;; jmp a
;;
;; Moves the execution pointer to the label `a`.
;; It finds the address for `a` by loking up `a` in the symbol table.
;;=======================================================================================================
(defn jmp [{:keys [symbol-table] :as memory} [a]]
  (assoc memory :eip (get symbol-table a -2)))

;;=======================================================================================================
;; cmp instruction
;;
;; Syntax:
;; cmp a b (a and b can be values or registers.)
;;
;; Compares `a` and `b` and stores the result in the :cmp internal register.
;; A comparison can be :eq (a = b)
;;                     :gt (a > b)
;;                     :lt (a < b)
;; Increments the eip
;;=======================================================================================================
(defn cmp [{:keys [registers] :as memory} [a b]]
  (let [av (get-value registers a)
        bv (get-value registers b)]
    (-> memory
        (assoc-in [:internal-registers :cmp] (cond (= av bv) :eq
                                                   (> av bv) :gt
                                                   (< av bv) :lt)))))

;;=======================================================================================================
;; Handles jne, jg, jl, jle, jge, je
;;
;; jne - jumps to label if previous cmp call was not equal (greater-than or less-than)
;; je  - jumps to labe if previous cmp call was equal
;; jg  - jumps to label if previous cmp call was greater-than
;; jge - jumps to label if previous cmp call was greater-than or equal-to
;; jl  - jumps to label if previous cmp call was less-than
;; jle - jumps to label if previous cmp call was less-than or equal-to
;;
;; Syntax (syntax is same for all the supported jumps):
;; jne foo
;; 
;; jumps to the label foo if the result of the previous cmp call fufills jmp predicate
;;=======================================================================================================
(defn cmp-jmp [{:keys [eip internal-registers symbol-table] :as memory} jump-type [a]]
  (let [cmp              (:cmp internal-registers)
        valid-predicates (cmp-jump-predicates jump-type)]
    (assoc memory :eip (if (valid-predicates cmp)
                         (symbol-table (keyword a))
                         (inc eip)))))

;;=======================================================================================================
;; call instruction
;;
;; Syntax:
;; call foo
;;
;; Moves eip pointer to the label foo.
;; Pushes the eip at the call site to the eip-stack as a ret target.
;;=======================================================================================================
(defn call [{:keys [eip symbol-table] :as memory} [a]]
  (let [target (symbol-table (keyword a))]
    (if target
      (-> memory
          (update :eip-stack conj eip)
          (assoc :eip target))
      (-> memory
          (assoc :eip -3)))))

;;=======================================================================================================
;; ret instruction
;;
;; Syntax:
;; ret
;;
;; Moves eip pointer to the top eip on the eip-stack
;;=======================================================================================================
(defn ret [{:keys [eip-stack] :as memory}]
  (let [target (peek eip-stack)]
    (if target
      (-> memory
          (assoc :eip (inc (peek eip-stack)))
          (update :eip-stack pop))
      (-> memory
          (assoc :eip -4)))))



;;=======================================================================================================
;; pop instruction
;;
;; Syntax:
;; pop a
;;
;; Pops a value off the stack into register a.
;; Increments the eip
;;
;; ERR: Will set :err field to "Popped empty stack" if stack is empty.
;;=======================================================================================================
(defn pop-stack [{:keys [stack] :as memory} [a]]
  (cond (empty? stack)
        (add-error memory 401 "Popped empty stack.")

        (not (keyword? a))
        (add-error memory 402 "Invalid pop target.")

        :else
        (-> memory
            (assoc-in [:registers a] (peek stack))
            (update :stack (if (empty? stack) identity pop))
            (assoc :last-edit-register a))))

;;=======================================================================================================
;; push instruction
;;
;; Syntax:
;; push a
;;
;; Pushes a (register or value) onto the stack.
;;=======================================================================================================
(defn push [{:keys [registers] :as memory} args]
  (let [x (get-value registers (first args))]
    (-> memory
        (update :stack conj x))))

;;=======================================================================================================
;; rep instruction
;;
;; Syntax:
;; rep a
;; rep
;;
;; If a value `a` is supplied (number or register) then rep sets a rep-counter and pushes it to the
;; rp-stack, also pushes current eip to eip-stack (in this case it is an rp target)
;; If no args are supplied, it only pushes the current eip to the eip-stack (in this case its a conditional
;; rp target).
;; `a` should be greater than zero. If it's passed zero, it will still run through the loop once.
;;=======================================================================================================
(defn rep [{:keys [eip registers] :as memory} args]
  (if (seq args)
    (let [rep-ctr (get-value registers (first args))]
      (if (number? rep-ctr)
        (-> memory
            (update-in [:rep-counters-stack] conj rep-ctr)
            (update-in [:eip-stack] conj eip))
        (add-error memory 501 (str "Invalid argument {" rep-ctr "} to rep"))))
    (update-in memory [:eip-stack] conj eip)))

;;=======================================================================================================
;; cer instruction.
;;
;; Clears the error flags in :internal-registers
;;   :err
;;   :err-msg
;;=======================================================================================================
(defn cer [memory]
  (if (:internal-registers memory)
    (-> memory
        (update-in [:internal-registers] dissoc :err)
        (update-in [:internal-registers] dissoc :err-msg))
    memory))

;;=======================================================================================================
;; rp instruction
;;
;; Syntax:
;; rp
;;
;; Decrements the top item on the RP stack. If it would be zero after decrementing then it increments the
;; eip. Otherwise it sets eip to the top value of the eip-stack.
;;
;; Errors:
;; If the rep counters stack is empty the error flag is set to 502 and error message is set to
;; "rp called with empty rep counters stack"
;;=======================================================================================================
(defn rp [{:keys [eip-stack] :as memory}]
  (if (empty? (memory :rep-counters-stack))
    (add-error memory 502 "rp called with empty rep counters stack")
    (let [counter (peek (memory :rep-counters-stack))]
      (if (<= counter 1) ; decrementing would reduce it to zero, so increment eip and pop the rp-stack.
        (-> memory
            (update :rep-counters-stack pop)
            (update :eip-stack pop)
            (update :eip inc))
        (-> memory ; otherwise decrement the top item on the rp-stack and set eip to top value on eip-stack.
            (update :rep-counters-stack pop)
            (update :rep-counters-stack conj (dec counter))
            (assoc :eip (inc (peek eip-stack))))))))

;;=======================================================================================================
;; Get the conditional function for the r*z instructions.
;;=======================================================================================================
(defn get-conditional-repeat-function [f]
  (condp = f
    :rz (fn [x] (not (zero? x)))
    :rnz zero?
    :rlez (fn [x] (> x 0))
    :rlz (fn [x] (>= x 0))
    :rgz (fn [x] (<= x 0))
    :rgez (fn [x] (< x 0))))

;;=======================================================================================================
;; r*z conditional repeats
;;
;; Syntax:
;; rz a      Repeats until `a` is zero.
;; rnz a     Repeats until `a` is not zero.
;; rlez a    Repeat until `a` is less than or equal to zero.
;; rgez a    Repeat until `a` is greater than or equal to zero.
;; rgz a     Repeat until `a` is greater than zero.
;; rlz a     Repeat until `a` is less than zero.
;;
;; Errors:
;; If the argument to r*z is not a number then eip is icremented and error flag is set to 503 with
;; error message: {instruction} called with non numeric argument.
;;=======================================================================================================
(defn conditional-repeat [{:keys [eip-stack registers] :as memory} instruction [a]]
  (let [condition (get-conditional-repeat-function instruction)
        v (get-value registers a)]
    (if (number? v)
      (if (condition v)
        (-> memory
            (assoc :eip (inc (peek eip-stack))))
        (-> memory
            (update :eip inc)
            (update :eip-stack pop)))
      (-> memory
          (add-error 503 (str instruction " called with non numeric argument."))
          (update :eip inc)))))

;;=======================================================================================================
;; if eip = -1: jump past last instruction  
;;    eip = -2: jump to non existent label.
;;=======================================================================================================
(defn get-termination-cause [eip]
  (condp = eip
    -1 "Terminated: EIP jumped past last instruction."
    -2 "Terminated: Jump to non existant label."
    -3 "Called non-existent function."
    -4 "Encoutered ret, with no value on eip stack."
    "Program exited without :end"))

;;=======================================================================================================
;; The interpreter.
;;=======================================================================================================
(defn interpret [instructions {:keys [eip] :as memory}]
  (let [[instruction & args] (nth instructions eip)
        memory (cond (= :mov instruction)
                     (-> (mov memory args)
                         (assoc :last-edit-register (first args))
                         (update :eip inc))

                     (#{:add :sub :mul :div :xor :and :or :rem} instruction)
                     (-> (math-func instruction memory args)
                         (assoc :last-edit-register (first args))
                         (update :eip inc))

                     (= :cat instruction)
                     (-> (str-cat memory args)
                         (assoc :last-edit-register (first args))
                         (update :eip inc))

                     (= :cer instruction)
                     (-> (cer memory)
                         (update :eip inc))

                     (= :inc instruction)
                     (-> (increment memory args)
                         (assoc :last-edit-register (first args))
                         (update :eip inc))

                     (= :dec instruction)
                     (-> (decrement memory args)
                         (assoc :last-edit-register (first args))
                         (update :eip inc))

                     (= :not instruction)
                     (-> (bitnot memory args)
                         (update :eip inc))

                     (= :len instruction)
                     (-> (strlen memory args)
                         (assoc :last-edit-register (first args))
                         (update :eip inc))

                     (#{:nop :label} instruction)
                     (update memory :eip inc)

                     (= :prn instruction)
                     (-> (prnout memory args)
                         (update :eip inc))

                     (= :jnz instruction)
                     (let [{:keys [eip] :as memory} (jnz memory args)]
                       (if (> eip (count instructions))
                         (assoc memory :eip -1)
                         memory))

                     (= :jz instruction)
                     (let [{:keys [eip] :as memory} (jz memory args)]
                       (if (> eip (count instructions))
                         (assoc memory :eip -1)
                         memory))

                     (= :jmp instruction)
                     (let [{:keys [eip] :as memory} (jmp memory args)]
                       (if (> eip (count instructions))
                         (assoc memory :eip -1)
                         memory))

                     (= :cmp instruction)
                     (-> memory
                         (cmp args)
                         (update :eip inc))

                     (#{:jne :jg :je :jl :jle :jge} instruction)
                     (let [{:keys [eip] :as memory} (cmp-jmp memory instruction args)]
                       (if (> eip (count instructions))
                         (assoc memory :eip -1)
                         memory))

                     (= :call instruction)
                     (call memory args)

                     (= :ret instruction)
                     (ret memory)

                     (= :push instruction)
                     (-> memory
                         (push args)
                         (update :eip inc))

                     (= :pop instruction)
                     (-> memory
                         (pop-stack args)
                         (update :eip inc))

                     (= :rep instruction)
                     (-> memory
                         (rep args)
                         (update :eip inc))

                     (= :rp instruction)
                     (rp memory)

                     (#{:rnz :rz :rgz :rlz :rgez :rlez} instruction)
                     (conditional-repeat memory instruction args)

                     (= :inp instruction)
                     (-> memory
                         (update :eip inc))

                     (= :end instruction)
                     (-> memory
                         (update :eip inc))

                     :else
                     memory)
        terminated? (and (or (> (memory :eip) (dec (count instructions)))
                             (neg? (memory :eip)))
                         (not (= :end instruction)))
        finished? (= :end instruction)
        waiting-on-input? (= :inp instruction)]
    {:memory (cond-> memory
               terminated? (assoc :output (append-output (memory :output) (get-termination-cause (memory :eip))))
               finished?   (assoc :output (append-output (memory :output) "Exited.")))
     :terminated? terminated?
     :waiting-on-input? waiting-on-input?
     :finished? (or finished? terminated?)}))