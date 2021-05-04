(ns exfn.interpreter)

;;=======================================================================================================
;; if x is a register, returns the value from the registers.
;; Otherwise return x (as it's a value not a register).
;;=======================================================================================================
(defn get-value [registers x]
  (if (keyword? x)
    (get registers x)
    x))

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

;;=======================================================================================================
;; Update the existing output with the new line of output.
;;=======================================================================================================
(defn append-output [existing new]
  (cond (and existing new)
        (str existing "\n" new)

        (and existing (not new))
        existing

        (= "" existing)
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
  (-> memory
      (update-in [:registers] assoc a (get-value registers b))
      (assoc :last-edit-register a)
      (update :eip inc)))

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
      (assoc :output (append-output output (get-value registers (first args))))
      (update :eip inc)))

;;=======================================================================================================
;; ADD instruction
;;
;; Syntax:
;; add a b
;;
;; Adds `a` (register) and `b` (number or register) and stores the result in `a`
;; Increments eip to next instruction.
;;=======================================================================================================
(defn add [{:keys [registers] :as memory} [a b]]
  (-> memory
      (update-in [:registers] assoc a (+ (get-value registers a) (get-value registers b)))
      (update :eip inc)))

;;=======================================================================================================
;; SUB instruction
;;
;; Syntax:
;; sub a b
;;
;; Subtracts `b` (number or register) from `a` (register) and stores the result in `a`
;; Increments eip to next instruction.
;;=======================================================================================================
(defn sub [{:keys [registers] :as memory} [a b]]
  (-> memory
      (update-in [:registers] assoc a (- (get-value registers a) (get-value registers b)))
      (update :eip inc)))

;;=======================================================================================================
;; MUL instruction
;;
;; Syntax:
;; mul a b
;;
;; Multiplies `a` and `b` (number or register) and stores the result in `a`
;; e.g.
;; 
;;     mov :a 10
;;     mov :b 5
;;     mul :a :b
;; Will leave :a = 50
;;
;; Increments eip to next instruction.
;;=======================================================================================================
(defn mul [{:keys [registers] :as memory} [a b]]
  (-> memory
      (update-in [:registers] assoc a (* (get-value registers a) (get-value registers b)))
      (update :eip inc)))

;;=======================================================================================================
;; DIV instruction
;;
;; Syntax:
;; div a b
;;
;; Divides (Integer division) `a` and `b` (number or register) and stores the result in `a`
;; e.g.
;; 
;;     mov :a 10
;;     mov :b 5
;;     div :a :b
;; Will leave :a = 2
;;
;; Increments eip to next instruction.
;;=======================================================================================================
(defn div [{:keys [registers] :as memory} [a b]]
  (-> memory
      (update-in [:registers] assoc a (quot (get-value registers a) (get-value registers b)))
      (update :eip inc)))

;;=======================================================================================================
;; XOR instruction
;;
;; Syntax:
;; xor a b
;;
;; Bit xor `a` and `b` (number or register) and stores the result in `a`
;; e.g.
;; 
;;     mov :a 10
;;     mov :b 5
;;     xor :a :b
;; Will leave :a = 15
;;
;; Increments eip to next instruction.
;;=======================================================================================================
(defn xor [{:keys [registers] :as memory} [a b]]
  (-> memory
      (update-in [:registers] assoc a (bit-xor (get-value registers a) (get-value registers b)))
      (update :eip inc)))

;;=======================================================================================================
;; AND instruction
;;
;; Syntax:
;; and a b
;;
;; Bit-and `a` and `b` (number or register) and stores the result in `a`
;; e.g.
;; 
;;     mov :a 1
;;     mov :b 1
;;     and :a :b
;; Will leave :a = 1
;;
;; Increments eip to next instruction.
;;=======================================================================================================
(defn bitand [{:keys [registers] :as memory} [a b]]
  (-> memory
      (update-in [:registers] assoc a (bit-and (get-value registers a) (get-value registers b)))
      (update :eip inc)))

;;=======================================================================================================
;; OR instruction
;;
;; Syntax:
;; or a b
;;
;; Bit-or `a` and `b` (number or register) and stores the result in `a`
;; e.g.
;; 
;;     mov :a 1
;;     mov :b 0
;;     or :a :b
;; Will leave :a = 1
;;
;; Increments eip to next instruction.
;;=======================================================================================================
(defn bitor [{:keys [registers] :as memory} [a b]]
  (-> memory
      (update-in [:registers] assoc a (bit-and (get-value registers a) (get-value registers b)))
      (update :eip inc)))

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
      (update-in [:registers] assoc a (str (get-value registers a) (get-value registers b)))
      (update :eip inc)))

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
  (-> memory
      (update-in [:registers] assoc a (inc (get-value registers a)))
      (update :eip inc)))

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
  (-> memory
      (update-in [:registers] assoc a (dec (get-value registers a)))
      (update :eip inc)))

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
;;
;; Increments eip to next instruction.
;;=======================================================================================================
(defn bitnot [{:keys [registers] :as memory} [a]]
  (-> memory
      (update-in [:registers] assoc a (bit-not (get-value registers a)))
      (update :eip inc)))

;;=======================================================================================================
;; nop instruction
;;
;; Syntax:
;; nop
;;
;; Does nothing. Label instructions are treated as nops.
;; Increments eip to next instruction.
;;=======================================================================================================
(defn nop [memory]
  (update memory :eip inc))

;;=======================================================================================================
;; nop instruction
;;
;; Syntax:
;; nop
;;
;; Does nothing.
;; Increments eip to next instruction.
;;=======================================================================================================
(defn strlen [{:keys [registers] :as memory} [a b]]
  (-> memory
      (update-in [:registers] assoc a (count (get-value registers b)))
      (update :eip inc)))

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
  (let [jmp (if (zero? (get-value registers a)) 1 (get-value registers b))]
    (assoc memory :eip (+ eip jmp))))

;;=======================================================================================================
;; jmp instruction
;;
;; Syntax:
;; jmp a
;;
;; Moves the execution pointer to the label `a`.
;; It finds the address for `a` by loking up `a` in the symbol tale.
;;=======================================================================================================
(defn jmp [{:keys [symbol-table] :as memory} [a]]
  (assoc memory :eip (get symbol-table a)))

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
                                                   (< av bv) :lt))
        (update :eip inc))))

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
    (prn cmp)
    (prn valid-predicates)
    (assoc memory :eip (if (valid-predicates cmp)
                         (symbol-table (keyword a))
                         (inc eip)))))


(comment (cmp-jmp {:eip 3 :internal-registers {:cmp :lt} :symbol-table {:foo 5}} :jne [:foo]))

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
    (-> memory
        (update :eip-stack conj eip)
        (assoc :eip target))))

;;=======================================================================================================
;; ret instruction
;;
;; Syntax:
;; ret
;;
;; Moves eip pointer to the top eip on the eip-stack
;;=======================================================================================================
(defn ret [{:keys [eip-stack] :as memory}]
  (-> memory
      (assoc :eip (inc (peek eip-stack)))
      (update :eip-stack pop)))

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
  (if (empty? stack)
    (-> memory
        (update-in [:internal-registers] assoc :err "Popped empty stack.")
        (update :eip inc))
    (-> memory
        (assoc-in [:registers a] (peek stack))
        (update :stack (if (empty? stack) identity pop))
        (assoc :last-edit-register a)
        (update :eip inc))))

;;=======================================================================================================
;; push instruction
;;
;; Syntax:
;; push a
;;
;; Pushes a (register or value) onto the stack.
;; Increments the eip
;;=======================================================================================================
(defn push [{:keys [registers] :as memory} args]
  (let [x (get-value registers (first args))]
    (-> memory
        (update :stack conj x)
        (update :eip inc))))

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
    (-> memory
        (update-in [:rep-counters-stack] conj (get-value registers (first args)))
        (update-in [:eip-stack] conj eip)
        (update :eip inc))
    (-> memory
        (update-in [:eip-stack] conj eip)
        (update :eip inc))))

;;=======================================================================================================
;; rp instruction
;;
;; Syntax:
;; rp
;;
;; Decrements the top item on the RP stack. If it would be zero after decrementing then it increments the
;; eip. Otherwise it sets eip to the top value of the eip-stack.
;;=======================================================================================================
(defn rp [{:keys [eip-stack] :as memory}]
  (let [counter (peek (memory :rep-counters-stack))]
    (if (<= counter 1) ; decrementing would reduce it to zero, so increment eip and pop the rp-stack.
      (-> memory
          (update :rep-counters-stack pop)
          (update :eip-stack pop)
          (update :eip inc))
      (-> memory ; otherwise decrement the top item on the rp-stack and set eip to top value on eip-stack.
          (update :rep-counters-stack pop)
          (update :rep-counters-stack conj (dec counter))
          (assoc :eip (inc (peek eip-stack)))))))

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
;;=======================================================================================================
(defn conditional-repeat [{:keys [eip-stack registers] :as memory} instruction [a]]
  (let [condition (get-conditional-repeat-function instruction)]
    (if (condition (get-value registers a))
      (-> memory
          (assoc :eip (inc (peek eip-stack))))
      (-> memory
          (update :eip inc)
          (update :eip-stack pop)))))

;;=======================================================================================================
;; The interpreter.
;;=======================================================================================================
(defn interpret [instructions {:keys [eip] :as memory}]
  (let [[instruction & args] (nth instructions eip)
        memory (cond (= :mov instruction)
                     (mov memory args)

                     (= :add instruction)
                     (add memory args)

                     (= :sub instruction)
                     (sub memory args)

                     (= :mul instruction)
                     (mul memory args)

                     (= :div instruction)
                     (div memory args)

                     (= :xor instruction)
                     (xor memory args)

                     (= :and instruction)
                     (bitand memory args)

                     (= :or instruction)
                     (bitor memory args)

                     (= :cat instruction)
                     (str-cat memory args)

                     (= :inc instruction)
                     (increment memory args)

                     (= :dec instruction)
                     (decrement memory args)

                     (= :not instruction)
                     (bitnot memory args)

                     (= :len instruction)
                     (strlen memory args)

                     (= :nop instruction)
                     (nop memory)

                     (= :prn instruction)
                     (prnout memory args)

                     (= :jnz instruction)
                     (jnz memory args)

                     (= :label instruction)
                     (nop memory)

                     (= :jmp instruction)
                     (jmp memory args)

                     (= :cmp instruction)
                     (cmp memory args)

                     (#{:jne :jg :je :jl :jle :jge} instruction)
                     (cmp-jmp memory instruction args)

                     (= :call instruction)
                     (call memory args)

                     (= :ret instruction)
                     (ret memory)

                     (= :push instruction)
                     (push memory args)

                     (= :pop instruction)
                     (pop-stack memory args)

                     (= :rep instruction)
                     (rep memory args)

                     (= :rp instruction)
                     (rp memory)

                     (#{:rnz :rz :rgz :rlz :rgez :rlez} instruction)
                     (conditional-repeat memory instruction args)

                     :else
                     memory)]
    {:memory memory
     :finished? (or (= :end instruction) (> (memory :eip) (count instructions)))}))

