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
;; Handle the mov instruction.
;;=======================================================================================================
(defn mov [registers x y]
  (assoc registers x (get-value registers y)))

;;=======================================================================================================
;; Handle unary operations
;;=======================================================================================================
(defn unary-op [registers op x]
  (update registers x op))

;;=======================================================================================================
;; Handle binary operations
;;=======================================================================================================
(defn binary-op [registers op x y]
  (assoc registers x (op (get registers x) (get-value registers y))))

;;=======================================================================================================
;; Return the predicate for cmp jumps that we want the jump check to satisfy.
;;=======================================================================================================
(defn cmp-jump-predicates [jump-instruction]
  (js/console.log "jump-instruction" jump-instruction)
  (cond (= :jge jump-instruction) #{:eq :gt}
        (= :jg  jump-instruction) #{:gt}
        (= :jne jump-instruction) #{:lt :gt}
        (= :je  jump-instruction) #{:eq}
        (= :jle jump-instruction) #{:eq :lt}
        (= :jl  jump-instruction) #{:lt}))

;;=======================================================================================================
;; Return the appropriate binary operation for the given binary instruction.
;;=======================================================================================================
(defn get-binary-operations [instruction]
  (cond (= :add instruction) +
        (= :sub instruction) -
        (= :mul instruction) *
        (= :div instruction) quot
        (= :xor instruction) bit-xor
        (= :or  instruction)  bit-or
        (= :and instruction) bit-and))

;;=======================================================================================================
;; Return the appropriate unary operation for the given unary instruction.
;;=======================================================================================================
(defn get-unary-operation [instruction]
  (cond (= :inc instruction) inc
        (= :dec instruction) dec))

;;=======================================================================================================
;; jump forward or backwards y steps if x is not zero.
;; x and y can both be registers so we get their value via (get-value).
;;=======================================================================================================
(defn jnz [registers x y]
  (if (zero? (get-value registers x))
    1
    (get-value registers y)))

;;=======================================================================================================
;; compare x and y and store if x > y, x = y or x < y
;; the result is stored in internal-registers :cmp register.
;;=======================================================================================================
(defn cmp [registers x y]
  (let [x-val (if (keyword? x) (get registers x) x)
        y-val (if (keyword? y) (get registers y) y)]
    (cond (= x-val y-val) :eq
          (> x-val y-val) :gt
          (< x-val y-val) :lt)))

;;=======================================================================================================
;; After a cmp either the comparison will be in one of three states, :eq, :gt, :lt
;;
;; Therefore, to check if we need to jump or not, we simple have to pass in a set of allowed states.
;; e.g,
;; jge :: we would pass in #{:eq :gt}, then we can check if the cmp register is either :eq or :gt.
;;
;; If it is in the set then we can return the location for the label (lbl) in the symbol table.
;; Otherwise we just return the eip incremented so we advance to the next instruction.
;;=======================================================================================================
(defn cmp-jmp [internal-registers symbol-table eip valid-comps lbl]
  (if (nil? (valid-comps (:cmp internal-registers)))
    (inc eip)
    (lbl symbol-table)))

;;=======================================================================================================
;; Handle call instructions.
;; We return the eip we want to jump to from the symbol table for the given label.
;;=======================================================================================================
(defn call [symbol-table label]
  (label symbol-table))

;;=======================================================================================================
;; Builds a string for the program return value from the arguments to the set: instruction.
;;=======================================================================================================
(defn set-message [registers & args]
  (assoc-in registers [:internal-registers :return-code] (reduce (fn [s a] (str s (get-value registers a))) args)))

;;=======================================================================================================
;; Gets the return value for the program. If its a number, it will parse it to an Integer.
;; Otherwise just return it as a string.
;; If return-registers? is set then return a vector of the return value and the registers.
;;=======================================================================================================
(defn return-value [registers return-registers?]
  (let [res (:return-code (:internal-registers registers))
        ret-value (cond (nil? res) 0
                        (re-matches #"^[\+\-]?\d*\.?[Ee]?[\+\-]?\d*$" res) (js/Number res)
                        :else res)]
    (if return-registers?
      [ret-value (dissoc registers :internal-registers)]
      ret-value)))

;;=======================================================================================================
;; Process the jump instructions and return the new eip.
;;=======================================================================================================
(defn process-jump [eip instruction registers internal-registers symbol-table eip-stack args]
  (prn "JMP" registers)
  ;; if we are jumping to a label, just return the location of the label in the symbol-table
  (cond (= :jmp instruction)
        (inc (get symbol-table (first args)))

        (= :jnz instruction)
        (let [[x y] args]
          (+ eip (jnz registers x y)))

        ;; check if its a cmp jump.
        (#{:jne :je :jge :jg :jle :jl} instruction)
        (let [pred (cmp-jump-predicates instruction)
              x    (first args)]
          (cmp-jmp internal-registers symbol-table eip pred x))

        (= :call instruction)
        (call symbol-table (first args))

        (= :ret instruction)
        (if (empty? eip-stack)
          -1
          (inc (peek eip-stack)))

        :else
        (inc eip)))

;;=======================================================================================================
;; Process regular instructions and return the new registers.
;;=======================================================================================================
(defn process-instruction [instruction {:keys [registers stack] :as memory} args]
  #_#_(prn "args::" args)
    (prn "memory::" registers stack)
  (cond (= :mov instruction)
        (let [[x y] args]
          (assoc memory :registers (mov registers x y)))

        (#{:inc :dec} instruction)
        (assoc memory :registers (unary-op registers (get-unary-operation instruction) (first args)))

        (#{:mul :add :sub :div :xor :and :or} instruction)
        (let [[x y] args]
          (assoc memory :registers (binary-op registers (get-binary-operations instruction) x y)))

        (= :cmp instruction)
        (let [[x y] args]
          (assoc-in memory [:internal-registers :cmp] (cmp registers x y)))

        (= :pop instruction)
        (if (empty? stack)
          (update-in memory [:internal-registers] assoc :err "Popped empty stack.")
          (-> memory
              (assoc :registers (mov registers (first args) (peek stack)))
              (update :stack (if (empty? stack) identity pop))))

        (= :push instruction)
        (let [x (get-value registers (first args))]
          (update memory :stack #(conj % x)))

        (= :msg instruction)
        (assoc memory :registers (apply (partial set-message registers) args))))

(defn build-symbol-table [asm]
  (reduce (fn [a [i ix]]
        (if (= (first ix) :label)
            (assoc a (second ix) i)
            a))
          {}
          (map vector (range) asm)))

;;=======================================================================================================
;; The interpreter.
;;
;; Recursively handle each instruction in our set of instructions.
;; Keep track of the eip.
;; eip-stack is a vector containing the return instruction pointers for call / ret
;; Exit conditions:
;;  * :end instruction
;;  * the eip is beyond the last instruction (when this occurs return -1 as the exit code).
;;  * the eip is -1, meaning we hit a ret with an empty eip-stack.
;;=======================================================================================================
(defn interpret [instructions {:keys [eip registers internal-registers stack symbol-table] :as memory}]
  #_(js/console.log memory)
  (let [[instruction & args] (nth instructions eip)]
    (prn eip)
    (let [new-eip   (if (#{:jmp :jnz :jne :je :jgl :jg :jle :jl :jge :ret :call} instruction)
                      (process-jump eip instruction registers internal-registers symbol-table (:eip-stack memory) args)
                      (inc eip))

          memory (if (#{:mov :mul :add :sub :dec :xor :and :or :div :inc :msg :cmp :push :pop} instruction)
                   (process-instruction instruction memory args)
                   memory)

          eip-stack (cond (= :ret instruction) (pop (:eip-stack memory))
                          (= :call instruction) (conj (:eip-stack memory) eip)
                          :else (:eip-stack memory))]
      (-> memory
          (assoc :eip new-eip)
          (assoc :eip-stack eip-stack)))))

(comment (interpret [[:mov :a 5]]
                    {:eip 0
                     :registers {}
                     :internal-registers {}
                     :stack []
                     :eip-stack []
                     :symbol-table []}))


#_(defn interpret [instructions {:keys [eip registers internal-registers stack]}]
  (let [symbol-table (build-symbol-table instructions)]
    (loop [eip 0
           memory {:registers {} :eip-stack [] :stack []}]
      #_(prn eip "::" memory)
      ; if we have an eip that points after the last instruction exit with a -1 error code and
      ; show the registers (including internal registers).
      (if (or (= eip (count instructions)) (= eip -1))
        (if return-registers?
          [-1 (memory :registers)]
          -1)
        ; else get the current instruction in the instructions list at the eip location and
        ; destructure into the instruction and its arguments.
        (let [[instruction & args] (nth instructions eip)]
          (cond (= :end instruction)
                (return-value (memory :registers) return-registers?)
                :else
                (let [new-eip   (if (#{:jmp :jnz :jne :je :jgl :jg :jle :jl :jge :ret :call} instruction)
                                  (process-jump eip instruction (memory :registers) symbol-table (:eip-stack memory) args)
                                  (inc eip))

                      memory (if (#{:mov :mul :add :sub :dec :xor :and :or :div :inc :msg :cmp :push :pop} instruction)
                               (process-instruction instruction memory args)
                               memory)

                      eip-stack (cond (= :ret instruction) (pop (:eip-stack memory))
                                      (= :call instruction) (conj (:eip-stack memory) eip)
                                      :else (:eip-stack memory))]

                  (recur new-eip (assoc memory :eip-stack eip-stack)))))))))