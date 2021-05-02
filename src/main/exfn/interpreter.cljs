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
        (= :or  instruction) bit-or
        (= :and instruction) bit-and
        (= :cat instruction) str))

;;=======================================================================================================
;; Return the appropriate unary operation for the given unary instruction.
;;=======================================================================================================
(defn get-unary-operation [instruction]
  (cond (= :inc instruction) inc
        (= :dec instruction) dec
        (= :len instruction) count
        (= :not instruction) bit-not))

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
;; Therefor
;; e, to check if we need to jump or not, we simple have to pass in a set of allowed states.
;; e.g,
;; jge :: we would pass in #{:eq :gt}, then we can check if the cmp register is either :eq or :gt.
;;
;; If it is in the set then we can return the location for the label (lbl) in the symbol table.
;; Otherwise we just return the eip incremented so we advance to the next instruction.
;;=======================================================================================================
(defn cmp-jmp [internal-registers symbol-table eip valid-comps lbl]
  (if (nil? (valid-comps (:cmp internal-registers)))
    (inc eip)
    ((keyword lbl) symbol-table)))

;;=======================================================================================================
;; Handle call instructions.
;; We return the eip we want to jump to from the symbol table for the given label.
;;=======================================================================================================
(defn call [symbol-table label]
  ((keyword label) symbol-table))

;;=======================================================================================================
;; Builds a string for the program return value from the arguments to the set: instruction.
;;=======================================================================================================
(defn set-message [registers & args]
  (assoc-in registers [:internal-registers :return-code] (reduce (fn [s a] (str s (get-value registers a))) args)))

;;=======================================================================================================
;; Process the jump instructions and return the new eip.
;;=======================================================================================================
(defn process-jump [eip instruction registers internal-registers symbol-table eip-stack rep-counters-stack args]
  ;; if we are jumping to a label, just return the location of the label in the symbol-table
  (js/console.log instruction)
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

        ;; if the top item on the rep-counters-stack is one, move to next line. Otherwise return the top eip-stack.
        (= :rp instruction)
        (if (= 1 (peek rep-counters-stack))
          (inc eip)
          (inc (peek eip-stack)))

        :else
        (inc eip)))

;;=======================================================================================================
;; Instruction Handlers
;;=======================================================================================================
(defn handle-unary-instruction [memory instruction args]
  (-> memory
      (assoc :registers (unary-op (:registers memory) (get-unary-operation instruction) (first args)))
      (assoc :last-edit-register (first args))))

(defn handle-binary-instruction [memory instruction args]
  (let [[x y] args]
    (-> memory
        (assoc :registers (binary-op (:registers memory) (get-binary-operations instruction) x y))
        (assoc :last-edit-register x))))

(defn pop-stack [{:keys [registers stack] :as memory} args]
  (if (empty? stack)
    (update-in memory [:internal-registers] assoc :err "Popped empty stack.")
    (-> memory
        (assoc :registers (mov registers (first args) (peek stack)))
        (update :stack (if (empty? stack) identity pop))
        (assoc :last-edit-register (first args)))))

(defn handle-mov-instruction [{:keys [registers] :as memory} args]
  (let [[x y] args]
    (-> memory
        (assoc :registers (mov registers x y))
        (assoc :last-edit-register x))))

(defn handle-cmp-instruction [{:keys [registers] :as memory} args]
  (let [[x y] args]
    (assoc-in memory [:internal-registers :cmp] (cmp registers x y))))

(defn handle-push-instruction [{:keys [registers] :as memory} args]
  (let [x (get-value registers (first args))]
    (update memory :stack #(conj % x))))

;;=======================================================================================================
;; Process regular instructions and return the new registers.
;;=======================================================================================================
(defn process-instruction [instruction {:keys [registers eip] :as memory} args]
  (cond (= :mov instruction)
        (handle-mov-instruction memory args)

        (#{:inc :dec :not} instruction)
        (handle-unary-instruction memory instruction args)

        (#{:mul :add :sub :div :xor :and :or :cat} instruction)
        (handle-binary-instruction memory instruction args)

        (= :cmp instruction)
        (handle-cmp-instruction memory args)

        (= :pop instruction)
        (pop-stack memory args)

        (= :push instruction)
        (handle-push-instruction memory args)

        (= :len instruction)
        (update-in memory [:registers] assoc (first args) (count (get-value registers (second args))))

        (= :msg instruction)
        (assoc memory :registers (apply (partial set-message registers) args))
        
        (= :rep instruction)        
        (if (seq args)
          (-> memory
              (update-in [:rep-counters-stack] conj (get-value registers (first args)))
              (update-in [:eip-stack] conj eip))
          (-> memory (update-in [:eip-stack] conj eip)))
        
        (= :rp instruction)
        (let [counter (peek (memory :rep-counters-stack))]
          (if (= 1 counter) ; decrementing would reduce it to zero.
            (-> memory
                (update :rep-counters-stack pop)
                (update :eip-stack pop))
            (-> memory
                (update :rep-counters-stack pop)
                (update :rep-counters-stack conj (dec counter)))))))

(defn build-symbol-table [asm]
  (reduce (fn [a [i ix]]
        (if (= (first ix) :label)
            (assoc a (second ix) i)
            a))
          {}
          (map vector (range) asm)))

;;=======================================================================================================
;; The interpreter.
;;=======================================================================================================
(defn interpret [instructions {:keys [eip registers] :as memory}]
  (let [[instruction & args] (nth instructions eip)
        new-eip              (if (#{:jmp :jnz :jne :je :jgl :jg :jle :jl :jge :ret :call :rp} instruction)
                               (process-jump eip instruction registers (memory :internal-registers) (memory :symbol-table) (:eip-stack memory) (memory :rep-counters-stack) args)
                               (inc eip))

        memory               (if (#{:mov :mul :add :sub :dec :xor :and :or :div :inc :msg :cmp :push :pop :cat :len :not :rep :rp} instruction)
                               (process-instruction instruction memory args)
                               memory)

        eip-stack            (cond (= :ret instruction) (pop (:eip-stack memory))
                                   (= :call instruction) (conj (:eip-stack memory) eip)
                                   :else (:eip-stack memory))
        
        output               (when (= :prn instruction)
                               (str (get-value registers (first args))))]
    [; new memory state
     (-> memory
         (assoc :eip new-eip)
         (assoc :eip-stack eip-stack))
       ; finished?
     (or (= (nth instructions new-eip) [:end]) (> new-eip (count instructions)))
       ;new output
     output]))