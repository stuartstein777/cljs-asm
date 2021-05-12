(ns main.exfn.scratch
  (:require [clojure.string :as str]))

(defn has-two-arguments? [args]
  (when (not= (count args) 2)
    (str "has 2 arguments.")))

(defn first-argument-is-a-register? [is-macro? args]
  (cond (and is-macro? (not (re-find #"%\d+" (first args))))
        "first argument in a macro can't be a constant (value or string). Should have form %1, %2 etc"

        (and (not is-macro?) (not (str/starts-with? (first args) ":")))
        "first argument must be a register."
        
        :else
        nil))

(def rules {"mov" {:macro-applicable [first-argument-is-a-register?]
                 :non-macro-applicable [has-two-arguments?] }})

(defn validate-instruction [instr args is-macro?]
  (let [{:keys [macro-applicable non-macro-applicable]} (rules instr)
        applied  (map #(partial % is-macro?) macro-applicable)
        all-rules (apply conj applied non-macro-applicable)]
    (keep (fn [f]
            (let [error (f args)]
              (when error
                (str "Invalid `" instr "` call, " error)))) all-rules)))

(comment (validate-instruction
          "mov"
          ["%1"]
          true))