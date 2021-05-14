(ns test.exfn.validators-tests
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [exfn.validators :refer [validate-instruction]]))

(deftest validate-instruction-tests
  (testing "Valid instructions should not return errors"
    (is (= [] (validate-instruction "mov" "mov :a :b" false)))
    (is (= [] (validate-instruction "mov" "mov :a 5" false)))
    (is (= [] (validate-instruction "add" "add :a :b" false)))
    (is (= [] (validate-instruction "add" "add :a 5" false)))
    (is (= [] (validate-instruction "sub" "sub :a :b" false)))
    (is (= [] (validate-instruction "sub" "sub :a 5" false)))
    (is (= [] (validate-instruction "mul" "mul :a :b" false)))
    (is (= [] (validate-instruction "mul" "mul :a 5" false)))
    (is (= [] (validate-instruction "div" "div :a :b" false)))
    (is (= [] (validate-instruction "div" "div :a 5" false)))
    (is (= [] (validate-instruction "xor" "xor :a :b" false)))
    (is (= [] (validate-instruction "xor" "xor :a 5" false)))
    (is (= [] (validate-instruction "and" "and :a :b" false)))
    (is (= [] (validate-instruction "and" "and :a 5" false)))
    (is (= [] (validate-instruction "or" "or :a :b" false)))
    (is (= [] (validate-instruction "or" "or :a 5" false)))
    (is (= [] (validate-instruction "inc" "inc :a" false)))
    (is (= [] (validate-instruction "not" "not :a" false)))
    (is (= [] (validate-instruction "dec" "dec :a" false)))
    (is (= [] (validate-instruction "jmp" "jmp foo" false)))
    (is (= [] (validate-instruction "cat" "cat :a :b" false)))
    (is (= [] (validate-instruction "cat" "cat :a `foo`" false)))
    (is (= [] (validate-instruction "cat" "cat :a 'foo'" false)))
    (is (= [] (validate-instruction "cat" "cat :a `foo 'bar'`" false)))
    (is (= [] (validate-instruction "cat" "cat :a 'foo `bar`'" false)))
    (is (= [] (validate-instruction "ret" "ret" false)))
    (is (= [] (validate-instruction "end" "end" false)))
    (is (= [] (validate-instruction "prn" "prn :a" false))))
  
  (testing "Invalid instructions should return errors"
    (is (= ["Invalid `mov` call, `mov` should have two arguments."
            "Invalid `mov` call, `mov` first argument must be a register."]
           (validate-instruction "mov" "mov a" false)))
    (is (= ["Invalid `mov` call, `mov` should have two arguments."
            "Invalid `mov` call, `mov` first argument must be a register."]
           (validate-instruction "mov" "mov" false)))
    (is (= ["Invalid `mov` call, `mov` should have two arguments."]
           (validate-instruction "mov" "mov :a" false)))
    (is (= ["Invalid `mov` call, `mov` should have two arguments."]
           (validate-instruction "mov" "mov :a :b :c" false)))
    (is (= ["Invalid `add` call, `add` should have two arguments."]
           (validate-instruction "add" "add :a :b :c" false)))
    ))

(comment (run-tests))