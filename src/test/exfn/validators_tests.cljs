(ns exfn.validators-tests
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [exfn.validators :refer [validate-instruction]]))

(deftest validate-instruction-tests
  (testing "Valid instructions should not return errors"
    (is (= [] (validate-instruction "mov" "mov :a :b" true)))
    (is (= [] (validate-instruction "mov" "mov %1 %2" true)))
    (is (= [] (validate-instruction "mov" "mov %1 :b" true)))
    (is (= [] (validate-instruction "mov" "mov :a %2" true)))
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
    (is (= [] (validate-instruction "pop" "pop :a" false)))
    (is (= [] (validate-instruction "push" "push :a" false)))
    (is (= [] (validate-instruction "jnz" "jnz :a :b" false)))
    (is (= [] (validate-instruction "jnz" "jnz :a 5" false)))
    (is (= [] (validate-instruction "jz" "jz :a :b" false)))
    (is (= [] (validate-instruction "jz" "jz :a 5" false)))
    (is (= [] (validate-instruction "not" "not :a" false)))
    (is (= [] (validate-instruction "dec" "dec :a" false)))
    (is (= [] (validate-instruction "jmp" "jmp foo" false)))
    (is (= [] (validate-instruction "cat" "cat :a :b" false)))
    (is (= [] (validate-instruction "cat" "cat :a `foo`" false)))
    (is (= [] (validate-instruction "cat" "cat :a 'foo'" false)))
    (is (= [] (validate-instruction "cat" "cat :a 'foo'" false)))
    (is (= [] (validate-instruction "len" "len :a 'foo'" false)))
    (is (= [] (validate-instruction "len" "len :a `foo`" false)))
    (is (= [] (validate-instruction "cmp" "cmp :a :b" false)))
    (is (= [] (validate-instruction "cat" "cat :a `foo 'bar'`" false)))
    (is (= [] (validate-instruction "cat" "cat :a 'foo `bar`'" false)))
    (is (= [] (validate-instruction "ret" "ret" false)))
    (is (= [] (validate-instruction "end" "end" false)))
    (is (= [] (validate-instruction "cer" "cer" false)))
    (is (= [] (validate-instruction "rp" "rp" false)))
    (is (= [] (validate-instruction "rep" "rep 5" false)))
    (is (= [] (validate-instruction "nop" "nop" false)))
    (is (= [] (validate-instruction "foo:" "foo:" false)))
    (is (= [] (validate-instruction "jne" "jne foo" false)))
    (is (= [] (validate-instruction "jge" "jge foo" false)))
    (is (= [] (validate-instruction "jle" "jle foo" false)))
    (is (= [] (validate-instruction "jl" "jl foo" false)))
    (is (= [] (validate-instruction "jg" "jg foo" false)))
    (is (= [] (validate-instruction "rnz" "rnz :a" false)))
    (is (= [] (validate-instruction "rz" "rz :a" false)))
    (is (= [] (validate-instruction "rgz" "rgz :a" false)))
    (is (= [] (validate-instruction "rlz" "rlz :a" false)))
    (is (= [] (validate-instruction "rgez" "rgez :a" false)))
    (is (= [] (validate-instruction "rlez" "rlez :a" false)))
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
      (is (= ["Invalid `add` call, `add` should have two arguments."]
             (validate-instruction "add" "add :a" false)))
      (is (= ["Invalid `add` call, `add` first argument must be a register."]
             (validate-instruction "add" "add 5 :b" false)))
      (is (= ["Invalid `sub` call, `sub` first argument must be a register."]
             (validate-instruction "sub" "sub 5 :b" false)))
      (is (= ["Invalid `sub` call, `sub` should have two arguments."
              "Invalid `sub` call, `sub` first argument must be a register."]
             (validate-instruction "sub" "sub 5" false)))
      ))

(comment (run-tests))