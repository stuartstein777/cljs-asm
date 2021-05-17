(ns exfn.errors-tests
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [exfn.errors :refer [first-argument-is-a-register?
                                 has-no-arguments?
                                 has-one-argument?
                                 has-two-arguments?
                                 one-argument-and-its-a-label]]))
  
(deftest first-argument-is-a-register?-tests
  (testing "first argument is a register should not return error"
    (is (nil? (first-argument-is-a-register? false "mov :a 5"))))
  (testing "First argument is label should return error"
    (is (= "first argument must be a register. Registers start with a :"
           (first-argument-is-a-register? false "mov a 5"))))
  (testing "First argument is a string should return error"
    (is (= "first argument must be a register. Registers start with a :"
           (first-argument-is-a-register? false "mov `a` 5")))
    (is (= "first argument must be a register. Registers start with a :"
           (first-argument-is-a-register? false "mov 'a' 5"))))
  (testing "First argument is number should return error"
    (is (= "first argument must be a register. Registers start with a :"
           (first-argument-is-a-register? false "mov 5 5"))))
  (testing "Within a macro call, first argument is prefixed with %, should not return error"
    (is (nil? (first-argument-is-a-register? true "mov %1 5"))))
  (testing "Within a macro call, first argument is a register, should not return error"
    (is (nil? (first-argument-is-a-register? true "mov :a 5"))))
  (testing "Within a macro call, first argument is a number, should return error"
    (is (= "first argument in a macro can't be a constant (value or string). Should have form %1, %2 etc"
           (first-argument-is-a-register? true "mov 5 5"))))
  (testing "Within a macro call, first argument is a label, should return error"
    (is (= "first argument in a macro can't be a constant (value or string). Should have form %1, %2 etc"
           (first-argument-is-a-register? true "mov foo 5")))))
  
(deftest has-no-arguments?-tests
  (testing "instuction has no argument, should not return error"
    (is (nil? (has-no-arguments? "ret"))))
  (testing "instuction has register first argument, should error"
    (is (= "does not expect arguments" (has-no-arguments? "ret :a"))))
  (testing "instuction has number first argument, should error"
    (is (= "does not expect arguments" (has-no-arguments? "ret 5"))))
  (testing "instuction has label first argument, should error"
    (is (= "does not expect arguments" (has-no-arguments? "ret foo"))))
  (testing "instuction has string first argument, should error"
    (is (= "does not expect arguments" (has-no-arguments? "ret 'foo'")))
    (is (= "does not expect arguments" (has-no-arguments? "ret `foo`")))))

(deftest has-one-argument?-tests
  (testing "instruction has one argument should not return error."
    (is (nil? (has-one-argument? "inc :a"))))
  (testing "instruction has no arguments. Should return error"
    (is (= "should only have one argument." (has-one-argument? "inc"))))
  (testing "instruction has two arguments. Should return error"
    (is (= "should only have one argument." (has-one-argument? "mov :a :b")))))
  
(deftest has-two-arguments?-tests
  (testing "instruction has two arguments, should not return error"
    (is (nil? (has-two-arguments? "mov :a :b")))
    (is (nil? (has-two-arguments? "mov a 0")))
    (is (nil? (has-two-arguments? "mov 5 0"))))
  (testing "instruction has one argument, should return error"
    (is (= "should have two arguments." (has-two-arguments? "mov :a"))))
  (testing "instruction has no arguments, should return error"
    (is (= "should have two arguments." (has-two-arguments? "ret")))))

(deftest one-argument-and-its-a-label-tests
  (testing "instruction has one argument and its a label, should not return error"
    (is (nil? (one-argument-and-its-a-label "jmp foo"))))
  (testing "instruction has no arguments, should return error"
    (is (= "expects one argument and it must be a label." (one-argument-and-its-a-label "jmp"))))
  (testing "instruction has two arguments, should return error"
    (is (= "expects one argument and it must be a label." (one-argument-and-its-a-label "jmp foo :bar"))))
  (testing "instruction has one argument that is a number, should return error"
    (is (= "expects one argument and it must be a label." (one-argument-and-its-a-label "jmp 5"))))
  (testing "instruction has one argument that is a register should return error"
    (is (= "expects one argument and it must be a label." (one-argument-and-its-a-label "jmp :x")))))

(comment (run-tests))