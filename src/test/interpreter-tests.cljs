(ns test.interpreter-tests
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [exfn.interpreter :refer [append-output
                                      build-symbol-table
                                      decrement
                                      get-math-fun
                                      increment
                                      interpret
                                      math-func
                                      mov
                                      prnout
                                      str-cat]]))

(deftest build-symbol-table-tests
  (is (= {:foo 2, :bar 4, :quax 5}
         (build-symbol-table [[:nop]
                              [:nop]
                              [:label :foo]
                              [:nop]
                              [:label :bar]
                              [:label :quax]])))

  (is (= {}
         (build-symbol-table [[:nop]
                              [:nop]
                              [:nop]
                              [:nop]
                              [:nop]
                              [:nop]]))))


(deftest conditional-repeats
  (testing "rgz repeats"
    (let [memory {:registers {:a -1} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rgz :a]] memory)]
      (is (and (= (-> result :memory :eip) 1)
               (= (-> result :memory :eip-stack) [0])))))

  (testing "rgz does not repeat"
    (let [memory {:registers {:a 1} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rgz :a]] memory)]
      (is (and (= (-> result :memory :eip) 3)
               (= (-> result :memory :eip-stack) [])))))

  (testing "rlz repeats"
    (let [memory {:registers {:a 0} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rlz :a]] memory)]
      (is (and (= (-> result :memory :eip) 1)
               (= (-> result :memory :eip-stack) [0])))))

  (testing "rlz does not repeat"
    (let [memory {:registers {:a -1} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rlz :a]] memory)]
      (is (and (= (-> result :memory :eip) 3)
               (= (-> result :memory :eip-stack) [])))))

  (testing "rlez does not repeat"
    (let [memory {:registers {:a 1} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rlez :a]] memory)]
      (is (and (= (-> result :memory :eip) 1)
               (= (-> result :memory :eip-stack) [0])))))

  (testing "rlez repeats"
    (let [memory {:registers {:a 0} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rlez :a]] memory)]
      (is (and (= (-> result :memory :eip) 3)
               (= (-> result :memory :eip-stack) [])))))

  (testing "rgez does not repeats"
    (let [memory {:registers {:a 0} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rgez :a]] memory)]
      (is (and (= (-> result :memory :eip) 3)
               (= (-> result :memory :eip-stack) [])))))

  (testing "rgez repeats"
    (let [memory {:registers {:a -1} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rgez :a]] memory)]
      (is (and (= (-> result :memory :eip) 1)
               (= (-> result :memory :eip-stack) [0])))))

  (testing "rz repeats"
    (let [memory {:registers {:a 1} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rz :a]] memory)]
      (is (= (-> result :memory :eip) 1))
      (is (= (-> result :memory :eip-stack) [0]))))

  (testing "rz does not repeat"
    (let [memory {:registers {:a 0} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rz :a]] memory)]
      (is (= (-> result :memory :eip) 3))
      (is (= (-> result :memory :eip-stack) []))))

  (testing "mov moves"
    (is (= {:registers {:a 7 :b 7}} (mov {:registers {:a 5 :b 7}} [:a :b])))
    (is (= {:registers {:a 9 :b 7}} (mov {:registers {:a 5 :b 7}} [:a 9])))))
  
(deftest math-func-tests
    (testing "add adds"
      (is (= {:registers {:a 12 :b 7} :internal-registers {:par 1}} (math-func :add {:registers {:a 5 :b 7}} [:a :b])))
      (is (= {:registers {:a 14 :b 7} :internal-registers {:par 0}} (math-func :add {:registers {:a 5 :b 7}} [:a 9]))))
    
    (testing "mul multiplies"
      (is (= {:registers {:a 8 :b 4} :internal-registers {:par 0}} (math-func :mul {:registers {:a 2 :b 4}} [:a :b])))
      (is (= {:registers {:a 45 :b 7} :internal-registers {:par 1}} (math-func :mul {:registers {:a 5 :b 7}} [:a 9]))))
    
    (testing "sub subtracts"
      (is (= {:registers {:a 5 :b 3} :internal-registers {:par 1}} (math-func :sub {:registers {:a 8 :b 3}} [:a :b])))
      (is (= {:registers {:a 2 :b 7} :internal-registers {:par 0}} (math-func :sub {:registers {:a 5 :b 7}} [:a 3]))))

    (testing "div divides"
      (is (= {:registers {:a 3 :b 5} :internal-registers {:par 1}} (math-func :div {:registers {:a 15 :b 5}} [:a :b])))
      (is (= {:registers {:a 2 :b 7} :internal-registers {:par 0}} (math-func :div {:registers {:a 6 :b 7}} [:a 3]))))
    
    (testing "xor bit-xors"
      (is (= {:registers {:a 10 :b 5} :internal-registers {:par 1}} (math-func :xor {:registers {:a 15 :b 5}} [:a :b])))
      (is (= {:registers {:a 5 :b 4} :internal-registers {:par 1}} (math-func :xor {:registers {:a 2 :b 4}} [:a 7]))))
    
    (testing "and bit-ands"
      (is (= {:registers {:a 5 :b 5} :internal-registers {:par 1}} (math-func :and {:registers {:a 15 :b 5}} [:a :b])))
      (is (= {:registers {:a 2 :b 4} :internal-registers {:par 0}} (math-func :and {:registers {:a 2 :b 4}} [:a 7]))))

    (testing "or bit-ors"
      (is (= {:registers {:a 15 :b 5} :internal-registers {:par 1}} (math-func :or {:registers {:a 15 :b 5}} [:a :b])))
      (is (= {:registers {:a 7 :b 4} :internal-registers {:par 0}} (math-func :or {:registers {:a 2 :b 4}} [:a 7])))))

(deftest get-math-fun-tests
  (is (= + (get-math-fun :add)))
  (is (= - (get-math-fun :sub)))
  (is (= quot (get-math-fun :div)))
  (is (= * (get-math-fun :mul)))
  (is (= bit-xor (get-math-fun :xor)))
  (is (= bit-or (get-math-fun :or)))
  (is (= bit-and (get-math-fun :and))))

(deftest append-output-tests
  (is (= "bar\nfoo" (append-output "bar" "foo")))
  (is (= "bar" (append-output "bar" nil)))
  (is (= "bar" (append-output nil "bar"))))

(deftest prn-tests
  (is (= {:registers {:a 5} :output "\nbar"} (prnout {:registers {:a 5} :output ""} ["bar"])))
  (is (= {:registers {:a 5} :output "\n5"} (prnout {:registers {:a 5} :output ""} [:a]))))

(deftest cat-tests
  (is (= {:registers {:a "foobar" :b "bar"}} (str-cat {:registers {:a "foo" :b "bar"}} [:a :b])))
  (is (= {:registers {:a "foobar"}} (str-cat {:registers {:a "foo" }} [:a "bar"]))))

(deftest increment-decrement-tests
  (is (= {:registers {:a 5} :internal-registers {:par 1}} (increment {:registers {:a 4}} [:a])))
  (is (= {:registers {:a 2} :internal-registers {:par 0}} (decrement {:registers {:a 3}} [:a]))))


(run-tests)