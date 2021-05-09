(ns test.interpreter-tests
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [exfn.interpreter :refer [append-output
                                      bitnot
                                      build-symbol-table
                                      cmp
                                      cmp-jmp
                                      decrement
                                      get-math-fun
                                      increment
                                      interpret
                                      jmp
                                      jnz
                                      jz
                                      math-func
                                      mov
                                      prnout
                                      str-cat
                                      strlen]]))

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
          result (interpret [[:rep] [:nop] [:rgz :a] [:end]] memory)]
      (is (and (= (-> result :memory :eip) 1)
               (= (-> result :memory :eip-stack) [0])))))

  (testing "rgz does not repeat"
    (let [memory {:registers {:a 1} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rgz :a] [:end]] memory)]
      (is (and (= (-> result :memory :eip) 3)
               (= (-> result :memory :eip-stack) [])))))

  (testing "rlz repeats"
    (let [memory {:registers {:a 0} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rlz :a] [:end]] memory)]
      (is (and (= (-> result :memory :eip) 1)
               (= (-> result :memory :eip-stack) [0])))))

  (testing "rlz does not repeat"
    (let [memory {:registers {:a -1} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rlz :a] [:end]] memory)]
      (is (and (= (-> result :memory :eip) 3)
               (= (-> result :memory :eip-stack) [])))))

  (testing "rlez does not repeat"
    (let [memory {:registers {:a 1} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rlez :a] [:end]] memory)]
      (is (and (= (-> result :memory :eip) 1)
               (= (-> result :memory :eip-stack) [0])))))

  (testing "rlez repeats"
    (let [memory {:registers {:a 0} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rlez :a] [:end]] memory)]
      (is (and (= (-> result :memory :eip) 3)
               (= (-> result :memory :eip-stack) [])))))

  (testing "rgez does not repeats"
    (let [memory {:registers {:a 0} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rgez :a] [:end]] memory)]
      (is (and (= (-> result :memory :eip) 3)
               (= (-> result :memory :eip-stack) [])))))

  (testing "rgez repeats"
    (let [memory {:registers {:a -1} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rgez :a] [:end]] memory)]
      (is (and (= (-> result :memory :eip) 1)
               (= (-> result :memory :eip-stack) [0])))))

  (testing "rz repeats"
    (let [memory {:registers {:a 1} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rz :a] [:end]] memory)]
      (is (= (-> result :memory :eip) 1))
      (is (= (-> result :memory :eip-stack) [0]))))

  (testing "rz does not repeat"
    (let [memory {:registers {:a 0} :eip 2 :eip-stack [0]}
          result (interpret [[:rep] [:nop] [:rz :a] [:end]] memory)]
      (is (= (-> result :memory :eip) 3))
      (is (= (-> result :memory :eip-stack) [])))))
  
(deftest mov-tests
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

(deftest bit-not-tests
  (is (= {:registers {:a -6}
          :internal-registers {:par 1}
          :last-edit-register :a}
         (bitnot {:registers {:a 5}} [:a]))))

(deftest strlen-tests
  (is (= {:registers {:a 6 :b "foobar"}}
         (strlen {:registers {:b "foobar"}} [:a :b])))
  (is (= {:registers {:a 0 :b ""}}
         (strlen {:registers {:b ""}} [:a :b]))))

(deftest jnz-jz-tests
  (is (= {:registers {:a 0 :b 5} :eip 3} 
         (jnz {:registers {:a 0 :b 5} :eip 2} [:a :b])))
  (is (= {:registers {:a 1 :b 5} :eip 7}
         (jnz {:registers {:a 1 :b 5} :eip 2} [:a :b])))
  
  (is (= {:registers {:a 0 :b 5} :eip 7}
         (jz {:registers {:a 0 :b 5} :eip 2} [:a :b])))
  (is (= {:registers {:a 1 :b 5} :eip 3}
         (jz {:registers {:a 1 :b 5} :eip 2} [:a :b]))))

(deftest jmp-tests
  (is (= {:symbol-table {:foo 55} :eip 55}
         (jmp {:symbol-table {:foo 55} :eip 2} [:foo])))
  (is (= {:symbol-table {} :eip -2}
         (jmp {:symbol-table {} :eip 2} [:foo]))))

(deftest cmp-tests
  (is (= {:registers {:a 5 :b 6} :internal-registers {:cmp :lt}}
         (cmp {:registers {:a 5 :b 6}} [:a :b])))
  (is (= {:registers {:a 5} :internal-registers {:cmp :lt}}
         (cmp {:registers {:a 5}} [:a 6])))
  (is (= {:registers {:a 6 :b 6} :internal-registers {:cmp :eq}}
         (cmp {:registers {:a 6 :b 6}} [:a :b])))
  (is (= {:registers {:a 6} :internal-registers {:cmp :eq}}
         (cmp {:registers {:a 6}} [:a 6])))
  (is (= {:registers {:a 7 :b 6} :internal-registers {:cmp :gt}}
         (cmp {:registers {:a 7 :b 6}} [:a :b])))
  (is (= {:registers {:a 7} :internal-registers {:cmp :gt}}
         (cmp {:registers {:a 7}} [:a 6]))))

(deftest cmp-jmp-tests
  (testing "cmp-jmp: jge"
    (is (= {:eip 6 :internal-registers {:cmp :lt} :symbol-table {:foo 10}}
           (cmp-jmp {:eip 5 :internal-registers {:cmp :lt} :symbol-table {:foo 10}}
                    :jge
                    [:foo])))
    (is (= {:eip 10 :internal-registers {:cmp :eq} :symbol-table {:foo 10}}
           (cmp-jmp {:eip 6 :internal-registers {:cmp :eq} :symbol-table {:foo 10}}
                    :jge
                    [:foo])))
    (is (= {:eip 10 :internal-registers {:cmp :gt} :symbol-table {:foo 10}}
           (cmp-jmp {:eip 6 :internal-registers {:cmp :gt} :symbol-table {:foo 10}}
                    :jge
                    [:foo]))))
  
  (testing "cmp-jmp jg"
    (is (= {:eip 6 :internal-registers {:cmp :lt} :symbol-table {:foo 10}}
           (cmp-jmp {:eip 5 :internal-registers {:cmp :lt} :symbol-table {:foo 10}}
                    :jg
                    [:foo])))
    (is (= {:eip 6 :internal-registers {:cmp :eq} :symbol-table {:foo 10}}
           (cmp-jmp {:eip                5
                     :internal-registers {:cmp :eq}
                     :symbol-table       {:foo 10}}
                    :jg
                    [:foo])))
    (is (= {:eip 10 :internal-registers {:cmp :gt} :symbol-table {:foo 10}}
           (cmp-jmp {:eip 6 :internal-registers {:cmp :gt} :symbol-table {:foo 10}}
                    :jg
                    [:foo]))))
  
  (testing "cmp-jmp jne"
    (is (= {:eip 10 :internal-registers {:cmp :lt} :symbol-table {:foo 10}}
           (cmp-jmp {:eip 5 :internal-registers {:cmp :lt} :symbol-table {:foo 10}}
                    :jne
                    [:foo])))
    (is (= {:eip 6 :internal-registers {:cmp :eq} :symbol-table {:foo 10}}
           (cmp-jmp {:eip                5
                     :internal-registers {:cmp :eq}
                     :symbol-table       {:foo 10}}
                    :jne
                    [:foo])))
    (is (= {:eip 10 :internal-registers {:cmp :gt} :symbol-table {:foo 10}}
           (cmp-jmp {:eip 6 :internal-registers {:cmp :gt} :symbol-table {:foo 10}}
                    :jne
                    [:foo]))))
  
  (testing "cmp-jmp je"
    (is (= {:eip 6 :internal-registers {:cmp :lt} :symbol-table {:foo 10}}
           (cmp-jmp {:eip 5 :internal-registers {:cmp :lt} :symbol-table {:foo 10}}
                    :je
                    [:foo])))
    (is (= {:eip 10 :internal-registers {:cmp :eq} :symbol-table {:foo 10}}
           (cmp-jmp {:eip                5
                     :internal-registers {:cmp :eq}
                     :symbol-table       {:foo 10}}
                    :je
                    [:foo])))
    (is (= {:eip 6 :internal-registers {:cmp :gt} :symbol-table {:foo 10}}
           (cmp-jmp {:eip 5 :internal-registers {:cmp :gt} :symbol-table {:foo 10}}
                    :je
                    [:foo]))))
  
  (testing "cmp-jmp jle"
    (is (= {:eip 10 :internal-registers {:cmp :lt} :symbol-table {:foo 10}}
           (cmp-jmp {:eip 5 :internal-registers {:cmp :lt} :symbol-table {:foo 10}}
                    :jle
                    [:foo])))
    (is (= {:eip 10 :internal-registers {:cmp :eq} :symbol-table {:foo 10}}
           (cmp-jmp {:eip                5
                     :internal-registers {:cmp :eq}
                     :symbol-table       {:foo 10}}
                    :jle
                    [:foo])))
    (is (= {:eip 6 :internal-registers {:cmp :gt} :symbol-table {:foo 10}}
           (cmp-jmp {:eip 5 :internal-registers {:cmp :gt} :symbol-table {:foo 10}}
                    :jle
                    [:foo]))))
  
  (testing "cmp-jmp jl"
    (is (= {:eip 10 :internal-registers {:cmp :lt} :symbol-table {:foo 10}}
           (cmp-jmp {:eip 5 :internal-registers {:cmp :lt} :symbol-table {:foo 10}}
                    :jl
                    [:foo])))
    (is (= {:eip 6 :internal-registers {:cmp :eq} :symbol-table {:foo 10}}
           (cmp-jmp {:eip                5
                     :internal-registers {:cmp :eq}
                     :symbol-table       {:foo 10}}
                    :jl
                    [:foo])))
    (is (= {:eip 6 :internal-registers {:cmp :gt} :symbol-table {:foo 10}}
           (cmp-jmp {:eip 5 :internal-registers {:cmp :gt} :symbol-table {:foo 10}}
                    :jl
                    [:foo])))))

(run-tests)