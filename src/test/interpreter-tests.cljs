(ns test.interpreter-tests
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [exfn.interpreter :refer [interpret]]))

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
      (is (and (= (-> result :memory :eip ) 1)
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
               (= (-> result :memory :eip-stack) [0]))))))

(run-tests)