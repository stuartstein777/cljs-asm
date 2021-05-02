(ns test.interpreter-tests
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [exfn.interpreter :refer [build-symbol-table interpret]]))

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


(deftest interpreter-tests
  (testing "Simple mov instruction is interpreted correctly."
    (is (= {:registers {:a 5}}
           (int)))))

(run-tests)