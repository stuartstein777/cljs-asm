(ns test.parser-tests
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [exfn.parser :refer [format-arg
                                 format-arguments
                                 get-macros
                                 get-value
                                 is-register?
                                 parse
                                 parse-line-of-code
                                 scrub-comments]]))
;; is-register tests
;; a register starts with :
(deftest is-register-tests?
  (is (true? (is-register? ":x")))
  (is (false? (is-register? "5")))
  (is (false? (is-register? "foo")))
  (is (false? (is-register? "b_010101"))))

(deftest get-value-tests
  (testing "tests that if its a register, return as keyword, else return as value"
    (is (= "5" (get-value "5")))
    (is (= :foo (get-value ":foo")))
    (is (= "foo" (get-value "foo")))))

(deftest format-arg-tests
  (testing "format-arg handles strings"
    (is (= "foo bar quax" (format-arg "'foo bar quax'")))
    (is (= "foo bar quax" (format-arg "`foo bar quax`"))))
  (testing "testing formats numberes"
    (is (= 5 (format-arg "5")))
    (is (= 5.5 (format-arg "5.5"))))
  (testing "registers are formatted to keywords"
    (is (= :foo (format-arg ":foo"))))
  (testing "if none of the above, make it a keyword"
    (is (= :foo (format-arg "foo")))))

(deftest format-arguments-tests
  (testing "2 argument instructions"
    (is (= [:mov :a 5] (format-arguments ["mov" ":a" "5"])))
    (is (= [:mov :a :b] (format-arguments ["mov" ":a" ":b"])))
    (is (= [:ret] (format-arguments ["ret"])))
    (is (= [:call :foo] (format-arguments ["call" "foo"])))
    (is (= [:inc :a] (format-arguments ["inc" ":a"])))
    (is (= [:rep 5] (format-arguments ["rep" "5"])))))

(deftest parse-line-of-code-tests
  (testing "[mov :a 5] to [:mov :a 5]"
    (is (= [:mov :a 5] (parse-line-of-code "mov :a 5"))))
  (testing "[mov :a b] to [:mov :a :b]"
    (is (= [:mov :a :b] (parse-line-of-code "mov :a b"))))
  (testing "[inc :a] to [:inc :a]"
    (is (= [:inc :a] (parse-line-of-code "inc :a"))))
  (testing "[dec :a] to [:dec :a]"
    (is (= [:dec :a] (parse-line-of-code "dec :a"))))
  (testing "[sub :x :y] to [:sub :x :y]"
    (is (= [:sub :x :y] (parse-line-of-code "sub :x :y"))))
  (testing "[jne lbl] to [:jne :lbl]"
    (is (= [:jne :lbl] (parse-line-of-code "jne lbl"))))
  (testing "[function:] to [:lbl function]"
    (is (= [:label :function] (parse-line-of-code "function:"))))
  (testing "[ret] to [:ret]"
    (is (= [:ret] (parse-line-of-code "ret"))))
  (testing "[end] to [:end]"
    (is (= [:end] (parse-line-of-code "end")))))

(deftest is-register?-tests
  (testing "a should return true"
    (is (true? (is-register? ":a"))))
  (testing "5 should return false"
    (is (false? (is-register? "5"))))
  (testing "b should return false"
    (is (false? (is-register? "b"))))
  (testing "-1 should return false"
    (is (false? (is-register? "-1")))))

(deftest parsing-tests
  (testing "parsing function calls"
    (is (= [[:mov :a 5]
            [:inc :a]
            [:call :function]
            [:end]
            [:label :function]
            [:div :a 2]
            [:ret]]
           (parse "; my first program
                   mov :a 5
                   inc :a
                   call function
                   end
                   function:
                   div :a 2
                   ret")))))

(deftest parser-with-long-register-names
  (is (= [[:mov :abc 5]
          [:inc :abc]]
         (parse "mov :abc 5
                 inc :abc"))))

(deftest scrubbing-comments-tests
  (is (= "inc a" (scrub-comments "inc a   ; some comment")))
  (is (= "" (scrub-comments "; full line comment")))
  (testing "we don't scrub comments from msg fields"
    (is (= "msg '(5+1)/2 = ' a ; another comment." (scrub-comments "msg '(5+1)/2 = ' a ; another comment.")))))

(deftest get-macro-tests
  (let [prepared-source (list ".macros"
                              "%square-and-sum"
                              "mul %1 %1"
                              "mul %2 %2"
                              "add %1 %2"
                              "%end"
                              "%add-ten"
                              "add %1 10"
                              "%end"
                              ".code"
                              "mov :a 2"
                              "mov :b 5"
                              "square-and-sum(:a, :b)"
                              "add-ten (:a)")
        prepared-source-no-macros (list ".code" "mov :a 2" "mov :b 5")]
    (is (= {"square-and-sum" ["mul %1 %1" "mul %2 %2" "add %1 %2"],
            "add-ten" ["add %1 10"]}
           (get-macros prepared-source)))
    (is (= {} (get-macros prepared-source-no-macros)))))

(deftest complex-parser
  (is (= (parse "; function calls.
                 mov :a 0    ; a = 0
                 mov :b 1    ; a = 0, b = 1
                 mov :c 2    ; a = 0, b = 1, c = 2
                 call foo   ; move eip to foo, push eip to eip-stack
                 mul :c :b    ; a = 0, b = 2, c = 4
                 cmp :a :b    ; :cmp = lt
                 jne quax   ; jump
                 mul :c 10   ;
                 ;; quax:: call bar and zero :b
                 quax:      ;
                 nop        ;
                 call bar   ; move eip to bar, push eip to eip-stack
                 xor :b :b    ; a = 7, b = 0, c = 3
                 end        ; a = 7, b = 0, c = 3
                 ;; foo:: increment b
                 foo:
                 inc :b      ; a = 0, b = 2, c = 2
                 ret        ; ret to foo call, pop eip stack
                 ;; bar:: add 7 to a and decrement c
                 bar:
                 add :a 7    ; a = 7, b = 2, c = 4
                 sub :c 1    ; a = 7, b = 2, c = 3
                 ret        ; ret to bar call, pop eip stack")
         [[:mov :a 0]
          [:mov :b 1]
          [:mov :c 2]
          [:call :foo]
          [:mul :c :b]
          [:cmp :a :b]
          [:jne :quax]
          [:mul :c 10]
          [:label :quax]
          [:nop]
          [:call :bar]
          [:xor :b :b]
          [:end]
          [:label :foo]
          [:inc :b]
          [:ret]
          [:label :bar]
          [:add :a 7]
          [:sub :c 1]
          [:ret]])))

(deftest macro-expansion-tests
  (testing "macro-expansion"
    (let [source ".macros
                  %initialize
                     mov %1 0
                     mov %2 0
                  %end
                  %square-and-sum
                     mul %1 %1
                     mul %2 %2
                     add %1 %2
                  %end
                  %add-ten
                     add %1 10
                  %end
                  .code
                    initialize(:a, :b)
                    mov :a 2
                    mov :b 5
                    mov :c 4
                    square-and-sum(:a, :b)
                    square-and-sum(:a, :c)
                    call foo
                    mov :s 'a = '
                    cat :s :a
                    prn :s
                    end


                    foo:
                       call bar
                       ret

                    bar:
                       add-ten (:a)
                       ret"]
       (is (= '([:mov :a 0]
                [:mov :b 0]
                [:mov :a 2]
                [:mov :b 5]
                [:mov :c 4]
                [:mul :a :a]
                [:mul :b :b]
                [:add :a :b]
                [:mul :a :a]
                [:mul :c :c]
                [:add :a :c]
                [:call :foo]
                [:mov :s "a = "]
                [:cat :s :a]
                [:prn :s]
                [:end]
                [:label :foo]
                [:call :bar]
                [:ret]
                [:label :bar]
                [:add :a 10]
                [:ret])
              (parse source))))))

(run-tests)