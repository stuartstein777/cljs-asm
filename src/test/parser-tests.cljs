(ns test.parser-tests
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [exfn.parser :refer [parse to-keywords is-register? parse-msg scrub-comments]]))


(deftest to-keywords-tests
  (testing "[mov a 5] to [:mov :a 5]"
    (is (= [:mov :a 5] (to-keywords "mov a 5"))))
  (testing "[mov a b] to [:mov :a :b]"
    (is (= [:mov :a :b] (to-keywords "mov a b"))))
  (testing "[inc a] to [:inc :a]"
    (is (= [:inc :a] (to-keywords "inc a"))))
  (testing "[dec a] to [:dec :a]"
    (is (= [:dec :a] (to-keywords "dec a"))))
  (testing "[sub x y] to [:sub :x :y]"
    (is (= [:sub :x :y] (to-keywords "sub x y"))))
  (testing "[jne lbl] to [:jne :lbl]"
    (is (= [:jne :lbl] (to-keywords "jne lbl"))))
  (testing "[function:] to [:lbl function]"
    (is (= [:label :function] (to-keywords "function:"))))
  (testing "[ret] to [:ret]"
    (is (= [:ret] (to-keywords "ret"))))
  (testing "[end] to [:end]"
    (is (= [:end] (to-keywords "end")))))

(deftest is-register?-tests
  (testing "a should return true"
    (is (true? (is-register? "a"))))
  (testing "5 should return false"
    (is (false? (is-register? "5"))))
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
                   mov a 5
                   inc a
                   call function
                   end
                   function:
                   div a 2
                   ret"))))
  (testing "complex parsing 2 - scrubbing comments."
    (is (= [[:mov :a 5]
            [:inc :a]
            [:call :foo]
            [:msg "(5+1)/2 = ;" :a]
            [:end]
            [:label :foo]
            [:div :a 2]
            [:end]]
           (parse "; my first program
                   mov a 5
                   inc a     ; increment a
                   call foo
                   msg '(5+1)/2 = ;' a ; another comment.
                   end
                   foo:
                   div a 2
                   end")))))

(deftest parser-with-long-register-names
  (is (= [[:mov :abc 5]
          [:inc :abc]]
         (parse "mov abc 5
                 inc abc"))))

(deftest msg-parser-tests
  (testing "'(5+1)/2 = ' a ' is greater than ' b"
    (is (= [:msg
            "(5+1)/2 = "
            :a
            " is greater than "
            :b] (parse-msg "msg '(5+1)/2 = ' a ' is greater than ' b"))))
  (testing "a msg with a comment at end of line and ; in the quote."
    (is (= [:msg
            "(5+1)/2 = "
            :a
            " is greater than "
            :b] (parse-msg "msg '(5+1)/2 = ' a ' is greater than ' b ; a comment")))))

(deftest scrubbing-comments-tests
  (is (= "inc a" (scrub-comments "inc a   ; some comment")))
  (testing "we don't scrub comments from msg fields"
    (is (= "msg '(5+1)/2 = ' a ; another comment." (scrub-comments "msg '(5+1)/2 = ' a ; another comment.")))))


(deftest complex-parser
  (is (= (parse "; function calls.
                            mov a 0    ; a = 0
                            mov b 1    ; a = 0, b = 1
                            mov c 2    ; a = 0, b = 1, c = 2
                            call foo   ; move eip to foo, push eip to eip-stack
                            mul c b    ; a = 0, b = 2, c = 4
                            cmp a b    ; :cmp = lt
                            jne quax   ; jump
                            mul c 10   ;
                            ;; quax:: call bar and zero :b
                            quax:      ;
                            nop        ;
                            call bar   ; move eip to bar, push eip to eip-stack
                            xor b b    ; a = 7, b = 0, c = 3
                            msg 'a = ' a ', b = ' b ', c = ' c
                            end        ; a = 7, b = 0, c = 3
                            ;; foo:: increment b
                            foo:
                            inc b      ; a = 0, b = 2, c = 2
                            ret        ; ret to foo call, pop eip stack
                            ;; bar:: add 7 to a and decrement c
                            bar:
                            add a 7    ; a = 7, b = 2, c = 4
                            sub c 1    ; a = 7, b = 2, c = 3
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
          [:msg "a = " :a ", b = " :b ", c = " :c]
          [:end]
          [:label :foo]
          [:inc :b]
          [:ret]
          [:label :bar]
          [:add :a 7]
          [:sub :c 1]
          [:ret]])))

(run-tests)