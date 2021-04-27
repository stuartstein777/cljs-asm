(ns exfn.events
  (:require [exfn.parser :refer [parse]]
            [re-frame.core :as rf]
            [clojure.set :as set]))

(rf/reg-event-db
 :initialize
 (fn [_ _]
   {:source      ""
    :code        []
    :memory {:eip                0
             :registers          {}
             :eip-stack          []
             :internal-registers {}
             :stack              []
             :symbol-table       []}
    :breakpoints #{}
    :running? false}))

;; Handles the user typing into the source control editor.
(rf/reg-event-db
 :update-source
 (fn [db [_ source]]
   (assoc db :source source)))

;; Handles when the user clicks the Parse button.
(rf/reg-event-db
 :parse
 (fn [{:keys [source] :as db} _]
   (let [parsed (parse source)]
     (assoc db :code parsed))))

;; ====================================================================
;; Source Code Editor events
;; ====================================================================

(rf/reg-fx
 :scroll-line-nos
 (fn [scroll-pos]
   (-> js/document
       (.getElementById "lineNos")
       (.-scrollTop)
       (set! scroll-pos))))

(rf/reg-event-fx
 :update-scroll
 (fn [{:keys [db]} [_ scroll-pos]]
   (js/console.log (str "scroll-pos: " scroll-pos))
   {:db (assoc db :scroll-pos scroll-pos)
    :scroll-line-nos scroll-pos}))

;; ===================================================================
;; Parsed code events
;; ===================================================================
 (rf/reg-event-db
  :toggle-breakpoint
  (fn [{:keys [breakpoints] :as db} [_ line-no]]
    (assoc db :breakpoints (if (some? (breakpoints line-no))
                             (set/difference  breakpoints #{line-no})
                             (conj breakpoints line-no)))))

;; ===================================================================
;; Code execution control events
;; ===================================================================
(rf/reg-event-db
 :toggle-running
 (fn [db _]
   (assoc db :running? (not (db :running?)))))
 


 ;;================== DEV TEST EVENTS ==================================
 (rf/reg-event-db
  :add-value-to-registers
  (fn [db [_ [k v]]]
    (update-in db [:memory :registers] assoc k v)))

(rf/reg-event-db
 :add-value-to-stack
 (fn [db [_ v]]
   (update-in db [:memory :stack] conj v)))

(rf/reg-event-db
 :test-code
 (fn [db _]
   (assoc db :source "; function calls.
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
ret        ; ret to bar call, pop eip stack")))