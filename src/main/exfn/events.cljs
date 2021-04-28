(ns exfn.events
  (:require [exfn.parser :refer [parse]]
            [re-frame.core :as rf]
            [exfn.interpreter :as interp]
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
             :symbol-table       {}}
    :breakpoints #{}
    :running? false
    :finished? false}))

;; Handles the user typing into the source control editor.
(rf/reg-event-db
 :update-source
 (fn [db [_ source]]
   (assoc db :source source)))

;; Handles when the user clicks the Parse button.
(rf/reg-event-db
 :parse
 (fn [{:keys [source] :as db} _]
   (let [parsed (parse source)
         symbol-table (interp/build-symbol-table parsed)]
     (-> db 
          (assoc :memory {:eip                0
                          :registers          {}
                          :eip-stack          []
                          :internal-registers {}
                          :stack              []
                          :symbol-table symbol-table})
          (assoc :code parsed)))))

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
(rf/reg-fx
 :scroll-current-code-into-view
 (fn [eip]
   (-> js/document
       (.getElementById "code-container")
       (.-scrollTop)
       (set! (* eip 25)))))
 
(rf/reg-event-db
 :toggle-running
 (fn [db _]
   (assoc db :running? (not (db :running?)))))
 
 (rf/reg-event-db
  :reset
  (fn [db _]
    (-> db
        (assoc :memory {:eip                0
                        :registers          {}
                        :eip-stack          []
                        :internal-registers {}
                        :stack              []
                        :symbol-table (:symbol-table (:memory db))})
        (assoc :running? false))))
 
;; make this a reg-event-fx
;; var table = document.getElementById ('code-container')
;; table.scrollTop = eip * 25
;; then call :scroll-current-code-into-view
 (rf/reg-event-fx
 :next-instruction
 (fn [{:keys [db]} _]
   (prn "eip:" (:eip (db :memory)))
   (let [memory (exfn.interpreter/interpret (db :code) (db :memory))]
     {:db (assoc db :memory memory)
      :scroll-current-code-into-view (:eip memory)})))

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
 :reset-eip
 (fn [db _]
   (assoc-in db [:memory :eip] 0)))
 
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