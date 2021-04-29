(ns exfn.events
  (:require [exfn.parser :refer [parse]]
            [re-frame.core :as rf]
            [exfn.interpreter :as interp]
            [clojure.set :as set]))

(rf/reg-event-db
 :initialize
 (fn [_ _]
   {:source      "; function calls.
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
pop d
pop e
xor b b    ; a = 7, b = 0, c = 3
end        ; a = 7, b = 0, c = 3
                      

;; foo:: increment b
foo:
inc b      ; a = 0, b = 2, c = 2
ret        ; ret to foo call, pop eip stack


;; bar:: add 7 to a and decrement c
bar:
add a 7    ; a = 7, b = 2, c = 4
sub c 1    ; a = 7, b = 2, c = 3
push 3
push 4
ret        ; ret to bar call, pop eip stack"
    :code        []
    :memory {:eip                0
             :registers          {}
             :eip-stack          []
             :internal-registers {}
             :stack              []
             :symbol-table       {}
             :last-edit-register nil}
    :breakpoints #{}
    :on-breakpoint false
    :has-parsed-code? false
    :finished? false
    :running? false
    :running-speed 700
    :ticker-handle nil}))

(defn dispatch-timer-event []
  (rf/dispatch [:next-instruction]))

;; Handles the user typing into the source control editor.
(rf/reg-event-db
 :update-source
 (fn [db [_ source]]
   (assoc db :source source)))

(rf/reg-fx
 :scroll-parsed-code-to-top
 (fn [_]
   (-> js/document
       (.getElementById "code-container")
       (.-scrollTop)
       (set! 0))))

;; Handles when the user clicks the Parse button.
(rf/reg-event-fx
 :parse
 (fn [{:keys [db]} _]
   (let [parsed (parse (db :source))
         symbol-table (interp/build-symbol-table parsed)]
     {:db
      (-> db
          (assoc :memory {:eip                0
                          :registers          {}
                          :eip-stack          []
                          :internal-registers {}
                          :stack              []
                          :symbol-table       symbol-table})
          (assoc :code parsed)
          (assoc :on-breakpoint false)
          (assoc :has-parsed-code? (pos? (count parsed)))
          (assoc :finished? false))
      :scroll-parsed-code-to-top _})))

(rf/reg-event-db
 :clear-parsed
 (fn [db _]
   (-> db
       (assoc :code [])
       (assoc :running? false)
       (assoc :has-parsed-code? false)
       (assoc :on-breakpoint false)
       (assoc :memory {:eip                0
                       :registers          {}
                       :eip-stack          []
                       :internal-registers {}
                       :stack              []
                       :symbol-table       []}))))

;; ====================================================================
;; Source Code Editor events
;; ====================================================================

; replace with
;(reagent/with-let [ref2 (reagent/atom nil)]
;   [:div
;    [:textarea {:on-scroll (fn [^js e] (when-some [node2 @ref2] ...))}]
;    [:textarea {:ref #(reset! ref2 %)}]])

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
(rf/reg-event-db
 :set-handle
 (fn [db [_ handle]]
   (assoc db :ticker-handle handle)))
 
(rf/reg-fx
 :toggle-running
 (fn [[running? handle speed]]
   (if running?
     (rf/dispatch [:set-handle (js/setInterval dispatch-timer-event speed)])
     (js/clearInterval handle))))
 
(rf/reg-fx
 :scroll-current-code-into-view
 (fn [eip]
   (-> js/document
       (.getElementById "code-container")
       (.-scrollTop)
       (set! (* eip 25)))))
 
 (rf/reg-event-fx
 :toggle-running
 (fn [{:keys [db]} _]
   {:db (assoc db :running? (not (db :running?)))
    :toggle-running [(not (db :running?)) (db :ticker-handle) (db :running-speed)]}))
 
(rf/reg-fx
 :end-if-finished
 (fn [[handle finished?]]
   (when finished?
     (js/clearInterval handle))))

 (rf/reg-event-fx
  :reset
  (fn [{:keys [db]} _]
    {:db (-> db
             (assoc :memory {:eip                0
                             :registers          {}
                             :eip-stack          []
                             :internal-registers {}
                             :stack              []
                             :symbol-table (:symbol-table (:memory db))})
             (assoc :running? false)
             (assoc :on-breakpoint false)
             (assoc :finished? false))
     :toggle-running [false (db :ticker-handle)]
     :scroll-parsed-code-to-top _}))

(rf/reg-event-fx
 :next-instruction
 (fn [{:keys [db]} _]
   (let [[memory finished?] (exfn.interpreter/interpret (db :code) (db :memory))
         breakpoints (db :breakpoints)
         db (-> db
                (assoc :memory memory)
                (assoc :finished? finished?))]
     (if (some? (breakpoints (:eip memory)))
       {:db (-> db
                (assoc :on-breakpoint true)
                (assoc :running? false))
        :scroll-current-code-into-view (:eip memory)
        :toggle-running [false (db :ticker-handle)]
        }
       {:db (assoc db :on-breakpoint false)
        :scroll-current-code-into-view (:eip memory)
        :end-if-finished [(db :ticker-handle) finished?]}))))

 ;;================== DEV TEST EVENTS ==================================
 (rf/reg-event-db
  :add-value-to-registers
  (fn [db [_ [k v]]]
    (update-in db [:memory :registers] assoc k v)))

(rf/reg-event-db
 :update-running-speed
 (fn [db [_ speed]]
   (assoc db :running-speed speed)))

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