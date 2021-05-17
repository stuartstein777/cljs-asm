(ns exfn.events
  (:require [exfn.parser :refer [parse]]
            [re-frame.core :as rf]
            [day8.re-frame.tracing :refer-macros [fn-traced]]
            [exfn.interpreter :as interp]
            [clojure.set :as set]))

(defn reset-db [db]
  (-> db
      (assoc-in [:memory :eip] 0)
      (assoc-in [:memory :eip-stack] [])
      (assoc-in [:memory :internal-registers] {})
      (assoc-in [:memory :last-edit-register] nil)
      (assoc-in [:memory :output] "$ Toy Asm Output >")
      (assoc-in [:memory :registers] [])
      (assoc-in [:memory :rep-counters-stack] [])
      (assoc-in [:memory :stack] [])
      (assoc-in [:memory :symbol-table] [])
      (assoc-in [:memory :termination-message] "")
      (assoc :breakpoints #{})
      (assoc :code [])
      (assoc :finished? false)
      (assoc :has-parsed-code? false)
      (assoc :on-breakpoint false)
      (assoc :expanded-registers #{})
      (assoc :parse-errors? false)
      (assoc :parse-errors "")
      (assoc :running? false)))

(rf/reg-event-db
 :initialize
 (fn [_ _]
   {:source             ".macros
    %print-nth
       call setordinal
       mov :s :msg1
       cat :s %2
       cat :s :ord
       cat :s :msg2
       cat :s %1
       prn :s
    %end
    %next-fib
        push %1
        add %1 %2
        pop %2
    %end
     %restart
          inc %1
          jmp start
     %end
     %check123
          cmp %1 1
          je st
         cmp %1 2
         je nd
         cmp %1 3
         je rd
     %end
.code
start:
   print-nth(:b, :ctr)
   next-fib(:a, :b)
   restart(:ctr)
   end

setordinal:
   ; if ctr is 1 then return 'st', if 2 return 'nd',
   ; if 3 return 'rd'
   check123(:ctr)

   ; if we got here, we are not = 1, 2 or 3. so need to 
   ; check if we are greater than 20
   cmp :ctr 20
   jg gt20
   mov :ord `th `
   ret
   gt20:
      ; we are greater than 20 so divide by 10
      ; and check if remainder is 1, 2 or 3
      mov :n :ctr
      rem :n 10 ; integer division
      check123(:n)
      mov :ord `th `
      ret
      st:
         mov :ord `st `
         ret
      nd:
         mov :ord `nd `
         ret
      rd:
         mov :ord `rd `
         ret

.data
   a 1
   b 1
   ctr 1
   msg1 `The `
   msg2 ` fibonacci number is `"
    :breakpoints        #{}
    :code               []
    :finished?          false
    :has-parsed-code?   false
    :memory             {:eip                 0
                         :registers           {}
                         :eip-stack           []
                         :internal-registers  {}
                         :stack               []
                         :termination-message ""
                         :symbol-table        {}
                         :rep-counters-stack  []
                         :last-edit-register  nil
                         :output              "$ Toy Asm Output >"}
    :on-breakpoint      false
    :parse-errors?      false
    :parse-errors       ""
    :expanded-registers #{}
    :running?           false
    :running-speed      250
    :ticker-handle      nil}))

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

(defn fill-data-registers [data]
  (zipmap (map first data) (map second data)))

;; Handles when the user clicks the Parse button.
(rf/reg-event-fx
 :parse
 (fn [{:keys [db]} _]
   (let [{:keys [code data errors symbol-table]} (parse (db :source))]
     ; if we have errors, this trumps any other output that might be there, so we want to display the errors.
     ; but if errors is an empty string, we can just proceed as normal and assoc the code, symbol table and data entries
     (if (= "" errors)
       (let [data-registers (fill-data-registers data)]
         {:db (-> (reset-db db)
                  (assoc-in [:memory :output] (if (db :running?)
                                                (str (-> db :memory :output) "\nUser terminated.")
                                                (-> db :memory :output)))
                  (assoc-in [:memory :symbol-table] symbol-table)
                  (assoc-in [:memory :registers] data-registers)
                  (assoc-in [:memory :data-registers] data-registers)
                  (assoc :has-parsed-code? true)
                  (assoc :has-parsed-code? (:expanded-registers db))
                  (assoc :code code)
                  (assoc :parse-errors? false)
                  (assoc :parse-errors errors))
        ;:scroll-parsed-code-to-top _
          :end-running (db :ticker-handle)})
       {:db (-> (reset-db db)
                (assoc :parse-errors? true)
                (assoc :parse-errors errors))
        :end-running (db :ticker-handle)}))))

; Handles when the user clicks the Clear Parsed button
(rf/reg-event-fx
 :clear-parsed
 (fn [{:keys [db]} _]
   {:db (-> db
            (assoc :code [])
            (assoc :running? false)
            (assoc :has-parsed-code? false)
            (assoc :expanded-registers #{})
            (assoc :on-breakpoint false)
            (assoc :memory {:eip                0
                            :registers          {}
                            :eip-stack          []
                            :internal-registers {}
                            :output             (if (db :running?)
                                                   (str (-> db :memory :output) "\nUser terminated.")
                                                   (-> db :memory :output))
                            :stack              []
                            :symbol-table       []}))
    :end-running (db :ticker-handle)}))

; Handles when the user clicks Clear Breakpoints button
(rf/reg-event-db
 :clear-breakpoints
 (fn [db _]
   (assoc db :breakpoints #{})))

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

(rf/reg-event-db
 :clear-source
 (fn [db _]
   (assoc db :source "")))

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
       (set! (- (* eip 25) 50)))))

(rf/reg-fx
 :scroll-output-to-end
 (fn [_]
   (let [stdout (-> js/document (.getElementById "stdout"))
         scroll (-> stdout (.-scrollHeight))]
     (-> stdout
         (.-scrollTop)
         (set! scroll)))))

(rf/reg-event-fx
 :toggle-running
 (fn [{:keys [db]} _]
   {:db (assoc db :running? (not (db :running?)))
    :toggle-running [(not (db :running?)) (db :ticker-handle) (db :running-speed)]}))

(rf/reg-fx
 :end-running
 (fn [handle]
   (js/clearInterval handle)))

(rf/reg-fx
 :end-if-finished
 (fn [[handle finished?]]
   (when finished?
     (js/clearInterval handle))))

(rf/reg-event-db
 :add-expanded-reg
 (fn [db [_ reg]]
   (-> db
       (update :expanded-registers conj reg))))

(rf/reg-event-db
 :remove-expanded-reg
 (fn [db [_ reg]]
   (js/console.log reg)
   (-> db
       (update :expanded-registers clojure.set/difference #{reg}))))

#_(rf/reg-event-db
 :add-value-to-stack
 (fn [db [_ v]]
   (update-in db [:memory :stack] conj v)))

(rf/reg-event-fx
 :reset
 (fn [{:keys [db]} _]
   {:db (-> db
            (assoc :memory {:eip                0
                            :registers          (-> db :memory :data-registers)
                            :data-registers     (-> db :memory :data-registers)
                            :eip-stack          []
                            :internal-registers {}
                            :stack              []
                            :rep-counters-stack []
                            :symbol-table       (:symbol-table (:memory db))
                            :output             (if (db :running?)
                                                  (str (-> db :memory :output) "\nUser terminated.")
                                                  (-> db :memory :output))})
            (assoc :running? false)
            (assoc :on-breakpoint false)
            (assoc :finished? false))
    :toggle-running [false (db :ticker-handle)]
    :scroll-parsed-code-to-top _}))

(rf/reg-event-fx
 :next-instruction
 (fn [{:keys [db]} _]
   (let [{:keys [memory finished? terminated?]} (exfn.interpreter/interpret (db :code) (db :memory))
         breakpoints (db :breakpoints)
         db (-> db
                (assoc :memory memory)
                (assoc :finished? finished?)
                (assoc :running? (if finished? false (db :running?))))]
     (cond
       ;; We are on a breakpoint.
       (some? (breakpoints (:eip memory)))
       {:db (-> db
                (assoc :on-breakpoint true)
                (assoc :running? false))
        :scroll-current-code-into-view (:eip memory)
        :toggle-running [false (db :ticker-handle)]}

       ;; Program was terminated.
       terminated?
       {:db (-> db
                (assoc :terminated? terminated?)
                (assoc :on-breakpoint false)
                (assoc :finished? true)
                (assoc :running? false))
        :end-if-finished [(db :ticker-handle) (or finished? terminated?)]}

       ;; Program finished by hitting an :end instruction (since it wasn't terminated.)
       finished?
       {:db (-> db
                (assoc :terminated? false)
                (assoc :on-breakpoint false)
                (assoc :finished? true)
                (assoc :running? false))
        :end-if-finished [(db :ticker-handle) (or finished? terminated?)]}

       ;; Otherwise it's ok to continue.
       :else
       {:db                            (assoc db :on-breakpoint false)
        :scroll-current-code-into-view (:eip memory)
        :scroll-output-to-end nil
        :end-if-finished               [(db :ticker-handle) (or finished? terminated?)]}))))

(rf/reg-event-db
 :clear-output
 (fn [db _]
   (assoc-in db [:memory :output] "$ Toy Asm Output >")))

 ;;================== DEV TEST EVENTS ==================================

(rf/reg-event-db
 :toggle-parse-errors
 (fn [db _]
   (assoc db :parse-errors? (not (db :parse-errors?)))))

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
ret        ; ret to bar call, pop eip stack")))