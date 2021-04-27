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
             :stack              []}
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