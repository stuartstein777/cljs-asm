(ns exfn.events
  (:require [exfn.parser :refer [parse]]
            [re-frame.core :as rf]
            [clojure.set :as set]))

(rf/reg-event-db
 :initialize
 (fn [_ _]
   {:source      ""
    :code        []
    :memory {:eip       0
             :registers {:internal-registers {}
                         :eip-stack          []}
             :stack     []}
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

(rf/reg-event-db
 :update-scroll
 (fn [db [_ scroll-pos]]
   (assoc db :scroll-pos scroll-pos)))

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