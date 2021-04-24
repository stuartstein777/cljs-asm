(ns exfn.events
  (:require [exfn.parser :refer [parse]]
            [re-frame.core :as rf]))

(rf/reg-event-db
 :initialize
 (fn [_ _]
   {:source ""
    :code []
    :eip 0
    :registers {:internal-registers {} :eip-stack []}
    :stack []}))

(rf/reg-event-db
 :update-source
 (fn [db [_ source]]
   (assoc db :source source)))

(rf/reg-event-db
 :parse
 (fn [{:keys [source] :as db} _]
   (let [parsed (parse source)]
     (assoc db :code parsed))))