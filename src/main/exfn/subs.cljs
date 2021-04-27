(ns exfn.subs
  (:require [re-frame.core :as rf]))

(rf/reg-sub
 :code
 (fn [db _]
   (:code db)))

(rf/reg-sub
 :source
 (fn [db _]
   (:source db)))

(rf/reg-sub
 :eip
 (fn [db _]
   (:eip (:memory db))))

(rf/reg-sub
 :breakpoints
 (fn [db _]
   (:breakpoints db)))

(rf/reg-sub
 :running?
 (fn [db _]
   (:running? db)))

(rf/reg-sub
 :scroll-pos
 (fn [db _]
   (:scroll-pos db)))

(rf/reg-sub
 :registers
 (fn [db _]
   (-> db :memory :registers)))

(rf/reg-sub
 :internal-registers
 (fn [db _]
   (-> db :memory :internal-registers)))

(rf/reg-sub
 :stack
 (fn [db _]
   (-> db :memory :stack)))