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

(rf/reg-sub
 :eip-stack
 (fn [db _]
   (-> db :memory :eip-stack)))

(rf/reg-sub
 :symbols
 (fn [db _]
   (-> db :memory :symbol-table)))

(rf/reg-sub
 :finished?
 (fn [db _]
   (db :finished?)))

(rf/reg-sub
 :has-parsed-code?
 (fn [db _]
   (db :has-parsed-code?)))

(rf/reg-sub
 :running-speed
 (fn [db _]
   (db :running-speed)))

(rf/reg-sub
 :on-breakpoint
 (fn [db _]
   (db :on-breakpoint)))

(rf/reg-sub
 :last-edit-register
 (fn [db _]
   (-> db :memory :last-edit-register)))

(rf/reg-sub
 :output
 (fn [db _]
   (db :output)))

(rf/reg-sub
 :rep-counters-stack
 (fn [db _]
   (-> db :memory :rep-counters-stack)))