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
   (:eip db)))

(rf/reg-sub
 :breakpoints
 (fn [db _]
   (:breakpoints db)))