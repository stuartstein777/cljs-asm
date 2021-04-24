(ns exfn.subs
  (:require [re-frame.core :as rf]))

(rf/reg-sub
 :code
 (fn [db _]
   (:code db)))