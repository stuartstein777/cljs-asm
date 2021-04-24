(ns exfn.app
  (:require [reagent.dom :as dom]
            [exfn.subs]
            [exfn.events]
            [re-frame.core :as rf]))

;; DEV NOTES
;; npx shadow-cljs watch app
;; then open web page in browser
;; then connect to repl.

;; -- Reagent Components ------------------------------------------------------------
(defn code-editor []
  [:div
   [:textarea {:on-change #(rf/dispatch-sync [:update-source (-> % .-target .-value)])
               :style {:margin "10px"
                       :width  "100%"
                       :height "100%"}}]
   [:button.btn.btn-primary {:on-click #(rf/dispatch [:parse])}
    "Parse"]])

(defn code []
  [:div
   (for [line @(rf/subscribe [:code])]
     [:div.row
      (for [i line]
        [:div.col i])])])

;; -- App ---------------------------------------------------------------------------
(defn app []
  [:div.container
   [:h1 "CLJS ASM"]
   [:div.row
    [:div.col.col-lg-4
     [code-editor]]
    [:div.col.col-lg-4
     [code]]
    [:div.col.col-lg-4]]])

;; -- Dev Helpers -------------------------------------------------------------------
(comment (rf/dispatch :initialize))

;; -- After-Load --------------------------------------------------------------------
;; Do this after the page has loaded.
;; Initialize the initial db state.
(defn ^:dev/after-load start
  []
  (dom/render [app]
              (.getElementById js/document "app")))

(defn ^:export init []
  (start))

(defonce initialize (rf/dispatch-sync [:initialize]))       ; dispatch the event which will create the initial state. 

