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
                       :height "100%"}
               :value @(rf/subscribe [:source])}]
   [:button.btn.btn-primary {:on-click #(rf/dispatch [:parse])}
    "Parse"]])

(defn code []
  [:div
   (let [code @(rf/subscribe [:code])
         code-with-lines (zipmap (range (count code)) code)
         eip @(rf/subscribe [:eip])]
     [:table.code
      (for [[line-no code-line] code-with-lines]
        [:tr {:key line-no
              :style {:background-color (if (= eip line-no) "goldenrod" "white")}}
         [:td.eip
          [:i.fas.fa-angle-double-right {:style {:visibility (if (= eip line-no) :visible :hidden)}}]]
         [:td
          [:span
           [:label.instruction (first code-line)]
           (for [i (rest code-line)]
             (if (keyword? i)
               [:label.register i]
               [:label.value i]))]]])])])

;; -- App ---------------------------------------------------------------------------
(defn app []
  [:div.content
   [:h1 "CLJS ASM"]
   [:div.row
    [:div.col.col-lg-4
     [code-editor]]
    [:div.col.col-lg-4
     [code]]
    [:div.col.col-lg-4]]])

;; -- Dev Helpers -------------------------------------------------------------------
(comment (rf/dispatch-sync [:initialize]))

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