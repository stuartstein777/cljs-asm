(ns exfn.app
  (:require
   [reagent.dom :as dom]
   [exfn.subs]
   [exfn.events]
   [exfn.helpers :as h]
   [re-frame.core :as rf]))

;; DEV NOTES
;; npx shadow-cljs watch app
;; then open web page in browser
;; then connect to repl.

;; -- Reagent Components ------------------------------------------------------------
(defn code-editor []
  (let [source @(rf/subscribe [:source])]
    [:div
     [:div.editor
      [:textarea.text-editor-line-nos {:readOnly true
                                       :value (h/get-source-line-numbers source)}]
      [:textarea.text-editor {:on-change #(rf/dispatch-sync [:update-source (-> % .-target .-value)])
                              :value     @(rf/subscribe [:source])
                              :wrap :off}]]]))

(defn code []
  [:div.code-holder
   (let [code @(rf/subscribe [:code])
         breakpoints @(rf/subscribe [:breakpoints])
         code-with-lines (zipmap (range (count code)) code)
         eip @(rf/subscribe [:eip])]
     [:table.code
      [:tbody
       (for [[line-no code-line] code-with-lines]
         [:tr.code-line {:key line-no
                         :style {:background-color (if (= eip line-no) "goldenrod" "white")}}
          [:td.breakpoint
           [:i.fas.fa-circle {:style {:color (if (some? (breakpoints line-no)) "red" "lightgray")}
                              :on-click #(rf/dispatch [:toggle-breakpoint line-no])}]]
          [:td.eip
           [:i.fas.fa-angle-double-right
            {:style {:visibility (if (= eip line-no) :visible :hidden)}}]]
          [:td.line-number
           line-no]
          [:td
           [:span
            [:label.instruction (first code-line)]
            (let [arguments (rest code-line)]
              (for [i (zipmap (range (count arguments)) arguments)]
                (if (keyword? (val i))
                  [:label.register {:key (key i)} (val i)]
                  [:label.value {:key (key i)} (val i)])))]]])]])])

(let [instr [:mov :a 5]]
  (prn "count" (count (rest instr)))
  (for [i (zipmap (range (count (rest instr))) (rest instr))]
    (do
      (prn i)
      (if (keyword? i)
        [:label.register {:key (key i)} (val i)]
        [:label.value {:key (key i)} (val i)]))))

(defn execution-controls []
  )
;; -- App ---------------------------------------------------------------------------
(defn app []
  [:div.content
   [:div.row
    [:div.col.col-lg-4
     [code-editor]]
    [:div.col.col-lg-4
     [code]]
    [:div.col.col-lg-4]]
   [:div.row 
    [:button.btn.btn-primary.parse-btn {:on-click #(rf/dispatch [:parse])} "Parse"]]
   [:div.row
    [execution-controls]]])

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